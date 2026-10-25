#!/usr/bin/env bash
#
# publish-all.sh — build, commit and push the site and all its satellite repos
# in one go. Written 2026-10-25; combines publish-danish.sh and
# publish-relativity.sh (both still work on their own) and adds ~/talks and
# ~/nordisk.
#
# The repos, and the order they are published in:
#   1. ~/danish-texts      PDFs + catalog.yaml   (GitHub Pages: /danish-texts/)
#   2. ~/relativity-texts  PDFs + catalog.yaml   (GitHub Pages: /relativity-texts/)
#   3. ~/talks             slides + sources      (GitHub Pages: /talks/)
#   4. ~/nordisk           the slide tool        (github.com/hhalvors/nordisk)
#   5. ~/hhalvors.github.io  this Hakyll site    (docs/ is the published site)
# The satellites go first, so that every link the new site makes already
# resolves when the site goes live.
#
# Usage:
#   ./publish-all.sh                  # build everything; show what would be
#                                     # published; commit nothing (a dry run)
#   ./publish-all.sh "message"        # build, then commit + push every repo
#                                     # that has changes (same message for all)
#   ./publish-all.sh -n ["message"]   # same, but skip the builds (when you
#                                     # have just built by hand)
#
# What gets committed in each repo:
#   danish-texts      catalog.yaml texts/ essays/ dossier.py Makefile only
#                     (its playbooks and CLAUDE.md stay untracked on purpose)
#   relativity-texts  everything (git add -A); .gitignore keeps out the rest
#   talks             everything (git add -A); .gitignore keeps out images,
#                     PowerPoint/Keynote, LaTeX clutter, *.with-notes
#   nordisk           everything (git add -A); nothing to build
#   site              docs/ plus changes to files git already tracks
#                     (site.hs, data/talks-master.yaml, ...). NEW files outside
#                     docs/ are never swept in: they are listed at the end, and
#                     you `git add` the ones you want.
# A repo with commits that were never pushed gets pushed too.
# Any failing build or push stops the script before anything later happens.
#
set -euo pipefail

SITE="$HOME/hhalvors.github.io"
DANISH="$HOME/danish-texts"
REL="$HOME/relativity-texts"
TALKS="$HOME/talks"
NORDISK="$HOME/nordisk"
[ -d "$NORDISK" ] || NORDISK="$HOME/.config/nordisk"   # its old home

BUILD=1
if [ "${1:-}" = "-n" ] || [ "${1:-}" = "--no-build" ]; then BUILD=0; shift; fi
MSG="${1:-}"

say() { printf '\n==> %s\n' "$*"; }

# ---------------------------------------------------------------- build
if [ "$BUILD" = 1 ]; then
  say "[build 1/3] danish-texts (make, make private)"
  make -C "$DANISH"
  make -C "$DANISH" private

  say "[build 2/3] relativity-texts (make mirror, make, make check)"
  make -C "$REL" mirror
  make -C "$REL"
  make -C "$REL" check

  say "[build 3/3] Hakyll site (stack build, site build)"
  cd "$SITE"
  stack build
  stack exec -- site build
  SITE_PRIVATE=1 stack exec -- site build   # private pages -> _private-site/, never committed
  # `site build` can miss a change to a generator or to a catalog reached
  # through a symlink; force a clean pass when a page is older than its inputs.
  stale=0
  for pair in "RelativityTexts.hs:docs/relativity/index.html" \
              "$REL/catalog.yaml:docs/relativity/index.html" \
              "DanishTexts.hs:docs/dansk/index.html" \
              "$DANISH/catalog.yaml:docs/dansk/index.html" \
              "Talks.hs:docs/talks.html" "TalksMaster.hs:docs/talks.html" \
              "data/talks-master.yaml:docs/talks.html"; do
    src="${pair%%:*}"; page="${pair#*:}"
    if [ -e "$src" ] && { [ ! -f "$page" ] || [ "$src" -nt "$page" ]; }; then stale=1; fi
  done
  if [ "$stale" = 1 ]; then
    say "a generated page is older than its inputs -> site rebuild"
    stack exec -- site rebuild
  fi
else
  say "skipping builds (-n)"
fi

# ---------------------------------------------------------------- status only
show() {  # name dir
  local n; n=$(git -C "$2" status --porcelain | wc -l | tr -d ' ')
  local ahead; ahead=$(git -C "$2" rev-list --count '@{u}..HEAD' 2>/dev/null || echo 0)
  printf '%-18s %4s changed file(s), %s unpushed commit(s)\n' "$1" "$n" "$ahead"
}
if [ -z "$MSG" ]; then
  say "Dry run: nothing committed. What would be published:"
  show danish-texts "$DANISH"; show relativity-texts "$REL"
  show talks "$TALKS";         show nordisk "$NORDISK"
  show site "$SITE"
  echo
  echo "Details: git -C ~/<repo> status --short"
  echo "To publish:  ./publish-all.sh \"your message\""
  exit 0
fi

# ---------------------------------------------------------------- publish
publish() {  # name dir  [paths to stage...]   (no paths = git add -A)
  local name="$1" dir="$2"; shift 2
  say "[publish] $name"
  if [ "$#" -gt 0 ]; then git -C "$dir" add -- "$@"; else git -C "$dir" add -A; fi
  if git -C "$dir" diff --cached --quiet; then
    echo "    nothing new to commit"
  else
    git -C "$dir" diff --cached --stat | tail -1
    git -C "$dir" commit -q -m "$MSG"
  fi
  if [ "$(git -C "$dir" rev-list --count '@{u}..HEAD' 2>/dev/null || echo 1)" != 0 ]; then
    git -C "$dir" push
  else
    echo "    up to date with GitHub"
  fi
}

publish danish-texts     "$DANISH" catalog.yaml texts/ essays/ dossier.py Makefile
publish relativity-texts "$REL"
publish talks            "$TALKS"
publish nordisk          "$NORDISK"
git -C "$SITE" add -u            # tracked source files: site.hs, data/*.yaml, ...
publish site             "$SITE" docs/

left=$(git -C "$SITE" status --porcelain | { grep -v '^??' || true; } | wc -l | tr -d ' ')
new=$(git -C "$SITE" ls-files --others --exclude-standard -- ':!docs' | { grep -v '\.bak-' || true; } | wc -l | tr -d ' ')
say "Done.  https://hanshalvorson.com/"
if [ "$new" != 0 ]; then
  echo "    Note: $new untracked file(s) in the site repo were NOT committed"
  echo "    (backups *.bak-* not counted). See: git -C ~/hhalvors.github.io status --short"
fi
[ "$left" = 0 ] || echo "    Warning: $left staged/modified file(s) left in the site repo."
