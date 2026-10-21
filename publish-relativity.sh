#!/usr/bin/env bash
#
# publish-relativity.sh — one-shot publish for the Relativity Texts collection.
# Modeled on publish-danish.sh (2026-10-20).
#
# A change to the relativity texts touches two repos:
#   * ~/relativity-texts     holds the LaTeX sources, the compiled PDFs, and
#                            catalog.yaml. Its PDFs are served directly as a
#                            GitHub project page: hhalvors.github.io/relativity-texts/...
#   * ~/hhalvors.github.io   holds this Hakyll site. The rule for
#                            relativity/index.html depends on
#                            data/relativity-texts.yaml (a symlink to
#                            ~/relativity-texts/catalog.yaml), so `site build`
#                            re-runs RelativityTexts.hs whenever the catalog changes.
#
# What this script does, in order:
#   1. make mirror      -> copy the editions listed in MIRROR.tsv from ~/danish-texts
#      make, make check -> rebuild out-of-date PDFs in ~/relativity-texts;
#                          validate catalog.yaml and the scans' hashes
#   2. site build       -> regenerate the Hakyll site (incl. /relativity/)
#   3. push relativity-texts -> publishes catalog.yaml, the sources and PDFs
#   4. push the site         -> publishes docs/ (the /relativity/ page)
#
# Usage:
#   ./publish-relativity.sh "commit message"   # build, commit, and push BOTH repos
#   ./publish-relativity.sh                     # build only; show status + next steps
#
set -euo pipefail

REL="$HOME/relativity-texts"
SITE="$HOME/hhalvors.github.io"
MSG="${1:-}"

echo "==> [1/4] Mirroring from danish-texts, building PDFs in relativity-texts (make mirror, make, make check)..."
make -C "$REL" mirror
make -C "$REL"
make -C "$REL" check

echo "==> [2/4] Rebuilding Hakyll site (site build)..."
cd "$SITE"
stack build
stack exec -- site build
# `site build` reruns a rule only when its inputs change, and it does not
# notice a change to the Haskell that renders the page, nor (on 2026-10-20,
# through the symlink) to catalog.yaml. So if the generator or the catalog is
# newer than the page it should have produced, force a clean pass.
PAGE="$SITE/docs/relativity/index.html"
if [ ! -f "$PAGE" ] || [ "$SITE/RelativityTexts.hs" -nt "$PAGE" ] || [ "$REL/catalog.yaml" -nt "$PAGE" ]; then
  echo "    /relativity/ is stale -> site rebuild"
  stack exec -- site rebuild
fi

if [ -z "$MSG" ]; then
  echo
  echo "Build complete. No commit message given -> nothing committed."
  echo "--- relativity-texts status ---------------------------------"
  git -C "$REL" status --short
  echo "--- hhalvors.github.io status -------------------------------"
  git -C "$SITE" status --short docs/
  echo
  echo "To publish, re-run with a message:"
  echo "    ./publish-relativity.sh \"your message\""
  exit 0
fi

echo "==> [3/4] Committing + pushing relativity-texts..."
# Unlike danish-texts, this repo tracks its playbooks and tools as well as the
# texts, so everything is staged; .gitignore keeps out RESUME-NOTES.md, .parts/,
# scans and LaTeX build files.
git -C "$REL" add -A
if git -C "$REL" diff --cached --quiet; then
  echo "    (relativity-texts: nothing to commit)"
else
  git -C "$REL" commit -m "$MSG"
  git -C "$REL" push
fi

echo "==> [4/4] Committing + pushing site..."
git -C "$SITE" add docs/
if git -C "$SITE" diff --cached --quiet; then
  echo "    (site: nothing to commit)"
else
  git -C "$SITE" commit -m "$MSG"
  git -C "$SITE" push
fi

echo "==> Done.  https://hanshalvorson.com/relativity/"
