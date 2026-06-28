# Talks / CV pipeline — ways to improve

Running list of deferred improvements to the talks→CV/website pipeline
(`data/talks-master.yaml`, `TalksMaster.hs`, `Talks.hs`, `cv.tex`). Captured
2026-06-28. Not urgent; everything currently builds and looks good.

## Content / data (`data/talks-master.yaml`)
- **Title-less rows (5):** roundtable (World Congress), comments (Button & Walsh),
  author-meets-critics (Central APA), and two lecture-series rows render as
  event+location only — the descriptive framing is lost in the CV. Give each a
  short `title` (or `note`) if you want it to show.
- **Announced vs delivered title:** the renderer prints `announced_title` where it
  differs from the delivered `title` (matches the old CV). Decide if you'd rather
  show the delivered title.
- **`cv_raw`:** now unused for invited talks (still used to render the Outreach
  list). Either drop it from the talk rows or keep it as a reference/fallback.
- **Proper-name events containing a place (6):** "Boston Colloquium for Philosophy
  of Science", "Rutgers Probability Seminar", "Rutgers University Philosophy
  Colloquium", "Nagoya Winter Workshop", "Cambridge-Copenhagen Symposium". Left
  intact (place is part of the proper name). Revisit if you want them split.
- **A few `event` values are really subtitles** (e.g. `climacus`: "The
  epistemological significance of Kierkegaard's Climacus works"). Consider folding
  into the title or a note.
- **`brandes`:** real talk, was missing from `cv.tex`; date is approximate
  (May 2025) — confirm exact date. Now generated into the CV automatically.
- **Header comment** in `talks-master.yaml` still says "DRAFT / NOT yet wired into
  the site build" — update it, since it now drives both the slides page and the CV.

## Tooling (from `docs/talks-pipeline-plan.md`, not yet built)
- `talks check` subcommand (validate ids, links on disk, deck-header drift).
- `talks stamp` + the `%%talk … %%end` canonical headers inside each deck.
- Diff generated CV talk section against the historical hand-written CV once, to
  confirm nothing was lost in the full migration (pre-2018 entries especially).
- Outreach list still renders from `cv_raw`; could be enriched + field-rendered
  like the invited talks for full uniformity.

## Notes
- Website `talks.html` (via `Talks.hs`) shows dates; the CV deliberately does not
  (year is in the margin). Intentional divergence.
- CV talk lines: built from `title`/`announced_title` + `(with …)` coauthors +
  `event` + `location`; no dates; reverse-sorted by year then month; `\yearlabel`
  puts the year in the left margin once per year.
