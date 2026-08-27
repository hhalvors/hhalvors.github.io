# hanshalvorson.com

Source for my website. It is a [Hakyll](https://jaspervdj.be/hakyll/) site:
`site.hs` is a Haskell program that reads the Markdown, LaTeX, BibTeX and YAML
in this repository and writes a static site into `docs/`, which is what GitHub
Pages serves.

```
stack build && stack exec site rebuild
```

## Layout

| | |
|---|---|
| `site.hs` | the build: every page's rule lives here |
| `docs/` | **generated output** — don't edit; it is overwritten on each build |
| `hh.bib` | publications, rendered into both `/publications.html` and the CV |
| `cv.tex` | the CV, built to `/cv.pdf` as part of the site build |
| `data/talks-master.yaml` | every talk, rendered into `/talks.html` and the CV |
| `templates/`, `css/` | page templates and styles |
| `src/`, `*.hs` | Haskell modules: BibTeX parsing, page generators, Pandoc filters |
| `bohr/`, `kierkegaard/`, `spacetime/`, `logic/` | project sections |
| `courses/` | course pages, lecture notes, problem sets |

## A note on the design

Two ideas do most of the work.

**One source, two renderings.** Where the CV and the website state the same
fact, both are generated from one file. `hh.bib` drives the publication list on
the site and the bibliography in the CV; `data/talks-master.yaml` drives the
talks page and the CV's talk sections. Neither can drift from the other,
because there is nothing to keep in sync.

**Prefer content that does not go stale.** Pages that make present-tense claims
about work in progress become misinformation the moment they are neglected, so
this site avoids them. Pages that state something durable — an argument, a
bibliography, a set of course materials — can sit untouched for years and
remain correct.

Some of the Haskell is idiosyncratic and some of it is older than the rest.
`src/filters/PubList.hs` is a reasonable place to start reading.
