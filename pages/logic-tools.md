---
title: "Logic Tools"
author: Hans Halvorson
---

Free software for learning symbolic logic, written to accompany *[How Logic
Works](/books/)* but usable with any course that teaches natural deduction in
the Lemmon style. Nothing to install, no account, no charge.

**[Open the tools →](https://lemmon-checker.onrender.com)**

---

## Check a proof you wrote by hand

Photograph a completed proof with your phone and the page is transcribed and
checked. You see the transcription beside the photograph and correct it before
anything is judged, so a misreading costs you an edit rather than a wrong
answer.

<div class="icon-links">
  <div><span class="icon"><i class="fa-solid fa-camera"></i></span> <a href="https://lemmon-checker.onrender.com/photo">Photograph a proof</a></div>
  <div><span class="icon"><i class="fa-solid fa-file-pdf"></i></span> <a href="https://lemmon-checker.onrender.com/template">Printable proof template</a> — proofs written on it are read more reliably</div>
</div>

## Check a proof you type

Enter a derivation line by line and every line is checked against the rules:
the dependencies, the citations, and the rule itself. When a line fails you are
told which requirement it missed, not merely that something is wrong.

<div class="icon-links">
  <div><span class="icon"><i class="fa-solid fa-list-check"></i></span> <a href="https://lemmon-checker.onrender.com/proof">Proof checker</a></div>
</div>

## Sentences, truth tables and models

<div class="icon-links">
  <div><span class="icon"><i class="fa-solid fa-table"></i></span> <a href="https://lemmon-checker.onrender.com/prop">Truth tables</a> — every valuation of a propositional sentence</div>
  <div><span class="icon"><i class="fa-solid fa-code-branch"></i></span> <a href="https://lemmon-checker.onrender.com/prop/dnf">Disjunctive normal form</a></div>
  <div><span class="icon"><i class="fa-solid fa-diagram-project"></i></span> <a href="https://lemmon-checker.onrender.com/model">Model checker</a> — build a finite structure and evaluate a sentence in it</div>
  <div><span class="icon"><i class="fa-solid fa-pen-ruler"></i></span> <a href="https://lemmon-checker.onrender.com/graph">Graphical model builder</a> — draw a structure and read off what holds</div>
</div>

---

## For instructors

The proof checker implements the twenty-one rules of *How Logic Works* exactly
as the book states them, including the dependency bookkeeping that most
software omits. It is deliberately strict about citation order and about
blank dependency cells, because those are where students' mistakes actually
live.

Students need no account and submit nothing to me; the tools run in the browser
against a small server and keep no record of what is checked. You are welcome
to link to any of these pages from a syllabus.

The whole suite is open source and written in Haskell. If your course uses a
different rule set, the checker is small enough to adapt.

There is also a translator between Lemmon-style and Fitch-style proofs, and a
short paper on the theory behind it. Translation in one direction turns out to
be entirely routine and in the other not, and the reason is worth knowing: a
Lemmon line records exactly what it depends on, while a Fitch line's
dependencies are merely bounded by the subproofs around it. If you teach one
notation and your colleagues teach the other, the paper says precisely what
survives the crossing.

<div class="icon-links">
  <div><span class="icon"><i class="fa-solid fa-file-lines"></i></span> <a href="https://lemmon-checker.onrender.com/paper">Dependency and Scope: on translating between Lemmon and Fitch proofs</a> — PDF, draft</div>
  <div><span class="icon"><i class="fa-brands fa-github"></i></span> <a href="https://github.com/hhalvors/lemmon-checker">Source on GitHub</a></div>
  <div><span class="icon"><i class="fa-solid fa-book"></i></span> <a href="https://lemmon-checker.onrender.com/instructions">How to write formulas, and the available rules</a></div>
  <div><span class="icon"><i class="fa-solid fa-graduation-cap"></i></span> <a href="/books/">How Logic Works</a> — solutions, key terms and errata</div>
</div>
