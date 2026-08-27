---
title: "Logic code"
author: Hans Halvorson
---

Computers can help a lot with logical thinking, but what's easy for us
to read or write is not necessarily natural for a computer to read or
write. For example, while I would prefer to write "$P\wedge (Q\to
R)$", my computer would rather read something like `And P (Implies Q
R)`.

So, if we want to use computers to do logic, then we need to translate
between our representational system and theirs. The code here aims to
convert back and forth between *plain text* representations of
formulas (with unicode characters for logical symbols) and data
structures. It would be nice to be able to convert directly from
*handwriting*, but that is beyond the scope of the current project.

This website is built with [Hakyll](https://jaspervdj.be/hakyll/),
which is built on [Pandoc](https://pandoc.org), and both are written
in [Haskell](https://www.haskell.org). Pandoc parses a document in
some standard format (e.g. markdown, docx, latex) into its own
internal format, viz. an object $X$ of type `Pandoc`, and it can then
write $X$ out again in any one of those standard formats. One also has
the option of writing "filters", i.e.\ functions that act on the
intermediate `Pandoc` object before writing it out. What's interesting
about this setup --- from the point of view of logic --- is that we
can embed data structures into the `Pandoc` object, and perform
computations on them before creating the output.

## Reading and writing proofs 

The script `lemmonFilter.hs` takes code blocks labelled `lemmon` and
creates a proof structure out of them, i.e. a list of proof lines,
where each proof line is a triple consisting of reference numbers,
formula, and justification. The input is expected to look like a
manually typed proof in Lemmon[^1] format, wrapped in triple tick
marks:

````
```lemmon
1    (1) ¬P                       A
2    (2) Q                        A
1,2  (3) ¬P∧Q                     1,2 ∧I
4    (4) (¬P∧(Q∨R))→¬(S∧(¬T∨U))   A
```
````

(It's relatively pain-free to write such proofs by hand. It's less
pleasant to deal with adding whitespace or setting tab stops to create
a nice-looking text document.) The parser expects whitespaces between
the three columns, that the first column has a comma separated list of
integers, and that the second column has an integer in parentheses. If
the formula after the line number is well-formed, the parser
transforms it into an object of type `Formula`.

There are many sorts of interesting operations one could do on
`Formula` and `Proof` objects. For example, we could ask what the main
connectives are of the formulas on the lines. Or we could ask if the
proof is (syntactically) valid. We could also ask if a line is
semantically valid, i.e.\ if the truth of the formulas on the
reference lines entails the truth of the formula on the line.

## Writing truth tables

Given a `Formula`, we can construct all of the relevant valuations
(i.e. rows in a truth table), and we can ask whether the formula is a
tautology, inconsistency, or contingency.  We can also write a script
to return a printed truth table of the formula. That's how I produced
the truth tables in [solns5](solns5.html).

## To do

- Extend parsing to quantified formulas
- Patch for emacs markdown mode that makes it easy to input Lemmon
  proofs
- Proof checker that operates on objects of type `Proof`
- Rendering to docx
- Define a Haskell data type for Fitch style proofs
- Transform between Lemmon and Fitch-style proofs
- Collaborate with these guys [carnap.io](https://carnap.io)

[^1]: This kind of proof format appears in E.J. Lemmon *Beginning
    Logic*, published 1965. A similar format appeared simultaneously
    in works by Patrick Suppes. This format has a much steeper
    learning curve than proof trees, and even than Fitch
    proofs. However, Lemmon-style systems have marked theoretical
    advantages over Fitch systems.

