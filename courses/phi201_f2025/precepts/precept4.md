---
title: "logic precept 4"
author: Hans Halvorson
---

# Key Concepts

- arguments: valid, invalid

- counterexample

- truth-value

- main connective

- sentences (syntactic): atomic, conjunction, negation, disjunction,
  conditional, biconditional

- sentences (semantic): tautology, inconsistency, contingency

- two sentences: equivalent, inconsistent, independent

# Sentence classification (syntactic)

**Exercise.** What is the **main connective** of each of the following
formulas?

1. $\neg (P\to Q)$

2. $\neg P\to Q$

3. $\neg (P\to \neg Q)$

4. $(P\wedge Q)\vee \neg (P\to Q)$

5. $((P\to Q)\to P)\to P$


# Sentence classification (semantic) 

**Exercise.** Classify each of the following sentences as tautology,
inconsistency, or contingency.

1. $(P\to \neg P)\to \neg P$

2. $(P\wedge Q)\vee (\neg P\wedge \neg Q)$

3. $(P\wedge (Q\wedge \neg R))\vee (\neg P\wedge (\neg Q\wedge R))$

4. $(P\leftrightarrow Q)\leftrightarrow R$

5. $(P\wedge Q)\vee \neg (P\to Q)$

6. $((P\to Q)\to R)\to Q$


**Exercise.** Show that if $B$ is a tautology, then $A\wedge B$ is
logically equivalent to $A$.

**Exercise.** Show that if $B$ is an inconsistency, then $A\vee B$ is
logically equivalent to $A$.


## For $n>1$ sentences

**Exercise:** What is the semantic relationship between $(P\wedge Q)$
and $\neg (P\to Q)$?


**Exercise:** If $\phi\wedge\psi$ is a contingency, then what are the
possibilities for $\phi$ and $\psi$? 

**Exercise:** If $\phi$ is a tautology, then what are the
possibilities for $\phi\wedge\psi$? What are the possibilities for
$\phi\vee\psi$?


## For arguments

**Exercises.** Determine whether the following arguments are valid or
not. Explain your answer by showing the existence of a row of a truth
table, or by pointing to a full truth table, or something of the
sort. Your answer should be articulated in English prose so that it
can convince anyone else who is familiar with truth tables.

1. $P\to (Q\to R)\:\vdash \: (P\wedge Q)\to R$

2. $P\to R\:\vdash (P\vee Q)\to R$

3. $(P\leftrightarrow Q)\leftrightarrow R \:\vdash\: P\vee R$

4. $\vdash\: (P\to Q)\vee (Q\to R)$



# Philosophical issues

Our target in this course is the notion of logical validity. But we
now have *two* different explications of this concept: one in terms of
provability, and one in terms of semantics (truth tables). Is one of
these two notions more basic than the other? What does it *really*
mean to say that an argument is valid? 




# Challenge problems 

1. Is the sentence $(P\wedge Q)\vee (P\wedge \neg Q)$ logically
   equivalent to some sentence that contains only $P$? Is there a
   general test for when a sentence with $P$ and $Q$ is equivalent to
   a sentence with just $P$?

2. How many possible truth tables are there for sentences that contain
   only the atomic sentence $P$?

3. For sentences with just the atomic sentence $P$, show that every
   truth table is represented by a sentence whose only connectives are
   $\wedge$ and $\neg$.


<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
   
   
<hr>   
   
# Aside for programmers 

A truth table for a sentence is a completely explicit representation
of the values of the set of all truth functions as applied to that
sentence. Given a list $\Sigma$ of propositional constants, a truth
function on $\Sigma$ is just an assignment of T and F to all the
elements in $\Sigma$. Such an assignment then extends naturally to all
sentences built out of the propositional constants in $\Sigma$.

We could tighten up this definition by first defining the type `WFF`
of well-formed formulas of propositional logic. Here's what the
definition of such a type might look like in the Haskell programming
language:

```haskell
data WFF
    = Var String         -- A propositional variable
    | Not WFF            -- Negation
    | And WFF WFF        -- Conjunction
    | Or WFF WFF         -- Disjunction
    | Imply WFF WFF      -- Implication
    | Iff WFF WFF        -- Biconditional (if and only if)
```

Here's an example of a WFF.

```haskell
exampleWFF :: WFF
exampleWFF = Imply (And (Var "p") (Not (Var "q"))) (Or (Var "r") (Var "s"))
```

This example is clear for a computer, but less clear to a human
eye. We prefer to see something like $(p\wedge\neg q)\to (r\vee s)$.

With the type `WFF` defined, we can then define a type
`TruthFunction`, which uses a type `Env` as an auxiliary. (An
Environment is essentially a truth-valuation, i.e. an assignment of
truth values to atomic sentences.)

```haskell
type Env = [(String, Bool)]
type TruthFunction = Env -> WFF -> Bool

eval :: TruthFunction
eval env (Var x) = case lookup x env of
                    Just val -> val
                    Nothing -> error "Variable not found"
eval env (Not wff) = not (eval env wff)
eval env (And wff1 wff2) = (eval env wff1) && (eval env wff2)
eval env (Or wff1 wff2) = (eval env wff1) || (eval env wff2)
eval env (Imply wff1 wff2) = (not (eval env wff1)) || (eval env wff2)
```

Here `eval` is defined recursively. For example, to determine the
truth value of `(And wff1 wff2)` it gets the truth value of `wff1` and
the truth value of `wff2` and applies the `&&` operation to those
Boolean values.

While we're at it, we can define a function that takes a WFF as input
and returns its **main connective**.

```haskell
mainConnective :: WFF -> String
mainConnective (Var _) = "Variable (no connective)"
mainConnective (Not _) = "Not"
mainConnective (And _ _) = "And"
mainConnective (Or _ _) = "Or"
mainConnective (Imply _ _) = "Imply"
```

The way that WFFs are defined in Haskell makes it easy to read off the
main connective, since it's simply that last constructor that is
applied in the definition of the relevant WFF. For us human beings, we
read strings of symbols, and we somehow -- with practice -- develop
the ability to transform this string into a data structure in our
minds. (We are "parsing" the string.) From this data structure in our
minds, we then "see" what the main connective is. 

One can define an algorithm that takes as input a string, and returns
the main connective (if that string represents a wff). However, this
kind of algorithm is usually pretty ugly, and it's certainly not
something that one recommends students to explicitly follow to find
the main connective of a wff.
   
