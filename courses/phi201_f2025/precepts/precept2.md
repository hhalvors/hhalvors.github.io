---
title: "logic precept 2"
author: Hans Halvorson
fontsize: 12pt
pdf-engine: xelatex
mainfont: "TeX Gyre Termes"
monofont: "Menlo"            # <-- has ∧ ∨ ¬ →
header-includes:
  - \usepackage{fullpage}
---

# Review pset1

- Minor problems with translation: never write a non-atomic sentence
  as if it were atomic

- Minor problems with proofs: each rule applies to a sentence type,
  and can only be applied to entire sentences of that type
  
  E.g. DN applied to $P\to \neg\neg Q$ does not give $P\to Q$
  
  E.g. MP cannot be applied to $P\vee (Q\to R)$
  
- The harder problems in part C

   ```
   (1) P∨(Q∧R)   A
   (2) P∨Q       1 ∧E
   ```

	```
	Line (2) is not a valid application of ∧E. To apply ∧E to a line,
	the formula on that line needs to be a conjunction. But the
	formula on line (1) is a disjunction.
	```
	
	```
   This new rule would be bad. Consider, for example, the following
   "proof" that would be permitted by the new rule.
   
   (1) (P∧Q)→R    A
   (2) P→R        new rule
   
   But we do not consider such inferences to be valid. For example,
   let P = "I want a new bicycle" and Q = "I am able to buy a new
   bicycle", and R = "I buy a new bicycle". Then the premise could be
   true while the conclusion is false -- so the argument is not
   valid.
   ```
	
# Warmup: Old proof rules in new form

**Exercise 2.5** ($\wedge$E, $\wedge$I, $\vee$I, MP, MT, DN)

1. $P\wedge (Q\wedge R)\:\dashv\vdash\: (P\wedge Q)\wedge R$ 

```
1  (1) p&(q&r)   A
1  (2) p         1 &E
1  (3) q&r       1 &E
1  (4) q         3 &E
1  (5) r         3 &E
1  (6) p&q       2,4 &I
1  (7) (p&q)&r   6,5 &I
```

Here we see, among other things, that and introduction tells us to
combine dependencies, but that we don't need to include
redundancies. In particular, we write just `1` as dependency in lines
6 and 7 instead of `1,1` or `1,1,1`.

2. $P\:\dashv\vdash\: P\wedge P$

3. $P\to \neg Q,Q\: \vdash \: \neg P$

4. $\neg \neg P\: \vdash \: \neg \neg P\wedge (P\vee Q)$

5. $\neg (P\wedge Q)\to R,\neg R\:\vdash \: P$

6. $P\to (Q\wedge R),A\to \neg R,P\:\vdash\: \neg A$

7. $\neg P\to\neg Q,Q\:\vdash \: P$

8. $P\:\vdash \: \neg \neg (P\vee Q)$


# New proof rules 

We will work in blocks. First block ends in problems A1 and A2 on the
pset. The focus is on "pure" applications of CP.

$P\to R\vdash (P\wedge Q)\to R$

$P\to Q\vdash (R\to P)\to (R\to Q)$

$P\to Q\vdash (Q\to R)\to (P\to R)$

(A1) $P\to Q\vdash P\to (Q\vee R)$

(A2) $P\to (Q\to R)\vdash Q\to (P\to R)$

Second block ends in problems A3 and A4. The focus is on the
"contrapositive maneuver".

$P\to Q\vdash \neg Q\to \neg P$

$\vdash (P\wedge Q)\to P$

(A3) $\neg P\vdash \neg (P\wedge Q)$

$\vdash P\to (P\vee Q)$

(A4) $\neg (P\vee Q)\vdash \neg P$

Third block ends in problems A5 and A6. We come back to this if we
have time.

$P\vdash (P\to Q)\to Q$

(A5) $P\vdash (P\to \neg P)\to \neg P$


The fourth block ends in problems B1 and B2.

$(P\wedge Q)\vee (P\wedge R)\vdash P$

$P\vee Q\vdash Q\vee P$

(B1) $P\vee (Q\wedge R)\vdash P\vee Q$

The fifth block ends in problems B3 and B4.

$\neg P\vdash P\to Q$

$P,\neg P\vdash Q$

(B3) $P\vee Q,\neg P\vdash Q$

(B4) $(P\to R)\wedge (Q\to R)\vdash (P\vee Q)\to R$


# Evaluating proofs 

**Exercise** Which of the following proofs with CP is correct? If a
proof is not correct, explain what is wrong with it, and say whether
there is a simple fix, or whether it is fatally flawed. (The following
"proofs" use a slightly different notation -- one that is easier to
input via keyboard. Hopefully the relation between the two notations
will become clear from the context.)

```
1   (1) p&q     A
1   (2) p       1 &E 
1   (3) q       2 &E 
    (4) p>q     2,3 CP
```

```
1  (1) q     A
2  (2) p     A
1  (3) p>q   2,1 CP
```


**Exercise** Which of the following proofs with or-elimination is
correct? If a proof is not correct, explain what is wrong with it, and
say whether there is a simple fix, or whether it is fatally
flawed. (The following "proofs" use a slightly different notation --
one that is easier to input via keyboard. Hopefully the relation
between the two notations will become clear from the context.)

```
1  (1) p|p    A
2  (2) p      A
1  (3) p      1,2,2,2,2 |E
```

```
1   (1) p|q    A
2   (2) p      A
3   (3) q      A
2,3 (4) p&q    2,3 &I
2,3 (5) p      4 &E
1   (6) p      1,2,2,3,5 |E 
```

# Additional practice problems

$P\:\vdash\: Q\to (P\wedge Q)$

$(P\to Q)\wedge (P\to R)\:\vdash\: P\to (Q\wedge R)$

$P\to (P\to Q)\:\vdash\: P\to Q$

$(P\vee Q)\to R\:\vdash\: P\to R$

$P\to (Q\to R),P\to Q\:\vdash \: P\to R$

$P\to (Q\to R)\:\vdash\: (P\to Q)\to (P\to R)$

$P\to Q\:\vdash \:\neg Q\to\neg P$ 

$(P\wedge Q)\to R\: \vdash \: P\to (Q\to R)$

$P\to (Q\to R)\:\vdash\: P\to (\neg R\to \neg Q)$

$(P\to Q)\to P\:\vdash\: (P\to Q)\to Q$ (Hint: Not as difficult as
it looks. Assume $(P\to Q)\to P$ and $P\to Q$. The latter can be used
both as the antecedent of a conditional, and as a conditional itself.)

$\vdash\:(P\wedge Q)\to (Q\wedge P)$

$\vdash\:Q\to (P\to Q)$

$\vdash\:Q\to (P\to P)$ 

$(P\to Q)\to P,Q\:\vdash \: P$	

$(P\to Q)\to P\: \vdash \: \neg P\to P$

$(P\to R)\wedge (Q\to R)\:\vdash\: (P\vee Q)\to R$

$P\vee (Q\vee R)\:\dashv\vdash\: (P\vee Q)\vee R$

$P\wedge (Q\vee R)\:\dashv\vdash\: (P\wedge Q)\vee (P\wedge R)$

$P\vee (Q\wedge R)\:\dashv\vdash\: (P\vee Q)\wedge (P\vee R)$

$\neg P\vee Q\:\dashv\vdash\: P\to Q$

$\neg P\vee \neg Q\:\vdash\: \neg (P\wedge Q)$

$P\to Q\:\vdash\: \neg (P\wedge \neg Q)$

$\neg (P\wedge Q)\:\vdash\: \neg P\vee \neg Q$

$\neg (P\to Q)\:\vdash\: P\wedge \neg Q$ 

$\vdash (P\to Q)\vee (Q\to P)$ 

$P\to (Q\vee R)\:\vdash\: (P\to Q)\vee (P\to R)$

$(P\wedge Q)\to \neg Q\:\vdash\:P\to \neg Q$

$P\wedge\neg Q\:\vdash\: \neg (P\to Q)$

$\vdash\: ((P\to Q)\to P)\to P$ (Hint: One possibility is to first
prove $\vdash P\vee \neg P$, and then argue by cases. The first
 case is easy if you remember "positive paradox". For the second
case, remember "negative paradox", i.e. that $\neg P$ implies $P\to
  Q$.)
   
$P\to (Q\vee R)\:\vdash \: \neg R\to (\neg Q\to \neg P)$

$P\to \neg Q\:\vdash\: (P\wedge Q)\to R$

$P\to \neg Q\:\vdash\: P\to (Q\to R)$

$\neg (P\to Q)\:\vdash \: P\to \neg Q$

$P\:\dashv\vdash\: (P\wedge Q)\vee (P\wedge\neg Q)$

$P\to (Q\to R)\:\dashv\vdash \: (P\to Q)\to (P\to R)$

$P\to\neg P\:\dashv\vdash\:\neg P$

$P\to (Q\to \neg Q)\:\dashv\vdash\: P\to \neg Q$
	  
$(P\to Q)\to (P\to \neg Q)\:\dashv\vdash\: P\to \neg Q$	  

$P\:\dashv\vdash\: P\wedge (Q\vee\neg Q)$

$P\:\dashv\vdash\: P\vee (Q\wedge\neg Q)$

$(P\to Q)\to Q\:\vdash\: (Q\to P)\to P$

$(P\to Q)\to R\:\vdash\: (P\to R)\to R$

$(P\to R)\to R\:\dashv\vdash\: P\vee R$ (Hint: it's easy to derive
$\neg P\to R$ from the sentence on the left.)
	
$(P\to Q)\to P\:\dashv\vdash \: P$ (Hint: assume $\neg P$ and
derive $P\to Q$.)








