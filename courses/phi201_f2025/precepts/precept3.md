---
title: "Logic Precept 3"
author: Hans Halvorson
---

# Review of $\vee$-elimination

## Prove these

1. $(P\to Q)\vee (P\to R)\vdash P\to (Q\vee R)$

2. $\neg P\vee\neg Q\vdash \neg (P\wedge Q)$

3. $\neg P\vee Q\vdash P\to Q$



# Reductio ad Absurdum

## Prove these

1. $P\to Q\vdash \neg (P\wedge \neg Q)$

2. $\neg P\vdash P\to Q$

3. $\neg (P\to Q)\vdash P\wedge\neg Q$


## pset problem 1

$(P\wedge Q)\to \neg Q\vdash P\to \neg Q$


## pset problem 2

$\neg (P\to Q)\vdash P$

## pset problem 3

$\neg (P\to Q)\vdash Q\to R$

## pset problem 4

$P\to Q\vdash \neg P\vee Q$

## pset problem 5

$(P\wedge\neg P)\vee Q\vdash Q$


## pset problem 6

$P\to (Q\vee R)\vdash \neg R\to (P\to Q)$


# Pierce's law

$\vdash ((P\to Q)\to P)\to P$

<!-- 

1.  $P\to Q\:\vdash\: \neg (P\wedge \neg Q)$

2.  $\neg (P\wedge Q)\:\vdash\: \neg P\vee \neg Q$

3.  $\neg (P\to Q)\:\vdash\: P\wedge \neg Q$ 

4. $\vdash (P\to Q)\vee (Q\to P)$ 

5. $P\to (Q\vee R)\:\vdash\: (P\to Q)\vee (P\to R)$

6.  $(P\wedge Q)\to \neg Q\:\vdash\:P\to \neg Q$

7. $P\wedge\neg Q\:\vdash\: \neg (P\to Q)$

8. $\vdash\: ((P\to Q)\to P)\to P$ (Hint: One possibility is to first
   prove $\vdash P\vee \neg P$, and then argue by cases. The first
   case is easy if you remember "positive paradox". For the second
   case, remember "negative paradox", i.e. that $\neg P$ implies $P\to
   Q$.)
   
9. $P\to (Q\vee R)\:\vdash \: \neg R\to (\neg Q\to \neg P)$

10. $P\to \neg Q\:\vdash\: (P\wedge Q)\to R$

11. $P\to \neg Q\:\vdash\: P\to (Q\to R)$

12. $\neg (P\to Q)\:\vdash \: P\to \neg Q$

13. $P\:\dashv\vdash\: (P\wedge Q)\vee (P\wedge\neg Q)$

14. $P\to (Q\to R)\:\dashv\vdash \: (P\to Q)\to (P\to R)$

15. $P\to\neg P\:\dashv\vdash\:\neg P$

16. $P\to (Q\to \neg Q)\:\dashv\vdash\: P\to \neg Q$
	  
17. $(P\to Q)\to (P\to \neg Q)\:\dashv\vdash\: P\to \neg Q$	  

18. $P\:\dashv\vdash\: P\wedge (Q\vee\neg Q)$

19. $P\:\dashv\vdash\: P\vee (Q\wedge\neg Q)$

20. $(P\to Q)\to Q\:\vdash\: (Q\to P)\to P$

21. $(P\to Q)\to R\:\vdash\: (P\to R)\to R$

22. $(P\to R)\to R\:\dashv\vdash\: P\vee R$ (Hint for $\vdash$: it's
    easy to derive $\neg P\to R$ from the sentence on the left.)
	
23. $(P\to Q)\to P\:\dashv\vdash \: P$ (Hint for $\vdash$: assume
    $\neg P$ and derive $P\to Q$.)
	
	
# Clarifications	

Just like CP and $\vee$E, using RA on a line does not require that
the relevant assumption occurs among the dependencies on that
line. Here's a classic example:

```
1   (1) p      A
2   (2) -p     A
3   (3) -q     A
1,2 (4) p&-p   1,2 &I
1,2 (5) --q    3,4 RA
1,2 (6) q      5 DN
```

To be clear, RA only allows you to add a negation sign, not to
subtract one. That's why, in the proof above, we had also to use a
step of DN.


# More proofs  -->

<!-- working backwards -->

<!-- briding the gap with other proofs you've already done -->
