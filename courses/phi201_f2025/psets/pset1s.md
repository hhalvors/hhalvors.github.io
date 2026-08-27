---
title: logic pset1
fontsize: 12pt
mainfont: "Libertinus Serif" 
header-includes:
  - \usepackage{newunicodechar}
  - \newunicodechar{∧}{\ensuremath{\land}}
  - \newunicodechar{∨}{\ensuremath{\lor}}
  - \usepackage{fullpage}
---

# A. 

**(2 points each, 8 points total)**

Represent the propositional structure of each of the following
sentences. First identify the atomic component sentences
(i.e. sentences that do not contain connectives) and abbreviate each
with a distinct capital letter. We have suggested letters after the
sentences. Then represent the form of the original sentence using the
symbols $\vee ,\wedge ,\neg ,\to$ for the connectives "or", "and",
"not", "if...then...". Make sure to include parentheses, if necessary
to disambiguate. 

1.  Aristotle was neither a great philosopher nor a great scientist.
    (P,S)
	
	```
	Both of the following are correct:
	¬P∧¬S
	¬(P∨S)
	```

2.  Mark will get an A in logic only if he does the homework or bribes
    the professor. (A,H,B)
	
	`A→(H∨B)`	

3.  Dumbledore will be killed, and either McGonagall will become head of
    school and Hogwarts will flourish, or else it won't flourish.
    (D,M,H)

	```
	Either one of the following is acceptable:
	D∧((M∧H)∨¬H)
	D∧((M∧H)∨(¬M∧¬H))
	The first of the two is logically weaker, as it allows that
	McGonagall becomes head of school and Hogwarts does not
	flourish. The second, but not the first, implies that M→H. I
	happen to think that the second is the more natural reading of 
	the English sentence.
	```

4.  Harry and Dumbledore are not both right about the moral status of
    Professor Snape. (H,D)
	
	```
	Either one of the following is acceptable:
	¬(H∧D)
	¬H∨¬D
	```


# B. 

**(4 points each, 20 points total)**

Prove that the following argument forms are valid. The premises are to
the left of the $\vdash$ symbol, the conclusion is to the right.  You
should number the lines of your proof, and each line must either be a
premise (i.e. an assumption) or be justified by one of the following
rules of inference: $\wedge$I, $\wedge$E, $\vee$I, MP, MT, or DN.

1.  $X ⊢ (X ∨ Z) ∧ (X ∨ Y)$

   	```
	(1) P             A
	(2) P∨R           1 ∨I
	(3) P∨Q           1 ∨I
	(4) (P∨R)∧(P∨Q)   2,3 ∧I
	```

2.  $P\:\vdash\: Q\vee (\neg\neg P\vee R)$

	```
	(1) P           A
	(2) ¬¬P         1 DN
	(3) ¬¬P∨R       2 ∨I
	(4) Q∨(¬¬P∨R)   3 ∨I
	```

3.  $\neg\neg Q\to P,\,\neg P\:\vdash\:\neg Q$

	```
	(1) ¬¬Q→P   A
	(2) ¬P      A
	(3) ¬¬¬Q    1,2 MT
	(4) ¬Q      3 DN
	```

4.  $Q\to (P\to R),\,\neg R\wedge Q\:\vdash\: \neg P$

    ```
	(1) Q→(P→R)   A
	(2) ¬R∧Q      A
	(3) Q         2 ∧E
	(4) P→R       1,3 MP
	(5) ¬R        2 ∧E
	(6) ¬P        4,5 MT
	```


# C.  

**(4 points each, 8 points total)**

1. Explain what's wrong with the following "proof".

   ```
   (1) P∨(Q∧R)   A
   (2) P∨Q       1 ∧E
   ```

	```
	Line (2) is not a valid application of ∧E. To apply ∧E to a line,
	the formula on that line needs to be a conjunction. But the
	formula on line (1) is a disjunction.
	```

2. If you were allowed to make up logic rules, do you think the
   following would be a good rule? 
   
   > Any time a conjunction $A\wedge B$ occurs on a line, even as a
   > part of a larger sentence, you may rewrite that line with only
   > $A$ in place of the conjunction $A\wedge B$, or with only $B$ in
   > place of $A\wedge B$.
   
   Explain your answer in a paragraph (no more than half a page).
   
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

