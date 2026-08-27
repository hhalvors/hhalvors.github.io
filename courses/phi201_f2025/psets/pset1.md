---
title: pset1
---

**Resources:** Lecture 1 and Chapters 1 and 2 (pp 1-24) of *How Logic
Works*.


# A.   

Represent the propositional structure of each of the following
sentences. First identify the atomic component sentences
(i.e. sentences that do not contain connectives) and abbreviate each
with a distinct capital letter. We have suggested letters after the
sentences. Then represent the form of the original sentence using the
symbols $\vee ,\wedge ,\neg ,\to$ for the connectives "or", "and",
"not", "if...then...". Make sure to include parentheses, if necessary
to disambiguate.

1. Elon Musk is neither a world-class chef nor a professional gamer. ($C,G$)
2. Pedro Pascal will win the Emmy only if he memorizes all his lines
   or buys the judges dinner. ($E,M,B$)
3. The Iron Throne will be destroyed, and either Jon Snow will become
   king and Westeros will prosper, or else it won’t prosper. ($D,K,W$)
4. Barbie and Ken are not both correct about the real estate value of
   Dreamhouse. ($B,K$)

# B. 

Prove that the following argument forms are valid. The premises are to
the left of the $\vdash$ symbol, the conclusion is to the right.  You
should number the lines of your proof, and each line must either be a
premise (i.e. an assumption) or be justified by one of the following
rules of inference: ∧I, ∧E, ∨I, MP, MT, or DN.

1.  $X ⊢ (X ∨ Z) ∧ (X ∨ Y)$

2.  $M ⊢ N ∨ (¬¬M ∨ O)$

3.  $¬¬D → F, ¬F ⊢ ¬D$

4.  $U → (T → V), ¬V ∧ U ⊢ ¬T$


# C.  

1. Explain what's wrong with the following "proof".

   -------------------------   -----------
   (1) $P ∨ (Q ∧ R)$           A
   (2) $P ∨ Q$                 1 ∧E
   -----------------------     -----------

2. If you were allowed to make up logic rules, do you think the
   following would be a good rule? 
   
   > Any time a conjunction $A∧B$ occurs on a line, even as a part of
   > a larger sentence, you may rewrite that line with only $A$ in
   > place of the conjunction $A∧B$, or with only $B$ in place of
   > $A∧B$.
   
   Explain your answer in a paragraph (no more than half a page).
