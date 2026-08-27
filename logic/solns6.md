---
title: "Quantifying: Solutions to Problems"
author: Hans Halvorson
---

## Exercise 6.1


1. No logicians are celebrities.

   $∀x(Lx→¬Cx)$
   
   
## Exercise 6.5

9. $∀x∀yRxy ⊢ ∀xRxx$

   ```lemmon
   1  (1) ∀x∀yRxy     A
   1  (2) ∀yRay       1 UE
   1  (3) Raa         2 UE
   1  (4) ∀xRxx       3 UI
   ```
   
## Exercise 6.13

1. $P→∃xFx ⊢ ∃x(P→Fx)$
   
   ```lemmon
   1     (1) P→∃xFx     A
         (2) P∨¬P       lem
   3     (3) P          A
   1,3   (4) ∃xFx       1,3 MP
   5     (5) Fa         A
   3,5   (6) P→Fa       3,5 CP
   1,3,5 (7) ∃x(P→Fx)   6 EI
   1,3   (8) ∃x(P→Fx)   4,5,7 EE
   9     (9) ¬P         A
   9     (10) P→Fa      9 npar
   9     (11) ∃x(P→Fx)  10 EI
   1     (12) ∃x(P→Fx)  2,3,8,9,11 ∨E
   ```
