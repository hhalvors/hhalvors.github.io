---
title: "Supposing: Solutions to Problems"
author: Hans Halvorson
---

## Exercise 3.1

1. $P\: ⊢ \: Q→(P∧Q)$

   ```lemmon
   1    (1) P         A
   2    (2) Q         A
   1,2  (3) P∧Q       1,2 ∧I
   1    (4) Q→(P∧Q)   2,3 CP
   ```
   
2. $(P→Q)∧(P→R)⊢P→(Q∧R)$

   ```lemmon
   1   (1) (P→Q)∧(P→R)     A
   2   (2) P               A
   1   (3) P→Q             1 ∧E
   1   (4) P→R             1 ∧E
   1,2 (5) Q               3,2 MP
   1,2 (6) R               4,2 MP
   1,2 (7) Q∧R             5,6 ∧I
   1   (8) P→(Q∧R)         2,7 CP
   ```
   
3. $P→(Q→R) ⊢ Q→(P→R)$

   ```lemmon
   1      (1) P→(Q→R)    A
   2      (2) Q          A
   3      (3) P          A
   1,3    (4) Q→R        3,1 MP
   1,2,3  (5) R          4,2 MP
   1,2    (6) P→R        3,5 CP
   1      (7) Q→(P→R)    2,6 CP
   ```
   
4. $P→Q ⊢ (Q→R)→(P→R)$

   ```lemmon
   1      (1) P→Q     A
   2      (2) Q→R     A
   3      (3) P       A
   1,3    (4) Q     1,3 MP
   1,2,3  (5) R     2,4 MP
   1,2    (6) P→R   3,5 CP
   1      (7) (Q→R)→(P→R)  2,6 CP
   ```

5. $P→(P→Q) ⊢ P→Q$

   ```lemmon
   1      (1)  P→(P→Q)  A
   2      (2)  P        A
   1,2    (3)  P→Q      1,2 MP 
   1,2    (4)  Q        3,2 MP 
   1      (5)  P→Q      2,4 MP 
   ```

6. $P→(Q→R) ⊢ (P∧Q)→R$

   ```lemmon
   1   (1) P→(Q→R)  A
   2   (2) P∧Q      A
   2   (3) P        2 ∧E
   2   (4) Q        2 ∧E
   1,2 (5) Q→R      1,3 MP
   1,2 (6) R        5,4 MP
   1   (7) (P∧Q)→R  2,6 CP
   ```
   
7. $(P∨Q)→R ⊢ P→R$

   ```lemmon
   1    (1) (P∨Q)→R   A
   2    (2) P         A
   2    (3) P∨Q       2 ∨I
   1,2  (4) R         1,3 MP
   1    (5) P→R       2,4 CP
   ```   

8. $¬P ⊢ ¬(P∧Q)$

   ```lemmon
   1 (1) ¬P       A
   2 (2) P∧Q      A
   2 (3) P        2 ∧E
     (4) (P∧Q)→P  2,3 CP
   1 (5) ¬(P∧Q)   4,1 MT
   ```
   
9. $¬(P∨Q)⊢¬P∧¬Q$

   ```lemmon
   1 (1) ¬(P∨Q)   A
   2 (2) P        A
   2 (3) P∨Q      2 ∨I
     (4) P→(P∨Q)  2,3 CP
   1 (5) ¬P       4,1 MT
   6 (6) Q        A
   6 (7) P∨Q      6 ∨I
     (8) Q→(P∨Q)  6,7 CP
   1 (9) ¬Q       8,1 MT
   1 (10) ¬P∧¬Q   5,9 ∧I
   ```
   
10. $P→¬P⊢¬P$
   
    ```lemmon
    1    (1) P           A
    2    (2) P→¬P        A
    1,2  (3) ¬P          2,1 MP
    1    (4) (P→¬P)→¬P   2,3 CP
    1    (5) ¬¬P         1 DN
    1    (6) ¬(P→¬P)     4,5 MT
         (7) P→¬(P→¬P)   1,6 CP
    2    (8) ¬¬(P→¬P)    2 DN
    2    (9) ¬P          7,8 MT
    ```
   
