---
title: logic pset2
fontsize: 12pt
pdf-engine: xelatex
mainfont: "TeX Gyre Termes"
monofont: "Menlo"            # <-- has ∧ ∨ ¬ →
header-includes:
  - \usepackage{fullpage}
---

# A.

Use Conditional Proof (and possibly the previous rules) to prove the
following sequents. Be sure to include dependency numbers in the
leftmost column of your proof.

1.  $P\to Q\:\vdash\: P\to (Q\vee R)$

	```
	1   (1) P→Q      A
	2   (2) P        A
	1,2 (3) Q        1,2 MP
	1,2 (4) Q∨R      3 ∨I
	1   (5) P→(Q∨R)  2,4 CP
	```


3.  $P\to (Q\to R)\:\vdash\: Q\to (P\to R)$

	```
	1     (1) P→(Q→R)   A
	2     (2) Q         A
	3     (3) P         A
	1,3   (4) Q→R       1,3 MP
	1,2,3 (5) R         4,2 MP
	1,2   (6) P→R       3,5 CP
	1     (7) Q→(P→R)   2,6 CP
	```
	
3.  $\neg P\:\vdash\:\neg (P\wedge Q)$ 

	```
	1  (1) ¬P       A
	2  (2) P∧Q      A
	2  (3) P        2 ∧E
	   (4) (P∧Q)→P  2,3 CP
	1  (5) ¬(P∧Q)   4,1 MT
	```
	
4.  $\neg (P\vee Q)\:\vdash\: \neg P$	

    ```
	1  (1) ¬(P∨Q)   A
	2  (2) P        A
	2  (3) P∨Q      2 ∨I
	   (4) P→(P∨Q)  2,3 CP
	1  (5) ¬P       4,1 MT
	```
	
	
5.  $P\:\vdash\: (P\to\neg P)\to \neg P$

	```
	1   (1) P          A
	2   (2) P→¬P       A
	1,2 (3) ¬P         2,1 MP
	1   (4) (P→¬P)→¬P  2,3 CP
	```
	
6. $P\:\vdash\: \neg (P\to\neg P)$

	```
	1   (1) P          A
	2   (2) P→¬P       A
	1,2 (3) ¬P         1,2 MP
	1   (4) (P→¬P)→¬P  2,3 CP
	1   (5) ¬¬P        1 DN
	1   (6) ¬(P→¬P)    4,5 MT
	```

# B.

Use $\vee$-elimination (and possibly the previous rules) to prove the
following sequents. Do *not* use reductio ad absurdum for any of these
proofs.

1.  $P\vee (Q\wedge R)\:\vdash\: P\vee Q$

	```
	1 (1) P∨(Q∧R)   A
	2 (2) P         A
	2 (3) P∨Q       2 ∨I
	4 (4) Q∧R       A
	4 (5) Q         4 ∧E
	4 (6) P∨Q       5 ∨I
	1 (7) P∨Q       1,2,3,4,6 ∨E
	```

2.  $P\wedge (Q\vee R)\:\vdash\: (P\wedge Q)\vee (P\wedge R)$

	```
	1   (1)  P∧(Q∨R)       A
	1   (2)  P             1 ∧E
	1   (3)  Q∨R           1 ∧E
	4   (4)  Q             A
	1,4 (5)  P∧Q           2,4 ∧I
	1,4 (6)  (P∧Q)∨(P∧R)   5 ∨I
	7   (7)  R             A
	1,7 (8)  P∧R           2,7 ∧I
	1,7 (9)  (P∧Q)∨(P∧R)   8 ∨I
	1   (10) (P∧Q)∨(P∧R)   3,4,6,7,9 ∨E
	```

3.  $P\vee Q,\neg P\:\vdash \: Q$

	```
	1   (1) P∨Q    A
	2   (2) ¬P     A
	3   (3) ¬Q     A
	2   (4) ¬Q→¬P  3,2 CP
	5   (5) P      A
	5   (6) ¬¬P    5 DN
	2,5 (7) ¬¬Q    4,6 MT
	2,5 (8) Q      7 DN
	9   (9) Q      A
	1,2 (10) Q     1,5,8,9,9 ∨E
	```

4.  $(P\to R)\wedge (Q\to R)\:\vdash\: (P\vee Q)\to R$

	```
	1    (1) (P→R)∧(Q→R)   A
	1    (2) P→R           1 ∧E
	1    (3) Q→R           1 ∧E
	4    (4) P∨Q           A
	5    (5) P             A
	1,5  (6) R             2,5 MP
	7    (7) Q             A
	1,7  (8) R             3,7 MP
	1,4  (9) R             4,5,6,7,8 ∨E
	1    (10) (P∨Q)→R      4,9 CP
	```
