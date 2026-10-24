---
title: "Operator algebras and quantum gravity"
subtitle: "Lessons for naturalized metaphysics"
author: Hans Halvorson
date: "Pittsburgh, October 24, 2026"
---

## Naturalized metaphysics

# What are we doing here?

- Metaphysics: what the world is like, at its most general, and perhaps at its most fundamental
- Naturalized: answerable to our best science, physics above all
- Three pictures of the project: a book of the world; a model of the world; a harmony with the manifest image

# 

> The world has a distinguished structure, a privileged description. For a representation to be fully successful, truth is not enough; the representation must also use the right concepts, so that its conceptual structure matches reality's structure. There is an objectively correct way to "write the book of the world".

--- Sider 2011, p. vii

# 

> [T]here is no Book of the World, whose every statement is true and which contains a complete description of the world at the most fundamental level; instead, there is a Model of the World, which exactly and completely represents the world at the most fundamental level.

--- Wallace 2022, p. 365

# 

> Only the inescapable demand holds, that a harmony must be aimed at between the scientific understanding of existence and the world of thought the single individual builds on the borders of science.

--- Høffding, "Filosofien og Livet" (1901), p. 10

# Naturalized metaphysics without the final model

- Wallace's ideal: a model that "exactly and completely represents the world at the most fundamental level"
- I don't think that limit makes sense: with Høffding, there is always a further presupposition to uncover
- My suspicion: that is why I value mathematical models that aren't meant to bring us closer to that ideal, such as the operator-algebraic ones

# Four stages in interpreting QFT

```{=latex}
\begin{enumerate}
\setlength{\itemsep}{1.3em}
\Large
\item A field of operators
\item A net of observable algebras
\item Taking effective field theory seriously
\item Operator algebras after gravity?
\end{enumerate}
```

## Stage 1: a field of operators

# Operators at points

```{=latex}
\begin{center}
\begin{tikzpicture}[scale=0.95, >=Stealth]
  \draw[nordiskMuted, ->] (-0.3,0) -- (8.6,0) node[right, font=\footnotesize] {$x$};
  \foreach \i in {0,...,7} {
    \pgfmathsetmacro{\x}{0.3+1.15*\i}
    \fill[nordiskMuted] (\x,0) circle (1.2pt);
    % vertical spring from axis to ball
    \draw[nordiskMuted] (\x,0) -- (\x,0.15) -- (\x-0.1,0.25) -- (\x+0.1,0.4) -- (\x-0.1,0.55) -- (\x+0.1,0.7) -- (\x,0.8) -- (\x,0.95);
    \fill[nordiskA] (\x,1.05) circle (3pt);
  }
  \foreach \i in {0,...,6} {
    \pgfmathsetmacro{\a}{0.3+1.15*\i+0.1}
    \pgfmathsetmacro{\d}{0.95/8}
    \draw[nordiskA!70] (\a,1.05) \foreach \k in {1,...,7} { -- ({\a+\k*\d}, {1.05+0.09*(-1)^\k}) } -- ({\a+0.95},1.05);
  }
  \node[nordiskA, font=\footnotesize, above] at (3.75,1.15) {$\Phi(x),\ \Pi(x)$};
\end{tikzpicture}
\end{center}
```

$$[\Phi(x), \Pi(y)] = i\,\delta(x - y) \qquad H = \tfrac12 \int \big(\Pi^2 + (\nabla\Phi)^2 + m^2\Phi^2\big)\,dx$$

- Teller (1995): the quantum field as "the association of an operator with each of the space-time points"
- Coupled oscillators; their normal modes are independent oscillators, one for each $k$

# Fock space and quanta

$$\mathcal F(\mathcal H) \;=\; \underbrace{\mathbb C}_{\text{vacuum}} \;\oplus\; \underbrace{\mathcal H}_{\text{one quantum}} \;\oplus\; \underbrace{(\mathcal H \otimes \mathcal H)_s}_{\text{two quanta}} \;\oplus\; \underbrace{(\mathcal H \otimes \mathcal H \otimes \mathcal H)_s}_{\text{three quanta}} \;\oplus\; \cdots$$

$$[a(\mathbf k), a^\dagger(\mathbf k')] = \delta^3(\mathbf k - \mathbf k') \qquad \psi^\dagger(\mathbf x) = \int d\tilde{k}\, \langle \mathbf k | \mathbf x \rangle\, a^\dagger(\mathbf k) \;\text{ creates a quantum at } \mathbf x$$

$$\Phi(x) = \int d\tilde{k}\, \big( a(\mathbf k)\, e^{-ik\cdot x} + a^\dagger(\mathbf k)\, e^{ik\cdot x} \big) \qquad N = \int d^3k\; a^\dagger(\mathbf k)\, a(\mathbf k)$$

- Symmetrized: how many quanta in each state, never which is which
- Local number: $\psi^\dagger(\mathbf x)\psi(\mathbf x)$ as a "number density"; $\int_V \psi^\dagger\psi$ counts the quanta in $V$

# The world according to Stage 1

- Quanta, not particles: no "primitive thisness"; quanta "can only be aggregated"
- Superpositions are propensities: e.g. to display one quantum or two
- What has the propensities? "'The world' is not itself a substance or a thing in any ordinary sense"; or properties need not be properties of things
- The field: $\Phi(x)$ a field determinable, whose values are expectation values $\langle \phi | \Phi(x) | \phi \rangle$ at each point

## Stage 2: a net of observable algebras

# Smear the field, build a net

$$\Phi(f) = \int \Phi(x)\, f(x)\, dx, \qquad \operatorname{supp} f \subset O \;\leadsto\; \mathcal A(O)$$

```{=latex}
\begin{center}
\begin{tikzpicture}[scale=0.95]
  % time slice
  \draw[nordiskMuted, thin] (-5.6,0) -- (5.6,0) node[right, font=\small] {$t = 0$};
  % larger region containing O1
  \draw[nordiskA, dashed, thick] (-4.3,0) -- (-2.4,1.9) -- (-0.5,0) -- (-2.4,-1.9) -- cycle;
  \node[nordiskA, font=\small] at (-3.75,1.2) {$\tilde O_1$};
  % O1
  \fill[nordiskA!18] (-3.5,0) -- (-2.4,1.1) -- (-1.3,0) -- (-2.4,-1.1) -- cycle;
  \draw[nordiskA, thick] (-3.5,0) -- (-2.4,1.1) -- (-1.3,0) -- (-2.4,-1.1) -- cycle;
  \node[nordiskA] at (-2.4,0.3) {$O_1$};
  \node[nordiskA, font=\small] at (-2.4,-0.35) {$\mathcal A(O_1)$};
  % O2
  \fill[nordiskB!18] (1.4,0) -- (2.6,1.2) -- (3.8,0) -- (2.6,-1.2) -- cycle;
  \draw[nordiskB, thick] (1.4,0) -- (2.6,1.2) -- (3.8,0) -- (2.6,-1.2) -- cycle;
  \node[nordiskB] at (2.6,0.3) {$O_2$};
  \node[nordiskB, font=\small] at (2.6,-0.35) {$\mathcal A(O_2)$};
  % spacelike separation
  \draw[nordiskMuted, <->, >=Stealth] (-0.3,-0.25) -- (1.2,-0.25);
  \node[nordiskMuted, font=\footnotesize] at (0.45,-0.55) {spacelike};
\end{tikzpicture}
\end{center}
```

$O_1 \subset \tilde O_1 \;\Rightarrow\; \mathcal A(O_1) \subset \mathcal A(\tilde O_1)$ $\qquad$ $O_1, O_2$ spacelike $\;\Rightarrow\; [\mathcal A(O_1), \mathcal A(O_2)] = 0$

# Symmetry and positive energy

```{=latex}
\begin{columns}[T]
\begin{column}{0.5\textwidth}
\centering
\begin{tikzpicture}[scale=1.0, >=Stealth]
  \draw[nordiskMuted, ->] (-1.2,-1.3) -- (-1.2,1.4) node[above, font=\scriptsize] {$t$};
  \draw[nordiskMuted, ->] (-1.3,-1.2) -- (4.6,-1.2) node[right, font=\scriptsize] {$x$};
  \fill[nordiskA!18] (0,-0.8) -- (0.8,0) -- (0,0.8) -- (-0.8,0) -- cycle;
  \draw[nordiskA, thick] (0,-0.8) -- (0.8,0) -- (0,0.8) -- (-0.8,0) -- cycle;
  \node[nordiskA] at (0,0) {$O$};
  \begin{scope}[shift={(3.0,0.25)}]
    \fill[nordiskA!18] (-0.39,-0.89) -- (0.89,0.39) -- (0.39,0.89) -- (-0.89,-0.39) -- cycle;
    \draw[nordiskA, thick] (-0.39,-0.89) -- (0.89,0.39) -- (0.39,0.89) -- (-0.89,-0.39) -- cycle;
    \node[nordiskA] at (0,0) {$gO$};
  \end{scope}
  \draw[nordiskB, ->, thick] (0.9,0.45) to[bend left=25] node[above, font=\scriptsize] {boost + translation $g$} (2.4,0.75);
\end{tikzpicture}

\vspace{0.6em}
{\small $U(g)\,\mathcal A(O)\,U(g)^* = \mathcal A(gO)$}

\vspace{0.3em}
{\footnotesize Poincaré covariance}
\end{column}
\begin{column}{0.5\textwidth}
\centering
\begin{tikzpicture}[scale=1.0, >=Stealth]
  \fill[nordiskB!15] (0,0) -- (1.6,1.6) -- (-1.6,1.6) -- cycle;
  \draw[nordiskB, thick] (1.6,1.6) -- (0,0) -- (-1.6,1.6);
  \draw[nordiskMuted, ->] (-1.9,0) -- (1.9,0) node[right, font=\scriptsize] {$\mathbf p$};
  \draw[nordiskMuted, ->] (0,-0.3) -- (0,2.0) node[above, font=\scriptsize] {$p^0$};
  \draw[nordiskB, domain=-1.25:1.25, samples=40] plot (\x,{sqrt(\x*\x+0.36)});
  \node[nordiskB, font=\scriptsize, anchor=west] at (0.75,0.62) {$p^2 = m^2$};
  \fill[black] (0,0) circle (2pt);
  \node[font=\scriptsize, anchor=north west] at (0.05,-0.02) {$\Omega$};
  \node[nordiskB, font=\small] at (-0.95,1.3) {$\overline{V}_{\!+}$};
\end{tikzpicture}

\vspace{0.6em}
{\small $\operatorname{spec} P \subset \overline{V}_{\!+}$, \quad $U(g)\,\Omega = \Omega$}

\vspace{0.3em}
{\footnotesize Positive energy in every frame; an invariant vacuum}
\end{column}
\end{columns}
```

# Philosophers take up the algebraic approach

```{=latex}
\begin{center}
\begin{tikzpicture}[scale=0.9, font=\small,
  root/.style={draw, rounded corners=3pt, line width=0.8pt, inner sep=5pt, align=center},
  kid/.style={rounded corners=3pt, inner sep=4pt, align=center}]
  \node[root, draw=nordiskA, text=nordiskA] (R) at (-3.2,1.8) {Redhead\\[-1pt]{\footnotesize London, Cambridge}};
  \node[root, draw=nordiskB, text=nordiskB] (E) at (3.2,1.8) {Earman\\[-1pt]{\footnotesize Pittsburgh}};
  \node[kid, fill=nordiskA!12] (S) at (-4.6,0) {Saunders\\[-1pt]{\footnotesize PhD 1989}};
  \node[kid, fill=nordiskA!12] (C) at (-1.8,0) {Clifton\\[-1pt]{\footnotesize PhD 1991}};
  \node[kid, fill=nordiskB!12] (A) at (1.8,0) {Arageorgis\\[-1pt]{\footnotesize PhD 1995}};
  \node[kid, fill=nordiskB!12] (U) at (4.6,0) {Ruetsche\\[-1pt]{\footnotesize PhD 1995}};
  \draw[nordiskA, thick] (R.south) -- (S.north);
  \draw[nordiskA, thick] (R.south) -- (C.north);
  \draw[nordiskB, thick] (E.south) -- (A.north);
  \draw[nordiskB, thick] (E.south) -- (U.north);
\end{tikzpicture}
\end{center}
```

- The model: general relativity, with clear axioms (Einstein's equations on Lorentzian manifolds) and clear structural commitments. QFT's were unclear: Fock space is just one infinite-dimensional Hilbert space
- The spurs, which Stage 1 had not grappled with: smearing; Haag's theorem; Stone--von Neumann uniqueness fails, so inequivalent representations
- The aim: a clearer, more careful picture of QFT's structural commitments. Stage 1 sharpened, not rejected

# Von Neumann algebras and types

- Type belongs to $\mathcal M$, not to $\mathfrak A$: it can depend on the representation

| Type | Trace of $1$ | Density matrices | Normal pure states |
|---|---|---|---|
| I$_n$, I$_\infty$ | $n$, $\infty$ | \textcolor{nordiskA}{$\checkmark$} | \textcolor{nordiskA}{$\checkmark$} |
| II$_1$ | $1$ | \textcolor{nordiskA}{$\checkmark$} | \textcolor{nordiskB}{$\times$} |
| II$_\infty$ | $\infty$ | \textcolor{nordiskA}{$\checkmark$} | \textcolor{nordiskB}{$\times$} |
| III | none | \textcolor{nordiskB}{$\times$} | \textcolor{nordiskB}{$\times$} |

# Modular flow is time translation

```{=latex}
\begin{columns}[T]
\begin{column}{0.40\textwidth}
\centering
\begin{tikzpicture}[scale=1.15, >=Stealth]
  \fill[nordiskA!12] (1.5,1.5) -- (0,0) -- (1.5,-1.5) -- cycle;
  \fill[nordiskB!12] (-1.5,1.5) -- (0,0) -- (-1.5,-1.5) -- cycle;
  \draw[nordiskMuted] (-1.5,-1.5) rectangle (1.5,1.5);
  \draw[nordiskMuted] (-1.5,-1.5) -- (1.5,1.5);
  \draw[nordiskMuted] (-1.5,1.5) -- (1.5,-1.5);
  \draw[nordiskA, thick, ->] (1.5,-1.5) .. controls (0.9,-0.4) and (0.9,0.4) .. (1.5,1.5);
  \draw[nordiskA, thick, ->] (1.5,-1.5) .. controls (0.6,-0.5) and (0.6,0.5) .. (1.5,1.5);
  \draw[nordiskB, thick, ->] (-1.5,1.5) .. controls (-0.9,0.4) and (-0.9,-0.4) .. (-1.5,-1.5);
  \draw[nordiskB, thick, ->] (-1.5,1.5) .. controls (-0.6,0.5) and (-0.6,-0.5) .. (-1.5,-1.5);
  \draw[nordiskA, line width=2pt] (1.5,-1.5) -- (1.5,1.5);
  \node[nordiskA, font=\footnotesize, anchor=west] at (1.55,0) {observer};
  \node[nordiskA] at (0.45,0) {$\mathcal A$};
  \node[nordiskB] at (-0.45,0) {$\mathcal A'$};
  \node[nordiskA, font=\footnotesize, anchor=west] at (1.55,0.9) {$\sigma_t$};
  \node[font=\footnotesize] at (0,1.75) {$\mathcal I^+$};
  \node[font=\footnotesize] at (0,-1.75) {$\mathcal I^-$};
\end{tikzpicture}
\end{column}
\begin{column}{0.56\textwidth}
\begin{itemize}
\item De Sitter vacuum on $\mathcal A$: thermal (KMS) for the patch's time translation, at $T_{dS} = 1/2\pi r_{dS}$
\item Modular flow $\sigma_t$ = time translation: geometric (in Minkowski: wedges, boosts; Bisognano--Wichmann, Unruh)
\item Type I would mean a density matrix $\rho$, and a modular operator $\rho \otimes \rho^{-1}$ with discrete spectrum
\item Time translation has continuous spectrum: not type I. In fact, III$_1$
\end{itemize}
\end{column}
\end{columns}
```

# Type III$_1$: entanglement everywhere

- Each local algebra: the hyperfinite type III$_1$ factor (given a scaling limit and the funnel property)
- No pure states and no density matrices for any region
- Reeh--Schlieder: the vacuum is cyclic for every local algebra
- A wedge and its complement: Bell's inequality maximally violated in every normal state

# No convergence on an ontology

- Against particles: "in every state, there are no particles in any local region" (Halvorson & Clifton 2002); no unique particle concept
- Against fields: the wavefunctional interpretation inherits the particle's troubles with inequivalent representations and with interactions (Baker 2009)
- By about 2012: aporia for the naturalized metaphysics of QFT

## Stage 3: taking effective field theory seriously

# Not the place to look

- Wallace (2006, 2011): interpret Lagrangian QFT with a real cutoff, the theory physicists use
- UV-inequivalent representations: "purely a mathematical artefact" (2006)
- AQFT: "we have no reason to believe its claims about what the world is like on sub-Planckian lengthscales" (2011)
- Not what the world would be like "where there was no gravity and QFT was exactly true," but "what is it rational to believe about QFT, given all the empirical data that we in fact have?" (2011)

# Effective realism

- Our best QFTs are effective: they fail at short distances, and we do not know what replaces them
- The RG picks out what is insensitive to that unknown physics; be a realist about that (Wallace 2006, 2011; Fraser 2018; Williams 2019)
- Fraser: "correlation functions over distances much longer than the cutoff scale" as "appropriate targets for realist commitment" (2018)
- Williams: robustness, "accessible ... in a variety of independent ways" (2019)

# Ruetsche: robustness is frailer than you think

- The RG acts on a space of theories that physicists construct; a true theory outside it is "a salient skeptical possibility" (2018; 2026)
- Her fable: Newtonian gravity's robust features, a force "acting instantaneously at a distance across Euclidean three-space," "grossly misrepresent the General Relativistic worlds." Moral: "theory space is bigger than you think" (2026)
- What stays robust (the couplings): "most empiricists would be happy to share in the endorsement" (2026)
- "It could well be that Effective Realism is pragmatism in denial!" (2026)
- Witten et al.: another case of theory space being bigger than the RG's?

# What does it promise to keep?

- Fraser: "a kind of coarse-grained representational success," capturing "(relatively) long-distance, low-energy, features of the world while distorting its fundamental structure" (2018)
- The commitments are indexed to a scale, and quantitative: correlation functions; robust entities, layered by scale (Williams)
- Compare worldview facts: determinism, field ontology, nonlocality
- Williams: short-distance structure is "largely irrelevant." But type III$_1$ is the same factor in every theory: entanglement structure, not a short-distance detail
- What changes with gravity in Witten et al. is architecture: an observer, a constraint, a change of type

# What the RG realists get right

- Look at sequences of theories, and the relations between them
- Early AQFT was too static: one theory, its axioms, its models
- The RG gives one kind of sequence: change the scale. Are there others?

## Stage 4? Operator algebras after gravity

# 

> We assume that a complete gravity theory can be approximated, in an appropriate class of states, by an ordinary quantum field theory weakly coupled to gravity.

--- Witten 2023, p. 12

# Witten et al.: an observer in de Sitter space

- The isometries are constraints: with fields alone, $\mathcal A^H = \mathbb C$
- Add an observer's clock, energy $q \geq 0$; dress the fields to the clock

| | Algebra | State of maximum entropy |
|---|---|---|
| Fields alone | III$_1$ | none |
| Fields + clock, $q \in \mathbb R$ | II$_\infty$ | none |
| Fields + clock, $q \geq 0$ | II$_1$ | empty de Sitter |

- Semiclassical states: $S = S_{\text{gen}} = A/4G_N + S_{\text{out}}$, up to a constant

# A limit of theories

```{=latex}
\begin{columns}[T]
\begin{column}{0.38\textwidth}
\centering
\begin{tikzpicture}[scale=0.95, >=Stealth]
  \draw[nordiskMuted, ->] (0,0) -- (0,3.0) node[above, font=\scriptsize, align=center] {$G$ (relative to $r_{dS}$)};
  \draw[nordiskMuted] (-0.3,0) -- (3.2,0);
  \foreach \g/\x in {2.4/2.15, 1.6/1.95, 1.0/1.8, 0.6/1.72, 0.33/1.67, 0.16/1.64} {
    \fill[nordiskA] (\x,\g) circle (1.6pt);
  }
  \draw[nordiskA, dotted, thick] (2.15,2.4) .. controls (1.75,1.2) and (1.62,0.4) .. (1.62,0.12);
  \node[nordiskA, font=\scriptsize, anchor=west] at (2.25,2.4) {$T(G)$};
  \fill[nordiskA] (1.62,0) circle (2.6pt);
  \node[nordiskA, font=\scriptsize, align=center, below] at (1.62,-0.1) {$\lim_{G\to 0} T(G)$\\[-1pt]II$_1$};
  \fill[black] (0,0) circle (2.6pt);
  \node[font=\scriptsize, align=center, below] at (0,-0.1) {$T(0)$\\[-1pt]AQFT: III$_1$};
  \draw[nordiskB, ->, thick, dashed] (0.12,0.12) to[bend left=30] node[above, font=\scriptsize, align=center] {+ constraint\\[-2pt]+ observer} (1.5,0.12);
\end{tikzpicture}
\end{column}
\begin{column}{0.60\textwidth}
\begin{itemize}
\item $T(G)$, $G > 0$: not known. But any $T(G)$ is diffeomorphism invariant, so the isometries are constraints
\item The limit: the fluctuations vanish, the constraint does not. So $\lim_{G\to 0} T(G) \neq T(0)$
\item $T(0)$ is the base; diffeomorphism invariance is the bridge principle (cf. Einstein)
\item Higher orders in $G$: open
\end{itemize}
\end{column}
\end{columns}
```

# Gravity against clean factorization

- Type I: a region is a tensor factor, with a clean cut at its edge (lattices, cutoffs, qubits)
- Gravity: an operator in a region carries its gravitational field to infinity, or to an observer
- Donnelly & Giddings: tensor factorization, and nets of commuting subalgebras, "conflict with basic gravitational properties" at leading order
- CLPW, at leading order: type II, not type I
- Which idealization should we suspect: the continuum, or the tensor factor?

# What I took from AQFT, and what survived

Structure, not details: what I took from AQFT, tested at the first order in $G$

| AQFT | After gravity, leading order |
|---|---|
| Modular theory | Survives: modular flow = time builds the crossed product |
| Not type I | Survives: no normal product state across $\mathcal A$, $\mathcal A'$ |
| Type III$_1$ | Changes: type II$_1$ |
| Algebras of regions | Dressed to an observer |
| Superselection sectors | Open |

The coarse structure carried the derivation; the detail changed

## Lessons

# Lesson 1: information from AQFT transfers

- Can information from AQFT be transferred to a more accurate theory?
- Yes. Witten et al.: type III$_1$ + modular flow = time + gravity's constraint + an observer with $q \geq 0$ $\Rightarrow$ type II$_1$, $S = S_{\text{gen}}$
- If UV structure is "a mathematical artefact" (Wallace), so is type III. Then what did Witten et al. derive from?
- AQFT is not the final ontology, but it is what you perturb from

# Lesson 2: what naturalized metaphysics can take from this

- What transfers is structure, not only numbers. Wallace: QFT gives structure above the Planck length. CLPW: at horizon scales the structure changes, while the numbers barely move
- Look at sequences of theories and the relations between them. The RG changes the scale; Witten et al. switch on a coupling: old structure + a new principle, as with Einstein's Newtonian limit and Bohr's correspondence principle
- Which routes we value depends on what we think theories are. A mirror must be the mathematics closest to the truth (for Wallace, the cutoff theory); a tool can be chosen for the reasoning it enables. Naturalized metaphysics without the final model

# Where next?

- Each stage added nuance to the last: a field of operators; a net of algebras; effective theories related by the RG (Wallace et al.); algebras with observers and constraints
- To the next generation: don't abandon the operator-algebraic approach; go beyond its earlier forms
- Observers, constraints, crossed products; what survives at higher orders in $G$?

# References (1)

- Baker, D. J. 2009. Against field interpretations of quantum field theory. *Brit. J. Phil. Sci.* 60: 585--609.
- Bohr, N. 1953. Physical science and the study of religions. In *Studia Orientalia Ioanni Pedersen Septuagenario*, 385--390. Copenhagen: Munksgaard.
- Chandrasekaran, V., R. Longo, G. Penington and E. Witten. 2023. An algebra of observables for de Sitter space. *JHEP* 2023(02): 082.
- Donnelly, W. and S. B. Giddings. 2016. Diffeomorphism-invariant observables and their nonlocal algebra. *Phys. Rev. D* 93: 024030.
- Fraser, J. D. 2018. Renormalization and the formulation of scientific realism. *Phil. Sci.* 85: 1164--1175.
- Høffding, H. 1905. Filosofien og Livet (1901). In *Mindre Arbejder. Anden Række*, 1--20. Copenhagen: Gyldendal.

# References (2)

- Halvorson, H. and R. Clifton. 2002. No place for particles in relativistic quantum theories? *Phil. Sci.* 69: 1--28.
- Ruetsche, L. 2018. Renormalization group realism: the ascent of pessimism. *Phil. Sci.* 85: 1176--1189.
- Ruetsche, L. 2026. *The Physics of Ignorance*. Oxford: Oxford University Press.
- Sider, T. 2011. *Writing the Book of the World*. Oxford: Oxford University Press.
- Teller, P. 1995. *An Interpretive Introduction to Quantum Field Theory*. Princeton: Princeton University Press.
- Wallace, D. 2006. In defence of naiveté: the conceptual status of Lagrangian quantum field theory. *Synthese* 151: 33--80.

# References (3)

- Wallace, D. 2011. Taking particle physics seriously: a critique of the algebraic approach to quantum field theory. *Stud. Hist. Phil. Mod. Phys.* 42: 116--125.
- Wallace, D. 2022. Stating structural realism: mathematics-first approaches to physics and metaphysics. *Phil. Perspectives* 36: 345--378.
- Williams, P. 2019. Scientific realism made effective. *Brit. J. Phil. Sci.* 70: 209--237.
- Witten, E. 2023. Algebras, regions, and observers. arXiv:2303.02837.

## Appendix

# Where AQFT sits

```{=latex}
\begin{center}
\begin{tikzpicture}[scale=1.05, >=Stealth]
  % axes
  \draw[nordiskMuted, ->] (0,0) -- (6.5,0) node[right, font=\small] {cutoff length $a$};
  \draw[nordiskMuted, ->] (0,0) -- (0,3.8) node[above, font=\small, align=center] {$G_N$, relative to the size of the universe ($\ell_P/r_{dS}$)};
  % physical regime
  \draw[nordiskMuted, dashed, thick, domain=0:5.4, samples=40] plot (\x,{0.07*\x*\x});
  \node[nordiskMuted, font=\footnotesize, align=left, anchor=west] at (5.5,1.95) {physical regime:\\ $a \sim \ell_P \propto \sqrt{G_N}$};
  \fill[nordiskMuted] (4,1.12) circle (2.2pt);
  \draw[nordiskMuted, dotted, thick] (4,1.05) -- (4,0.1);
  \node[nordiskMuted, font=\footnotesize, anchor=west] at (4.08,0.6) {gravity as cutoff only};
  % Wallace--Williams
  \fill[nordiskB] (4,0) circle (2.6pt);
  \draw[nordiskB, line width=1.8pt, ->] (4.15,0) -- (5.9,0);
  \node[nordiskB, font=\footnotesize] at (5.0,-0.3) {RG flow};
  \draw[nordiskB, dashed, thick, ->] (3.85,0) -- (0.25,0);
  \node[nordiskB, font=\footnotesize] at (2.0,-0.3) {continuum limit?};
  \node[nordiskB, font=\footnotesize, align=center] at (3.1,-1.05) {Wallace, Williams: gravity enters only as the cutoff, $a \sim \ell_P$\\ finitely many degrees of freedom per region: type I};
  % Witten et al.
  \draw[nordiskA, line width=1.8pt, ->] (0,0.12) -- (0,3.1);
  \node[nordiskA, font=\footnotesize, align=left, anchor=west] at (0.25,2.95) {Witten et al.: expand in $G_N$, starting from AQFT\\ constraint and observer: type II at leading order};
  % AQFT
  \fill[nordiskMuted] (0,0) circle (2.4pt);
  \node[font=\footnotesize, anchor=north east] at (-0.08,-0.08) {AQFT: III$_1$};
\end{tikzpicture}
\end{center}
```
