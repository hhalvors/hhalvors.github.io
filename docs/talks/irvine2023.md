---
author: Hans Halvorson
date: October 27, 2023
title: "What Models Say"
theme: "Copenhagen"
---

# Introduction

## Using models 

- **Fact 1:** Contemporary physics uses (mathematical) models to
  represent physical reality.
  
- **Fact 2:** Contemporary physics uses language together with these
  models.
  
  - Language is used to specify what the models are.

  - Language is used to specify which parts of the models are
    representationally significant.


## Using models

- Semantic view: To accept a theory is to believe that one if its
  models represents the intended object or system.

- No. "$M$ represents" is not specific enough. 

  - Example: "$42$ represents." 
  
  - Example: "$\mathbb{R}$ represents." 


## Artifact versus Content


- mention versus use


# Three examples 


## Features of the wavefunction

> "On the Ghirardi-Rimini-Weber (GRW) theory (or, for that matter, on
> any theory of collapse), the world will consist of exactly one
> physical object—the universal wave function. What happens, all that
> happens, is that the function changes its shape in accord with the
> theory’s dynamical laws."


## Features of the wavefunction 

- Which **features** of a wavefunction belong to its representational
  content, and which features are representational artifacts?
  
- Some might say: It's not a good example, since the wavefunction
  formalism hasn't yet undergone a Hartry-Field-like purification
  process.
  
- Wavefunction realists should grant that some features of the
  wavefunction are artifacts.
  
  - "Thomas is thinking about $\psi$" is an artifact.
  
  - "The value of 
	

## Features of the wavefunction

- Real features of $\psi$ 

  - Expectation values
  
	  - But: even expectation values are not invariant under all
        isomorphisms, e.g. the isomorphism that takes "position is
        represented by the operator $Q$" to "position is represented
        by the operator $P$".
		
- Pseudo-features of $\psi$ 

  - $\psi$ is a subset of $\mathbb{R}^{3n}\times \mathbb{C}$, and
    there is a $c\in\mathbb{C}$ such that $\langle \emptyset
    ,c\rangle\in \psi$.
  
  - $\psi$ is represented by "$\psi$"
  
  
  
## Features of the wavefunction

- Possibly real features of $\psi$

  - The value of $\psi$ at a particular point $a\in X$
  
    - What about Lebesque measure zero sets? 
  
    - What about $U(1)$ gauge freedom?
	
  - The value of $d\psi /dx$ at a particular point $a\in X$

  
  
## Reflection

- There's a sense in which a pseudo-feature of $\psi$ is not a feature
  of $\psi$, but a feature of something related to $\psi$, e.g. a
  person's mental representation of $\psi$


## Features of a group

- Real features of $G$ 

  - Cardinality
  
  - Abelian or non-abelian
  
  - Cyclic of order $n$
  
- Pseudo-features of $G$

  - $G$ contains some particular element $a$.
  
  - $G=G$ and the continuum hypothesis is true.
  


## Features of a group
  
- Real features of $G$: liberalized

  - $G$ has $n$ normal subgroups.

  - $G$ has $2$ irreducible representations.
  
 
  


## Features of a spacetime

- Pseudo-features of $(M,g_{ab})$

  - The value of the scalar curvature at a particular point $a\in M$

- Real features of $(M,g_{ab})$

  - The scalar curvature has no upper bound.
  
  
## Features of a spacetime

- Possibly real features of $(M,g_{ab})$

  - Inextendible? 
  

## Note

- Do physicists sometimes take models with different features to be
  representationally equivalent?
  
- Yes, if we take "features" in a liberal sense.

  - Example: distinct groups that are isomorphic
  
- Is there a more refined sense of "features" such that models are
  representationally equivalent only if they have the same features?
  
  - Example: if $T_1$ and $T_2$ are Morita equivalent, then for each
    model $M$ of $T_1$ there is a **Morita twin** model $N$ of $T_2$.
	
  - Example: Hamiltonian and Lagrangian mechanics
  
  - Example: Spacetimes and Einstein algebras


## Proposals 

- For a property $\Phi$ of $M$ to be real, then $\Phi$ must be
  mathematical (in some sense to be made precise).

- For a property $\Phi$ of $M$ to be real, then $\Phi$ must be
  invariant under isomorphisms.
  
  - Problem: But what is an isomorphism? Two mathematical objects can
    be isomorphic relative to one category and non-isomorphic relative
    to another category.
	
  - Example: $(M,g_{ab})$ and $(M,g'_{ab})$


# Features of models 

## Overview

- Even for the special case of a first-order theory $T$, it is not
  clear what counts as a real property of a model $M$ of $T$.
  
  - Extreme conservative: elementary properties
  
  - Moderate conservative: second-order properties
  
  - Moderate liberal: Väänänen's sort logic (designed to account for
    relational properties with other mathematical objects)
  
  - Liberal: set-theoretic predicates
  
- Note: All notions but the last are *signature dependent*.


## Elementary properties of models 

- Sentences of first-order logic provide potential properties of
models: if $M\vDash \phi$, then the property expressed by $\phi$ is
possessed by $M$.

- Example: "$M$ has two elements" is true just in case $M\vDash
\exists _{=2}(x=x)$. 

- Fact: These elementary properties are isomorphism invariant.


## Elementary properties of models

- But there are also (isomorphism invariant) properties of models that
  are not elementary.

  - Example: "$M$ is uncountably infinite."
  
  - Example: "$M$ is a compact topological space."
  
  - Example: "$M$ can be embedded in $\mathbb{R}^4$."  



## Set-theoretic predicates of models 

- Suppes: If $\Phi$ is a predicate in the language of ZF set theory
such that $ZF\vDash\Phi (M)$, then $\Phi$ represents a property of
$M$.

- Most of these set-theoretic predicates are irrelevant for the way
that physicists use $M$ to represent reality.

  - Wager: no physicist will object to your taking an element $a\in M$
    and replacing it with $a'$.



## Relational properties of models 

- Idea 1: A model's relations to other models is an important aspect
  of its use in physics.
  
  - Example: Inextendible spacetimes
  
  - But is it inextendible because of something about its internal
    structure?
  
- Idea 2: A model's symmetries are an important aspect of its use in
  physics.
  
  - Example: In EM, how we understand the content of a model depends
    on what we take to be isomorphisms between models.
	  
  
  
## Relational properties of models 

- Category theory is a first-order theory with two sort symbols $O$
  and $A$, two function symbols $d_0,d_1:A\to O$, a function symbol
  $1:O\to A$, and a partial function symbol $\circ :A\times A\to A$.
  
- A model $\mathbf{C}$ of category theory consists of two sets
  $\mathbf{C}_0$ and $\mathbf{C}_1$.

- If we take a property of $a\in \mathbf{C}_0$ to be a sentence $\phi
  (x)$ such that $\mathbf{C}\models \phi (a)$, then the typical
  property of objects are relational features such as: 
  
  - Being embeddable in certain other kinds of objects.
  
  - Having a certain number of automorphisms.



## Practical questions 

- What do we take to be satisfactory theoretical achievement in
  physics
  
  - Are we looking for **the one true model**?
  
  - Is the situation suboptimal if there are two distinct
    representations of the same situation?
	
  - Is the situation suboptimal if we have a higher-level theory that
    cannot be syntactically reduced to a more fundamental theory?
	

## Practical questions 

- Does a solution to the measurement problem require "new physics"?

  - The answer depends in part on whether macro-reality is
    **reducible** to the wavefunction.
	
  - Idea: Bohmians have a language-first notion of reduction.
	

## The language-first police 

> "A physical theory should clearly and forthrightly address two
> fundamental questions: what there is, and what it does. The answer
> to the first question is provided by the ontology of the theory, and
> the answer to the second by its dynamics. The ontology should have a
> sharp mathematical description, and the dynamics should be
> implemented by precise equations describing how the ontology will,
> or might, evolve."


## Between language and mathematics 

- The "language first" and "math first" views get closer when we
  realize that:
  
  1. Linguistic notions can be made more precise and more flexible.
	 
	 E.g. translation schemes do not need to match simple object
     descriptors to simple object descriptors.
  
  2. Precise mathematical notions are usually expressible in language.
  
     E.g. sentences about mathematical objects are preserved under
     isomorphism.
  


## Intro 

- The relation between **model** and **world** was hoped to be more
direct and transparent than the relation between language and world.

- This hope proved to be naive.

1. The model $M$ is intended to be isomorphic with the represented
   object.



## Model isomorphism criterion

- Theories $T_1$ and $T_2$ are equivalent only if the models of the
  two correspond one-to-one, where correspondence maintains
  isomorphism.
  
- The model isomorphism criterion is appropriate for those who believe
  that the goal is to find a model that is isomorphic with the
  represented object.


- Against: Even conservative accounts have that theories in different
  languages can be equivalent. But models in different languages
  cannot be isomorphic.
  
- For: Moderate views of theoretical equivalence entail that if $T_1$
  is equivalent to $T_2$, then for each model $M_1$ of $T_1$, there is
  a twin model of $M_2$, and these models can be constructed from each
  other.



## Conceptual housekeeping

- Proposal: As far as physics practice goes, a mathematical object $M$
  has no features in itself.
  
  - A mathematical object has properties *qua* member of a category
    $\mathbf{C}$.
	
	- Properties of "$M$ in $\mathbf{C}$" are invariant under
      isomorphisms in $\mathbf{C}$.
  
  - When physicists use $M$, they keep mental note of the background
    category $\mathbf{C}$.
	
  
  
## Conceptual housekeeping

- Example: Despite the familiar notation, $\mathbb{R}$ is not a well
  defined mathematical object.
  
  - It won't help to say: $\mathbb{R}$ is a generic name for any set
    in a certain isomorphism class.

- Application: The notion of **reduction** is category relative.

  ## Semantics versus Syntax 

- Idea: Used in the right way, mathematics can increase the
  objectivity of our descriptions. Used in the wrong way, mathematics
  obscures our descriptions.
  
  - "There were 42 people" is more objective than "there were a lot of
    people."
  
  - "The world is modelled by 42" is obscure.
  
- Much of mathematics is "new language" in the sense of David Lewis.
  
  - New mathematical objects don't *say* anything until we connect
    them to old language. 
  
  - Mathematical models don't *represent* anything.




