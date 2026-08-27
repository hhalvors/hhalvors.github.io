---
title: "Reference Frames"
author: Hans Halvorson
---

## The dual role of reference frames 

I am, as far as I can tell, always at some particular place, and
always in some particular state of motion. As such, I have a (local)
reference frame, specified by my location and momentum. I might be
able to move to a different place, or change my momentum, but my
reference frame -- in this sense --- is a consequence of my physical
state.

There is another sense in which my reference frame is *not* a
consequence of my physical state. In particular, when solving a
physics problem, I can choose a reference frame. Similarly, when
describing an event, I can choose a reference frame from which to
describe it. I don't need to be in that place or state of motion to
describe things as if I were. In this sense, reference frames are a
conceptual tool --- something we use to see the world rather than a
description of our place in the world.

We might say then that the concept of a reference frame is both
physical (determined by one's current state) and semantic (a tool for
describing and sharing information). In this sense, the notion of a
reference frame might be an extension or precisification of the notion
of a *perspective*. When I say "my perspective", I might simply mean
where I'm sitting and where my head is turned --- as in, e.g., "from
my perspective, the Statue of Liberty is not visible." But I can also
use "my perspective" to mean the way that I conceive things. For
example, I might say "my perspective on US politics is extremely
grim", which isn't meant to tell you anything much about my current
physical state.

### The physical notion of reference frame: relationalism


### The semantic notion of reference frame: contextualism


## The mathematics of reference frames

The notion of a frame of reference has played an important role in
physics. However, the very power and richness of the notion makes it
subject to abuse. For example, it is common for people to impose a
sort of criterion of reality on objects that they must be "independent
of reference frame". But what in the world is that even supposed to
mean?

In the tradition of 20th century analytic philosophy, we can try to
improve conceptual clarity by means of *explication*. To explicate a
concept means something like providing a good mathematical
(structural) representative of that concept. For example, we have an
intuitive notion of infinity, and an explication of that concept is
provided by the mathematical notion of injective and surjective
functions: a set $S$ is infinite just in case there if a function
$f:S\to S$ that is injective but not surjective.

So what then are some mathematical things that play the role of the
intuitive concept of a reference frame? There are several
inter-related candidates, and only some of these candidates generalize
to the contemporary setting (where we can't always depend on having
global coordinate charts).

- In Galilean relativity and special relativity, we can represent a
  reference frames as an (equivalence classes of) pairs consisting of
  a position (element of the manifold) and velocity (element of the
  tangent space at that point). In this case, $(p,\vec{v})$ uniquely
  determines a timelike line (geodesic) in the relevant spacetime. And
  each single timelike geodesic uniquely determines an entire family
  of parallel geodesics. In the case of Galilean spacetime, the
  spatial hypersurfaces are already determined, and do not depend on
  the frame of reference. But in Minkowski spacetime, there is a
  unique spatial hypersurface orthogonal to $\vec{v}$.
  
- In Galilean and special relativity, an instantaneous state
  $(p,\vec{v}\in T_p)$ also picks out a "preferred" coordinate
  system. The mathematics here is straightforward. What is not
  straightforward is what "preferred" is supposed to mean.
  
- So long as our spacetime theory uses a manifold $M$ and a notion of
  permitted trajectories of material objects, then we can say that a
  reference frame is a family of such lines that fill space.   
  
- In GTR, a pair $(p,\vec{v}\in T_p)$ could also be said to be a
  reference frame. But now the notion is being stretched, because ...
  
- There's another mathematical idea that is (indirectly) related to
  reference-frame dependence: basis-dependent representations of
  tensorial objects. For example, lqet $v$ be a vector in an
  $n$-dimensional vector space $V$ over $\mathbb{R}$. Relative to a
  basis, $v$ is represented as an $n$-tuple of real numbers. This well
  known fact generalizes to all tensors over $v$, and it seems to have
  led to quite a bit of confusion about how differential geometry
  functions. In particular, it is not uncommon to hear talk about
  "non-tensorial quantities", as if there were some kind of space of
  quantities, some of which are tensorial and some of which are
  not. But in most such cases, the so called non-tensorial quantities
  are simply basis-dependent representations of tensors.
  
- Basis-dependent representations of tensors are closely related to
  coordinate-dependent representations of tensor fields. In
  particular, let $p\in M$, and let $(U,\varphi )$ be a coordinate
  chart such that $p\in U$. This chart then defines a basis on $T_p$
  relative to which tensors over $T_p$ have a numerical
  representation. However, this numerical representation doesn't have
  any immediate ontological or epistemological significance --- at
  least not until one says something more.



### Reference frames and coordinates 
