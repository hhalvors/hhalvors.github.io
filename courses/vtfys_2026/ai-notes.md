# Notes for Week 5 AI section

Hey Claude: when we get to the part where we explain DNNs (based on
transformer models), please help me create an illustrative Tikz
picture. I want to clarify the sense in which these 'models' are
opaque to humans.

Here are my notes for the AI section. The students were assigned to
read the article "Understanding from Machine Learning Models" by Emily
Sullivan; so I'll spend most of the time interacting with that
article.

- Confession: I'm an expert on the foundations of QM, but not on
  AI. So this part of the lecture is "bikset sammen"
  
- Two perspectives on AI: theoretical and practical. 

  1. Theoretical: Videnskabsteori has traditionally been focused on
     *scientists* as cognitive agents. These agents have always used
     tools to increase understanding. But now we have tools that seem
     borderline intelligent themselves. Does that change our
     understanding of the task of science?
	 
  2. Practical: Science produces (at least) two kinds of goods: (a)
     Intrinsic: The goods it gives back to the scientists and their
     lives in the form of helping them to fulfill their human
     potential. (b) Extrinsic: The goods it produces for our material
     lives and the economy. Use of science in AI threatens to make the
     second kind of good completely enerådig. 
	 
	 What do we want science to be? It is partly up to us! 
	 
- A fact about AI, especially Deep Neural Networks

  Opacity / Black Boxing: Even experts don't know how they work
  
  Mismatch between human reasoning and the path that DNNs take from
  inputs to outputs
  
- Note for later  

  One challenging thing about Sullivan's article is that the word
  "model" is doing double duty: 
  
  1. Models in the traditional sense used in scientific practice
  
  2. Models in the sense of LLMs


## The double meaning of "model"

One terminological trap in Sullivan's article (and in the AI-and-science literature
generally): the word **model** is doing double duty.

### Sense 1: scientific model (philosophy of science)

A representation of a target system that has *interpretable content*.

- Picks out certain features of the world, idealizes others away
- States a mechanism, a structure, a reason *why*
- Humanly legible: scientists can inspect it and read off qualitative consequences
  without full calculation (De Regt & Dieks's CIT)
- Propositional or mathematical in a way that supports inference and explanation
- Examples: kinetic theory of gases, Schwarzschild metric, BCS theory

Key point: a scientific model *says something about what is going on in the world*.

### Sense 2: ML model (statistics / machine learning)

A parameterized mathematical function fitted to data by optimization.

- A very large composition of linear transformations and nonlinearities
- "Knowledge" is distributed across billions of numerical weights — not humanly readable
- Does not represent a target system in any interpretable way
- Maps inputs to outputs; does not say *why* the mapping is what it is
- Examples: AlphaFold, GPT, neural weather models

Key point: an ML model is the *output of a fitting procedure*, not the result of theorizing.

### The historical slide

The word "model" in ML comes from statistics, where "the model" originally *did* have
interpretable content (a linear model, a Gaussian model — both make readable claims
about a data-generating process). Neural networks inherited the terminology but
quietly abandoned the interpretability as models grew larger. The slide from Sense 1
to Sense 2 is itself philosophically significant.

### Sullivan's link uncertainty — restated with the distinction

Sullivan's concept of **link uncertainty** is essentially asking: is the Sense 2 model
connected to a Sense 1 model in any identifiable way? If we know *which features* of
the training data ground the ML model's predictions, we can begin to extract a
scientific model from it. If we don't — if the link is uncertain — the ML model gives
us how-possibly understanding at best, never how-actually.

### Two versions of the black-box problem

- **AlphaFold**: the *honest* case. Makes no claim to understand; produces a 3D
  structure. No pretense of explanation.
- **LLMs**: the *deceptive* case. Produce output with the surface grammar of
  Sense 1 explanation — chains of reasons, causal language, DN-style inference.
  Have been trained on physics textbooks and can reproduce Mode 1 (propositional)
  explanations. Whether any anskuelig representation underlies the output is
  an open question. The form of understanding is mimicked without its substance
  being obviously present.

### Architecture note (for accuracy)

Both AlphaFold 2 and modern LLMs are largely **transformer-based** (attention
mechanism, Vaswani et al. 2017). AlphaFold 2 uses a transformer-like Evoformer plus
a Structure Module; it is a hybrid rather than a pure transformer. The shared
architecture reinforces that the black-box problem is not a quirk of any particular
design — it is a general feature of large learned models regardless of domain.

### Payoff for the slides

The disambiguation matters because students may assume an ML "model" does what a
scientific model does — provides a window into the mechanism. Making the distinction
explicit shows that using the same word for both things conceals the very question
Sullivan is trying to raise: can a Sense 2 model ever do the explanatory work of a
Sense 1 model?




# What is Sullivan's question? 

  (p 110) "Are scientists trading understanding for some other
  epistemic or pragmatic good when they choose an opaque and complex
  machine learning model?"
  
  Discuss
  

# What is Sullivan's point? And so what?

  1. Black-boxing does not necessarily undermine understanding

  2. Link Uncertainty does undermine understanding
  
  "It is not implementation black-boxing that gets in the way of
  understanding, it is link uncertainty." (p 116)
  
  "When we focus on the types of explanatory questions we can ask of
  the models, the lingering problems for understanding that remain are
  not foremost due to the implementation black boxes, but because
  there is a certain level of link uncertainty." (p 122)
  

# Tracing Sullivan's argument  

"Before considering the complex case of DNNs, it is worthwhile to
first consider a simple case to illustrate the way that models explain
and provide understanding of phenomenon [sic]." (p 111)

## Example: Schelling on segregation

NB: This example is rather different than typical model-based
explanations in physics. It's almost more like an Aristotelian
explanation where the outcome is explained by (a) structural
constraints, and (b) the "strategies" of the actors. 


## Black Boxes

"Black box explanations are commonplace in scientific inquiry." (p
113)

Discuss: Do you think Sullivan is right about this? Can you
distinguish different kinds of black-boxing and which are considered
acceptable in physics?





## The internal/external distinction (Sullivan's blind spot?)

When a physicist derives a result, her brain is — at the physical level — just as opaque as a DNN
If you traced the neural mechanisms, you would not see the reasoning
To understand why she gets the right answers, you have to get inside the structure of the reasoning itself

Leibniz's mill (Monadology, §17): even if the brain were enlarged to the size of a mill and you could walk around inspecting all its machinery, you would never encounter thought anywhere in the mechanism

Sullivan focuses on implementation opacity vs. link uncertainty
But there is a prior question: is there an internal perspective to be had at all?
With the physicist: yes — her reasoning has a logical structure you can inhabit
With a DNN: open question


Does a DNN model have an inside?


## Sullivan's optimism

"So too in the deep patient case, once the link uncertainty is
resolved, the deep patient model is able to explain and enable
understanding of disease development." (p 125)

HH: How would the link uncertainty ever be resolved in this case? What
physical objects or processes would the nodes and arrows in the DNN be
linked to?

## Other examples

Sullivan mentions to other DNN examples, one that she thinks has less
link uncertainty. One that she thinks has more link uncertainty.

(I don't know if there will be time to mention these examples. This a
lot of material to cover in 45 minutes!)

1. Classifying moles as cancerous or not. She says that this model has
   less link uncertainty.

2. Determining sexual orientation based on pictures. She says that
   this model has more link uncertainty. 



## Other questions

### In physics, do we permit models that have elements that do not
refer to something in the world?

- Some people think that the quantum wavefunction is "just a
  calculational device"
  
- Passage to the complex plane in proving theorems in QFT. (Irving
  Segal worried about this)
