---
title: A Lemma on Fixed Points
author: Phil Freeman
date: 2011/01/27
description: An equation involving fixed points of functors.
tags: Type Theory, Recursion
math: yes
---

I was reading some of my older notes on least fixed point types and found this. It\'s hardly groundbreaking I\'m sure you\'ll agree, but I thought it was quite a nice little result.

## Lemma

Suppose

$F : (X, Y) \rightarrow (F_1 X, F_2 (X, Y))$

is a functor with an initial algebra. Then the initial algebra is isomorphic to

$\mu_F \cong (\mu F_1, \mu (X \rightarrow F_2 (\mu F_1, X)))$

when all defined terms exist.

## Example

Let $F (X, Y) = (1 + X, 1 + X \times Y)$.

By the lemma we have

$\mu F = (\mu (X \rightarrow 1 + X), \mu (Y \rightarrow 1 + Y \times \mu (X \rightarrow 1 + X)))$

That is, $\mu F = (\mathbb{N}, [\mathbb{N}])$.

## Proof

We show that the term on the right of the isomorphism relation is an initial $F$-algebra. Then, by uniqueness, it must be isomorphic to $\mu F$.

Let

$\chi (X) = F_2(\mu F_1, X)$

First we show that $(\mu F_1, \mu \chi)$ is an $F$-algebra. The algebra morphism is

$\begin{array}{ccc}
  F (\mu F_1, \mu \chi) & =           & (F_1 \mu F_1, F_2 (\mu F_1, \mu \chi)) \\
                        & =           & (F_1 \mu F_1, \chi (\mu \chi)) \\
                        & \rightarrow & (\mu F_1, \mu \chi)
\end{array}$

where the two components of the final map are the initial algebra morphisms for $\mu F_1$ and $\mu \chi$.

It is required to prove that $(\mu F_1, \mu \chi)$ is an initial algebra, that is, for any $F$-algebra $F A \rightarrow A$, there is a unique morphism of algebras $(\mu F_1, \mu \chi) \rightarrow A$.

We seek morphisms $\mu F_1 \rightarrow \pi_1 A$ and $\mu \chi \rightarrow \pi_2 A$ such that the following diagrams commute:

$\require{AMSmath}
    \newcommand{\ra}[1]{\kern-1.5ex\xrightarrow{\ \ #1\ \ }\phantom{}\kern-1.5ex}
    \newcommand{\ras}[1]{\kern-1.5ex\xrightarrow{\ \ \smash{#1}\ \ }\phantom{}\kern-1.5ex}
    \newcommand{\da}[1]{\bigg\downarrow\raise.5ex\rlap{\scriptstyle#1}}
    \begin{array}{ccc}
      F_1 \mu F_1& \ra{} & F_1 \pi_1 A \\
      \da{\operatorname{out}} & & \da{\phi} \\
      \mu F_1& \ras{} & \pi_1 A
    \end{array}
    \begin{array}{ccc}
      \chi \mu \chi & \ra{} & \chi \pi_2 A \\
      \da{\operatorname{out}} & & \da{} \\
      \mu \chi& \ras{\pi_2 A} & \pi_2 A
    \end{array}$

The first morphism is determined uniquely by the definition of $\mu F_1$.

We just need to show that $\pi_2 A$ is a $\chi$-algebra. Then, the second morphism is given uniquely by the definition of $\mu \chi$.

We have morphisms

$\begin{array}{ccc}
  f       & :: & \mu F_1 \rightarrow \pi_1 A \\
  \pi_2 a & :: & F_2 (\pi_1 A, \pi_2 A) \rightarrow \pi_2 A
\end{array}$

which compose to give

$\begin{array}{ccc}
  \chi \pi_2 A & =           & F_2 (\mu F_1, \pi_2 A) \\
               & \rightarrow & F_2 (\pi_1 A, \pi_2 A) \\
               & \rightarrow & \pi_2 A
\end{array}$

﻿This completes the proof.
