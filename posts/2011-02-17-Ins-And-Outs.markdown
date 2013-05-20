---
title: The Ins and Outs of Fixed Point Types
author: Phil Freeman
date: 2011/02/17
description: The algebra and coalegbra morphisms for least and greatest fixed point types
tags: Recursion
math: yes
---

The purpose of this note is to derive the $F$-(co-)algebra structures on the least and greatest fixed points of $F$ expressed in terms of universal and existential types. The method also extends to mutually recursive types by virtue of the fact that these types are the fixed points of functors on the power category $\mathbb{C}^n$ for some $n$, as explained in my last post.

## Least Fixed Points

We defined the least fixed point of $F$ by identifying an inhabitant with its fold function `cata`:

$\mu F = \forall T. (F T \rightarrow T) \rightarrow T$

We are also given a mapping on arrows:

$\operatorname{fmap} :: \forall S T. (S \rightarrow T) \rightarrow FS \rightarrow FT$

The goal is to define an algebra structure and a coalgebra structure on this type:

$\begin{array}{ccc}
  \operatorname{in}  & :: & \mu F   \rightarrow F \mu F\\
  \operatorname{out} & :: & F \mu F \rightarrow   \mu F
\end{array}$

To define the algebra structure `out`, we use `fmap` to lift `cata` up one level in the structure:

> out x = { \\T. \\seed. seed (fmap (\\y. cata[T] y seed) x) }

To define the inverse `in`, observe that since $\operatorname{out} \circ \operatorname{in} = \operatorname{id}$, we have

> in = fmap out . fmap in . in = cata fmap out

by the universal property of `cata`.

## Greatest Fixed Points

The case of greatest fixed points is exactly dual. We defined the greatest fixed point as

$\begin{array}{ccc}
  \nu F & = & \exists T. T \times (T \rightarrow F T) \\
        & = & \forall R. (\forall T. T \rightarrow (T \rightarrow FT) \rightarrow R) \rightarrow R
\end{array}$

Here we have constructed $\nu F$ by identifying an inhabitant with its unfold function `ana`: 

$\operatorname{ana} :: \forall T. T \rightarrow (T \rightarrow FT) \rightarrow \nu F$

Again we want to define algebra and coalgebra structures:

$\begin{array}{ccc}
  \operatorname{in}  & :: & \nu F   \rightarrow F \nu F\\
  \operatorname{out} & :: & F \nu F \rightarrow   \nu F
\end{array}$

This time we will start with the coalgebra structure. The only way we can get something of type $F \nu F$ is to unpack the existential type:

> in x = let x = (T, t, f) in fmap (\\y. ana y f) (f t)

To define the map `out`, we reason as follows:

> out = out . fmap out . fmap in = ana fmap in

by the universal property of `ana`.

## Mutually Recursive Types

The methods above continue to work in the power category $\mathbb{C}^n$ and so by replacing functors and arrows in the power category with tuples of functors and tuples of arrows in the category $\mathbb{C}$, we can express mututally recursive types and functions in the category $\mathbb{C}$.

The second example below shows how this can be done for the case of the (co-)even and (co-)odd natural numbers defined as a mutually recursive pair of types.

Using just the methods `cata`, `ana`, `in` and `out` we can build a large collection of library methods manipulating odd/even (co-)naturals. For example, addition and multiplication of even numbers can be defined by

> add e1 e2 = cata e1 <const e2, out1> out2
> 
> mult e1 e2 = cata e1 <zero, add (const e2 inr)> add e2

where `zero = out1` inl and `out1` and `out2` denote the two parts of the morphism out in the category $\mathbb{C}^2$.

Alternatively, in C#, using the definitions given in the example code below, we could write

    Func<Even, Even, Even> add = (e1, e2) => e1.Cata<Even, Odd>(
        f1 => f1.IsZero
            ? e2
            : Nat.Out1(f1),
        Nat.Out2);

    Func<Even, Even, Even> mult = (e1, e2) => e1.Cata<Even, Even>(
        f1 => f1.IsZero
            ? zero
            : add(e2, f1.Pred),
        f2 => add(e2, f2.Pred));
