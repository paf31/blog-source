---
title: Mutually Recursive Types and Functions
author: Phil Freeman
date: 2011/02/16
description: Encoding mutually recursive least and greatest fixed points in C#
tags: CSharp, Recursion
---

The methods used in my previous posts (here and here) to encode least and greatest fixed points using universal and existential types can be extended to encode least and greatest fixed points of functors on product categories which correspond to mutually recursive types. Catamorphisms and anamorphisms for these types are mutually recursive functions.

## Example

If $\mathbb{C}$ is our usual category of types then we work over $\mathbb{C}^2$, the category whose objects are pairs of objects of $\mathbb{C}$ and whose morphisms are pairs of morphisms of $\mathbb{C}$ between the appropriate objects.

Consider the endofunctor on  $\mathbb{C}^2$ given by $F (X, Y) = (1 + Y, X)$

The least fixed point of this functor is a pair of types $(\text{Even}, \text{Odd})$ such that

$\begin{array}{ccc}
  \text{Even} & = & 1 + \text{Odd} \\
  \text{Odd}  & = & \text{Even}
\end{array}$

We can interpret this pair of types as the even and odd natural numbers.

We can express the functor $F$ as a pair of functors in the category of types $\mathbb{C}$:

    class F1<X, Y>
    {
        public bool IsZero { get; set; }

        public Y Pred { get; set; }

        public F1() { IsZero = true; }

        public F1(Y pred)
        {
            IsZero = false;
            Pred = pred;
        }
    }

    class F2<X, Y>
    {
        public X Pred { get; set; }

        public F2(X pred) { Pred = pred; }
    }

Now, using the encoding $\mu F = \forall T. (F T \rightarrow T) \rightarrow T$ and the fact that an arrow in $\mathbb{C}^2$ is just a pair of arrows in $\mathbb{C}$, we have the definition:

    interface Even
    {
        X Cata<X, Y>(Func<F1<X, Y>, X> f1, Func<F2<X, Y>, Y> f2);
    }

    interface Odd
    {
        Y Cata<X, Y>(Func<F1<X, Y>, X> f1, Func<F2<X, Y>, Y> f2);
    }

Similarly, using the encoding for greatest fixed points:

$\begin{array}{ccc}
  \nu F & = & \exists T. T \times (T \rightarrow F T) \\
        & = & \forall R. (\forall T. T \rightarrow (T \rightarrow FT) \rightarrow R) \rightarrow R
\end{array}$

we have the definitions

    interface CoEvenFunc<R>
    {
        R Apply<X, Y>(X x, Func<X, F1<X, Y>> f1, Func<Y, F2<X, Y>> f2);
    }

    interface CoOddFunc<R>
    {
        R Apply<X, Y>(Y y, Func<X, F1<X, Y>> f1, Func<Y, F2<X, Y>> f2);
    }

    interface CoEven
    {
        R Apply<R>(CoEvenFunc<R> f);
    }

    interface CoOdd
    {
        R Apply<R>(CoOddFunc<R> f);
    }

The type of coeven numbers looks like the type of even numbers with an additional value of infinity. Similarly, the coodd numbers have an additional infinite value, whose successor is the coeven infinite value and vice versa.

A catamorphism or anamorphism for these types makes the mutual recursion obvious:

    Func<Even, int> ToInt =
        e => e.Cata<int, int>(
            f1 => f1.IsZero
                ? 0
                : f1.Pred + 1,
            f2 => f2.Pred + 1);

    Func<int, CoEven> FromInt =
        n => CoNat.Ana<int, int>(n,
            n1 => n1 == 0
                ? new F1<int, int>()
                : new F1<int, int>(n1 - 1),
            n2 => new F2<int, int>(n2 - 1));
