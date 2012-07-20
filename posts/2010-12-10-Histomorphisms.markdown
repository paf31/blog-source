---
title: Histomorphisms, Dynamic Programming and the Knapsack Problem
author: Phil Freeman
date: 2010/12/10
---

Histomorphisms generalize the concept of course-of-value iteration to arbitrary recursive data types. They are useful for expressing solutions to problems which depend optimally on solutions to structurally smaller problems. That is, histomorphisms are useful for applying the concepts of dynamic programming over recursive datatypes.

Using some the methods in my previous posts, we can express histomorphisms in C# over a fixed recursive data type in a type-safe way. In a language such as Haskell, we can generalize the method so that it applies to any recursive data type, but in C# the lack of type-level functions prevents us from doing so.

First, a little theory. Recall the definition of catamorphism as the unique $F$-algebra morphism from the initial $F$-algebra:

$\require{AMSmath}
    \newcommand{\ra}[1]{\kern-1.5ex\xrightarrow{\ \ #1\ \ }\phantom{}\kern-1.5ex}
    \newcommand{\ras}[1]{\kern-1.5ex\xrightarrow{\ \ \smash{#1}\ \ }\phantom{}\kern-1.5ex}
    \newcommand{\da}[1]{\bigg\downarrow\raise.5ex\rlap{\scriptstyle#1}}
    \begin{array}{ccc}
      F \mu_F& \ra{F (|\phi|)} & F A \\
      \da{in} & & \da{\phi} \\
      \mu_F& \ras{(|\phi|)} & A
    \end{array}$

We want to use answers to subproblems to generate﻿ the solution to the full problem. That is, we want to use the values of $(|\phi|) x$ for structurally smaller values $x$ of $\mu_F$.

In order to use intermediate results, we replace the recursive data structure with one which is structurally similar, but which also stores the partially calculated results. That structure is given by the cofree comonad over $F$, denoted here $\operatorname{CoFree}_F$:

$\operatorname{CoFree}_F X = \nu ( Y \rightarrow X \times F Y )$

Here, we\'ve replaced the functor $F$ with the functor $Y \rightarrow X \times F Y$, which carries an extra $X$ at each step of the recursion.

We can think of an element of $\operatorname{CoFree}_F X$ as an $X$-annotated version of the recursive data type $\mu_F$.

The diagram above now becomes:

$$\require{AMSmath}
    \newcommand{\ra}[1]{\kern-1.5ex\xrightarrow{\ \ #1\ \ }\phantom{}\kern-1.5ex}
    \newcommand{\las}[1]{\kern-1.5ex\xleftarrow{\ \ \smash{#1}\ \ }\phantom{}\kern-1.5ex}
    \newcommand{\ras}[1]{\kern-1.5ex\xrightarrow{\ \ \smash{#1}\ \ }\phantom{}\kern-1.5ex}
    \newcommand{\da}[1]{\bigg\downarrow\raise.5ex\rlap{\scriptstyle#1}}
    \begin{array}{ccc}
      F \mu_F & \ra{F (|\epsilon|)} & F \operatorname{CoFree}_F X & \ra{[\phi, id]} & X \times F \operatorname{CoFree}_F X \\
      \da{\operatorname{in}} & & \da{\epsilon} & & \da{id} \\
      \mu_F & \ras{(|\epsilon|)} & \operatorname{CoFree}_F X & \las{\operatorname{out}} & X \times \operatorname{CoFree}_F X
    \end{array}$$

The left square is a catamorphism, where the algebra morphism  is defined by the commutativity of the right square.

Composing with the map $extract :: \operatorname{CoFree}_F X \rightarrow X$ gives the desired map histo $\phi :: \mu_F \rightarrow X$.

$\operatorname{histo} \phi$ is given by following the arrows around the top of the diagram: first we apply the histomorphism to a structurally smaller value of $$\mu F$$ before using $\phi$ to accumulate results from structurally smaller values, saving the result along with previous results, and then extracting the final result from the comonad.

Now, let\'s express this in C#. Note, the lack of memoized functions in C# makes the following implementation non-optimal. It is provided simply to illustrate the ideas presented above.

We are going to solve the knapsack problem,  a problem which is particularly amenable to a dynamic programming solution, and whose solution is expressible as a histomorphism. The problem is stated as follows: given a collection of $n$ objects of weights $w_1, \dots, w_n$ and values $v_1, \dots, v_n$, find the maximum total value $\operatorname{KS}(W)$ of a set of objects which can be carried in a knapsack capable of carrying a maximum weight $W$.

Notice that $\operatorname{KS}(0)=0$, being the total value of the only set of objects which can be carried in a knapsack of maximum weight zero, the empty set.

Also, for $W \lt 0$, we can calculate $\operatorname{KS}(W)$ based on $\operatorname{KS}(n)$ for $n \lt W$, as follows:

$\operatorname{KS}(W) = ( \operatorname{KS}(W - 1), v1 + \operatorname{KS}(W - w1), \dots, vn + \operatorname{KS}(W - wn) )$

The domain of our histomorphism is going to be the natural numbers, the least fixed point of the functor $F X = 1 + X$. The codomain will be the primitive C# type int. First, let\'s define the natural numbers:

    class NatF<T>
    {
        public Either<Unit, T> Data;
    }

    interface Nat
    {
        T Cata<T>(Func<NatF<T>, T> f);   
    }

We also need to define a data type representing the cofree comonad over F. Using the encoding of greatest fixed points described here, we have the following:

    class CoFreeNatF<L, T>
    {
        public L Leaf;
        public NatF<T> Data;   
    }

       
    class ECoFreeNatF<L, T>
    {
        public T Seed;
        public Func<T, CoFreeNatF<L, T>> Generator;
    }

       
    interface CoFreeNatFunc<L, R>
    {
        R Apply<T>(ECoFreeNatF<L, T> value);
    }

       
    interface CoFreeNat<L>
    {
        R Apply<R>(CoFreeNatFunc<L, R> f);
    }

Here we\'ve given the name `CoFreeNatF` to the functor defining the cofree comonad over $F$, and `ECoFreeNatF` to the term $X \times (X \rightarrow \operatorname{CoFree}_{Nat} X)$ appearing inside the existential type defining `CoFreeNat` as a greatest fixed point type.

With those definitions, and the definition of a catamorphism given here, we can define a histomorphism as follows:

    public static Func<Nat, L> Histo<L>(Func<NatF<CoFreeNat<L>>, L> histoSeed)
    {
        Func<NatF<CoFreeNat<L>>, CoFreeNat<L>> seed =
            leafy => new CoFreeNatF<L, CoFreeNat<L>>
            {
                Leaf = histoSeed(leafy),
                Data = leafy
            }.Out();

        return n => n.Cata<CoFreeNat<L>>(seed).Extract<L>();
    }

To give this some meaning, we need some utility methods in order to extract data from the comonad. In particular, we are missing the definition of the functions

> out :: CoFreeNatF L (CoFreeNat L)-> CoFreeNat L

and

> extract :: CoFreeNat L -> L

$\operatorname{out}$ lets us combine a previously calculated value and a value in the comonad to give a new value in the comonad. It is the arrow on the bottom of the right hand square in the diagram above.

We have to divide into two cases, the zero case and the successor case. Here we use the class names Node and Chain respectively:

    class Node<L> : CoFreeNat<L>
    {
        private L leaf;

        public Node(L leaf)
        {
            this.leaf = leaf;
        }

        public R Apply<R>(CoFreeNatFunc<L, R> f)
        {
            return f.Apply<Unit>(new ECoFreeNatF<L, Unit>
            {
                Seed = new Unit(),
                Generator = u => new CoFreeNatF<L, Unit>
                {
                    Leaf = leaf,
                    Data = new NatF<Unit>
                    {
                        Data = Either<Unit, Unit>.inl(new Unit())
                    }
                }
            });
        }
    }

    class ChainFunction<L, R> : CoFreeNatFunc<L, R>
    {
        private L leaf; private CoFreeNatFunc<L, R> f;

        public ChainFunction(L leaf, CoFreeNatFunc<L, R> f)
        {
            this.leaf = leaf; this.f = f;
        }

        public R Apply<T>(ECoFreeNatF<L, T> value)
        {
            return f.Apply<Either<Unit, T>>(new ECoFreeNatF<L, Either<Unit, T>>
            {
                Seed = Either<Unit, T>.inl(new Unit()),
                Generator = e => e.Case(u => new CoFreeNatF<L, Either<Unit, T>>
                {
                    Leaf = leaf,
                    Data = new NatF<Either<Unit, T>>
                    {
                        Data = Either<Unit, Either<Unit, T>>.inr(Either<Unit, T>.inr(value.Seed))
                    }
                }, t =>
                {
                    var next = value.Generator(t); return new CoFreeNatF<L, Either<Unit, T>>
                    {
                        Leaf = next.Leaf,
                        Data = new NatF<Either<Unit, T>>
                        {
                            Data = Either<Unit, Either<Unit, T>>.inr(next.Data.Data)
                        }
                    };
                })
            });
        }
    }

    class Chain<L> : CoFreeNat<L>
    {
        private L leaf; private CoFreeNat<L> pred;

        public Chain(L leaf, CoFreeNat<L> pred)
        {
            this.leaf = leaf; this.pred = pred;
        }

        public R Apply<R>(CoFreeNatFunc<L, R> f)
        {
            return pred.Apply<R>(new ChainFunction<L, R>(leaf, f));
        }
    }

    public static CoFreeNat<L> Out<L>(this CoFreeNatF<L, CoFreeNat<L>> n)
    {
        return n.Data.Data.Case<CoFreeNat<L>>(unit => new Node<L>(n.Leaf),
            pred => new Chain<L>(n.Leaf, pred));
    } 

We also have the inverse, unrolling one level of the comonad to give a previously-calculated value and another value of the comonad:

    class InFunction<L> : CoFreeNatFunc<L, CoFreeNatF<L, CoFreeNat<L>>>
    {
        public CoFreeNatF<L, CoFreeNat<L>> Apply<T>(ECoFreeNatF<L, T> n)
        {
            var value = n.Generator(n.Seed);

            return new CoFreeNatF<L, CoFreeNat<L>>
            {
                Leaf = value.Leaf,
                Data = value.Data.Data.Case(unit => new NatF<CoFreeNat<L>>
                {
                    Data = Either<Unit, CoFreeNat<L>>.inl(new Unit())
                }, t => new NatF<CoFreeNat<L>>
                {
                    Data = Either<Unit, CoFreeNat<L>>.inr(new AnaCoFreeNat<L, T>(t, n.Generator))
                })
            };
        }
    }

    public static CoFreeNatF<L, CoFreeNat<L>> In<L>(this CoFreeNat<L> n)
    {
        return n.Apply(new InFunction<L>());
    }

We also need to extract values from the comonad:

    class ExtractFunction<L> : CoFreeNatFunc<L, L>
    {
        public L Apply<T>(ECoFreeNatF<L, T> tree)
        {
            return tree.Generator(tree.Seed).Leaf;
        }
    }

    public static L Extract<L>(this CoFreeNat<L> leafy)
    {
        return leafy.Apply<L>(new ExtractFunction<L>());
    }

I will leave the definition of anamorphism into the comonad as a simple exercise.

At last, we can define the solution to the knapsack problem. For brevity here I\'m going to fix the weights and values here. We have three types of object: a type of value 2 and weight 2, a type of value 3 and weight 3 and a type of value 5 and weight 4.

    Func<NatF<CoFreeNat<int>>, int> seed = n =>
        n.Data.Case<int>(
            unit => 0,
            chain => new[] {                       
            /* KS(n - 1)     */ chain.Extract(),
            /* KS(n - 2) + 2 */ chain.In().Data.Data.Case(
                                    u => 0,
                                    n1 => n1.Extract() + 2),                       
            /* KS(n - 3) + 3 */ chain.In().Data.Data.Case(
                                    u => 0,
                                    n1 => n1.In().Data.Data.Case(
                                        u => 0, n2 =>
                                        n2.Extract() + 3)),
            /* KS(n - 4) + 5 */ chain.In().Data.Data.Case(
                                    u => 0,
                                    n1 => n1.In().Data.Data.Case(
                                        u => 0,
                                        n2 => n2.In().Data.Data.Case(
                                            u => 0,
                                            n3 => n3.Extract() + 5)))
            }.Max());

    var solution = Histo<int>(seed);

Eager readers may want to try the following:

* Generalize the above to handle an arbitrary set of weights and values.
* Express the fibonnaci sequence $\operatorname{fib } 0 = \operatorname{fib } 1 = 1, \operatorname{fib } n = \operatorname{fib }(n - 1) + \operatorname{fib }(n - 2)$ for $n > 1$ as a histomorphism.
* Define histomorphisms over another recursive data type such as binary trees or lists.
