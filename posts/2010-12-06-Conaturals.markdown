---
title: Greatest Fixed Points Part 2 - The Conaturals
author: Phil Freeman
date: 2010/12/06
description: The Conatural numbers and their arithmetic, using greatest fixed points.
tags: C#, Recursion
---

As a continuation of my last post on the encoding of greatest fixed point types, I\'d like to give a treatment of another interesting greatest fixed point type - the conatural numbers, the greatest fixed point of the functor

> F (X) = 1 + X

Using the encoding of greatest fixed points from my last post, we can define the conaturals as follows:

    class Conat<T>
    {
        public T Seed { get; set; }
        public Func<T, Either<Unit, T>> Generator { get; set; }
    }

    interface ConatFunction<R>
    {
        R Apply<T>(Conat<T> n);
    }

    interface Conat
    {
        R Apply<R>(ConatFunction<R> f);
    }

Let\'s interpret this: a conatural number with a (hidden) base type T is a finite or infinite chain of elements of T. Two conaturals are equivalent as existential packages if and only if their chains are both infinite or have the same finite length.

Let\'s define some conatural numbers:

    class ConatOne : Conat
    {
        public R Apply<R>(ConatFunction<R> f)
        {
            return f.Apply<Unit>(new Conat<Unit>
            {
                Seed = new Unit(),
                Generator = n => Either<Unit, Unit>.inl(new Unit())
            });
        }
    }

    class ConatInfinity : Conat
    {
        public R Apply<R>(ConatFunction<R> f)
        {
            return f.Apply<Unit>(new Conat<Unit>
            {
                Seed = new Unit(),
                Generator = n => Either<Unit, Unit>.inr(new Unit())
            });
        }
    }

The chain corresponding to the conatural `one` is parameterized by the `Unit` type and terminates after one step.

The chain corresponding to the conatural `infinity` is parameterized by the `Unit` type and cycles indefinitely.

To define a successor conatural, we can replace the underlying type T with the type `1 + T`, the extra unit type being required here to add an extra step to the chain defining the successor:

    class SuccessorConat<T> : Conat<Either<Unit, T>>
    {
        public SuccessorConat(Conat<T> n)
        {
            Seed = Either<Unit, T>.inl(new Unit());
            Generator = e => e.Case<Either<Unit, Either<Unit, T>>>(
                u => Either<Unit, Either<Unit, T>>.inr(Either<Unit, T>.inr(n.Seed)),
                e1 => n.Generator(e1).Case(
                    u1 => Either<Unit, Either<Unit, T>>.inl(new Unit()),
                    e2 => Either<Unit, Either<Unit, T>>.inr(Either<Unit, T>.inr(e2))));
        }
    }

Addition of conaturals can be defined by summing base types, essentially gluing the end of the first chain to the start of the first:

    class ConatAdd<T1, T2> : Conat<Either<T1, T2>>
    {
        public ConatAdd(Conat<T1> n, Conat<T2> m)
        {
            Seed = Either<T1, T2>.inr(m.Seed);
            Generator = e => e.Case<Either<Unit, Either<T1, T2>>>(
                t1 => n.Generator(t1).Case<Either<Unit, Either<T1, T2>>>(
                    u2 => Either<Unit, Either<T1, T2>>.inl(new Unit()),
                    e2 => Either<Unit, Either<T1, T2>>.inr(Either<T1, T2>.inl(e2))),
                t2 => m.Generator(t2).Case<Either<Unit, Either<T1, T2>>>(
                    u1 => Either<Unit, Either<T1, T2>>.inr(Either<T1, T2>.inl(n.Seed)),
                    e1 => Either<Unit, Either<T1, T2>>.inr(Either<T1, T2>.inr(e1))));
        }
    }

Multiplication can be defined similarly over the product type. We can visualize this as traversing the elements of one chain several times, once for each step in the second chain:

    class ConatMult<T1, T2> : Conat<Tuple<T1, T2>>
    {
        public ConatMult(Conat<T1> n, Conat<T2> m)
        {
            Seed = Tuple.Create<T1, T2>(n.Seed, m.Seed);
            Generator = p => n.Generator(p.Item1).Case<Either<Unit, Tuple<T1, T2>>>(
                    u1 => m.Generator(p.Item2).Case<Either<Unit, Tuple<T1, T2>>>(
                        u2 => Either<Unit, Tuple<T1, T2>>.inl(new Unit()),
                        t2 => Either<Unit, Tuple<T1, T2>>.inr(Tuple.Create<T1, T2>(n.Seed, t2))),
                    t1 => Either<Unit, Tuple<T1, T2>>.inr(Tuple.Create<T1, T2>(t1, p.Item2)));
        }
    }
