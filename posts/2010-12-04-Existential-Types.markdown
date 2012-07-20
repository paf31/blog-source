---
title: Existential Types in C#
author: Phil Freeman
date: 2010/12/04
---

I was thinking about existential types recently, what they might be useful for, and if it was possible to encode them in C#.

First, a quick recap. A value of an existential type `exists X. T` looks like a pair consisting of a type `A` and a value `a` of type `T[X\A]` which is the result of substituting the type `A` for the bound type variable `X` in `T`. This creates a notion of information hiding where the interface being defined is given by the type expression `T`, and the implementation is hidden in the type `A` and the term `a` which the developer can only access by unpacking the existential package.

As an example, think of the situation where we want to assign unique identifiers to a set of objects. For our purposes, we don\'t care what the actual representation of the identifiers will be, so long as they uniquely identify the objects in question. We would need two functions: a way of creating a new identifier, and a way of mapping an existing identifier to the object it uniquely identifies.

We might encode this as follows:

    interface KeyProvider<K, T> 
    {
        K NextKey(T t);

        T Map(K key);
    }

Here, the type variable `K` stands for the type of the identifiers, the type we want to hide. The type variable `T` denotes the type of object we hope to uniquely identify.

We might create an implementation using the `System.Guid` class or by using a counter, but the user should not be able to get at the underlying type.

Fortunately, there is a nice isomorphism

> exists X. T = forall R. (forall X. T -> R) -> R 

Basically, this says that if you want a value of any type, R, say, and you can compute it given an existential package with any underlying type X, then I can give you that value by instantiating your generic function with the underlying type of the existential package and applying it to its payload.

The isomorphism looks like

> pack A a => \\R. \\f. f[A] a

We can unpack the type by providing a result type R and passing in a function of type `forall X. T -> R`

Translating this into C# using generics for universals we get:

    interface KeyProviderFunction<R, T>
    {
        R Apply<K>(KeyProvider<K, T> provider);
    }

    interface KeyProvider<T>
    {
        R Apply<R, T>(KeyProviderFunction<R, T> f);
    }

With this kind of method dispatch, we\'ve created a version of the KeyProvider interface with the type variable K hidden.

Now, to use an implementation of KeyProvider<T>, we simply have to provide a function of type KeyProviderFunction<R, T> for some result type R.

## Peano Arithmetic ##

Here\'s an example encoding Peano arithmetic over a hidden data type.

An algebra for the Peano axioms is a type with a zero, and a successor function. For illustration purposes, we\'ll also add a printing function ToString:

    interface PeanoArithmetic<T>
    {
        T Zero { get; }
    
        T Successor(T t);
 
        string ToString(T t);
    }
 
Now we can existentially quantify over the type parameter T to define a Peano algebra with a hidden underlying type:

    interface PeanoArithmeticFunction<R>
    {
         R Apply<T>(PeanoArithmetic<T> arithmetic);
    }
     
    interface PeanoArithmetic
    {
         R Apply<R>(PeanoArithmeticFunction<R> f);
    }

Here is a simple implementation using Integers as the underlying type:

    class PeanoArithmeticImplementation : PeanoArithmetic
    {
        class PeanoArithmeticMethods : PeanoArithmetic<int>
        {
             public int Zero
             {
                 get { return 0; }
             }
     
             public int Successor(int n)
             {
                 return n + 1;
             }
     
             public string ToString(int n)
             {
                 return n.ToString();
             }
        }
 
        public R Apply<R>(PeanoArithmeticFunction<R> f)
        {
             return f.Apply<int>(new PeanoArithmeticMethods());
        }
    }

Even though the type parameter has been hidden, the existentially quantified type can still be used. We can, for example, print the numbers 1, 2, 3 as follows:
     
    class Program
    {
         class OneTwoThree : PeanoArithmeticFunction<string>
         {
             public string Apply<T>(PeanoArithmetic<T> a)
             {
                 var _0 = a.Zero;
                 var _1 = a.Successor(_0);
                 var _2 = a.Successor(_1);
                 var _3 = a.Successor(_2);
                 
                 return string.Format("{0}, {1}, {2}",
                     a.ToString(_1),
                     a.ToString(_2),
                     a.ToString(_3));
             }
         }
 
         static void Main(string[] args)
         {
             PeanoArithmetic peano = new PeanoArithmeticImplementation();
             Console.WriteLine(peano.Apply<string>(new OneTwoThree()));
         }
    }
