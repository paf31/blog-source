---
title: Finite Automata and Typed String Parsing
author: Phil Freeman
date: 2011/02/02
description: A type-safe version of the scanf function in C#.
tags: C#, Parsing
---

Define a finite deterministic automaton with values in type `T` and inputs from an alphabet `I` as follows:

> exists S . S * (I -> S -> Maybe T * Maybe S)

The intuition is that the hidden type `S` represents the possible states of the automaton, and that at each step, given an input of type `I` and the current state, we can optionally yield a value of type `T` and return a new state or fail.

This type is isomorphic to the greatest fixed point type `Nu (X -> Maybe T * (I -> Maybe X))` so we can save ourselves some boilerplate code dealing with existential types by writing the recursive types explicitly, remembering that our type refers to the greatest fixed point:

    class FDA<I, T>
    {
        public Maybe<T> Value
        {
            get;
            set;
        } 
     
        public Func<I, FDA<I, T>> Continue
        {
            get;
            set;
        }
             public FDA(Maybe<T> value, Func<I, FDA<I, T>> cont)
        {
            Value = value;
            Continue = cont;
        }
    }

## Extension Methods

This type has the structure of a monad, and we can write extension methods which allow us to use query syntax to build up automata:

    public static FDA<I, T1> Select<I, T, T1>(this FDA<I, T> a, Func<T, T1> f)
    {
        return new FDA<I, T1>(a.Value.Select(f),
            i =>
            {
                var next = a.Continue(i);
                return next == null ? null : next.Select(f);
            });
    }

    public static FDA<I, T1> SelectMany<I, T, T1>(this FDA<I, T> a,
        Func<T, FDA<I, T1>> f)
    {
        return a.Value.HasValue ?
            f(a.Value.Value)
            : new FDA<I, T1>(new Maybe<T1>(),
                i =>
                {
                    var next = a.Continue(i);
                    return next == null ? null : next.SelectMany(f);
                });
    }

    public static FDA<I, T2> SelectMany<I, T, T1, T2>(this FDA<I, T> a,
        Func<T, FDA<I, T1>> s,
        Func<T, T1, T2> projector)
    {
        return a.SelectMany(x => s(x).Select(y => projector(x, y)));
    }

We can inject a value into the monad by succeeding at the first step with the supplied value:

    public static FDA<I, T> Accept<I, T>(T t)
    {
        return new FDA<I, T>(new Maybe<T>(t), i => null);
    }

The monad\'s empty value just fails immediately:

    public static FDA<I, T> Reject<I, T>()
    {
        return new FDA<I, T>(new Maybe<T>(), i => null);
    }

We can also create an automaton which takes any input and returns the same value as its output. It\'s also useful to combine this with a predicate:

    public static FDA<I, I> Any<I>()
    {
        return new FDA<I, I>(new Maybe<I>(), i =>
            new FDA<I, I>(new Maybe<I>(i), i1 => null));
    }

    public static FDA<I, I> Any<I>(Func<I, bool> predicate)
    {
        return new FDA<I, I>(new Maybe<I>(), i => predicate(i)
            ? new FDA<I, I>(new Maybe<I>(i), i1 => null) : null);
    }

Using this method, we can create an automaton which only accepts a single value:

    public static FDA<I, I> Match<I>(I i) where I : IEquatable<I>
    {
        return FDA.Any<I>(i1 => i.Equals(i1));
    }

We can also join two automata in a standard way by trying the input on the first automaton and then on the second:

    public static FDA<I, T> Or<I, T>(this FDA<I, T> a1, FDA<I, T> a2)
    {
        return new FDA<I, T>(a1.Value.HasValue ? a1.Value : a2.Value,
            i => a1.Continue(i) ?? a2.Continue(i));
    }

Finally, we can join an automaton\'s final states to its intial state to create an automaton which yields many values of the same type:

    public static FDA<I, IEnumerable<T>> AllowMany<I, T>(this FDA<I, T> a)
    {
        return (from t in a
                from ts in a.AllowMany()
                select new[] { t }.Concat(ts))
               .Or(Accept<I, IEnumerable<T>>(Enumerable.Empty<T>()));
    }

## Running the Automaton

Now we have a collection of ways to create automata, we need a way to deconstruct them. We can leave the monad by providing a stream of inputs in the form of an `IEnumerable<I>`:

    public static Maybe<T> Accepts<I, T>(this FDA<I, T> a, IEnumerable<I> input)
    {
        if (input.Any())
        {
            var next = a.Continue(input.FirstOrDefault());

            if (next != null)
            {
                return next.Accepts(input.Skip(1));
            }
        }

        return a.Value;
    }

## Example

As an example, we can write an automaton which reads two digit integers:

    FDA<char, byte> Digit = FDA.Any<char>(char.IsNumber)
        .Select(c => byte.Parse(c.ToString()));

We can test this FDA by applying the `Accepts` method:

> Digit.Accepts("99").Value
> 
> 99

Here is an automaton which sums a sequence of one or two digit integers separated by plus symbols:

    FDA<char, int> Sum =
        from d1 in Digit
        from r in
            (from plus in FDA.Match('+')
             from sum in Sum
             select d1 + sum)
             .Or(from d2 in Digit
                 from r1 in
                     (from plus in FDA.Match('+')
                      from sum in Sum
                      select sum).Or(FDA.Accept<char, int>(0))
                 select d1 * 10 + d2 + r1)
                 .Or(FDA.Accept<char, int>(d1))
        select r;

Again, we can test by using the `Accepts` method:

> Sum.Accepts("12+3+45+6+78+9")
> 
> 153

## An Application to Typed Format Strings

Given a format string of the kind discussed in my last post, we can compile an automaton which recognises strings of the correct format:

    interface FormatString<S> where S : FormatStringShape
    {
        FDA<char, FormatStringValues<S>> Scan();
    }

It is easy to compile string literals to automata - we simply match character by character. Similarly, it is easy to compile format strings with composite shapes - if we can compile the two parts then we just compose the two compiled parts.

The tricky part is compiling variables, i.e. holes in the format string. One way we can restrict the types of format strings is to require the length of the variable to appear in the format string shape.

First we encode the natural numbers as singleton types:

    interface Nat { }

    class Z : Nat { }

    class S<N> : Nat where N : Nat { }

Now, we can modify the type of variable format string shapes to incorporate the length of the variable we expect:

    class VariableShape<T, N> : FormatStringShape where N : Nat { }

Now a format string of this shape takes a predicate and a reader function which parses strings of the appropriate length:

    class VariableFormatString<T, N> :
        FormatString<VariableShape<T, N>> where N : Nat
    {
        public Func<string, T> Reader { get; set; }
        public Predicate<char> Predicate { get; set; }

        public VariableFormatString(Predicate<char> predicate,
            Func<string, T> reader)
        {
            Predicate = predicate;
            Reader = reader;
        }

        public FDA<char, FormatStringValues<VariableShape<T, N>>> Scan()
        {
            return FDA.ReadVector<char, N>(Predicate).Select(cs =>
                (FormatStringValues<VariableShape<T, N>>)
                    new VariableValues<T, N>(Reader(new string(cs))));
        }
    }

Here, `ReadVector` is a helper method which parses a fixed number of characters given by the type parameter `N` to give a string.

We can now write string formats which include string formats and variables of prescribed lengths. For example, here is a format string which formats US social security numbers into three integer parts of lengths three, two and four:

    var ssnFormat =
        FormatStrings.Variable<int, S<S<S<Z>>>>(char.IsNumber, int.Parse)
        .Then(FormatStrings.Literal("-"))
        .Then(FormatStrings.Variable<int, S<S<Z>>>(char.IsNumber, int.Parse))
        .Then(FormatStrings.Literal("-"))
        .Then(FormatStrings.Variable<int, S<S<S<S<Z>>>>>(char.IsNumber, int.Parse));

Now, using an appropriate set of helper methods to extract values from the value set, we can parse strings in the appropriate format:

    var ssn = ssnFormat.Scan("123-45-6789");

    var ssn1 = ssn.Value.L().L().L().L().Extract();
    var ssn2 = ssn.Value.L().L().R().Extract();
    var ssn3 = ssn.Value.R().Extract();

The three integers, `ssn1`, `ssn2`, `ssn3` evaluate to 123, 45 and 6789 respectively.
