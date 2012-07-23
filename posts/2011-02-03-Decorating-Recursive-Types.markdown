---
title: Decorating Recursive Types
author: Phil Freeman
date: 2012/02/03
description: An attempt to extend recursive types in a way which is compatible with subclassing.
tags: C#, Recursion
---

Suppose we have a recursive type and we want to add additional functionality in such a way that each structurally smaller value also gets that additional functionality. For example, consider the following type, whose values look like nameless lambda terms (de Bruijin terms):

    interface Exp { }

    class App : Exp
    {
        public Exp Function { get; set; }

        public Exp Argument { get; set; }

        public App(Exp function, Exp argument)
        {
            Function = function;
            Argument = argument;
        }

        public override string ToString()
        {
            return string.Format("{0} {1}",
                Function, Argument);
        }
    }

    class Var : Exp
    {
        public int Index { get; set; }

        public Var(int index) { Index = index; }

        public override string ToString()
        {
            return Index.ToString();
        }
    }

    class Abs : Exp
    {
        public Exp Body { get; set; }

        public Abs(Exp body) { Body = body; }

        public override string ToString()
        {
            return "\\. " + Body.ToString();
        }
    }

The three subclasses `App`, `Var` and `Abs` correspond to function applications, variables and lambda abstractions respectively. We can define simple untyped lambda terms, for example:

    new Abs
    (
        new App
        (
            new Var(0),
            new Var(0)
        )
    );

    "\. 0 0"

Now suppose that we want to add additional properties, methods or events to the class in such a way that all of the subtrees expose the same new functionality.

We might think of subclassing `Exp`, but that\'s no good since only the root of the tree would see the new functionality.

One thing we can do is to use the functor which defines `Exp` as a recursive type to leave holes for the structurally smaller values in the tree. Here, we have named the type of the holes by the type variable `E`:

    interface Exp<E> where E : Exp<E> { }

    class App<E> : Exp<E> where E : Exp<E>
    {
        public E Function { get; set; }

        public E Argument { get; set; }

        public App(E function, E argument)
        {
            Function = function;
            Argument = argument;
        }

        public override string ToString()
        {
            return string.Format("{0} {1}",
                Function, Argument);
        }
    }

    class Var<E> : Exp<E> where E : Exp<E>
    {
        public int Index { get; set; }

        public Var(int index) { Index = index; }

        public override string ToString()
        {
            return Index.ToString();
        }
    }

    class Abs<E> : Exp<E> where E : Exp<E>
    {
        public E Body { get; set; }

        public Abs(E body) { Body = body; }

        public override string ToString()
        {
            return "\\. " + Body.ToString();
        }
    }

Notice first of all that the various subclasses don\'t explicitly mention instances of `Exp<E>`. They only refer to their child nodes by the type variable `E`.

Also notice the type constraint where `E : Exp<E>` appearing throughout the code: this serves to make sure that however we decorate the type `Exp`,  its children are also instances of `Exp` and are decorated in the same way, so that we don\'t have different types of `Exp`s throughout the tree.

We can "tie the knot" and remove the mysterious parameter `E` by defining the recursive type `Exp` as

    interface Exp : Exp<Exp> { }

What\'s going on here? Well an `Exp` is an expression whose holes are filled in by expressions of type `Exp`. In turn, a subexpression appearing in a hole has subexpressions of type `Exp`, and so on...

We can also wire up types for all of our subclasses, and our original code remains valid:

    class App : App<Exp>, Exp
    {
        public App(Exp function, Exp argument)
            : base(function, argument) { }
    }

    class Abs : Abs<Exp>, Exp
    {
        public Abs(Exp body)
            : base(body) { }
    }

    class Var : Var<Exp>, Exp
    {
        public Var(int index)
            : base(index) { }
    }

However, now we are free to replace the particular kind of expression appearing in any holes, as long as every subexpression is decorated in the same way. This is how we are going to add new functionality to the class.

For example, suppose we want to create a new version of the structure where terms are given explicit names. Here it is:

    interface DecoratedExp : Exp, Exp<DecoratedExp> { }

    class DecoratedApp : App<DecoratedExp>, DecoratedExp
    {
        public DecoratedApp(DecoratedExp function, DecoratedExp argument)
            : base(function, argument) { }
    }

    class DecoratedAbs : Abs<DecoratedExp>, DecoratedExp
    {
        public DecoratedAbs(string name, DecoratedExp body)
            : base(body) { Name = name; }

        public string Name { get; set; }

        public override string ToString()
        {
            return "\\" + Name + ". " + Body.ToString();
        }
    }

    class DecoratedVar : Var<DecoratedExp>, DecoratedExp
    {
        public DecoratedVar(string name, int index)
            : base(index) { Name = name; }

        public string Name { get; set; }

        public override string ToString() { return Name; }
    }

Now each `Var` and each `Abs` carries an explicit name. We can create decorated lambda terms as long as all subterms are decorated in the same way:

    Exp dec = new DecoratedAbs
    ("x",
        new DecoratedApp(
            new DecoratedVar("x", 0),
            new DecoratedVar("x", 0)
        )
    );

    "\x. x x"

But an attempt to mix expression types within a structure is not allowed:

    /* Generates a type error */

    new DecoratedAbs
    ("x",
        new App(
            new DecoratedVar("x", 0),
            new Var(0)
        )
    );
