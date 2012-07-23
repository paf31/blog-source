---
title: Value Lifting, Bounded Existentials and the Visitor Pattern
author: Phil Freeman
date: 2010/12/15
description: Encoding balanced trees in C# using generics
tags: C#
---

Suppose we want to define a type of balanced binary trees in C#. That is, we want to define a type `Balanced<T>` of trees such that it is a type error to attempt to create a tree in `Balanced<T>` which has nodes at different depths. How might we go about this?

We could use nested datatypes. For example,

> Balanced T = T + Balanced (Pair T)

However, it is hard to reason about such a type, and it is not obvious how to generalize this idea to more complex types. Also, it would be nice if we could encode the depth of a tree with its type.

It would be nice if we could define

> Balanced : Nat -> *
>
> Balanced 0 = T
>
> Balanced n = Pair Balanced (n - 1)

but we don\'t have the luxury of parameterizing a type with a term such as a natural number in this way.

What we can do is lift the values of the type Nat to singleton types:

    public interface Nat { }

    public class Z : Nat { }

    public class S<N> : Nat where N : Nat { }

Now we can parameterize balanced trees as follows:

    public interface BalancedTree<T, N> where N : Nat { }

    public class Node<T> : BalancedTree<T, Z>
    {
        public T Value
        {
            get;
            set;
        }
    }

    public class Branch<T, N> : BalancedTree<T, S<N>> where N : Nat
    {
        public T Value
        {
            get;
            set;
        }

        public BalancedTree<T, N> Left
        {
            get;
            set;
        }

        public BalancedTree<T, N> Right
        {
            get;
            set;
        }
    }

We now have a well-typed version of a balanced tree of a given depth. We can also wrap the natural number type parameter in an existential package to define a type of balanced trees of an unknown depth:

    public interface TreeVisitor<T, R>
    {
        R Visit<N>(BalancedTree<T, N> e) where N : Nat;
    }

    public interface BalancedTree<T>
    {
        R AcceptVisitor<R>(TreeVisitor<T, R> v);
    }

The problem is finding a good way to get values out of the existential package. We want our functions to take different values based on the subclass of Nat appearing in the existential package. In C#, we can use the visitor pattern to distinguish behavior between subclasses. Let\'s alter our definition of the visitor class to branch based on the subclass of BalancedTree on the left hand side of the function:

    public interface TreeVisitor<T, R>
    {
        R VisitEmpty(Empty<T> e);
        R VisitBranch<N>(Branch<T, N> b) where N : Nat;
    }

and add methods to the tree classes to dispatch visitors to the correct subclass:

    public class Empty<T> : BalancedTree<T, Z>
    {
        public R AcceptVisitor<R>(TreeVisitor<T, R> v)
        {
            return v.VisitEmpty(this);
        }


        ...
    }

    public class Branch<T, N> : BalancedTree<T, S<N>> where N : Nat
    {
        public R AcceptVisitor<R>(TreeVisitor<T, R> v)
        {
            return v.VisitBranch<N>(this);
        }

        ...
    }

Now we can define methods on our existential type using the visitor pattern. For example, here is a map which takes a balanced tree and flattens it into a list:

    class FlattenVisitor<T> : TreeVisitor<T, IEnumerable<T>>
    {
        public IEnumerable<T> VisitEmpty(Empty<T> e)
        {
            yield return Value;
        }

        public IEnumerable<T> VisitBranch<N>(Branch<T, N> b) where N : Nat
        {
            var leftList = b.Left.AcceptVisitor<IEnumerable<T>>(this);
            var rightList = b.Right.AcceptVisitor<IEnumerable<T>>(this);
            return leftList.Concat(rightList);

        }

    }

    public static IEnumerable<T> Flatten<T>(this BalancedTree<T> b)
    {
        return b.AcceptVisitor<IEnumerable<T>>(new FlattenVisitor<T>());
    }

We can also forget the depth of a tree by wrapping it in an existential package:

    class ForgetfulTree<T, N> : BalancedTree<T> where N : Nat
    {
        private readonly BalancedTree<T, N> b;

        public ForgetfulTree(BalancedTree<T, N> b)
        {
            this.b = b;
        }

        public R AcceptVisitor<R>(TreeVisitor<T, R> v)
        {
            return b.AcceptVisitor<R>(v);
        }
    }
     

    public static BalancedTree<T> Forget<T, N>(this BalancedTree<T, N> b) where N : Nat
    {
        return new ForgetfulTree<T, N>(b);
    }

Finally, here is an example calculation:

    BalancedTree<int, S<S<S<Z>>>> treeOfKnownDepth =
        new Branch<int, S<S<Z>>>
    {
        Left = new Branch<int, S<Z>>
        {
            Left = new Branch<int, Z>
            {
                Left = new Empty<int> { Value = 1 },
                Right = new Empty<int> { Value = 2 }
            },
            Right = new Branch<int, Z>
            {
                Left = new Empty<int> { Value = 3 },
                Right = new Empty<int> { Value = 4 }
            }
        },
        Right = new Branch<int, S<Z>>
        {
            Left = new Branch<int, Z>
            {
                Left = new Empty<int> { Value = 5 },
                Right = new Empty<int> { Value = 6 }
            },
            Right = new Branch<int, Z>
            {
                Left = new Empty<int> { Value = 7 },
                Right = new Empty<int> { Value = 8 }
            }
        }
    }

We can take this 'tree of known depth' and forget its depth:

> var treeOfUnknownDepth = treeOfKnownDepth.Forget();
 
We can then calculate:

> treeOfUnknownDepth.Flatten();
> 
> [ 1, 2, 3, 4, 5, 6, 7, 8 ]
