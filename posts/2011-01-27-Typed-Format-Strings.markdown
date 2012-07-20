---
title: Strongly-Typed Format Strings
author: Phil Freeman
date: 2011-01-27
---

The `string.Format` method is very useful for pretty-printing data:

> string.Format("{0},{1} (Born {2:d})", "Smith", "John", new DateTime(1900, 1, 1))
> 
> Smith, John (Born 1/1/1900)

One problem is that this method is not strongly typed. There is nothing to stop us supplying the wrong types of parameters:

> string.Format("{0},{1} (Born {2:d})", "Smith", 12345, new Currency())

Providing the wrong number of arguments results in an exception:

> string.Format("{0},{1} (Born {2:d})", "Smith", "John") 
> 
> 'string.Format("{0},{1} (Born {2:d})", 1, new Currency())' threw an exception of type 'System.FormatException'

and the type system can do nothing to help the developer find these bugs at compile time.

Instead, we can wrap the `string.Format` method in a couple of different ways so that the method becomes strongly typed, and the mistakes above result in a compile-time error. This is inspired by an attempt to translate C's printf method into a strongly typed functional language (see [1]).

## First Version

First, we define a collection of singleton types which define the set of possible format string \'shapes\':

    interface FormatStringShape { }
    
    class LiteralShape : FormatStringShape{ }
    
    class VariableShape<T> : FormatStringShape { }
    
    class CompositeShape<T1, T2> : FormatStringShape
        where T1 : FormatStringShape
        where T2 : FormatStringShape
    { }

Here, the three classes are just placeholders which will appear in our types later on - we are not going to actually instantiate a `FormatStringShape` at any point.

`LiteralShape` is the shape of a string literal (a format string with no arguments). `VariableShape<T>` is the shape of a format string with one argument of type `T`. `CompositeShape<T1, T2>` is a shape obtained by gluing two smaller shapes together.

With this notation, the shape of the format string in the example above would be the type

> CompositeShape\<VariableShape\<string\>, CompositeShape\<LiteralShape, CompositeShape\<VariableShape\<string\>, CompositeShape\<LiteralShape, CompositeShape\<VariableShape\<DateTime\>, LiteralShape\>\>\>\>\>\>

Phew! The good news is that these types won't appear in the finished methods, they\'re just here to classify the types of format strings and format string argument sets that we can create.

Now for each shape, we need to define an appropriate set of arguments. We do this by structural induction on the types of shapes:

    interface FormatStringValues<S> where S : FormatStringShape { }
    
    class LiteralValues : FormatStringValues<LiteralShape> { }
    
    class VariableValues<T> : FormatStringValues<VariableShape<T>>
    {
        public T Value { get; set; }
    
        public VariableValues(T value) { Value = value; }
    }
    
    class CompositeValues<T1, T2> : FormatStringValues<CompositeShape<T1, T2>>
        where T1 : FormatStringShape
        where T2 : FormatStringShape
    {
        public FormatStringValues<T1> Left { get; set; }
    
        public FormatStringValues<T2> Right { get; set; }
    
        public CompositeValues(FormatStringValues<T1> left,
            FormatStringValues<T2> right)
        {
            Left = left;
            Right = right;
        }
    } 

Note a literal takes no arguments, a variable of type `T` takes one argument of type `T` and a composite of two shapes takes two sets of arguments, one for each of its parts.

With those definitions, a format string object is given by the interface:

    interface FormatString<S> where S : FormatStringShape
    {
        StringBuilder Append(StringBuilder builder, 
            FormatStringValues<S> values);
    }

Note that the `Append` method can only be called with values of the correct shape! 

We could just return a string, but to reduce string concatenations, we chain the `StringBuilder` through the computation as state.

Again, we define implementations, by structural induction on shape types:

    class LiteralFormatString : FormatString<LiteralShape>
    {
        public string Value { get; set; } 
    
        public LiteralFormatString(string value) { Value = value; }
        
        public StringBuilder Append(StringBuilder builder, 
             FormatStringValues<LiteralShape> values)
        {
            return builder.Append(Value);
        }
     }
    
    class VariableFormatString<T> : FormatString<VariableShape<T>>
    {
        public Func<T, string> Formatter { get; set; }
    
        public VariableFormatString(Func<T, string> formatter) 
        { 
            Formatter = formatter; 
        }
    
        public StringBuilder Append(StringBuilder builder,
            FormatStringValues<VariableShape<T>> values)
        {
            return builder.Append(Formatter(((VariableValues<T>) values).Value));
        }
    }
     
    class CompositeFormatString<T1, T2> : FormatString<CompositeShape<T1, T2>>
         where T1 : FormatStringShape
         where T2 : FormatStringShape
    {
        public FormatString<T1> Left { get; set; }
        
        public FormatString<T2> Right{ get; set; }
         
        public CompositeFormatString(FormatString<T1> left, 
            FormatString<T2> right)
        {
            Left = left;
            Right = right;
        }
    
        public StringBuilder Append(StringBuilder builder, 
            FormatStringValues<CompositeShape<T1, T2>> values)
        {
            var cValues = (CompositeValues<T1, T2>) values;
            return Right.Append(Left.Append(builder, cValues.Left), cValues.Right);
        }
    }

A string format for a literal is just the string literal. A string format with a variable shape of type `T` is a `Func<T, string>` which turns the value into a suitable string. A composite string format is a pair of string formats of the correct shapes.

Note that this approach requires explicit casts for the argument list types.

Now, with a few extension methods (left as an exercise!) we can create format strings whose types determine their shape, and therefore prevent type errors at compile-time:

    var formatString =
        FormatStrings.Variable<string>(s => s)
            .Then(FormatStrings.Literal(", "))
            .Then(FormatStrings.Variable<string>(s => s))
            .Then(FormatStrings.Literal(" (Born "))
            .Then(FormatStrings.Variable<DateTime>(d => d.ToShortDateString()))
            .Then(FormatStrings.Literal(")"));

Formatting a string looks like this:

    formatString.Format(
        FormatStrings.Value("Smith")
            .Then(FormatStrings.Nothing())
            .Then(FormatStrings.Value("John"))
            .Then(FormatStrings.Nothing())
            .Then(FormatStrings.Value(new DateTime(1900, 1, 1)))
            .Then(FormatStrings.Nothing()))); >> Smith, John (Born 1/1/1900)

This approach has a couple of problems. The first is the presence of the casts when formatting the value lists. The second is a performance issue - for short argument lists we can improve performance by wrapping `string.Format` directly.

## Second Version

In this approach, we use the visitor pattern to turn the `FormatString<S>` directly into a format string. Similarly, we can visit the `FormatStringValues<S>` and collect a list of parameters. We can then call `string.Format` as usual.

We define two visitor interfaces:

    interface FormatStringValuesVisitor<R>
    {
        R VisitLiteral(LiteralValues l);
    
        R VisitVariable<T>(VariableValues<T> v);
    
        R VisitComposite<T1, T2>(CompositeValues<T1, T2> c)
            where T1 : FormatStringShape
            where T2 : FormatStringShape;
    }
    
    interface FormatStringVisitor<R>
    {
        R VisitLiteral<T>(LiteralFormatString<T> l);
    
        R VisitVariable<T>(VariableFormatString<T> v);
    
        R VisitComposite<T1, T2>(CompositeFormatString<T1, T2> c)
            where T1 : FormatStringShape
            where T2 : FormatStringShape;
    }

Now we can define two visitors to form the format string and the values list respectively:

    class BuildFormatStringVisitor : FormatStringVisitor<string>
    {
        private int index;
    
        public string VisitLiteral(LiteralFormatString one)
        {
            return one.Value;
        }
    
        public string VisitVariable<T>(VariableFormatString<T> e)
        {
            return "{" + index++ + "}";
        }
        
        public string VisitComposite<T1, T2>(CompositeFormatString<T1, T2> c)
            where T1 : FormatStringShape
            where T2 : FormatStringShape
        {
            return c.Left.AcceptVisitor(this) + c.Right.AcceptVisitor(this);
        }
    }

    class CollectValuesVisitor : ValuesVisitor<object[]>
    {
        public IEnumerable<object> VisitLiteral(LiteralValues e)
        {
            yield break;
        }
    
        public IEnumerable<object> VisitVariable<T>(VariableValues<T> one)
        {
            yield return one.Value;
        }
    
        public IEnumerable<object> VisitComposite<T1, T2>(CompositeValues<T1, T2> c)
            where T1 : FormatStringShape
            where T2 : FormatStringShape
        {
            return c.Left.AcceptVisitor(this).Concat(c.Right.AcceptVisitor(this));
        }
    }

Formatting a string is now a case of calling the following extension method:

    public static string Format<S>(this FormatString<S> formatString,
        FormatStringValues<S> values)
        where S : FormatStringShape
    {
        string formatString = literals.AcceptVisitor(new BuildFormatStringVisitor());
        var args = values.AcceptVisitor(new CollectValuesVisitor());
        return string.Format(formatString, args.ToArray());
    }

## References

[1] Functional Unparsing by Olivier Danvy, 1998.
