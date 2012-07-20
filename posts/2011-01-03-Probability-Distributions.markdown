---
title: LINQ to Probability Distributions
author: Phil Freeman
date: 2011/01/03
---

Here is an example of an interesting monad which I have not seen expressed anywhere using LINQ\'s extension methods.

## Probability Distributions

Define a probability distribution with entries in type T as a list of outcomes with probabilities:

    public class Outcome<T>
    {
        public T Value
        {
            get;
            set;
        }

        public double Probability
        {
            get;
            set;
        }

        public Outcome(T value, double probability)
        {
            Value = value;
            Probability = probability;
        }
    }

    public class Distribution<T>
    {
        public IEnumerable<Outcome<T>> Outcomes
        {
            get;
            set;
        }

        public Distribution(IEnumerable<Outcome<T>> outcomes)
        {
            Outcomes = outcomes;
        }
    }

We can inject a value into the monad by returning a distribution which returns a single value with certainty:

    public static Distribution<T> Return<T>(T t)
    {
        return new Distribution<T>(new Outcome<T>[] { new Outcome<T>(t, 1.0) });
    }

## Extension Methods

Distributions are functorial: a function on outcomes lifts to a function on distributions:

    public static Distribution<T> Select<S, T>(this Distribution<S> d,
        Func<S, T> f)
    {
        return new Distribution<T>(d.Outcomes.Select(p =>
            new Outcome<T>(f(p.Value), p.Probability)));
    }

The monad\'s bind operation combines a distribution with a dependent distribution in the way one would expect by multiplying probabilities:

    public static Distribution<T> SelectMany<S, T>(this Distribution<S> d,
        Func<S, Distribution<T>> s)
    {
        return new Distribution<T>(d.Outcomes.SelectMany(p =>
            s(p.Value).Outcomes.Select(p1 =>
                new Outcome<T>(p1.Value, p.Probability * p1.Probability))));
    }

Here we are simply applying the rule $P(A \text{ and } B) = P(A | B) \times P(B)$.

Distributions also form a monad-with-zero - for every type T, there is an empty distribution with values in T:

    public static Distribution<T> Empty<T>()
    {
        return new Distribution<T>(Enumerable.Empty<Outcome<T>>());
    }

Now, any monad-with-zero has a means of filtering out values according to a predicate:

    public static Distribution<T> Where<T>(this Distribution<T> d, Func<T, bool> p)
    {
        return from possibility in d
               from oneOrZero in p(possibility) ? Return(possibility) : Empty<T>()
               select oneOrZero;
    }

With these methods, we have the means to calculate monad comprehensions, that is a way of combining, mapping and filtering values in the monad to form complicated distributions.

## Calculating With Distributions

There are a number of methods that we can use to remove a wrapped value from the monad. For any distribution, we can find the sum of probabilities in a distribution. It is also useful to combine a filter with a sum of probabilities:

    public static double Probability<T>(this Distribution<T> d)
    {
        return d.Outcomes.Sum(p => p.Probability);
    }

    public static double Probability<T>(this Distribution<T> d,
        Func<T, bool> predicate)
    {
        return d.Where(predicate).Outcomes.Sum(p => p.Probability);
    }

For numerical distributions, we can find the mean and standard deviation:

    public static double Average(this Distribution<double> d)
    {
        return d.Outcomes.Select(o => o.Value * o.Probability).Sum();
    }

    public static double Variance(this Distribution<double> d)
    {
        var average = d.Average();
        return d.Select(o => (o - average) * (o - average)).Average();
    }

The Distribution of a Die-Roll

We can define the distribution of a single die-roll as follows:

    var fairDie = new Distribution<int>(new[]
    {
        new Outcome<int>(1, 1.0 / 6.0),
        new Outcome<int>(2, 1.0 / 6.0),
        new Outcome<int>(3, 1.0 / 6.0),
        new Outcome<int>(4, 1.0 / 6.0),
        new Outcome<int>(5, 1.0 / 6.0),
        new Outcome<int>(6, 1.0 / 6.0)
    });

Our extension methods allow us to form the distribution of the sum of the rolls of two independent fair dice:

    var sum = from d1 in fairDie
              from d2 in fairDie
              select (double) d1 + d2;

We can use our extension methods to find the probabilities of various events:

> Console.WriteLine("P(S = 7) = {0:.##}", sum.Probability(s => s == 7));
> 
> P(S = 7) = .17
> 
> Console.WriteLine("P(S Odd) = {0:.##}", sum.Probability(s => s % 2 > 0));
> 
> P(S Odd) = .5
> 
> Console.WriteLine("Mean = {0:.##}", sum.Average());
> 
> Mean = 7
> 
> Console.WriteLine("Std. Dev. = {0:.##}", Math.Sqrt(sum.Variance()));
> 
> Std. Dev. = 2.42

We can also model non-independent events - in the following game, the player gets to replace the fair die with a loaded die if a fair coin lands heads-up:

    var coinToss = new Distribution<bool> (new []
    {
        new Outcome<bool>(true, 1.0 / 2.0),
        new Outcome<bool>(false, 1.0 / 2.0)
    });

    var loadedDie = new Distribution<int>(new[]
    {
        new Outcome<int>(1, 1.0 / 10.0),
        new Outcome<int>(2, 1.0 / 10.0),
        new Outcome<int>(3, 1.0 / 10.0),
        new Outcome<int>(4, 1.0 / 10.0),
        new Outcome<int>(5, 1.0 / 10.0),
        new Outcome<int>(6, 1.0 / 2.0)
    });

    var roll = from heads in coinToss
               let die = heads ? loadedDie : fairDie
               from roll1 in die
               from roll2 in die
               select (double) roll1 + roll2;

As one would expect, the player\'s expected sum is now higher:

> Console.WriteLine("Mean = {0:.##}", roll.Average());
> 
> Mean = 8

## References

[1] Probabilistic Functional Programming In Haskell (M. Erwig, S. Kollmansberger, 2006)
