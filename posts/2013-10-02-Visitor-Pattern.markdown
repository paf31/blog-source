---
title: The Visitor Pattern and Pattern Matching
author: Phil Freeman
date: 2013/10/02
description:
tags: Haskell, C#
---

For many developers using Object Oriented programming languages, the Visitor Pattern is one of the first design patterns to provide a major roadblock in terms of understanding. It doesn't help that the typical examples are not particularly enlightening, but in my experience, it can be very difficult to explain to a beginner what is going on and why the design pattern is useful, especially with all of the syntactic noise that comes with a language like Java or C#.

I find it much easier to explain the visitor pattern by taking a detour through pattern matching, which can provide useful motivating examples.

## Motivating Example

Suppose we want to write a method which calculates payroll for different types of employees. We're given the following business requirement: all employees are of one of two types: 

- Hourly employees
- Salaried employees

One way to model this in an Object Oriented language would be to create an abstract class called `Employee` with three concrete subclasses `HourlyEmployee`, `SalariedEmployee`. `Employee` would be defined as an interface with a method for calculating the employee's monthly pay, and each employee class would be responsible for implementing it.

It might look something like this:

~~~
public interface Employee {

    public abstract int calculatePay();
}

public class HourlyEmployee implements Employee {

    public int hourlyRate;
    public int hoursWorked;

    public int calculatePay() {
        return hoursWorked * hourlyRate;
    }
}

public class SalariedEmployee implements Employee {

    public int salary;

    public int calculatePay() {
        return salary / 12;
    }
}
~~~

## The Problem

You complete the assignment and the code is deployed. It works so well that other developers want to make use of your code as a library in their own work.

Now suppose another business requirement is given: the number of paid vacation hours will be determined by the employee type; hourly employees will be given paid time off depending on the number of hours worked, salaried employees will be given a fixed amount of time off, etc.

Now the problem with the original approach rears its head. The developer responsible for this change should use your code as a library, and will not have access to the original code or the means to modify it. Here's the issue:

*Subclass polymorphism easily allows the addition of new types of object, but does not easily allow the addition of new methods*

This is the problem that the Visitor Pattern solves.

*The Visitor Pattern easily allows the addition of new methods, but does not easily allow the addition of new types of object*

We know that the types of employees are not likely to change, but we would like to add new methods to the Employee type, so it seems as if the Visitor pattern would be more appropriate.

When deciding if subclass inheritance is the correct solution for a problem, as yourself *"would I prefer the consumers of my API to be able to add new subclasses, or new operations more easily?"*

## A Non-Solution using Reflection

One anti-pattern which I sometimes see is to solve this problem by using reflection. In C#, the proposed solution might look something like this:

~~~
public int computeVacationHours(Employee employee) {
	if (employee is HourlyEmployee) {
		HourlyExployee hourlyEmployee = (HourlyEmployee) employee;
		return ...
	} else if (employee is SalariedEmployee) {
		SalariedEmployee salariedEmployee = (SalariedEmployee) employee;
		return ...
	} else {
		throw new ArgumentException("Unknown employee type!");
	}
}
~~~

But we can do better. It would be nice if we didn't have to throw an exception in the final line. What we would really like is to statically check that all cases have been handled. If we could somehow prevent additional subclasses of `Employee` from being defined elsewhere, we wouldn't need to throw an exception. This problem can be solved using the visitor pattern.

## Solution Using Pattern Matching

When I am trying to explain the purpose and implementation of the visitor pattern to another developer, I like to show them how I would solve the problem in a language with pattern matching. I then attempt to sell them the Visitor Pattern as the OOP-ification of pattern matching.

In Haskell, I would define the type of employees as follows:

~~~{.haskell}
data Employee 
  = HourlyEmployee { hourlyRate :: Double, hoursWorked :: Douvle }
  | SalariedEmployee { salary :: Double }
~~~

Hopefully, this is self-explanatory, but in case it isn't: reading the vertical bar as "or", and the double colon as "of type", this would read as "An `Employee` is either an `HourlyEmployee` with an `hourlyRate` of type `Double`, or a `SalariedEmployee` with a salary of type `Double`.

Note that an `Employee` is either hourly or salaried, but never both, and an `Employee` constructed with the `SalariedEmployee` does not have an `hourlyRate`, for example.

With that, the original pay rate calculation, using pattern matching, would look like this:

~~~{.haskell}
calculatePay :: Employee -> Double
calculatePay (HourlyEmployee hourlyRate hoursWorked) = hourlyRate * hoursWorked
calculatePay (SalariedEmployee salary) = salary / 12.0
~~~

Notice how the code is structured: it is expressed as a series of cases, each of which matches its input data using a specific pattern. The pattern binds some variables such as `hoursWorked`, which can then be used in the remainder of the computation, on the right hand side of the equals sign.

## Solution Using the Visitor Pattern

If the visitor pattern is just the "OOP-ification of pattern matching", then we should be able to derive it by attempting to translate the previous code into Java.

First, we encapsulate a computation which works by case analysis using an interface:

~~~
public interface EmployeeCaseAnalysis<Result> {

    Result matchHourlyEmployee(Double hourlyRate, Double hoursWorked);
	
	Result matchSalariedEmployee(Double salary);
}
~~~

An implementation of EmployeeCaseAnalysis<Result> needs to provide two methods: one calculates the result for an hourly employee, and the other calculates the result for a salaried employee.

The `Result` type parameter here is used to indicate the type of the result of the computation. More commonly, you will see a version of this class with no type parameter, which would be used when simply performing an action on the data, not transforming it.

~~~
public interface EmployeeCaseAnalysis {

    void matchHourlyEmployee(Double hourlyRate, Double hoursWorked);
	
	void matchSalariedEmployee(Double salary);
}
~~~

Now, we define the `Employee` abstract class. What is an `Employee`? Well, given any computation by case analysis, we want to be able to perform it and get a result. Here is the result:

~~~
public interface Employee {

    void performCaseAnalysis(EmployeeCaseAnalysis caseAnalysis);
	
	Result performCaseAnalysis<Result>(EmployeeCaseAnalysis<Result> caseAnalysis);
}
~~~

A concrete class of `Employee` must provide a way to perform a case analysis with no result, or a case analysis with a result, no matter what its result type.

In fact, there are only two sensible ways to implement this specification: we can either call the `matchHourlyEmployee` method on the provided `EmployeeCaseAnalysis` object, if we have an `hourlyRate` and a number of `hoursWorked`, or we can call the `matchSalariedEmployee` method if we have a `salary`.

Let's call these two implementations `HourlyEmployee` and `SalariedEmployee`:

~~~
public class HourlyEmployee implements Employee {

    public int hourlyRate;
	public int hoursWorked;

    public void performCaseAnalysis(EmployeeCaseAnalysis caseAnalysis) {
        caseAnalysis.matchHourlyEmployee(hourlyRate, hoursWorked);
    }

    public Result performCaseAnalysis<Result>(EmployeeCaseAnalysis<Result> caseAnalysis) {
        return caseAnalysis.matchHourlyEmployee(hourlyRate, hoursWorked);
    }
}

public class SalariedEmployee implements Employee {

    public int salary;

    public void performCaseAnalysis(EmployeeCaseAnalysis caseAnalysis) {
        caseAnalysis.matchSalariedEmployee(salary);
    }

    public Result performCaseAnalysis<Result>(EmployeeCaseAnalysis<Result> caseAnalysis) {
        return caseAnalysis.matchSalariedEmployee(salary);
    }
}

After some renaming, the code above looks just like code implemented using the visitor pattern. What I have called `EmployeeCaseAnalysis` might be renamed to `EmployeeVisitor`, and the `performCaseAnalysis` methods would most likely be renamed to `visit` or `apply`. Here is the full version after the renamings:

~~~
public interface Employee {

    void visit(EmployeeVisitor visitor);
	
	Result visit<Result>(EmployeeVisitor<Result> visitor);
}

public interface EmployeeVisitor {

    void visitHourlyEmployee(Double hourlyRate);
	
	void visitSalariedEmployee(Double salary);
}

public interface EmployeeVisitor<Result> {

    Result visitHourlyEmployee(Double hourlyRate);
	
	Result visitSalariedEmployee(Double salary);
}

public class HourlyEmployee implements Employee {

    public int hourlyRate;
	public int hoursWorked;

    public void visit(EmployeeVisitor visitor) {
        visitor.visitHourlyEmployee(hourlyRate, hoursWorked);
    }

    public Result visit<Result>(EmployeeVisitor<Result> visitor) {
        return visitor.visitHourlyEmployee(hourlyRate, hoursWorked);
    }
}

public class SalariedEmployee implements Employee {

    public int salary;

    public void visit(EmployeeVisitor visitor) {
        visitor.visitSalariedEmployee(salary);
    }

    public Result visit<Result>(EmployeeVisitor<Result> visitor) {
        return visitor.visitSalariedEmployee(salary);
    }
}

It is also common to pass along the entire subclass of `Employee` to the `visitX` methods in `EmployeeVisitor`.

~~~
public interface EmployeeVisitor {

    void visitHourlyEmployee(HourlyEmployee employee);
	
	void visitSalariedEmployee(SalariedEmployee employee);
}
~~~

But I will skip that here.

It is now possible to implement our original payroll computation as a subclass of `EmployeeVisitor`:

~~~
public class PayrollCalulator implements EmployeeVisitor<Double> {

    Double visitHourlyEmployee(Double hourlyRate, Double hoursWorked) {
		return hourlyRate * hoursWorked;
	}
	
	Double visitSalariedEmployee(Double salary) {
		return salary / 12.0;
	}
}
~~~

However, now our coworker can implement the computation of the number of vacation hours without modifying the original code!

~~~
public class VacationCalculator implements EmployeeVisitor<Integer> {
   
    public int maxVacationHours;
	public int vacationHoursAccruedPerHourWorked;

    Integer visitHourlyEmployee(Double hourlyRate, Double hoursWorked) {
		return Math.min(maxVacationHours, 
		    Math.floor(hoursWorked / vacationHoursAccruedPerHourWorked));
	}
	
	Integer visitSalariedEmployee(Double salary) {
		return maxVacationHours;
	}
}
~~~

## A Recipe for Visitors

That is the essence of the visitor pattern. We can summarise what we did in a number of bullet points which can be executed any time we'd like to be able to perform abstract case analyses in an object oriented language.

Suppose we'd like to perform case analysis on our new data type `Foo`:

1. Add an interface caled `FooVisitor`.
1. Add a void interface method to `FooVisitor` for each possible case in a case analysis, whose method arguments provide the additional data which is available in that case.
1. Optionally repeat steps 1 and 2 for a generic interface `FooVisitor<Result>`, in which all interface methods return a result of type `Result`.
1. Add an interface called `Foo`.
1. Add a void interface method `visit` to `Foo` which takes a `FooVisitor`.
1. Optionally add an interface method `visit` to `Foo` which takes a `FooVisitor<Result>` and returns a `Result`.
1. For each case, add a class implementing `Foo`, whose implementations of `visit` call the appropriate method on `FooVisitor`.

It's a lot of boilerplate (which can be automated), but which gives us the flexibility to define new methods without modifying the original code.

## A Brief Type-Theoretic Detour

You may wish to skip this section as it is not really necessary for understanding the visitor pattern, but I have included it since it may be helpful.

Suppose for the time being that I have a data type which supports case analysis with only two cases, as in the `Employee` example. The following argument works with any number of cases, but this assumption will simplify what follows. Let's call the data type `Foo`, and the two cases `FooA` and `FooB`.

In Haskell, we could define `Foo` using a type-synonym: `type Foo = Either FooA FooB`.

`Either` is what is known as a "sum type", because it is analogous at a type level to a sum in algebra. It is defined as follows:

~~~{.haskell}
data Either a b = Left a | Right b
~~~
	
That is, a value of type `Either a b` is *either* constructed using `Left`, in which case it contains a value of type `a`, or with `Right`, in which case it contains a value of type `b`. A value of type `Either a b` contains *either* an `a` or a `b`, but not both.

Now, consider a function which takes a value of type `Foo` and returns something of type `result`. Let's call such a thing an `FooFunction result`:

~~~{.haskell}
type FooFunction result = Foo -> result
~~~~

Now `Foo` is defined by a type synonym, which we can expand. I'll write in pseudo-Haskell here, and the symbol `~` will be used to indicate that two types are *isomorphic*, i.e. that they contain the same values in a possibly different representation.

    FooFunction result ~ Either FooA FooB -> result
	
We can now use the fact that the function type `Either a b -> result` is isomorphic to a pair of functions `(a -> result, b -> result)`. This can be witnessed by writing out the isomorphism explicitly in Haskell, but should be somewhat obvious: if we want a `result` given only an `Either a b`, and we don't know whether we have a `Left` or a `Right`, then we need to be able to handle both cases, so we need two functions, one which returns a `result` given an `a`, and one which returns a result given a `b`.

    FooFunction result ~ (FooA -> result, FooB -> result)
	
To aid understanding, I will represent the pair of functions as a record, giving the two functions useful names:

    FooFunction result ~ { visitFooA :: FooA -> result, visitFooB :: FooB -> result }
	
Now consider the following type: `forall result. FooFunction result -> result`. I claim that this type is isomorphic to the original type `Foo`, but I will omit a proof. Given that, you should be convinced that

    Foo ~ forall result. { visitFooA :: FooA -> result, visitFooB :: FooB -> result } -> result
	
and that this isomorphism captures the idea behind the visitor pattern at the type level.
 
## Extending the Visitor Pattern

The idea behind the visitor pattern can be combined with other features of languages like Java and C# to create new patterns and enable new forms of abstraction. Here are some ideas:

- Use generic functions to emulate *existential types*.
- Use generic type parameters to emulate *generalized algebraic data types*.

I will leave these as extended exercises for the interested reader, but details can be found in some of my previous posts.

## The Expression Problem and Type Classes

We have seen that the visitor pattern is useful when we have a fixed set of subclasses and would like to extend the set of operations on those subclasses. This is in contrast to subclass polymorphism which allows the addition of new subclasses for a fixed set of operations.

This begs the question: can we have the benefits of both? That is, can we write an abstraction which easily allows the definition of new subclasses, and new operations, without modification to the original code, and with support from the compiler.

This problem is called the Expression Problem and has been the subject of a great deal of research. In Haskell, the problem is solved using *type classes*, also known in other languages as *traits*. New "subclasses" can be defined by adding new data types and writing associated *type class instances*, and new operations can be defined by writing new *type classes* and retroactively providing instances for all existing data types.

For example, we might have two different data types for our two employee types in one module:

~~~{.haskell}
module Employee.Types

data HourlyEmployee = HourlyEmployee { hourlyRate :: Double, hoursWorked :: Double }

data SalariedEmployee = SalariedEmployee { salary :: Double }
~~~

And we could define a type class which represents the operation of computing monthly pay, in a second module:

~~~{.haskell}
module Employee.Payroll

class Payable employee where
    pay :: employee -> Double
~~~

We can define type class instances for our existing employee types:

~~~{.haskell}
instance Payable HourlyEmployee where
    pay (HourlyEmployee hourlyRate hoursWorked) = hourlyRate * hoursWorked
	
instance Payable SalariedEmployee where
    pay (SalariedEmployee salary) = salary / 12.0
~~~

Now when a coworker would like to calculate paid vacation allowance, they can do so in another module by defining another type class and associated instances:

~~~{.haskell}
maxTimeOff :: Double
hoursOffPerHourWorked :: Double

class Vacation employee where
    timeOff :: employee -> Int
	
instance Vacation HourlyEmployee where
    timeOff (HourlyEmployee _ hoursWorked) = min maxTimeOff (hoursWorked / hoursOffPerHourWorked)
	
instance Vacation SalariedEmployee where
    timeOff (SalariedEmployee _) = maxTimeOff
~~~

Now, despite their previous advice, management hands down a third business requirement that there will be a new employee type, who is to be paid a lump sum on completion of the allocated work, and who receives no paid vacation:

~~~{.haskell}
module Employee.Contract

data ContractEmployee = ContractEmployee { workCost :: Double, wasWorkCompletedThisMonth :: Bool }

instance Payable ContractEmployee where
    pay (ContractEmployee _ False) = 0.0
	pay (ContractEmployee workCost True) = workCost

instance Vacation ContractEmployee where
    timeOff (ContractEmployee _ _) = 0
~~~

This time, an employee was able to implement a new data type supporting existing operations by implementing existing type classes, without the need to modify any existing code.

Type classes allow Haskell developers to do more while writing less code. They allow a form of ad-hoc polymorphism and enable a degree of separation which is not possible in other languages.
