---
title: Why You Should Use PureScript
author: Phil Freeman
date: 2017/08/09
description:
tags: PureScript
---

### Better Abstractions

PureScript has type classes, and we use them to create _very_ reusable code. Write less code by using a language which allows you to reuse more.

How many times have you written [`traverse`](https://pursuit.purescript.org/packages/purescript-foldable-traversable/3.4.0/docs/Data.Traversable#t:Traversable) in a language without type classes? Entire NPM libraries exist just to implement `traverse` once. All of these things can be accomplished with the same `Traversable` instance:

- Waiting for a list of AJAX requests to complete
- Validating a list of form fields
- Ensuring a list of nullable values are all non-null
- Finding a list of values which maximizes the value of some function

If I change the word "list" in each of these examples for the word "stream" or "array" or "tree", I can continue to use `traverse`, because all of these structures implement the same type class!

In languages without higher kinded polymorphism and type classes, you have to write `traverse` once for each container/`Applicative` pair - that's a lot of functions! Instead, just write `traverse` once for each container and treat them uniformly using a type class.

Better abstractions also let you express more general ideas, and to talk about concepts abstractly.

### Better Libraries

[Pursuit](https://pursuit.purescript.org/) hosts documentation for hundreds of PureScript libraries, supporting all sorts of different types of development:

- Single page applications
- Web services
- Server-side rendering
- Database applications
- Mobile development
- Testing and benchmarking
- Graphics and audio
- Game development
- Data structures and algorithms

You name it, Pursuit has a library for it. And if we don't have the exact library you want, you can easily wrap any existing JavaScript libraries available on NPM using the foreign function interface!

### More Guarantees

In JavaScript, we get very few guarantees about our code. For example, we rarely can know for sure that

- Our data is even in the right format
- A value is never `null`
- We handled exceptions in the right places
- We removed a piece of debugging code before deploying to production
- We handled all possible cases in a complex pattern match
- We didn't leak a reference to a mutable data structure
- We took the right steps to avoid possible SQL injection attacks
- Our application doesn't accidentally [launch any missiles](https://hackage.haskell.org/package/acme-missiles-0.3/docs/Acme-Missiles.html).

PureScript's type system provides a way to prevent all of these bugs, and many more.

### Better Records

Extensible records are a great fit for JavaScript, but of limited use when you can't use common JavaScript idioms.

In PureScript, [we can express](https://github.com/purescript/purescript-typelevel-prelude/) many common patterns such as mapping over and traversing the properties of a record, merging and zipping records and more, all in a type-safe way.

You can even [abstract over the labels in a record](https://pursuit.purescript.org/packages/purescript-profunctor-lenses/3.2.0/docs/Data.Lens.Record#v:prop). Never write another lens again, when you can derive a lens for any record property automatically using the type system.

### Better Tools

PureScript supports [instant rebuilds](https://github.com/paf31/24-days-of-purescript-2016/blob/master/15.markdown), so that you can see errors in your editor immediately. Editor plugins will even fix warnings and minimize imports for you.

Put types to work! [Type-directed search](https://github.com/paf31/24-days-of-purescript-2016/blob/master/23.markdown) will use type information to automatically find the programs which fit into the gaps in your code.

Let the compiler write your code for you. PureScript will derive instances of many [common](https://github.com/paf31/24-days-of-purescript-2016/blob/master/3.markdown) [type](https://github.com/paf31/24-days-of-purescript-2016/blob/master/4.markdown) [classes](https://github.com/paf31/24-days-of-purescript-2016/blob/master/5.markdown). For those it doesn't support, you can often use [generic deriving](https://github.com/paf31/24-days-of-purescript-2016/blob/master/11.markdown) to fill in the implementation.

### Better Techniques

PureScript supports many of the development paradigms pioneered in Haskell. For example: 

- [Datatype-generic programming](https://github.com/paf31/24-days-of-purescript-2016/blob/master/11.markdown)
- Type-level programming with [functional dependencies](https://github.com/paf31/24-days-of-purescript-2016/blob/master/10.markdown)
- [Property-based testing](https://github.com/purescript/purescript-quickcheck)
- Denotational design

PureScript is also a great language for implementing the latest techniques.

PureScript also makes a great playground for testing out ideas for new programming language features. You can even build [alternative backends](https://github.com/purescript/documentation/blob/master/ecosystem/Alternate-backends.md) for the compiler if you'd like to test out ideas which don't target JavaScript.

### A Great Community

Find like-minded individuals on Slack, [Reddit](http://reddit.com/r/purescript) and IRC who are enthusiastic about bringing pure typed functional programming to the web browser.

## Common Complaints

### "Sharing code is difficult"

This used to be true, but now there are great options like [`purescript-bridge`](https://hackage.haskell.org/package/purescript-bridge) and [`servant-purescript`](https://github.com/eskimor/servant-purescript).

Datatype-generic programming makes it very easy to derive most of the code you'll need for any shared types, including the most important one, JSON serialization.

### "Strictness not a good default"

This is an interesting complaint. I use Haskell every day, and I make use of laziness extensively. There are great blog posts on the internet ([for example](http://augustss.blogspot.com/2011/05/more-points-for-lazy-evaluation-in.html)) about the pitfalls of dropping laziness in a Haskell-like language.

I think it is a mistake to use PureScript as if it were a lazy language, just like it is a mistake to use Haskell as if it were strict (space leaks are a good example of this common mistake).

This means you cannot naively port Haskell code to PureScript and expect it to work. It might work, but be slow, or it might crash due to a loop, or just not even compile.

But we have great tools to support pure FP in a strict language, some new, and others inspired by techniques pioneered in other strict FP languages like Scala.

Ultimately, if you really want all the benefits of laziness-by-default, you should use GHCJS, but be aware that for many use cases, opt-in laziness in a strict-by-default language turns out to be a perfectly practical option.

### "Hiring is hard"

The Haskell community has proven this to be false - hire remote PureScript developers (and be willing to train them if necessary) and you will have no shortage of qualified candidates.

Alternatively, PureScript can be adopted gradually by leaning on the foreign function interface to integrate with existing JavaScript code. So you can hire JavaScript developers with some functional programming experience, and get the benefits of PureScript as you transition existing code over time.

### "Training is hard"

Learning PureScript is not easy, but it is very possible. It can take a while.

Training others is another tricky problem, and it also takes time.

I'd recommend working through [the Haskell book](http://haskellbook.com/) first if you are not familiar with Haskell or pure functional programming. Then work through [the PureScript book](https://leanpub.com/purescript) and read through the documentation of the core libraries on Pursuit to get an idea of the way PureScript code works.

### "Too much category theory"

You don't need to know category theory to be proficient in PureScript, although it certainly helps with some of the more esoteric core libraries. Learning category theory is a long term investment.

If you are interested in learning the basics, I would recommend working through the first chapters of one of the standard category theory texts, some of which are aimed at computer scientists.

### "Not enough support"

At the end of the day, nobody is paid to work on the PureScript compiler. This might change one day, but for now, it is a mistake to expect long term support. I recommend treating PureScript like any other open source library.

That said, if you do need better support, that can probably be arranged in exchange for hard currency.

### "Documentation is lacking"

We try our best to provide high-quality documentation, but it is a hard problem to communicate complex ideas clearly.

[PRs are welcome](https://github.com/purescript/documentation)! If you have ideas for how to improve documentation, please let us know, but please don't be upset if we tell you that we're not going to rename `Functor` to `Mappable`.

### "PureScript changes too quickly"

PureScript will continue to see breaking changes until we hit 1.0, but that milestone is getting closer. Things are stabilizing, and we don't expect many breaking changes between 0.12.0 and 1.0.

### "I don't like Bower"

There are reasons why we use Bower, which Harry has [written about in detail](http://harry.garrood.me/blog/purescript-why-bower/). Bower may or may not be a good fit for JavaScript any more, but it's still a great fit for PureScript.

But you don't have to use Bower. Alternative package managers such as [`psc-package`](https://github.com/purescript/psc-package/) and [Purify](https://github.com/chrisdone/purify) already exist.

Some users use PureScript with [Nix](https://nixos.org/nix/). Others use Git directly. PureScript assumes nothing about the package manager you use.

### "I don't want to use Node"

See above. `psc-package` and Purify in particular do not depend on Node.

For the REPL, you can [use the `--port` option to evaluate code in the browser](https://github.com/paf31/24-days-of-purescript-2016/blob/master/8.markdown), instead of using Node.
