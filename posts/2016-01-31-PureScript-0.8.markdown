---
title: Announcing PureScript 0.8
author: Phil Freeman
date: 2016/01/31
description:
tags: Haskell
---

It has been a very long release cycle, but [the 0.8 release of the PureScript compiler](https://github.com/purescript/purescript/releases/tag/v0.8.0) is now ready, after about six months of work. Since 0.7, the PureScript compiler and its tools have seen some rather interesting developments, and I'd like to write a little about each of them.

## Google Summer of Code

This year, PureScript took part in the [Google Summer of Code](https://developers.google.com/open-source/gsoc/) program, thanks to the Haskell organisation, who generously gave us two of their own slots. I'm happy to report that both projects were a fantastic success.

### Exhaustivity Checking

The first GSOC project involved making changes to the PureScript compiler to alert the user whenever a pattern match does not match all possible cases. This is called _exhaustivity checking_.

For example, the following pattern match is incomplete:

```text
f :: Maybe Int -> Int
f (Just x) = x
```text

and the compiler now helpfully gives us two suggestions:

```text
A case expression could not be determined to cover all inputs.
The following additional cases are required to cover all inputs:

  Nothing

Or alternatively, add a Partial constraint to the type of the enclosing value.
Non-exhaustive patterns for values without a `Partial` constraint will be disallowed in PureScript 0.9.
```text

We can either choose to handle the missing case, or use the type system to track partiality using the `Partial` type class:

```text
f :: Partial => Maybe Int -> Int
f (Just x) = x
```text

In the second case, it becomes the responsibility of the caller to ensure that any calls are safe.

Thanks to [Nicolas Del Piano](https://github.com/nicodelpiano) for his work on this feature this summer!

### Pursuit

The second GSOC project was a package database for PureScript, implementing type search using Hoogle. The result was [Pursuit](http://pursuit.purescript.org), which is a little like a combination of Haskell's Hackage with Hoogle or Hayoo.

Pursuit is accompanied by a command-line tool, `psc-publish`, which is distributed with the PureScript compiler. `psc-publish` is responsible for creating package definitions which can be uploaded to Pursuit using a GitHub OAuth token.

The website speaks for itself, so please check it out! Many thanks to [Harry Garrood](https://github.com/hdgarrood/) for his time spent on the project this summer.

## Performance Improvements

Compiler performance was one of the key goals we identified at the start of the 0.8 milestone. The compiler had been getting gradually slower for a while, and we were aware of at least one major performance regression, which was even causing automated builds to fail due to a lack of memory.

The 0.7.5.1 release contained two major changes to improve performance:

- Space leak fixes
- Parallel builds

First, after a large amount of testing, we identified a large space leak due to the use of the `WriterT` monad transformer. It turns out that even the `Control.Monad.Trans.Writer.Strict` variant is not strict enough to avoid a tower of thunks (per the [documentation](https://hackage..org/package/transformers-0.4.3.0/docs/Control-Monad-Trans-Writer-Strict.html), _"although the output is built strictly, it is not possible to achieve constant space behaviour with this transformer"_), and in fact it is generally recommended that `WriterT` not be used at all in production code.

The fix was simple enough: since all of our code was only using `WriterT` _indirectly_ via the `MonadWriter` type class, we were able to keep the interface the same but switch in a safe implementation based on `IO`. (This is not quite as trivial as it sounds - implementing `tell` is simple, `listen` and `pass` slightly less so, and ensuring the same semantics when combined with `ExceptT` was slightly tricky...)

The second change was to introduce _parallel builds_. Thanks to Haskell's lightweight threads and excellent concurrency primitives such as `MVar`, this code was a joy to write. Each module gets its own thread, and `MVar`s are used to signal completion of compilation to downstream dependencies.

On a single core machine, the compilation time for large projects might actually increase, but on 4 and 8 core machines, the performance improvements have been considerable. For example, here is an approximate comparison based on the `purescript-halogen` library, a large project involving over 200 modules, running on my MacBook Pro over four cores:

<table class="table table-striped">
  <thead>
    <th></th>
    <th>0.7.0</th>
    <th>0.7.5.1</th>
  </thead>
  <tbody>
    <tr>
      <td>Fresh Build</td>
      <td>~12s</td>
      <td>~5s</td>
    </tr>
    <tr>
      <td>Rebuild</td>
      <td>~3s</td>
      <td>~0.8s</td>
    </tr>
    <tr>
      <td>Memory Used</td>
      <td>~1.5GB</td>
      <td>~100MB</td>
    </tr>
  </tbody>
</table>

## Generic Deriving

As of version 0.7.3, the PureScript compiler now supports _generic deriving_, thanks to [Gershom Bazerman](http://gbaz.github.io).

I've written about this feature in detail [here](http://www.purescript.org/learn/generic/).

## Field puns

PureScript now supports _field puns_ which make it very easy to construct and deconstruct extensible records.

For example, suppose you are reading a JSON object which represents a person:

```text
read value = do
  name    <- readProp "name" value
  address <- readProp "address" value
  return $ Person { name: name, address: address }
```text

The last line is quite noisy, and can be shortened using field puns:

```text
read value = do
  name    <- readProp "name" value
  address <- readProp "address" value
  return $ Person { name, address }
```text

The same works for binders, allowing us to deconstruct a person record using the same syntax:

```text
printPerson (Person { name, address }) =
  printName name <> ": " <> printAddress address
```text

## Improved error messages

The quality of the error messages generated by the compiler has been improved a great deal in recent versions.

Improvements range from simple changes to the way information is laid out, to including new, relevant information in error messages.

I urge you to compare the error messages for yourself, but here is a quick example. Given the following incorrect definition:

```text
f :: forall a. a -> a
f _ = 0
```text

the 0.7 compiler would have generated this error message:

```text
Error in module Test:
Error in value declaration f:
Error at Test.purs line 4, column 7 - line 4, column 7:
  Expression
    1
  does not have type
    a0
```text

Not terrible, but here is the new version:

```text
Error found:
in module Test 
at Test.purs line 4, column 7 - line 4, column 7

  Could not match type

    Int

  with type

    a0

while trying to match type Int
  with type a0
while checking that expression 1
  has type a0
in value declaration f

where a0 is a rigid type variable
  bound at line 4, column 1 - line 4, column 7
```text

The basic problem is clearly identified at the top of the error message, followed by a short explanation of the context in which the error occurred, and descriptions of any type variables appearing in the error.

We still have lots of improvements to make in this area, but I think we've made some great steps.

## Warnings

Version 0.8 includes a large collection of new warnings, to help identify possibly incorrect code. For example:

- Warnings for unused imports
- Warnings for implicit imports, including a list of minimal explicit imports
- Warnings for redundant imports and exports
- Warnings for type wildcards, to infer types of subexpressions
- Warnings for missing type declarations, including the inferred type
- Warnings for shadowed type variables and names

## Pulp 6.0

[Pulp](https://github.com/bodil/pulp) is an automation tool for PureScript projects, making it simple to manage dependencies, build, run and test projects, and generate documentation. While not part of the compiler proper, Pulp has become an invaluable part of the PureScript toolset.

Recently, Pulp was rewritten from scratch in PureScript, making it one of the largest open source PureScript projects, and one of the first examples of a package written in PureScript being published to the NPM repository. 

## Editor Support

Editor support in the compiler has improved markedly in version 0.8. There have been two main improvements:

- The new `--json-errors` flag
- A new "externs" file format

The `--json-errors` flag can be used to render error messages and warnings as a JSON data structure, making it easier for editors to access line numbers and module names in errors. The output of `--json-errors` also includes any suggested text replacements, which makes it much easier for plugins to implement things like automatically minifying import lists, and adding type declarations.

Externs files are generated by the PureScript compiler, and used during incremental compilation. In earlier versions, externs files were actual PureScript modules which used the FFI to make already-compiled modules available to the compiler. However, this approach was slow and involved unnecessary typechecking, so the latest versions of the compiler use a JSON representation for externs, and skip unnecessary steps in compilation when processing these files.

The new JSON representation makes it possible for external tools to read information about PureScript source code. For example, the `psc-ide` tool reads the files and uses the data to enable features like type-lookup and autocomplete in various editors (currently, Emacs and Atom are supported).

## Try PureScript!

The [Try PureScript](https://try.purescript.org/) website has been completely overhauled for the 0.8 release, including a new backend API and the ability to test out any of the PureScript core libraries.

In addition, the new API-based approach has made it easier to deploy similar sites for other libraries, and so far we have deployed [Try Thermite!](https://paf31.github.io/try-thermite/) and [Try Flare!](https://sharkdp.github.io/try-flare/), which allow interactive code editing for two popular PureScript UI libraries. Eventually, I would like to deploy something similar for the Halogen library, as well as the code samples from the PureScript book.

## Future Work

PureScript 0.8 contains certain changes which will become breaking in version 0.9. To prepare for these, we've added new warnings to the compiler, which will warn users whenever a deprecated approach is being used.

### Partial Functions

The first change involves the `Partial` constraint mentioned in the _Exhaustivity Checking_ section above. In version 0.8, partial functions will generate a warning, and users will be able to silence the warning by adding a `Partial` constraint to their function. However, in 0.9, this warning will become an _error_, and users will be required to fix the partiality problem, either by adding the constraint, or by making the function total.

The motivation for this change is the following: we want to avoid partial functions, since they make code error-prone. However, partial functions can be useful, so we sometimes want to be able to use them. The `Partial` constraint gives us the best of both worlds, since it allows us to signal to callers that a function is partial in a way that is checked and propagated by the typechecker.

### Operators as Aliases

In PureScript, it is possible to define custom operators. For example:

```text
(+*) :: Int -> Int -> Int
(+*) x y = x * y + y
```text

However, in version 0.8, this code will generate a warning:

```text
The operator (+*) was declared as a value rather than an alias for a named function.
Operator aliases are declared by using a fixity declaration, for example:

  infixl 9 someFunction as +*
  
Support for value-declared operators will be removed in PureScript 0.9.
```text

This warning indicates that we should instead define our operator as an _alias_ for a regular (named) function:

```text
myAdd :: Int -> Int -> Int
myAdd x y = x * y + y

infixl 9 myAdd as +*
```text

The motivation for this restriction is an improvement in the quality of generated code. Compare the generated code for a simple application `2 +* 2`:

```text
$plus$times(2)(2)
```text

to the generated code using the operator as an alias:

```text
myAdd(2)(2)
```text

In the previous version, we were forced to generated a mangled function name for the operator. With operator aliases, we have a nice compromise: we can generate nice names for operators, but still use them with operator syntax in the PureScript source.

### Deprecated Import Syntax

In 0.8, any modules using class imports will receive a warning like this:

```text
Class export uses deprecated syntax that omits the 'class' keyword:

  Monad

Should instead use the form:

  class Monad

The deprecated syntax will be removed in PureScript 0.9.
```text

In 0.9, this new class import syntax will become the default, and imports will look like this:

```text
import Prelude ( class Monad
               , bind
               , Unit
               , unit
               )
```text

Note that empty parentheses are no longer required when importing a type without its data constructors.

In addition, any "open" imports (which do not specify the imported names explicitly, e.g. `import Prelude`) will generate a warning going forward. Explicit imports are recommended instead.

### Source Maps

A [pull request](https://github.com/purescript/purescript/pull/1693) which will bring _source map support_ is almost ready, and should be merged before release 0.8.1. This means the ability to debug directly on PureScript source code in web browsers which support source maps!

## Conclusion

I hope you'll agree that PureScript 0.8 has some pretty great new features! I'd like to thank everyone who was involved in the implementation, filing bugs, writing documentation, and testing out new features.

We also have a fairly full roadmap for 0.9, which will focus on new typechecker features, improved support for alternative compiler backends, and code generation. As always, we're looking for new contributors, and happy to mentor any developers who are looking to work on open source Haskell. I hope to meet you on #purescript IRC!
