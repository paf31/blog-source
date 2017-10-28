---
title: The Game of Pattern Matching
author: Phil Freeman
date: 2017/10/28
description:
tags: Haskell
---

Here is a simple game for two players which you might like to play at your next Haskell meetup.

### Setup

First, the two players agree on an algebraic data type, expressible in Haskell. Recursive types tend to work best.

For example, we might choose the type of `[()]` of lists of elements of type `()`.

### Gameplay

The goal is to write an exhaustive function `f` from the chosen type to `()`, by pattern matching. The return type isn't important, so I use `()` here arbitrarily.

So the players agree on the type signature:

```
f :: [()] -> ()
```

Player one goes first, and writes down the first line of the function, which consists of a pattern recognizing some subset of the values of the chosen type.

For example, player one might choose the following:

```
f (_ : _ : _) = ()
```

Notice that the only thing which is important here is the pattern itself.

Player two goes next, but there is an additional rule - a player cannot introduce a _redundant pattern_. For example, it would be an illegal move for player two to write down the pattern `(_ : _ : _ : _)` at this point, since that pattern doesn't match any new values.

Play continues in this way, and the last player to write down a non-redundant pattern loses.

So, for example, player two might legally write down the next line as:

```
f [] = ()
```

forcing player one to cover the only remaining case:

```
f [_] = ()
```

and so player two would win.

### Explanation

This game is an example of a [poset game](https://en.wikipedia.org/wiki/Poset_game), where the poset is given by the choice of type. It generalizes [Nim](https://en.wikipedia.org/wiki/Nim), since choosing an N-ary sum of natural numbers recovers a game of Nim with N heaps.

It is possible for gameplay to go on indefinitely. For example, in the game above, the players could alternately choose patterns matching lists of increasing fixed length: `[]`, `[_]`, `[_, _]`, ..., and the function would never become exhaustive. However, it is possible to force a finite game by choosing a pattern which matches a cofinite number of values.

Try it and let me know what you think!
