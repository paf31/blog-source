---
title: Teaching Haskell To Juggle
author: Phil Freeman
date: 2013/05/25
description: 
tags: Haskell
---

<link rel="stylesheet" href="../assets/juggling.css" type="text/css"></link>

<script src="../assets/jquery.min.js" type="text/javascript"></script>

<script src="../assets/juggling.min.js" type="text/javascript"></script>

While recently learning some new juggling tricks, I decided to write a few Haskell functions to find valid patterns.

Siteswaps are a method of describing the throws involved in various juggling patterns. I'm only interested here in so-called *vanilla* siteswaps, where a single ball is thrown from each hand, and hands take turns to throw balls. I'll give a very basic overview of vanilla siteswaps and associated ideas, before showing the Haskell code. Interested readers are encouraged to survey the online literature covering the theory of siteswaps, which is extensive.

The numbers in a siteswap pattern represent the heights to which the various balls are thrown. Odd numbers indicate throws to the opposite hand, and even numbers indicate throws to the same hand. The larger a number, the higher the throw, and the longer the ball will be in the air.

For example, the siteswap pattern `33` represents the three ball cascade. We can read it as follows: the first hand throws a ball to height 3 into the second hand on beat 1, followed by the second hand throwing a ball to height 3 into the first hand on beat 2. After this, the pattern repeats. The first ball will land in the second hand on beat 4, three beats after it was thrown.

The siteswap pattern `441` represents a 3-ball pattern which can be inserted as a trick into the regular 3-ball cascade. The notation indicates that the trick consists of a self-throw from each hand to height 4 on beats 1 and 2, followed by the first hand throwing a ball quickly to the second hand on beat 3.

<div class="j" j="[4,4,1]" style="float: left;"></div>

<div class="j" j="[3,3,3,3,3,3,4,4,1]" style="float: left;"></div>

<div style="clear: left;"></div>

The 3-ball pattern `441` (left), and the same pattern inserted into the 3-ball cascade (right).

The number of props used in a pattern is given by the average of the numbers in the pattern. We can see therefore that the 3-ball cascade does indeed use 3 props, as does `441`.

Not every sequence of integer values constitutes a valid siteswap pattern. We need to make sure that on each beat, at most one ball lands in the appropriate hand. For example, if we know that on a given beat, a ball is already scheduled to land in the opposite hand on the next beat, then we cannot throw a 1, since that will result in a collision.

This determines a graph of underlying states and valid transitions, whose paths correspond to the valid siteswap patterns. The Haskell code below will allow us to query this graph, and ask things like "what is the complete list of valid n-ball tricks which can be inserted into a n-ball cascade?"

The nodes of the graph will need to contain enough data to answer whether or not a ball is scheduled to land `n` beats in the future for all `n < N` for some `N`. We could therefore take the type of states to be a list of booleans of length `N`. However, it is more conventional to pack these booleans as bits into a single integer and to refer to states by this integer, so we will take the type of states to be a (newtype-wrapped) integer. The number of bits is equal to the maximum height to which a ball will be thrown.

For example, the 3-ball cascade stays in state 7 = 2^0^ + 2^1^ + 2^2^ = 111~2~. On each beat `n`, a ball is scheduled to land on beats `n`, `n+1`, and `n+2`. Throwing a ball to height 3 will schedule a ball to land on beat `n+3`.

We will represent heights, patterns and states as follows:

~~~{.haskell}
import Data.Bits
import Data.Maybe
import Data.List
import Control.Monad
import Control.Applicative
 
newtype Height = Height { height :: Int } deriving (Show, Eq, Ord)
 
newtype Pattern = Pattern { pattern :: [Height] } deriving (Show, Eq)
 
newtype State = State { state :: Int } deriving (Show, Eq, Ord)
~~~

We can transition to a new state by throwing a ball to a given height. If the first bit of the state is zero, then there is no ball scheduled to land, hence nothing available to throw, and the only valid throw is a fake throw to "height" 0. If the first bit is a one, then we can throw to any height `h` such that the bit at position `h + 1` is a zero.

A valid throw is indicated by a return value constructed using `Just`. An invalid throw returns `Nothing`.

~~~{.haskell}

throw :: Height -> State -> Maybe State
throw (Height 0) (State s) | (s .&. 1) == 0 = 
  Just $ State $ s `shiftR` 1
throw (Height h) (State s) | h > 0 && ((1 `shift` h) .&. s) == 0 = 
  Just $ State $ (s `shiftR` 1) .|. (1 `shift` (h - 1))
throw _ _ = Nothing
~~~
  
We can also use `foldM` to attemt to throw a pattern. The return value will be `Nothing` if any throw was invalid.

~~~{.haskell}
throwMany :: Pattern -> State -> Maybe State
throwMany p s0 = foldM (flip throw) s0 (pattern p)
~~~

To find valid patterns, we need to find cycles in the graph of states and throws.

A graph can be represented by a list of vertices together with a list of edges:

~~~{.haskell}  
type Graph v e = ([v], [(v, v, e)])
~~~

Cycles are just paths whose start and end vertices coincide. We can find paths by choosing a start vertex and performing a depth first traversal of the graph, keeping track of the vertices we have already visited:

~~~{.haskell}  
paths :: (Eq v) => Graph v e -> [(v, v, [e])]
paths (vs, es) = 
  [ (v1, v2, p) 
  | v1 <- vs
  , (v2, p) <- paths' v1 es [] ] 
  where
  paths' v es visited = (v, []) : 
    [ (v3, (e:p))
    | (v1, v2, e) <- es
    , v1 == v
	, not $ v2 `elem` visited
	, (v3, p) <- paths' v2 es (v1:visited) ]
~~~

Cycles are then given by filtering the list of paths:

~~~{.haskell}  
cycles :: (Eq v) => Graph v e -> [(v, [e])]
cycles (vs, es) = [ (v1, p ++ [e]) 
                  | (v1, v2, p) <- paths (vs, es)
                  , not $ null p
                  , (v2', v3, e) <- es
                  , v2 == v2'
                  , v3 == v1 ]
~~~

In order to generate the graph of states and throws, we need to fix a maximum height and a number of balls to throw.

The number of 1 bits in a state represents the number of balls in the air:

~~~{.haskell}    
countBits :: Int -> Int
countBits 0 = 0
countBits 1 = 1
countBits n = let (d, r) = n `divMod` 2 in countBits d + countBits r
~~~

The function `throw` is the transition function which can be used to tell whether or not a throw corresponds to an edge in the graph. Here is a function which generically constructs a graph from such a function:

~~~{.haskell} 
generateGraph :: (Eq v) => (e -> v -> Maybe v) -> [v] -> [e] -> Graph v e
generateGraph transition vs es =
  let edges = [ (v1, v2, e) 
			  | v1 <- vs 
			  , v2 <- vs
			  , e <- es
			  , transition e v1 == Just v2] in (vs, edges)
~~~

We can now construct the state transition graph for a number of balls with a maximum throw height by using `generateGraph` with `throw`. We can use `countBits` to construct the set of states given the maximum height `maxHeight`.

~~~{.haskell} 
graph :: Height -> Int -> Graph State Height
graph (Height maxHeight) numBalls = 
  let maxState 	= (1 `shiftL` maxHeight) - 1
      states 	= map State $ filter ((== numBalls) . countBits) [0..maxState] in
  generateGraph throw states (map Height [0..maxHeight])
~~~

Enumerating valid patterns for a number of balls with a maximum throw height is as simple as finding cycles in this graph:

~~~{.haskell} 
validPatterns :: Height -> Int -> [(State, [Height])]
validPatterns maxHeight numBalls = cycles $ graph maxHeight numBalls
~~~

For example, we can find all 3-ball patterns with a maximum height of 4, along with their initial states, as follows:

    ghci> map (state *** map height) $ validPatterns (Height 4) 3
    [(7,[3,3]),(7,[3,4,2]),(7,[3,4,4,1]),(7,[3,4,4,4,0]),(7,[4,2]),(7,[4,4,1]),...

Finally, we can answer this question from earlier in the post: what is the complete list of valid n-ball tricks which can be inserted into a n-ball cascade?

A trick can be inserted into the n-ball cascade whenever its initial and final states equal the steady state 2^n^-1 of the n-ball cascade.
    
~~~{.haskell} 
tricks :: Height -> Int -> [[Height]]
tricks maxHeight numBalls = 
  map snd 
  $ filter ((== (State (2^numBalls - 1))) . fst) 
  $ validPatterns maxHeight numBalls
~~~

For example, here are the tricks with maximum height 4 which can be inserted into the 3-ball cascade:

    ghci> map (map height) $ tricks (Height 4) 3
    [[3,3],[3,4,2],[3,4,4,1],[3,4,4,4,0],[4,2],[4,4,1],[4,4,4,0]]

Addendum: Juggling Animator in Elm
---

I originally wrote this juggling animator in Haskell using the HOpenGL library for rendering, and later ported the code to Elm.

First, we need to define some constants, including the pattern to throw:

~~~{.haskell}
-- The fraction of time spent holding the ball in the hand
pauseTime = 0.35
 
-- The number of milliseconds per beat
timePerThrow = 300
 
-- The displacement of the "hands" during a throw
handMovement = 0.2
 
-- The pattern to animate
pattern = [5]
~~~

A key function is `rotatePattern`, which is used to rotate a list of heights by a number of steps. This is used to shift a pattern after a throw occurs, or to adjust a pattern for each ball being juggled.

~~~{.haskell}
rotatePattern : Int -> [a] -> [a]
rotatePattern n xs =
  if | n < length xs -> drop n xs ++ take n xs
     | otherwise     -> rotatePattern (n - length xs) xs
~~~

Given the current time (in units of beats), we can calculate the height of the current throw, and the coordinates of the current ball by repeatedly rotating the pattern:

~~~{.haskell}
heightAtTime : [Int] -> Float -> Int
heightAtTime hs t =
  let h = head hs in
  if | t < toFloat h     -> h
     | otherwise         -> heightAtTime (rotatePattern h hs) (t - toFloat h)
 
xAtTime : [Int] -> Float -> Float
xAtTime hs t =
  let h = head hs in
  if | t < pauseTime                       -> t / pauseTime * handMovement
     | (t < toFloat h) && (h `mod` 2 == 0) -> handMovement * (toFloat h - t) / (toFloat h - pauseTime)
     | t < toFloat h                       -> handMovement + (1 - handMovement) * (t - pauseTime) / (toFloat h - pauseTime)
     | h `mod` 2 == 0                      -> xAtTime (rotatePattern h hs) (t - toFloat h)
     | otherwise                           -> 1 + xAtTime (rotatePattern h hs) (t - toFloat h)
 
yAtTime : [Int] -> Float -> Float
yAtTime hs t  =
  let h = head hs in
  if | t < pauseTime -> 0
     | t < toFloat h -> (t - pauseTime) / (toFloat h - pauseTime)
     | otherwise     -> yAtTime (rotatePattern h hs) (t - toFloat h)
~~~

The `x`-coordinate needs to be clipped to the range `0-1`. Reflections occur at integer values of `x`, since the `x` coordinate represents the total displacement along the `x`-axis.

~~~{.haskell}
clip : Float -> Float
clip x =
  if | x > 1     -> 1 - clip (x - 1)
     | x < 0     -> clip (0 - x)
     | otherwise -> x
~~~

We can now compute the position of each ball. I have hard-coded the width and height of the canvas to 300 pixels here, but these could be provided as parameters.

~~~{.haskell}
project : Float -> Float -> Int -> Int -> (Float, Float)
project x y h maxHeight = 
  let
    xProj = 280.0 * x - 140.0
    yProj = 280.0 * (4.0 * toFloat h * y * (1.0 - y) / toFloat maxHeight) - 140.0
  in
    (xProj, yProj)
    
positionOfBallAt : [Int] -> Int -> Float -> Int -> (Float, Float)
positionOfBallAt heights maxHeight time index =
  let 
    x      = xAtTime heights time
    y      = yAtTime heights time
    cx     = clip (toFloat index + x)
    h      = heightAtTime heights time
  in
    project cx y (h - 1) maxHeight
~~~

`positionOfBallAt` gives the position of the ball, and `circle` is used to render it. The `frame` function renders each ball as a circle at its current position:

~~~{.haskell}
frameFor : [Int] -> Int -> Int -> Int -> Int -> Form
frameFor heights timePeriod maxHeight millis index =
  let 
    startMillis     = index * timePerThrow
    relativeTime    = toFloat ((millis - startMillis) `mod` timePeriod) / toFloat timePerThrow
    rotatedPattern  = rotatePattern index heights
    position        = positionOfBallAt rotatedPattern maxHeight relativeTime index
  in 
    circle 10 |> filled black |> move position
 
frame : [Int] -> Int -> Element
frame heights millis = 
  let
    period          = 2 * sum heights
    timePeriod      = timePerThrow * period
    maxHeight       = maximum heights
  in
    [0 .. period - 1] |> List.map (frameFor heights timePeriod maxHeight millis) |> collage 300 300 
~~~

`main` simply invokes `frame` to render the current scene once every millisecond (or as fast as possible):

~~~{.haskell}  
main : Signal Element
main = lift (frame pattern . round) (every millisecond)
~~~