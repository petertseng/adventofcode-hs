# Advent of Code

These are my solutions to http://adventofcode.com

All solutions are written in Haskell.

[![Build Status](https://travis-ci.org/petertseng/adventofcode-hs-2015.svg?branch=master)](https://travis-ci.org/petertseng/adventofcode-hs-2015)

## Input

In general, all solutions can be invoked in both of the following ways:

* Without command-line arguments, takes input on standard input.
* With 1+ command-line arguments, reads input from the first, which must be the path to an input file.
  Arguments beyond the first are ignored.

Some may additionally support other ways:

* 4 (Advent Coins): Pass the secret key in ARGV.
* 10 (Look and Say): Pass the seed sequence in ARGV.
* 11 (Passwords): Pass the initial password in ARGV.
* 20 (Factors): Pass the target number of gifts in ARGV.
* 21 (RPG): Pass the Boss's HP, damage, and armor in ARGV, in that order.
* 22 (Wizard): Pass the Boss's HP and damage in ARGV, in that order. Pass `-d` flag to see example battles.
* 25 (Triangular): Pass the row and column number in ARGV, in that order.

## Highlights

I wrote these as a learning experience in Haskell.
I noticed a few things along the way.

### Travis

This seems to be an interesting house of cards.
Travis doesn't support 7.10, so many projects use the hvr-ghc setup.
This has a few interesting hacks, most notable of which is using the `compiler:` field in the Travis matrix to separate caches per GHC version.
It's fascinating to see how it all developed, but I wonder if it will break one day.

### Cabal

I knew the language decently but had no idea about managing a project written in the language.

I had never worked with Cabal before, and it shows in the early commits of this repository:
Note the custom build script.
Notably, I could not figure out how to get the code for two separate days to be in the same directory.
When I would direct GHC to compile one day, I'd get the binary for the other day.
Turns out it's because the main module always gets named Main.
Eventually I learned it's not so hard to get a project set up to be compatible with `cabal build`, etc.

For similar reasons I decided to experiment with having day 9 (Passwords) be a library that would be used by the executable and that would also have separate tests.
This seemed to work out well.

### Performance

Compared to my Ruby solutions, I'm quite pleased with performance.
Notes on performance:

* The most notable example is that in Ruby I had to use a sweep line approach in order for day 6 (Light Grid) to have reasonable performance.
  In Haskell, I stuck with the array approach and it was still faster than the Ruby sweep line implementation.
* Day 9 (Hamiltonian) and Day 13 (Optimal Seating) are understandably slightly slower than Ruby because Haskell is using the naive brute force approach while Ruby uses the Held-Karp algorithm.
* Day 20 (Factors), the second-slowest problem in Ruby (taking about 2 seconds) takes about 0.2 seconds in Haskell.
  Many other problems had order-of-magnitude speedups as well, of course, but this one was quite visible.

It's been said that non-strict evaluation makes it difficult to reason about performance at times.
I only encountered a bit of this, so it remains to be seen how I will deal with this in projects where this is critical.
I did run out memory a few times when developing some solutions that did not properly cache results but needed to.

### Types

I'm pleased with how the types worked out.

I admit I'm not as general as I could be in some places (using Int instead of Num, [] instead of Foldable).
Since I'm not writing a library for others to use, this is not so disastrous.

I got very good use out of the State monad (for caching computations) and STUArrays.

I remain mystified at how `printf` can work (even taking a variable number of arguments).
The regex interface is quite nice as well.

There were times where it was difficult to understand how to solve a type problem, and thus the code would not compile.
The two most prominent examples:

* How to make code that would accept a STUArray whose elements can be either Bool or Int in day 6 (Light Grid)
* Day 22 (Wizard) could have really done with a `scanM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> [m a]`.
  This is not possible in general; it must be `scanM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m [a]`.
  I imagine it is not possible in general to transpose monads like I wanted.

### Packages

As of 2019, building Haskell packages on my development machine is (mostly) broken, so I was forced to implement the following days without packages, when I otherwise would have preferred to use packages:

* Day 05 (Nice Strings): Regex
* Day 11 (Passwords): Regex
* Day 12 (JSON Numbers): JSON

Luckily, MD5 is not broken, so Day 04 (Advent Coins) still works.

The real version of Day 05 and Day 12 are in the `deps` branch.
The no-package versions feel too imperative with the manual iteration.

### Closing Thoughts

I'm sure I was frustrated at some times because of some of the above points, but in general I'm happy with how these turned out.
Haskell's a really cool language.
