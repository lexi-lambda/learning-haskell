#lang scribble/base

@(require "../shared.rkt")

@title[#:tag "day-1"]{Day 1: Getting up and running}

Alright. So. I'm going to try this again. Starting from scratch and doing it right this time.

I'll spare you the gory details, but to make a long story short, it worked!

@terminal{
$ ghc-mod
ghc-mod: No command given (try --help)
}

Now we're talking. Time to run a simple @tt{cabal @|~~|init}, and finally, @italic{finally} I'm able
to actually work in a reasonable environment.

So. Here I am. What now?

@section{Hello again, Haskell.}

Alright, so, here's the thing: I'm not very good at Haskell. In fact, I barely know the language. I
know it well enough to write your usual simple programs—fibonacci numbers, factorial functions,
etc.—so I'm not going in completely cold. That said, this language is probably a little too
complicated for me to just blindly charge into on my own.

After a little bit of looking around, I found a @hyperlink["http://www.seas.upenn.edu/~cis194/"]{
pretty neat Haskell course offered by UPenn}. All the lecture notes and homework assignments are
online, so this seems like a reasonable way to get a guided introduction to the language. I've heard
the name @italic{Real World Haskell} thrown around a few times, so maybe I'll look into that next, but
first I just want to reacquaint myself with this language.

Alright. Homework assignment one! I took a brief glance at the lecture notes, but I didn't really read
them—I think I remember enough to at least complete whatever this assignment is. We'll see if I'm
wrong.

So apparently, this homework assignment is about implementing an algorithm to validate credit card
numbers. Seems straightforward enough. First, it wants me to implement @tt{lastDigit} and
@tt{dropLastDigit} functions. Let's give this a shot.

@codeblock{
lastDigit :: Integer -> Integer
lastDigit = `mod` 10}

I was feeling pretty good about myself. I remembered backticks for infix notation and that I can use
partial application of operators' second arguments! I can already see the road to hell is paved with
point-free style. Anyway, despite my jubilations, that doesn't even work—GHC complains about a parse
error. Maybe I need more parens?

@codeblock{
lastDigit :: Integer -> Integer
lastDigit = (`mod` 10)}

And it works! Perfect. Now let me try @tt{dropLastDigit}. Armed with my newfound knowledge, this
should be a snap.

@codeblock{
dropLastDigit :: Integer -> Integer
dropLastDigit = (/ 10)}

Hmm. That doesn't work, either, complaining about type mismatches and missing instantiations. My gut
tells me that this is because @tt{/} probably performs non-integral division, and GHC doesn't like
that I'm throwing the precision away. Or maybe it flat-out doesn't work on integers? I'm not sure.

What I @italic{do} remember is that @hyperlink["https://www.haskell.org/hoogle/"]{Hoogle} exists, and
it's awesome. I try a quick query for @tt{Integer -> Integer -> Integer}, which nets me nothing
related to division. Oh, well. It was worth a shot. In desperation, I decided to type in @tt{divison},
and as I was doing so, @tt{div} appeared before my eyes, which apparently has the type @tt{Integral a
=> a -> a -> a}. I'm not sure why that didn't come up in my other search. I guess there were just too
many other results.

Anyway, using @tt{div} does what I want.

@codeblock{
dropLastDigit :: Integer -> Integer
dropLastDigit = (`div` 10)}

Alright, what's next? A function to split a number into its digits in reverse order called, not
surprisingly, @tt{toRevDigits}. I think I remember enough about Haskell to make this work.

@codeblock{
toRevDigits :: Integer -> [Integer]
toRevDigits n
  | n <= 0    = []
  | otherwise = lastDigit n : toRevDigits $ dropLastDigit n}

Hmm. That doesn't work. I'm not sure why, but I'm @italic{almost} certain it has to do with order of
operations. I'm far too lazy to actually figure that out, so parens should do the job fine.

@codeblock{
toRevDigits :: Integer -> [Integer]
toRevDigits n
  | n <= 0    = []
  | otherwise = lastDigit n : toRevDigits (dropLastDigit n)}

Sure enough, that fixed things. Still, something about me doesn't like the asymmetry between the two
expressions on either side of the cons (is it still called that in Haskell?). I remember something
about @tt{where} clauses; let's try that.

@codeblock{
toRevDigits :: Integer -> [Integer]
toRevDigits n
  | n <= 0    = []
  | otherwise = d : toRevDigits ds
     where d  = lastDigit n
           ds = dropLastDigit n}

Well, that seemed to work, but is it nicer...? I don't know. Now it just seems more verbose, honestly.
I can't really decide, but I guess I'll just leave it as-is for now.

Next problem: write a @tt{doubleEveryOther} function which does precisely what it sounds like. Aha!
Pattern-matching time.

@codeblock{
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []       = []
doubleEveryOther [x]      = [x]
doubleEveryOther (x:y:ys) = x : (y * 2) : doubleEveryOther ys}

...and it compiles! Yay, I finally did something right. I actually had to fiddle with the parens
around the last pattern—I forgot they were necessary—but otherwise everything appears to be in order.

Now, a function that sums all the digits in a list of numbers. I don't @italic{think} I'm supposed to
be aware of higher-order functions at this point in this classwork, but I don't really care. This is
a perfect opportunity to use them.

@codeblock{
sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toRevDigits}

I originally typed @tt{concat . map}, but @tt{ghc-mod} helpfully pointed out that I could use
@tt{concatMap} instead. I figure it's more efficient since it doesn't need to build the intermediate
list.

Finally, one more function: putting everything together to write a simple credit card validation
function. I like these sorts of problems: all the parts already exist, so it's obviously easy to put
them together.

@codeblock{
luhn :: Integer -> Bool
luhn n = sumDigits (doubleEveryOther $ toRevDigits n) `mod` 10 == 0}

Done.

Well, that wasn't terribly interesting, though it was helpful to get used to working in the language
again. I think I'll go through the next assignment, and if it doesn't get better, I might have to
switch to something else. For now, though, I feel somewhat accomplished, despite having written an
extremely trivial set of functions.
