#lang scribble/base

@(require "../shared.rkt")

@title[#:tag "day-2"]{Day 2: Into the rabbit hole}

Okay. Time to keep going. Hopefully things will get a little more interesting this time.

The next section is entitled @italic{Polymorphism and Functional Programming}. This at least sounds
more interesting! Let's take a look at what it has to offer.

@section{@italic{Mastermind} in Haskell}

Apparently, this homework assignment is based on the @italic{Mastermind} puzzle game, which is
actually a pretty cool concept. The introduction mentions something about typeclass constraints, which
I actually remember from my pervious dabblings, though it looks like they haven't been formally
introduced in this class yet.

The first assignment is to write a function that takes two @tt{Code} instances, which are just lists
of colors, and finds how many "exact" matches they share. To me, this sounds like a useful time to use
@tt{zip}! Then I guess I could use @tt{foldl'} and sum everything manually, but I'm sure there's a
nicer way.

Well, first, I can use @tt{map} with zip to get a list of @tt{Bool}s, right? Although then I'd need a
function of type @tt{(a, a) -> Bool}. To Hoogle I go again.

Hrm. Still nothing. Well, I'm sure there's a nice way of doing it, but for now I can just write my own
helper function.

@codeblock{
pairEq :: (a, a) -> Bool
pairEq (a, b) = a == b}

Nope, that still complains about types. Oh, this is where typeclasses come in, right? I seem to
remember the relevant typeclass is called @tt{Eq}, let's see if I remember the syntax.

@codeblock{
pairEq :: Eq a => (a, a) -> Bool
pairEq (a, b) = a == b}

There we go. GHC is happy now. Now things should be fairly easy...

@codeblock{
exactMatches :: Code -> Code -> Int
exactMatches xs ys = map pairEq pairs
  where pairs = zip xs ys}

Wait, no, now I have a @tt{[Bool]}. I need to count up all the @tt{True} values. In Racket, I'd use
@tt{count}. Does Haskell have something similar? A quick query for @tt{[Bool] -> Int} doesn't give me
anything helpful (c'mon Hoogle, I talked about how cool you were and you've done nothing but let me
down), though I guess it wouldn't be hard to piece together with some simple function composition.

@codeblock{
exactMatches :: Code -> Code -> Int
exactMatches xs ys = length . filter id $ map pairEq pairs
  where pairs = zip xs ys}

There we go! It's even pretty elegant, too, except perhaps for the @tt{pairEq} function in there.
Moving on, the next step is to write a function that counts how many of each color there are within a
list. The code provided includes a @tt{colors} list that simply contains all of the possible elements.

@codeblock{
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]}

I have mixed feelings about that, I guess. It feels a little redundant to have to manually list out
all the elements when the information is known statically, and updating the list will be necessary if
the @tt{Peg} type changes. In Racket, writing a macro for this would be trivial, but alas, this is
Haskell. Perhaps there is a way, I'm just not aware of it.

Anyway, this isn't really a hard problem. Here's my solution.

@codeblock{
countColors :: Code -> [Int]
countColors ps = map countColor colors
  where countColor c = length $ filter (== c) ps}

Maybe it could be better? I don't know. It looks fine to me. Now I'm supposed to use that function to
find @tt{all possible} matches between two codes, but order doesn't matter. This is pretty simple,
too.

@codeblock{
matches :: Code -> Code -> Int
matches xs ys = sum $ map (\(a, b) -> min a b) pairs
  where pairs = zip (countColors xs) (countColors ys)}

The lambda still feels annoying. In fact... looks like ghc-mod is telling me something about a
function called @tt{uncurry}. What's its type?

@codeblock{(a -> b -> c) -> (a, b) -> c}

Oh, @italic{awesome}. This is perfect. In fact... I think this makes my @tt{pairEq} function
redundant!

@codeblock{
exactMatches :: Code -> Code -> Int
exactMatches xs ys = length . filter id $ map (uncurry (==)) pairs
  where pairs = zip xs ys

matches :: Code -> Code -> Int
matches xs ys = sum $ map (uncurry min) pairs
  where pairs = zip (countColors xs) (countColors ys)}

Much nicer on both counts. Of course, this only works on pairs, not other tuples. I seem to remember
that the usual approach to this in Haskell is just to add more functions (or use typeclasses). Maybe
there's an equivalent for three-tuples?

Well, searching for @tt{(a -> b -> c -> d) -> (a, b, c) -> d} gives me nothing, as does @tt{uncurry3}.
Maybe it's just not common enough to warrant a built-in function? I'll look into it more if I ever
find myself actually wanting that function.

@section{Oh right, types are @italic{complicated}}

The assignment defines a @tt{Move} type that consists of a @tt{Code} and two @tt{Int}s, which
correspond to the number of exact and inexact matches, respectively. It wants me to implement a
function to build a @tt{Move} given a guess @tt{Code} and the "secret" @tt{Code}.

Well, I already have a function to get the @italic{exact} matches and another function to get @italic{
all} the matches, so writing an @tt{inexactMatches} function should be pretty simple.

@codeblock{
inexactMatches :: Code -> Code -> Int
inexactMatches xs ys = matches xs ys - exactMatches xs ys}

You know what, that sucks. To me, this looks like simple composition between the @tt{-}, @tt{matches},
and @tt{exactMatches} functions. There has to be a way to write this more nicely, right? I seem to
remember that "lambdabot" on #haskell has a point-free generation function (amusingly named
"pointless"), so maybe that will help?

@irc["haskell"]{
<lexi-lambda> @"@"pl \a b -> f a b - g a b
<lambdabot> ap (ap . ((-) .) . f) g}

Eww. That's not helpful at all. So much for that idea. Time to actually ask people who know what
they're doing.

@irc["haskell"]{
<lexi-lambda> Fiddling with Haskell, had a random question:
<lexi-lambda> is there a nicer way to write a function like this?
<lexi-lambda> \a b -> f a b - g a b}

I had a few people suggest it looked pretty nice as-is, so maybe I'm just being silly. But still,
there @italic{has} to be a nice way to do this, right? It's so simple! I got a few more suggestions,
none of which were any nicer than the original. Here are a few:

@codeblock{
curry $ (-) <$> uncurry f <*> uncurry g

curry $ liftA2 (-) $ uncurry f $ uncurry g

up op f g x = f x `op` g x
(^-^) = up (up (-))}

Meh. None of those are nice at all.

@irc["haskell"]{
<mniip> getCompose (liftA2 (-) (Compose max) (Compose min))}

Oh hey, that looks neat! Even if it's not what I'd like it to be, that's getting closer. Maybe if I
check out how it works I can bang it into shape. So what exactly are @tt{getCompose} and @tt{Compose}
and why are they necessary?

Apparently, @hyperlink["http://hackage.haskell.org/package/transformers-0.4.3.0/docs/Data-Functor-
Compose.html#v:Compose"]{@tt{Compose}} is for "composition of functors". What am I getting myself
@italic{into}?

So the documentation says that @tt{Compose} has a single data constructor, @tt{Compose}. It also lists
a value on the same page called @tt{getCompose} that has the type @tt{f (g a)}. Wait, @tt{getCompose}
doesn't even appear to be a function type? Maybe @tt{f} is a partially-applied function type? Or maybe
that doesn't make any sense and I'm making a fool of myself and demonstrating that I don't really get
how Haskell's type system works.

I think it's time to do some reading.

@subsection{Monads, functors, and applicative functors}

I decided to go back and ask some more questions on IRC. My immediate confusion was resolved—
@tt{Compose} is a record type, so @tt{getCompose} just gets at the field. That makes sense, sort of,
though I still don't really understand how it works. It still feels awfully verbose to me. Why can't
I just do @tt{liftA2 (-) max min}?

Let's hop into GHCi and see if it can help me. What are the types of these two long expressions?

@terminal{
> :t getCompose (liftA2 (-) (Compose max) (Compose min))
getCompose (liftA2 (-) (Compose max) (Compose min))
  :: (Ord a, Num a) => a -> a -> a
> :t liftA2 (-) max min
liftA2 (-) max min :: (Ord a, Num (a -> a)) => a -> a -> a}

Alright, so the first one is obviously the type I want. Why does the second one have a constraint
@tt{Num (a -> a)} (and can that ever even be possibly instantiated?)? I guess I don't even really know
what @tt{liftA2} does.

@terminal{
> :t liftA2
liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
> :t liftA2 (-)
liftA2 (-) :: (Applicative f, Num c) => f c -> f c -> f c}

Okay, so the first argument is pretty obvious, but what is @tt{Applicative} and what does it mean
here?

@blockquote{
  A functor with application, providing operations to
  @itemlist[
   @item{embed pure expressions (@tt{pure}), and}
   @item{sequence computations and combine their results (@tt{<*>}).}]
}

Okay, so @tt{Applicative} has a function like @tt{Monad}'s @tt{return} to "lift" values, called
@tt{pure}. Then it can also sequence values using @tt{<*>}. I still have no idea what that means here.
What is @tt{f} getting instantiated with?

Well, I'm providing @tt{min} and @tt{max}, which are functions, and it looks like there's an instance
of @tt{Applicative} for @tt{((->) a)}. But what does that instance @italic{do}?

A quick search has found me a Stack Overflow question, which in turn has redirected me to Chapter 11
of @italic{Learn You a Haskell}. It would probably be helpful for me to read through all of this
patiently but I don't want to do that. Show me the code!

@codeblock{
instance Applicative ((->) r) where  
    pure x = (\_ -> x)  
    f <*> g = \x -> f x (g x)}

Aha! That's helpful. How was I supposed to know that, anyway? Whatever. It looks like @tt{<*>} is just
simple function composition for functions. Or wait, no, that's what @tt{fmap} is. This passes @tt{f}
two arguments, @tt{x} and @tt{(g x)}. How is this useful at all?

(Oh, also, I just learned that @tt{<$>} is an abbreviation for @tt{fmap}, so that's good to know.)

Phew. So I've been reading around on this page for quite a long while now. I sort of understand how
various individual instances of these typeclasses work, but I feel like I'm utterly failing to see
any "bigger picture" about what @italic{all} instances of a particular typeclass do. How would I have
@italic{any} idea what @tt{fmap} does with @tt{IO} if I didn't know the implementation?

I guess I really just don't see the purpose of having these "general-purpose" typeclasses if the
operations they define are so different in each case that they're really just separate functions for
different types. Or am I missing something that ties them together?

@section{It's types all the way down}

Alright, I finally bit the bullet and took the time to read a fair portion of the RYAH chapter. I'm
sure I still barely understand anything, but I think I have enough of an idea to at least reason about
things. From what I can tell, the reason I can't do @tt{liftA2 (-) max min} is because my functions
are binary.

While I @italic{do} now understand how instances of @tt{Applicative} can work with @tt{fmap} to great
utility, I'm still not entirely sure how the whole process works. If I understood the concepts more, I
think just looking at the types of things would be more helpful, but unfortunately, I'm not there yet.

So this does a thing.

@codeblock{
> (,) <$> (Just) <*> (not) $ True
(Just True,False)}

Which is... cool, I guess? I understand what it does—it applies the two functions to @tt{True}, then
applies the first function to both of them. But why?

Let's see. Both @tt{<$>} and @tt{<*>} are left-associative and have the same precedence. What's the
result of the first operation? Well, it's just @tt{fmap (,) Just}. What's the implementation of
@tt{fmap} on functions? It's just composition, so we get @tt{(,) . Just}.

To make this easier on myself, I'll visualize this as the following:

@codeblock{
\a b -> (Just a, b)}

So now what? Now we get to @tt{<*>}. What's @tt{(<*>) (\a b -> (Just a, b)) not}?

@terminal{
> :t (<*>) (\a b -> (Just a, b)) not
(<*>) (\a b -> (Just a, b)) not :: Bool -> (Maybe Bool, Bool)
> :t (,) <$> (Just) <*> not
(,) <$> (Just) <*> not :: Bool -> (Maybe Bool, Bool)}

Okay. So @tt{a <*> b} for functions is just @tt{\x -> a x (b x)}. Expanding that out, we get:

@codeblock{
\x -> (\a b -> (Just a, b)) x (not x)}

Well that's remarkably simple. Why is it so hard for me to understand? Maybe because I just don't get
why it's a useful function to have. Why is this such a general pattern? It seems awfully arbitrary.

I guess all this does is take a function of arity 2 and pass it the result of a single argument
threaded through two functions of arity 1. @italic{Why is this so important?} I don't understand. I'm
confused. What about a function of arity 3?

@terminal{
> :t (,,) <$> (Just) <*> (Left) <*> (*3)
(,,) <$> (Just) <*> (Left) <*> (*3)
  :: Num a => a -> (Maybe a, Either a b, a)}

Alright, that... actually makes sense. Cool. Just to understand how the extra argument affects things,
let me expand this all out again.

@codeblock{
(,,) <$> Just = \a b c -> (Just a, b, c)
     <*> Left = \x -> (\a b c -> (Just a, b, c)) x (Left x)
              = \x c -> (Just x, Left x, c)
     <*> (*3) = \y -> (\x c -> (Just x, Left x, c)) y (y * 3)
              = \y -> (Just y, Left y, y * 3)}

Well. I guess the values are proliferated through the argument list by alpha-conversion as the tree
expands, eventually collapsing down to a single-argument function.

None of this makes my original problem any simpler, though, does it? In all of these cases, the
functions passed in need to be unary. It's been so long I've almost forgotten my original function.

@codeblock{
\a b -> (f a b) - (g a b)}

Now I'm sort of curious... how can I generalize some system for transforming @tt{(-)} into a function
that will accept four arguments: two functions and two values to apply to those functions. Wouldn't
this be theoretically pretty similar to @tt{<*>}? Obviously the simple way would be doing this:

@codeblock{
lift op f g x y = op (f x y) (g x y)}

But that's terribly uninteresting and specialized. Ideally what I'd want would be some series of
operations that would compound a function that takes @italic{n} values with @italic{n} functions of
arity 2 and produce a function that accepts 2 values and produces a result.

Well, let's call my magic operator @tt{<**>}, and the lifting operator (the equivalent of @tt{pure})
will be called @tt{pure2}. Do these names make sense? Probably not. I don't care.

Therefore, my function should look like this:

@codeblock{
inexactMatches = pure2 (-) <**> matches <**> exactMatches}

(Wow, I really had to think for a moment there to remember what I was originally trying to solve.)

So how should this expand, ideally? Well, maybe if I write out the expansion steps like I did above, a
more general pattern will appear.

@codeblock{
pure2 o = \_ _ a b -> (o a b)
 <**> f = \x y -> (\_ _ a b -> (o a b)) x y (f x y)
        = \x y -> (\a b -> (o a b)) (f x y)
 <**> g = \p q -> (\x y -> (\a b -> (o a b)) (f x y)) p q (g p q)
        = \p q -> (\a b -> (o a b)) (f p q) (g p q)
        = \p q -> o (f p q) (g p q)}

Hey! That's not special at all, that's actually just a different implementation of @tt{Applicative}
for functions. It may have taken me thirty minutes just to figure out how that should properly expand,
but now that I've done it, it seems pretty obvious.

@subsection{Build your own instances… maybe?}

So then how can I make this "alternate" instance for the same type? Well, I don't think I can, but
maybe I can do it by making a small wrapper type. I seem to remember reading something like that ages
ago.

Okay, so first I need to define my wrapper type.

@codeblock{
newtype BinaryFunction a b = BinaryFunction (a -> b)}

That seems right. Now I need to implement a @tt{Functor} instance for it, which should still just be
function composition.

@codeblock{
instance Functor (BinaryFunction a) where
  fmap a (BinaryFunction b) = BinaryFunction (a . b)}

Then all that's left to do is implement @tt{Applicative}.

@codeblock{
instance Applicative (BinaryFunction a) where
  pure f = BinaryFunction (\_ _ -> f)
  (BinaryFunction f) <*> (BinaryFunction g) = BinaryFunction (\x y -> f x y (g x y))}

Hmm, that doesn't work, but I'm not sure why. I think it's because @tt{g}'s type implies it can only
be applied to one argument and @tt{f}'s type implies it can only be applied to one. So maybe I need to
modify my @tt{newtype} wrapper to explicity declare it can take two arguments?

@codeblock{
newtype BinaryFunction a b c = BinaryFunction (a -> b -> c)

instance Functor (BinaryFunction a b) where
  fmap a (BinaryFunction b) = BinaryFunction (a . b)}

Wait, no, now @tt{fmap} is giving me errors. It seems that now, since @tt{a} can only be applied to a
single argument, this no longer works. Darn.

Is it possible that this isn't an @tt{Applicative} after all? What if I tried implementing my own
typeclass with the slightly different behavior? Would that work, or is my conclusion incorrect?

@codeblock{
class BinaryFunctor a where
  fmap2 :: }

Wait, how does this even work? I think the original @tt{Applicative} law should have an analogue here.

@codeblock{
fmap2 f g = pure2 f <*> g}

Reasoning backwards from my "definitions" for @tt{pure2} and @tt{<*>}...

@codeblock{
pure2 f <*> g = \x y -> (\_ _ a b -> (f a b)) x y (g x y)}

That seems right... although honestly at this point I don't have any idea if what I'm doing makes any
sense anymore. Anyway, if I define @tt{fmap2} just for functions, I get this.

@codeblock{
fmap2 :: (a -> b -> c) -> (d -> e -> a) -> d -> e -> b -> c
fmap2 f g = \x y -> (\_ _ -> f) x y (g x y)}

Wow. That was much more complicated than I expected, honestly. Maybe this is a little less
generalizable than I thought. In fact, is it even possible? The type of @tt{fmap} is pretty simple.

@terminal{
> :t fmap
fmap :: Functor f => (a -> b) -> f a -> f b}

Specialized for function types, that's still pretty straightforward.

@codeblock{
fmap :: (a -> b) -> (r -> a) -> (r -> b)}

But with my @tt{fmap2}, the arity of the resulting function is different from the arity of the second
argument, so it doesn't seem like they could possibly be represented by the same type, anyway.

It's clear I'm in @italic{way} over my head on this one. I don't really have any idea what I'm doing.
What was I supposed to be doing in the first place?

@section{Back to @italic{Mastermind}}

Right. @italic{Mastermind}. That was a quite the detour. Maybe I'll come back to it later, or maybe
I'll just realize that what I was trying to do was misguided and didn't make any sense anyway. Who
knows? Whatever. I should probably just finish what I was working on before all this nonsense got me
distracted.

@codeblock{
inexactMatches :: Code -> Code -> Int
inexactMatches xs ys = matches xs ys - exactMatches xs ys}

@bold{Fine.} You know what? It's good enough. @bold{Moving on.}

@blockquote{
A @tt{Move} is a new datatype that is constructed with a @tt{Code} and two @tt{Int}s. The first
@tt{Int} is the number of exact matches that the @tt{Code} has with the secret and the second @tt{Int}
is the number of nonexact matches.}

Alright, this is pretty stupidly easy compared to what I was just dealing with.

@codeblock{
getMove :: Code -> Code -> Move
getMove secret guess = Move secret e i
  where e = exactMatches secret guess
        i = inexactMatches secret guess}

Is it beautiful? No. Does it work? Yes. Good.

@blockquote{
We will now define a concept that will be important in playing the Mastermind game. This is the
concept of @italic{consistency}; we say that a @tt{Code} is consistent with a @tt{Move} if the
@tt{Code} could have been the secret that generated that move. In other words, if the guess inside the
@tt{Move} has the same number of exact and non-exact matches with the provided Code as it did with the
actual secret, then the @tt{Code} is consistent with the @tt{Move}.}

@codeblock{
isConsistent :: Move -> Code -> Bool
isConsistent (Move guess e i) guess' = e == e' && i == i'
  where e' = exactMatches guess' guess
        i' = inexactMatches guess' guess}

Easy stuff. Now we just implement a function to filter out all the remaining viable codes.

@codeblock{
filterCodes :: Move -> [Code] -> [Code]
filterCodes m = filter $ isConsistent m}

Now we just need to create a function that will generate all possible permutations of a list of colors
given a certain code length.

@codeblock{
allCodes :: Int -> [Code]
allCodes 0 = []
allCodes 1 = map (:[]) colors
allCodes n = concatMap (\c -> map (c:) $ allCodes (n - 1)) colors}

Now we can write a function that actually solves Mastermind puzzles. The assignment wants me to always
start by guessing purely @tt{Red} just for consistency.

@codeblock{
solve :: Code -> [Move]
solve secret = loop [initialMove]
  where codeLen = length secret
        initialMove = getMove secret $ replicate codeLen Red
        isConsistentWithAll ms c = all (`isConsistent` c) ms
        nextMove ms = getMove secret $ head $ filter (isConsistentWithAll ms) $ allCodes codeLen
        loop ms@"@"(Move _ e _ : _)
          | e == codeLen = ms
          | otherwise    = loop (nextMove ms : ms)}

I'm sure that could be much more efficient (it filters through the entire list of moves every time
instead of just threading a list of remaining options through), but I don't really care for this small
problem. I've overengineered this assignment enough, and honestly, I'm ready to move on.
