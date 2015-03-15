#lang scribble/base

@(require "../shared.rkt")

@title[#:tag "day-3"]{Day 3: Moving forward, albeit slowly}

Alright. It's been a few days since I finished up the last section. I ran back to Racket for a while
so I could feel safe amongst my brackets and parentheses, but I can't ignore Haskell that much longer.
Before I started this project, I think I viewed Haskell as little more than a pure, lazy language with
typeclasses for genericism which also happened to leverage monads to perform side effects. Now I'm
feeling a little less sure of myself.

@section{First, some review}

I posted this to Reddit and Hacker News, and while the Hacker News discussion ended up, erm, shall we
say... "mixed", the Reddit comments were quite helpful.

I think the most exciting this to find out is that I was actually on the right track trying to get my
custom @tt{Applicative} instance to work. The @italic{correct} implementation of @tt{Functor} was the
following, helpfully provided to me by @/u/{tomejaguar}.

@codeblock{
newtype BinaryFunction a b c = BinaryFunction (a -> b -> c)
instance Functor (BinaryFunction a b) where
  fmap a (BinaryFunction b) = BinaryFunction (\x y -> a (b x y))}

Of course, looking back on this, this feels painstakingly obvious. If @tt{pure} hoists the function so
that it will ignore the first two arguments passed to it, this just doesn't pass those arguments
altogether. With that in place, my original attempt at @tt{Applicative} should work fine.

@codeblock{
instance Applicative (BinaryFunction a b) where
  pure f = BinaryFunction (\_ _ -> f)
  (BinaryFunction f) <*> (BinaryFunction g) = BinaryFunction (\x y -> f x y (g x y))}

Now, admittedly, when I was trying to implement @tt{BinaryFunction}, that original version using
@tt{Compose} was sitting in the back of my head. After all, this is starting to look suspiciously
similar, isn't it? Add some record syntax to @tt{BinaryFunction}, and you'd basically have the
exact same thing. Indeed, as @/u/{mjmrotek} pointed out, @tt{Compose} is just a more general version
of my attempt.

That said, I still don't really understand how @tt{Compose} works. Here's the definition for
@tt{Compose} itself, copied from the source.

@codeblock{
newtype Compose f g a = Compose { getCompose :: f (g a) }}

Just for completeness, let's look at the types for the constructor and field accessor.

@terminal{
> :t Compose
Compose :: f (g a) -> Compose f g a
> :t getCompose
getCompose :: Compose f g a -> f (g a)}

What exactly does @tt{f (g a)} mean? I guess both @tt{f} and @tt{g} are higher-kinded types which each
take a single type parameter. So then what is @tt{Compose min}?

@terminal{
> :t min
min :: Ord a => a -> a -> a
> :t Compose min
Compose min :: Ord a => Compose ((->) a) ((->) a) a}

Alright, so @tt{f} and @tt{g} are both @tt{(a ->)}, and @tt{a} is just @tt{a}. Composing these
together like in the constructor would yield @tt{(a -> (a -> a))}, which is precisely the type I would
expect.

Now I can look at the instance for @tt{Functor}.

@codeblock{
instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose x) = Compose (fmap (fmap f) x)}

In the provided case, the type of @tt{f} is @tt{(a ->)} and the type of @tt{x} is @tt{a -> a -> a}.
For functions, @tt{fmap} is just @tt{(.)}, so that becomes @tt{(.) ((.) f) x}. Hmm, those two
immediate usages of composition are a little confusing.

@terminal{
> :t fmap (+1) (Compose max)
fmap (+1) (Compose max)
  :: (Ord a, Num a) => Compose ((->) a) ((->) a) a
> :t getCompose $ fmap (+1) (Compose max)
getCompose $ fmap (+1) (Compose max)
  :: (Ord a, Num a) => a -> a -> a}

I think I need to get my head around successive/nested composition a little bit more. Haskell's
"everything takes one value and returns one value" style, which effectively implements auto-currying,
can make composition a little more confusing than I'm used to.

Let me break this down a little more.

@terminal{
> :t (.) ((.) (+1)) max
(.) ((.) (+1)) max :: (Ord c, Num c) => c -> c -> c
> :t (.)
(.) :: (b -> c) -> (a -> b) -> a -> c
> :t ((.) (+1))
((.) (+1)) :: Num c => (a -> c) -> a -> c}

The type of @tt{(.)} is, of course, obvious, though looking at it did remind me something important. I
should probably be thinking about function types like @tt{a -> a -> a} as @tt{a -> (a -> a)}, which
would obviously satisfy the type @tt{a -> b} where @tt{b} is just @tt{a -> a}.

With this in mind, what is @tt{(.) (+1)}? Well, @tt{(+1)} is just a unary function, so it makes
filling in the type for the first argument of @tt{(.)} obvious. The result of the expression is a
function that takes a function that operates on any input and converts it to a number, which is then
passed to the incrementing function, which creates the final result.

To keep things clearer as I try to juggle types in my head, I'll replace that expression with
@tt{\f x -> (f x) + 1}. Substituting that into the original expression yields @tt{max . (\f x -> (f x)
+ 1)}. Again, the composition takes the result of the @italic{second} argument, then passes it to the
first argument. Therefore, the resulting function will take two numbers, find the maximum, and then
increment the result.

This explains the behavior of that wacky @tt{((.).(.))} function that someone commented about on the
reddit thread.

@terminal{
> :t ((.).(.))
((.).(.)) :: (b -> c) -> (a -> a1 -> b) -> a -> a1 -> c}

This actually seems to be function composition for a unary function with a binary function, which is
sort of cool. In Racket, it would just be @tt{(compose f g)}, even if @tt{g} required two arguments,
but in Haskell, all functions are @italic{really} unary—there's no luxury of functions which cannot be
partially applied.

What happens if I add more dots?

@terminal{
> :t (.).(.).(.)
(.).(.).(.)
  :: (b -> c) -> (a -> a1 -> a2 -> b) -> a -> a1 -> a2 -> c}

The pattern does, indeed, continue. That's sort of nice to know. I feel like I should be able to
logically understand that @italic{of course} that's what composing composition does, but I definitely
cannot do that yet.

Now that all that's figured out, what about @tt{Applicative} for @tt{Compose}?

@codeblock{
instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure x = Compose (pure (pure x))
    Compose f <*> Compose x = Compose ((<*>) <$> f <*> x)}

Wow. I have no idea what that does.

Okay, @tt{pure (pure x))} is obvious (and I almost wish it were written @tt{Compose . pure . pure}...,
I think I'm already losing myself to Haskell's convenient composition), but what about that crazy mess
for @tt{<*>}?

@terminal{
> :t (\f x -> (<*>) <$> f <*> x)
(\f x -> (<*>) <$> f <*> x)
  :: (Applicative f1, Applicative f) =>
     f (f1 (a -> b)) -> f (f1 a) -> f (f1 b)}

Wow! I have no idea how that's implemented, but the type actually seems to make some sense! Given an
instance of @tt{Applicative} wrapped in another @tt{Applicative} from @tt{a -> b}, then providing
@tt{a} yields @tt{b}. I still have lots of questions, though. In this example, what is @tt{f}? What is
@tt{f1}?

This is one of those situations where I wish I could somehow have GHC tell me the types instantiated
with something. I'm not even really sure what that means, though, since the types could clearly be
anything. Whatever. Let me just reason it out myself.

I think where this is sort of tripping me up is how the @tt{Applicative} instance is for
@tt{Compose f g}. Obviously, this is because @tt{Applicative} wants a type of kind @tt{* -> *}, so the
@tt{Compose} type constructor needs to be partially applied. But what does that mean? In the original
type @tt{Compose f (g a)}, what is @tt{a}?

If @tt{Compose} is created with @tt{(+)}, then @tt{a} is simply any type that is a member of the
@tt{Num} typeclass. The @tt{a} type corresponds to the function's return value while @tt{f} and @tt{g}
correspond to its inputs. I guess more generally, @tt{f} could be a function type, but @tt{g} could be
some entirely unrelated functor.

So back to the original @tt{Applicative} instance. The mixing of infix and prefix operators is still
something I find a little confusing, so let me make everything prefix to make things more clear.

@terminal{
> :t \f x -> ((<*>) ((<$>) (<*>) f) x)
\f x -> ((<*>) ((<$>) (<*>) f) x)
  :: (Applicative f1, Applicative f) =>
     f (f1 (a -> b)) -> f (f1 a) -> f (f1 b)}

Same thing, just all prefix. Working from the inside-out, what the hell is @tt{(<*>) (<$>)}?

@terminal{
> :t (<*>) (<$>)
(<*>) (<$>) :: Functor f => ((a -> b) -> f a) -> (a -> b) -> f b}

Hmm. I still don't really get how this works.

You know what? Screw it. I could probably spend hours dissecting this tiny, insignificant piece of
code. How could such a small snippet be such a headache? I @italic{think} I can explain this away with
some relatively high-level conceptual reasoning.

In the @tt{Applicative} instance for @tt{Compose}, both of its first two type parameters are
@tt{Applicative}, so the value contained is wrapped in two layers to @tt{Applicative} containers.
(This "container" metaphor is wrong, and I already @italic{know} it to be wrong, since the containers
simply seem to be manifested by the types and don't actually box anything, but I get that. I think
my mental model is sound, at least for that piece.) Therefore, the type for @tt{<*>} for @tt{Compose}
is obvious—it takes an applicable value wrapped in the two layers of @tt{Applicative}s, a value that
can be supplied to the first value (also wrapped in the same two layers), and finally produces the
result of that application (still wrapped in the same two layers).

With functions, this idea of "application" is fairly explicit, since it literally means function
application. With other types, though, it could mean various other things. The name @tt{Applicative}
is starting to make some sense to me—it generalizes the idea of function application to other types.
Sort of neat, but not quite within my grasp to understand in full just yet.

Perhaps I'll figure it out in time. Now, I think, is not that time.

@section{Getting back to business}

Phew. That was a lot of confusing, abstract thinking about types. I think I'm ready to go back to
something simple. Something practical. Alright, random college class, what have you got to show me?

Aha! Writing a programming language interpreter. Should be @italic{downright easy} in comparison to
what I've just dealt with.

Alright, so the assignment provides a bunch of datatypes to represent the AST of an arbitrary
imperative programming language. The assignment is about manipulating those ASTs. Seems simple enough.
The first exercise deals with manipulating @tt{State}s, which represent variable bindings in our
mini-language.

A @tt{State} is actually simply a @tt{String -> Int}, and an empty state maps anything to @tt{0}. This
allows us to write an @tt{extend} function that extends a given state, making this task relatively
straightforward.

@codeblock{
extend :: State -> String -> Int -> State
extend s identifier value =
  \identifier' -> if identifier == identifier'
    then value
    else s identifier'}

Neat and simple. Defining the @tt{empty} state is obviously trivial.

@codeblock{
empty :: State
empty _ = 0}

Now we can implement an @tt{evalE} function, simply to evaluate expressions. It will take a @tt{State}
for resolving variable lookups.

@codeblock{
evalE :: State -> Expression -> Int
evalE st (Var s) = st s
evalE _  (Val i) = i
evalE st (Op e op e') =
  case op of
    Plus   -> v + v'
    Minus  -> v - v'
    Times  -> v * v'
    Divide -> v `div` v'
    Gt     -> bTi $ v >  v'
    Ge     -> bTi $ v >= v'
    Lt     -> bTi $ v <  v'
    Le     -> bTi $ v <= v'
    Eql    -> bTi $ v == v'
  where v  = evalE st e
        v' = evalE st e'
        bTi b = if b then 1 else 0}

Using @tt{case} for the operator pattern-matching makes that relatively clean. Next up, implementing
desugaring! "Desugaring" is, after all, the lesser cousin of macros, so this is something I can get
behind.

@codeblock{
desugar :: Statement -> DietStatement
desugar (Assign s e)    = DAssign s e
desugar (Incr s)        = DAssign s (Op (Var s) Plus (Val 1))
desugar (If e s s')     = DIf e (desugar s) (desugar s')
desugar (While e s)     = DWhile e (desugar s)
desugar (Sequence s s') = DSequence (desugar s) (desugar s')
desugar Skip            = DSkip
desugar (For pre cond post s) =
  DSequence (desugar pre)
            (DWhile cond (DSequence (desugar s)
                                    (desugar post)))}

Mmm, my parentheses, how I've missed you. Implementing this reminds me how much I like that Lisp
expressions are basically just ASTs already.

Anyway, now we can create a simple statement evaluator.

@codeblock{
evalSimple :: State -> DietStatement -> State
evalSimple st (DAssign s e) = extend st s (evalE st e)
evalSimple st (DIf e s s') = if evalE st e /= 0 then evalSimple st s
                                                else evalSimple st s'
evalSimple st (DWhile e s) = if evalE st e /= 0 then evalSimple (evalSimple st s) (DWhile e s)
                                                else st
evalSimple st (DSequence s s') = evalSimple (evalSimple st s) s'
evalSimple st DSkip = st}

Quite nice; maybe even pretty! Of course, none of these are really demonstrating any of Haskell's
unique features—all of these could be implemented more or less the same in any functional programming
language. This assignment, especially, gives me a very Racket-y vibe. It would definitely be simple to
port this almost verbatim to Racket using its extensive pattern-matching facilities.

Now the final bit is to implement a @tt{run} function to run programs. For some @italic{baffling}
reason, the homework assignment includes this note:

@blockquote{
  @italic{Note:} @tt{run} should be defined in terms of @tt{desugar} and @tt{evalSimple}. It should
  not be implemented from scratch.
}

After all that, whoever would implement @tt{run} from scratch must be out of their mind. I'm almost
amused to imagine that some poor student would be too clueless to understand how to implement @tt{run}
using the functions that have already been implemented considering how @italic{stupidly easy} its
implementation is.

@codeblock{
run :: State -> Statement -> State
run st = evalSimple st . desugar}

And that's it! An extremely simple interpreter for an AST in Haskell. As I stated above, nothing very
special going on here, but a cool example of Haskell's simple functional programming features. And
honestly, it was a nice break after all that nonsense about @tt{Applicative} and @tt{Compose}.

@section{Some closing notes}

I'm sorry if this was a relatively boring day in comparison to the last one. I almost feel a little
guilty for throwing in the towel with understanding the @tt{Compose} implementation, but I'm in over
my head on that one. Perhaps for another time.

Anyway, I'd like to note a few extra things that either happened in between the previous day and this
one or things I've come across while writing this.

First of all, I had a few people recommend avoiding @tt{$}. I used it in one spot while writing this
assignment, but in general, I've made a decision to avoid using it unless it really improves the
clarity of my code. At first I was a little afraid to sprinkle parens everywhere considering my Lisp
experience, but I think a balance has become a little more apparent.

I've come to a similar conclusion with point-free code. I did, for example, write @tt{run} using
point-free style, but in most cases, it doesn't feel like it's conducive to writing readable code.
Even with @tt{run}, it wasn't truly @italic{completely} point-free.

@codeblock{
run :: State -> Statement -> State
run st = evalSimple st . desugar}

I @italic{could} implement that with @tt{(. evalSimple) . desugar}, but... why? That's completely
useless. On the other hand, expressing the second parameter as being related to composition of
@tt{evalSimple} and @tt{desugar} makes logical sense.

Anyway, I think that's it for now. Honestly, at this point, I'm just excited to get to monads, since
I think they're a really cool concept, and I think I @italic{mostly} understand how they work. Soon!
