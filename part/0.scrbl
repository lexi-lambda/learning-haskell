#lang scribble/base

@(require "../shared.rkt")

@(define ~~ (string-append -~- -~-))

@title{Day 0: A whole lot of nothing}

@irc["racket"]{
<lexi-lambda> Alright, I've given in and I'm trying out Haskell some more. How long do you think it'll be before I miss macros?
<notjack> I give it a week
<lexi-lambda> Could be less than that, honestly
<notjack> I'm not a very aggressive better
}

Well. Here I go again. Hopefully this time I'll make it a little farther into the land of Haskell.

I've tried Haskell before (though not extensively), so I've actually gotten my setup working in the
past. Specifically, I've used @hyperlink["https://atom.io/"]{Atom} as an editor with the rather nice
@hyperlink["https://atom.io/packages/ide-haskell"]{ide-haskell} package. I was honestly pretty pleased
with that setup, so the first step is getting it up and running again.

After taking a look through @tt{cabal @|~~|help} once again, I made a new directory, typed @tt{cabal
init}, and hoped for the best. Everything seems to be configured properly, so time to launch @tt{atom}
and started typing away.

Alright, so something's clearly wrong—I'm not getting any feedback from ide-haskell. Looking in the
Atom console, I found this lovely pearl of wisdom:

@terminal{
ghc-mod: /path/to/setup-config: hGetContents: invalid argument (invalid byte sequence)
}

Huh. I have no idea what that means.

Well, I remember having to install @tt{ghc-mod} from last time I did this. It still looks like it's
installed fine, but I guess it's borked. I don't know why (I haven't touched it), but I guess I can
just reinstall it. I'm not actually at all familiar with Cabal, so I'll just delete the files and
hope for the best.

@terminal{$ cabal install ghc-mod}

Wow. That's... a lot of compilation. I vaguely remember all this from last time, but I forgot how
many dependencies this darn thing has. Oh, well.

Ten minutes later, and it looks like everything's finally done. Will it work?

@terminal{
$ ghc-mod
ghc-mod: /path/to/setup-config: hGetContents: invalid argument (invalid byte sequence)
}

@bold{Nope.}

@irc["racket"]{
<lexi-lambda> Argh. I think I screwed something up, and at this point I just want to nuke my installation and start from scratch.
<notjack> Perhaps I should amend my estimate of when you go back to racket to, say, before the end of the night
<lexi-lambda> this is quite possible
}

Okay, so what now? Google, please save me!

Hmm. Alright. I've found a Stack Overflow question that's linked me to a GitHub issue which seems to
illuminate the problem.

@irc["racket"]{
<lexi-lambda> Apparently the issue is that cabal doesn't play nice if it's version 1.22 or higher and not running on GHC 7.10
<lexi-lambda> But GHC 7.10 doesn't seem to be the current release version?
}

I'll be honest: this seems terribly stupid. How did I end up with a version of Cabal that isn't
compatible with my version of GHC? Especially considering GHC 7.10 seems to still be pre-release
software.

Okay, I take it back—it's not an issue with Cabal, I guess, it's an issue with how ghc-mod interfaces
with GHC's APIs to get source information which depend on Cabal... ugh. I don't even know. Nor do I
even really care. How do I fix it?

@terminal{cabal install --constraint "Cabal < 1.22" cabal-install}

Perfect. Now to just wait until all that finishes... wait, no, it almost immediately errored out. I
think I've somehow messed up my dependencies in my random attempts at fixing things. Either way,
my packages seem to be pretty broken.

It's getting late, and I've made absolutely no progress. I don't know what's wrong, but I don't even
really care. It's fine. I shouldn't have wasted time on trying to get this to work, anyway.

@irc["racket"]{
<lexi-lambda> Back to Racket it is.
<lexi-lambda> I'll probably just wait for 7.10 to be released. :P
<notjack> Problem solved :P
}

And now, sleep.
