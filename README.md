# Holistic

An HLS Parser written in Zig using parser combinators

## Why?

This is not intended (at least as of now) to be production code. This is my playground to learn Zig using a real world problem.

## Why?!

I have recent experience with HLS and long ago (from my financial industry days) I played around with parser combinators, which I find interesting.

## Yes, but... Why?!?!

Zig is an interesting language and I feel I've been personally missing a low-level language in my toolbox. My past experience with C and C++ is very _very_ limited.
Go is a great language and I am in contact with it on a weekly basis at least, but I don't think it suffices.

Now, if you're curious to why parser combinators for HLS, I believe zig offers an unique approach to parser combinators in the use of `comptime`, though I still have to learn more about it.

Consider the following HLS tags:

```hls
#EXTM3U
#EXT-X-VERSION
#EXT-X-START
#EXT-X-PROGRAM-DATE-TIME
#EXTINF
```

These are three of many tags that exist in the HLS spec.

Note anything similar? Well, all of them have a shared prefix. And a few of them share some sections of the tag as well.

This means that we could, in theory, combine parsers by their shared prefix and efficiently route the input to the correct parser. The `efficiently` part is the key here, which I believe `comptime` is a great solution for.

## Cool, have you solved it?

Not yet. This is, as I said, my playground for learning zig and I want to put my theory to the test with it.

If I succeed, awesome, maybe this becomes a real library someday;
If I fail successfully, I learn something in the process;
If I fail catastrophically, then I should take a step back, reassess, and learn with the outcome.

Therefore, there's always a win hidden somewhere. The only time I lose is if I get stuck. Let's make sure this doesn't happen.
