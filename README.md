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

This could be "zipped" (for the lack of a better term, sorry) into the following structure:

```
#EXT
   ├ M3U
   ├ INF
   └ -X-
       ├ VERSION
       ├ START
       └ PROGRAM-DATE-TIME
```

Then, if we look at the first character of each subsequent tag below `#EXT`:
```
M   01001101
I   01001001
-   00101101
```

Intuitively, we can see that `chr >> 2 & 0b1111` results in an unique number for each character that can give us an index to an array:
```
M 3
I 2
- 11
```

Calculating the shift and the mask, however, is a computationally expensive operation as computers don't operate on intuition, but we can compute them
on compile time if we know (and we do) during compilation which parsers we want to zip together.
This then can serve as an optimized no-clash hashing algorithm, specially crafted at compile time, to efficiently select the right parser for the right tag.

## Cool, have you solved it?

It is going well, but nowhere near done yet.

So far, this is the best working example I have:
```zig
const extm3u = Parser.Str("#EXTM3U").map(handlers.extm3u);
const extversion = Parser.Seq(&.{ Parser.Str("#EXT-X-VERSION:"), parsers.Remaining }).map(handlers.extversion);
const basic_tags = Parser.Zip(&.{
    extm3u,
    extversion,
});
const seq = Parser.Seq(&.{ basic_tags, parsers.MaybeEOL }).map(.first);
const fullLine = Parser.Seq(&.{ parsers.Remaining, parsers.MaybeEOL }).map(.first);
const small_manifest =
    \\#EXTM3U
    \\
    \\#EXT-X-VERSION:11
    ;

var cursor: parsers.ParserState = .{ .buffer = small_manifest, .output = parsers.ParseResult.empty };

while (cursor.buffer.len > 0) {
    cursor = seq.parse(cursor) catch |err| val: {
        std.debug.print("Got error {!}. Falling back to EOL. Current buffer: {s}\n", .{ err, cursor.buffer });
        break :val try fullLine.parse(cursor);
    };
    swtich(cursor.output) {
        .hls => // Do something with the parsed manifest data
        .bin => // Handle empty lines or URLS in from the manifest
        inline else => unreachable
    }
}
```


This is, as I said, my playground for learning zig and I want to put my theory to the test with it.

If I succeed, awesome, maybe this becomes a real library someday;
If I fail successfully, I learn something in the process;
If I fail catastrophically, then I should take a step back, reassess, and learn with the outcome.

Therefore, there's always a win hidden somewhere. The only time I lose is if I get stuck. Let's make sure this doesn't happen.

## Where are you and how can I (yes, you reading this) help?

I have picked up a few things already and this is starting to feel natural, which is good.
At the same time, this is a one-person journey and there's a risk I'm piling up vices, anti-patterns or suboptimal ways of solving problems.

Of course you're not obliged to do anything, but I would truly appreciate if you could help me out correcting those, pointing the right direction, guidance, etc.

It is a journey I'll walk on my own, but I'll be truly grateful for anyone willing to help in any way.

There are a couple of "rules" I've set for this project:

- I want to, as much as possible, avoid using the heap (aka allocators). Why? Because I think it is a fun challenge and it is a nice way to compartmentalize subjects - adding them in the future by refactoring will be a fun learning process;
- It should allow for custom HLS tags, as long as they're known in compile time;
- Performance is not the goal, but it shouldn't be neglected either.

That's it. Very simple. So, beforehand, I thank you for your interest in this project and for helping me in this journey.
