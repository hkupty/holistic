const std = @import("std");
const mem = std.mem;
const math = std.math;
const testing = std.testing;
const time = std.time;

const HLSVersion = struct { number: u8 };

const HLSField = union(enum) {
    Version: HLSVersion,
};

const ParseResult = union(enum) {
    empty: void,
    bin: []const u8,
    hsl: HLSField,
};

const ParserState = struct {
    buffer: []const u8,
    output: ParseResult,
};

/// Defines all possible errors that can happen when parsing
const ParserError = error{
    /// This is returned when the parsed content doesn't match what the parser expects.
    /// It should be a recoverable error unless returned by a top-level parser.
    Mismatch,

    /// The parser couldn't finish processing the data because there wasn't enough input to parse.
    PrematureEndOfInput,

    /// The feature is not yet supported/implemented, likely due to holistic being experimental
    Unsupported,

    /// Failed to process due to other errors
    ParserBuildError,
};

const ParseResultMapper = struct {
    map: *const fn (ParseResult) ParserError!ParseResult,

    const identity: ParseResultMapper = .{ .map = struct {
        fn identity(state: ParseResult) ParserError!ParseResult {
            return state;
        }
    }.identity };
};

fn debugParser(parser: Parser) void {
    switch (parser) {
        .str => |s| std.debug.print("String[{s}]\n", .{s.ref}),
        .seq => |s| {
            std.debug.print("Sequence[\n", .{});
            for (s.inner) |p| {
                std.debug.print("  ", .{});
                debugParser(p);
            }
            std.debug.print("]\n", .{});
        },
        .sel => |s| {
            std.debug.print("Select[\n", .{});
            for (s.inner) |p| {
                if (p == null) {
                    continue;
                }
                std.debug.print("  ", .{});
                debugParser(p.?);
            }
            std.debug.print("]\n", .{});
        },
    }
}

/// A parser that delegates the execution to two or more parsers
const SequenceParser = struct {
    inner: []const Parser,
    handler: ParseResultMapper = .identity,

    fn parse(self: SequenceParser, state: ParserState) ParserError!ParserState {
        var stateCursor = state;
        for (self.inner) |parser| {
            stateCursor = try parser.parse(stateCursor);
        }

        return .{ .buffer = stateCursor.buffer, .output = try self.handler.map(stateCursor.output) };
    }

    fn flatConcat(comptime left: Parser, comptime right: Parser) SequenceParser {
        const inner = comptime resolve: {
            const sz = switch (left) {
                .seq => |l| l.inner.len,
                inline else => 1,
            } + switch (right) {
                .seq => |r| r.inner.len,
                inline else => 1,
            };

            var inner: [sz]Parser = undefined;
            var ix = 0;
            switch (left) {
                .seq => |s| for (s.inner) |p| {
                    inner[ix] = p;
                    ix += 1;
                },
                inline else => {
                    inner[ix] = left;
                    ix += 1;
                },
            }
            switch (right) {
                .seq => |s| for (s.inner) |p| {
                    inner[ix] = p;
                    ix += 1;
                },
                inline else => {
                    inner[ix] = right;
                    ix += 1;
                },
            }

            break :resolve inner;
        };
        return .{ .inner = &inner };
    }

    fn concat(self: *const SequenceParser, comptime other: Parser) Parser {
        return comptime switch (other) {
            .seq => |seq| .{ .seq = .{ .inner = self.inner ++ seq.inner } },
            inline else => .{ .seq = .{ .inner = self.inner ++ [_]Parser{other} } },
        };
    }

    fn peek(self: SequenceParser) ?u8 {
        return self.inner[0].peek();
    }

    fn headRef(self: SequenceParser) []const u8 {
        return switch (self.inner[0]) {
            .str => |p| p.ref,
            .seq => |p| p.headRef(),
            inline else => unreachable,
        };
    }
};

/// A parser that matches against a constant string
const StringParser = struct {
    ref: []const u8,

    fn parse(self: *const StringParser, state: ParserState) ParserError!ParserState {
        if (self.ref.len > state.buffer.len) {
            return ParserError.PrematureEndOfInput;
        }

        const target = state.buffer[0..self.ref.len];

        if (mem.eql(u8, self.ref, target)) {
            return .{
                .buffer = state.buffer[self.ref.len..state.buffer.len],
                .output = .{ .bin = self.ref },
            };
        } else {
            // std.debug.print("Got {s} but it doesn't match {s}\n", .{ target, self.ref });
            return ParserError.Mismatch;
        }
    }

    fn concat(self: StringParser, comptime other: Parser) Parser {
        return comptime switch (other) {
            .str => |str| .{ .str = .{ .ref = self.ref ++ str.ref } },
            inline else => .{ .seq = SequenceParser.flatConcat(.{ .str = self }, other) },
        };
    }

    fn peek(self: *const StringParser) ?u8 {
        return self.ref[0];
    }
};

const SelectParser = struct {
    shift: u3,
    mask: u8,
    inner: []const ?Parser,

    fn parse(self: SelectParser, state: ParserState) ParserError!ParserState {
        const index = (state.buffer[0] >> self.shift) & self.mask;
        const parser = self.inner[index] orelse return ParserError.Mismatch;

        return parser.parse(state);
    }

    fn concat(self: *const SelectParser, comptime other: Parser) Parser {
        return buildFrom(self.inner ++ other);
    }

    fn peek(_: *const SelectParser) ?u8 {
        return null;
    }

    inline fn buildFrom(comptime parsers: []const Parser) ParserError!SelectParser {
        comptime if (parsers.len == 0) {
            return ParserError.ParserBuildError;
        };
        comptime var buffer: [parsers.len:0]u8 = undefined;
        @memset(&buffer, 0);
        inline for (parsers, 0..) |p, ix| {
            const chr = comptime p.peek() orelse return ParserError.ParserBuildError;
            inline for (buffer) |b| {
                if (chr == b) {
                    return ParserError.ParserBuildError;
                }
            }
            buffer[ix] = chr;
        }

        const window = comptime math.log2_int_ceil(usize, parsers.len);
        const mask: u8 = (1 << window) - 1;
        comptime var shift: u3 = 0;

        const selectParsers: [1 << window]?Parser = comptime build: {
            var parserArray: [1 << window]?Parser = undefined;

            while (shift <= (8 - window)) : (shift += 1) {
                for (0..parsers.len - 1) |i| {
                    @memset(&parserArray, null);

                    inner: {
                        const data_i = parsers[i].peek().?;
                        const i_key = (data_i >> shift) & mask;
                        parserArray[i_key] = parsers[i];
                        for (i + 1..parsers.len) |j| {
                            const data_j = parsers[j].peek().?;
                            const j_key = (data_j >> shift) & mask;
                            parserArray[j_key] = parsers[j];
                            if (i_key == j_key) {
                                break :inner;
                            }
                        }

                        break :build parserArray;
                    }
                }
            }

            unreachable;
        };

        return .{ .shift = shift, .mask = mask, .inner = &selectParsers };
    }
};

const Parser = union(enum) {
    str: StringParser,
    seq: SequenceParser,
    sel: SelectParser,

    pub fn parseRaw(self: Parser, input: []const u8) ParserError!ParserState {
        const initialState = ParserState{ .buffer = input, .output = ParseResult.empty };

        return self.parse(initialState);
    }

    fn parse(self: Parser, input: ParserState) ParserError!ParserState {
        return switch (self) {
            inline else => |impl| impl.parse(input),
        };
    }

    fn concat(self: Parser, other: Parser) Parser {
        return switch (self) {
            inline else => |p| p.concat(other),
        };
    }

    fn peek(self: Parser) ?u8 {
        return switch (self) {
            inline else => |p| p.peek(),
        };
    }

    fn ref(self: Parser) []const u8 {
        return switch (self) {
            .seq => |seq| seq.headRef(),
            .str => |str| str.ref,
            inline else => unreachable,
        };
    }

    pub fn split(comptime self: Parser, comptime ix: usize) ParserError![2]Parser {
        comptime switch (self) {
            .seq => unreachable, // TODO: Implement
            .sel => unreachable, // TODO: Implement
            .str => |strParser| if (ix > strParser.ref.len) {
                return ParserError.ParserBuildError;
            } else {
                return [_]Parser{
                    .{ .str = .{ .ref = strParser.ref[0..ix] } },
                    .{ .str = .{ .ref = strParser.ref[ix..] } },
                };
            },
        };
    }

    /// Produces a new parser of type `StringParser` which will match against the supplied `ref`
    pub fn Str(comptime str: []const u8) Parser {
        return .{ .str = StringParser{ .ref = str } };
    }

    /// Produces a new Parser which could be `SequenceParser`, but if all arguments are of the same
    /// type, there's a chance they'll be combined to a single parser of the same type instead.
    pub fn Sequence(comptime parsers: []const Parser) Parser {
        comptime var cursor = parsers[0];
        comptime var ix = 1;
        inline while (ix < parsers.len) : (ix += 1) {
            cursor = comptime cursor.concat(parsers[ix]);
        }

        return cursor;
    }

    pub fn Select(comptime parsers: []const Parser) ParserError!Parser {
        return .{ .sel = try SelectParser.buildFrom(parsers) };
    }

    pub fn Zip(comptime parsers: []const Parser) ParserError!Parser {
        switch (parsers.len) {
            0 => return ParserError.ParserBuildError,
            1 => return parsers[0],
            else => {},
        }

        return comptime init: {
            const headPrefix: []const u8 = switch (parsers[0]) {
                .seq => |seq| seq.headRef(),
                .str => |str| str.ref,
                inline else => return ParserError.ParserBuildError, // HACK: No other parser type is supported here
            };

            var commonPrefix = headPrefix;

            for (1..parsers.len) |parserIndex| {
                const str = parsers[parserIndex].ref();

                for (commonPrefix, 0..) |chr, ix| {
                    if (chr != str[ix]) {
                        commonPrefix = commonPrefix[0..ix];
                        break;
                    }
                }
            }

            if (commonPrefix.len == 0) {
                break :init ParserError.ParserBuildError; // Unzippable
            }

            const size = calc: {
                var sz = 0;
                for (parsers) |p| {
                    if ((p.ref().len - commonPrefix.len) > 0) {
                        sz = sz + 1;
                    }
                }
                break :calc sz;
            };

            var newParsers: [size]Parser = undefined;
            var ix = 0;
            for (parsers) |p| {
                if ((p.ref().len - commonPrefix.len) > 0) {
                    switch (p) {
                        .str => |str| newParsers[ix] = Parser.Str(str.ref[commonPrefix.len..]),
                        inline else => unreachable, // TODO: fix sequence
                    }
                    ix = ix + 1;
                }
            }

            const select = try Parser.Select(&newParsers);
            const seq = Parser.Sequence(&.{ Parser.Str(commonPrefix), select });

            break :init seq;
        };
    }
};

test "Basic parser testing" {
    const basic = Parser.Str("hello");

    // Successfully parses into a ParseResult array list
    const result = try basic.parseRaw("hello");
    try switch (result.output) {
        .bin => |data| testing.expectEqual("hello", data),
        else => testing.expect(false),
    };

    // Fails due to End of input
    try testing.expectError(ParserError.PrematureEndOfInput, basic.parseRaw("hel"));

    // Fails due to mismatch
    try testing.expectError(ParserError.Mismatch, basic.parseRaw("world"));
}

test "Basic splitting" {
    const helloworld = comptime Parser.Str("helloworld");
    const split = try comptime helloworld.split(5);
    // const hello = comptime Parser.Str("hello", ParseResult.empty);
    // const world = comptime Parser.Str("world", ParseResult.empty);

    // combining
    const combined = Parser.Sequence(split[0..]);

    const baseOut = try helloworld.parseRaw("helloworld");
    const newOut = try combined.parseRaw("helloworld");

    try testing.expectEqualDeep(baseOut.buffer, newOut.buffer);
}

test "sequence on parser strings concatenate them" {
    const hello = comptime Parser.Str("hello");
    const space = comptime Parser.Str(" ");
    const world = comptime Parser.Str("world");

    const combined = Parser.Sequence(&.{ hello, space, world });

    // The generated parser is a `.str` parser, not a `.seq` parser
    try switch (combined) {
        .str => testing.expect(true),
        inline else => testing.expect(false),
    };

    const r1 = try combined.parseRaw("hello world");

    try testing.expectEqualDeep(ParseResult{ .bin = "hello world" }, r1.output);
}

test "select parser" {
    const p1 = comptime Parser.Str("alpha");
    const p2 = comptime Parser.Str("beta");
    const p3 = comptime Parser.Str("gamma");
    const p4 = comptime Parser.Str("delta");
    const p5 = comptime Parser.Str("eta");
    const p6 = comptime Parser.Str("tau");
    const px = comptime Parser.Str("epsylon");

    try testing.expectError(ParserError.ParserBuildError, Parser.Select(&.{ p5, px }));
    const select = try Parser.Select(&.{ p1, p2, p3, p4, p5, p6 });

    const res1 = try select.parseRaw("alpha");
    try testing.expectEqualDeep(ParseResult{ .bin = "alpha" }, res1.output);

    const res2 = try select.parseRaw("beta");
    try testing.expectEqualDeep(ParseResult{ .bin = "beta" }, res2.output);

    try testing.expectError(ParserError.Mismatch, select.parseRaw("epsylon"));
}

test "zip parsers" {
    const p1 = comptime Parser.Str("era");
    const p2 = comptime Parser.Str("engine");

    const zipped = try Parser.Zip(&.{ p1, p2 });

    var result = try zipped.parseRaw("engine");
    try testing.expectEqualDeep(ParseResult{ .bin = "ngine" }, result.output);
    result = try zipped.parseRaw("era");
    try testing.expectEqualDeep(ParseResult{ .bin = "ra" }, result.output);
}

fn bench_parser(name: []const u8, parser: *const Parser, data: []const u8, comptime max_loops: usize) !void {
    var measurements: [max_loops]u64 = [_]u64{0} ** max_loops;

    var loops: usize = 0;

    var timer = try time.Timer.start();
    while (loops < max_loops) : (loops += 1) {
        defer measurements[loops] = timer.lap();
        _ = try parser.parseRaw(data);
    }

    var smaller = measurements[0];
    var bigger = measurements[0];
    var sum: u64 = 0;

    std.mem.sort(u64, &measurements, {}, comptime std.sort.asc(u64));

    for (measurements) |run| {
        if (run > bigger) {
            bigger = run;
        }
        if (run < smaller) {
            smaller = run;
        }
        sum += run;
    }
    const mgn = std.math.log10(sum);
    const total = sum / std.math.pow(u64, 10, mgn - 1);

    const p50ix = comptime @ceil(0.5 * @as(f32, max_loops));
    const p90ix = comptime @ceil(0.9 * @as(f32, max_loops));
    const p99ix = comptime @ceil(0.99 * @as(f32, max_loops));

    const unit = switch (mgn) {
        1 => "ns",
        2 => "ns",
        3 => "ns",
        4 => "us",
        5 => "us",
        6 => "us",
        7 => "ms",
        8 => "ms",
        9 => "ms",
        else => "s",
    };

    std.debug.print("{s}: {d} loops\n", .{ name, loops });
    std.debug.print(" min   {d} ns/op\n", .{smaller});
    std.debug.print(" max   {d} ns/op\n", .{bigger});
    std.debug.print(" avg   {d} ns/op\n", .{sum / loops});
    std.debug.print(" p50   {d} ns/op\n", .{measurements[p50ix]});
    std.debug.print(" p90   {d} ns/op\n", .{measurements[p90ix]});
    std.debug.print(" p99   {d} ns/op\n", .{measurements[p99ix]});
    std.debug.print(" total {d} {s}\n", .{ total, unit });
}
