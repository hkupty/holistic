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
    Failure,
};

const SequenceParser = struct {
    inner: []const Parser,

    fn parse(self: *const SequenceParser, state: ParserState) ParserError!ParserState {
        var stateCursor = state;
        for (self.inner) |parser| {
            stateCursor = try parser.parse(stateCursor);
        }

        return stateCursor;
    }

    fn flatConcat(comptime left: Parser, comptime right: Parser) SequenceParser {
        var inner: []const Parser = undefined;

        comptime switch (left) {
            .seq => |seq| inner = seq.inner,
            inline else => [_]Parser{left},
        };

        comptime switch (right) {
            .seq => |seq| inner = inner ++ seq.inner,
            inline else => inner = inner ++ [_]Parser{right},
        };

        return .{ .inner = inner };
    }

    fn concat(self: *const SequenceParser, comptime other: Parser) Parser {
        return comptime switch (other) {
            .seq => |seq| .{ .seq = .{ .inner = self.inner ++ seq.inner } },
            inline else => .{ .seq = .{ .inner = self.inner ++ [_]Parser{other} } },
        };
    }
};

const StringParser = struct {
    ref: []const u8,

    fn parse(self: *const StringParser, state: ParserState) ParserError!ParserState {
        if (self.ref.len > state.buffer.len) {
            return ParserError.PrematureEndOfInput;
        }

        const target = state.buffer[0..self.ref.len];

        if (!mem.eql(u8, self.ref, target)) {
            return ParserError.Mismatch;
        } else {
            return .{
                .buffer = state.buffer[self.ref.len..state.buffer.len],
                .output = .{ .bin = self.ref },
            };
        }
    }

    fn concat(self: *const StringParser, comptime other: Parser) Parser {
        return comptime switch (other) {
            .str => |str| .{ .str = .{ .ref = self.ref ++ str.ref } },
            inline else => .{ .seq = SequenceParser.flatConcat(self, other) },
        };
    }
};

const Parser = union(enum) {
    str: StringParser,
    seq: SequenceParser,

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

    pub fn split(comptime self: Parser, comptime ix: usize) ParserError![2]Parser {
        comptime switch (self) {
            .seq => unreachable, // TODO: Implement
            .str => |strParser| if (ix > strParser.ref.len) {
                return ParserError.Failure;
            } else {
                return [_]Parser{
                    .{ .str = .{ .ref = strParser.ref[0..ix] } },
                    .{ .str = .{ .ref = strParser.ref[ix..] } },
                };
            },
        };
    }

    /// Produces a new parser of type `StringParser` which will match against the supplied `ref`
    pub fn Str(comptime ref: []const u8) Parser {
        return .{ .str = StringParser{ .ref = ref } };
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

    try testing.expectEqualDeep(r1.output, ParseResult{ .bin = "hello world" });
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
