const std = @import("std");
const mem = std.mem;
const math = std.math;
const time = std.time;
const hls = @import("hls.zig");

/// The output of a parser
pub const ParseResult = union(enum) {
    /// This value should be emitted when a parser swallows the input or when the value shouldn't be usec.
    empty: void,

    /// This value represents untransformed input, just segmented according to the parser spec.
    bin: []const u8,

    /// This value represents a processed output that represents an HLS tag or a piece of data relevant to build an HLS tag.
    hls: hls.HLSField,

    /// This field represents a collection of results
    seq: []const ParseResult,
};

pub const ParserState = struct {
    buffer: []const u8,
    output: ParseResult,
};

/// Defines all possible errors that can happen when parsing
pub const ParserError = error{
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

pub const ParseResultMapper = struct {
    map: *const fn (ParseResult) ParserError!ParseResult,

    const identity: ParseResultMapper = .{ .map = struct {
        fn identity(state: ParseResult) ParserError!ParseResult {
            return state;
        }
    }.identity };

    const last: ParseResultMapper = .{ .map = struct {
        fn last(state: ParseResult) ParserError!ParseResult {
            return switch (state) {
                .seq => |s| s[s.len - 1],
                inline else => state,
            };
        }
    }.last };
};

pub fn debugParser(parser: Parser) void {
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
        .cap => |s| std.debug.print("Capture[{s}]\n", .{s.terminator}),
        .rep => |s| std.debug.print("Repeat[{!} x {d}]\n", .{ s.inner, s.times }),
    }
}

/// A parser that delegates the execution to two or more parsers
const SequenceParser = struct {
    inner: []const Parser,
    handler: ParseResultMapper = .identity,

    fn parse(self: SequenceParser, state: ParserState) ParserError!ParserState {
        var stateCursor = state;
        // HACK: This is to avoid heap allocation, should be revisited in the future
        var intermediary = [_]ParseResult{ParseResult.empty} ** 32;
        for (self.inner, 0..) |parser, ix| {
            stateCursor = try parser.parse(stateCursor);
            intermediary[ix] = stateCursor.output;
        }

        return .{
            .buffer = stateCursor.buffer,
            .output = try self.handler.map(.{ .seq = intermediary[0..self.inner.len] }),
        };
    }

    fn map(comptime self: SequenceParser, handler: ParseResultMapper) SequenceParser {
        return .{ .inner = self.inner, .handler = handler };
    }

    fn flatten(comptime self: SequenceParser) Parser {
        comptime var size = self.inner.len;
        inline for (self.inner) |parser| {
            switch (parser) {
                .seq => |seq| size += seq.inner.len - 1,
                inline else => {},
            }
        }

        comptime switch (size) {
            0 => @compileError("Cannot flatten empty sequence parser"),
            1 => return self.inner[0],
            else => {},
        };

        var new: [size]Parser = undefined;
        var ix: usize = 0;
        inline for (self.inner) |p| {
            switch (p) {
                .seq => |seq| {
                    inline for (seq.inner) |pp| {
                        new[ix] = pp;
                        ix += 1;
                    }
                },
                else => {
                    new[ix] = p;
                    ix += 1;
                },
            }
        }

        return .{ .seq = .{ .inner = &new, .handler = self.handler } };
    }

    // --- Remove --- //

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
    handler: ParseResultMapper = .identity,

    fn parse(self: StringParser, state: ParserState) ParserError!ParserState {
        if (self.ref.len > state.buffer.len) {
            return ParserError.PrematureEndOfInput;
        }

        const target = state.buffer[0..self.ref.len];

        if (mem.eql(u8, self.ref, target)) {
            return .{
                .buffer = state.buffer[self.ref.len..state.buffer.len],
                .output = try self.handler.map(.{ .bin = self.ref }),
            };
        } else {
            // std.debug.print("Got {s} but it doesn't match {s}\n", .{ target, self.ref });
            return ParserError.Mismatch;
        }
    }

    fn map(comptime self: StringParser, handler: ParseResultMapper) StringParser {
        return .{ .ref = self.ref, .handler = handler };
    }

    fn peek(self: *const StringParser) ?u8 {
        return self.ref[0];
    }
};

const CaptureParser = struct {
    terminator: []const u8,
    optional: bool = false,

    fn parse(self: CaptureParser, state: ParserState) ParserError!ParserState {
        if (self.terminator.len == 0) {
            return .{ .buffer = &.{}, .output = .{ .bin = state.buffer } };
        }

        if (state.buffer.len == 0) {
            return ParserError.PrematureEndOfInput;
        }

        const ix = std.mem.indexOf(u8, state.buffer, self.terminator) orelse {
            return switch (self.optional) {
                true => .{ .buffer = &.{}, .output = .{ .bin = state.buffer } },
                false => ParserError.Mismatch,
            };
        };

        return .{ .buffer = state.buffer[ix + 1 ..], .output = .{ .bin = state.buffer[0..ix] } };
    }

    fn peek(_: CaptureParser) ?u8 {
        return null;
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

    fn peek(_: *const SelectParser) ?u8 {
        return null;
    }

    inline fn buildFrom(comptime parsers: []const Parser) SelectParser {
        comptime if (parsers.len == 0) {
            @compileError("Cannot select between parsers if no parser is provided");
        };
        comptime var buffer: [parsers.len:0]u8 = undefined;
        @memset(&buffer, 0);
        inline for (parsers, 0..) |p, ix| {
            const chr = comptime p.peek() orelse @compileError("No character to disambiguate");
            inline for (buffer) |b| {
                if (chr == b) {
                    @compileError("Two or more parsers clash with same character");
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

const RepeatParser = struct {
    times: usize,
    inner: *const Parser,
    handler: ParseResultMapper = .identity,

    fn parse(self: RepeatParser, state: ParserState) ParserError!ParserState {
        var partial = [_]ParseResult{ParseResult.empty} ** 32;
        var cursor = state;

        const offset = off: {
            for (0..self.times) |ix| {
                cursor = self.inner.parse(cursor) catch |err| {
                    switch (err) {
                        ParserError.PrematureEndOfInput => break :off (self.times - ix),
                        inline else => return err,
                    }
                };
                partial[ix] = cursor.output;
            }
            break :off 0;
        };

        return .{
            .buffer = cursor.buffer,
            .output = try self.handler.map(.{ .seq = partial[0..(self.times - offset)] }),
        };
    }

    fn map(comptime self: RepeatParser, handler: ParseResultMapper) RepeatParser {
        return .{
            .times = self.times,
            .inner = self.inner,
            .handler = handler,
        };
    }
};

pub const Parser = union(enum) {
    str: StringParser,
    seq: SequenceParser,
    sel: SelectParser,
    cap: CaptureParser,
    rep: RepeatParser,

    pub fn parseRaw(self: Parser, data: []const u8) ParserError!ParserState {
        return self.parse(.{ .buffer = data, .output = ParseResult.empty });
    }

    pub fn map(comptime self: Parser, handler: ParseResultMapper) Parser {
        return switch (self) {
            .str => |p| .{ .str = p.map(handler) },
            .seq => |p| .{ .seq = p.map(handler) },
            .rep => |p| .{ .rep = p.map(handler) },
            inline else => unreachable,
        };
    }

    fn parse(self: Parser, input: ParserState) ParserError!ParserState {
        return switch (self) {
            inline else => |impl| impl.parse(input),
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
            .cap => unreachable,
            .rep => unreachable,
            .str => |strParser| if (ix > strParser.ref.len) {
                return ParserError.ParserBuildError;
            } else {
                return [_]Parser{
                    .{ .str = .{ .ref = strParser.ref[0..ix], .handler = strParser.handler } },
                    .{ .str = .{ .ref = strParser.ref[ix..], .handler = strParser.handler } },
                };
            },
        };
    }

    /// Produces a new parser of type `StringParser` which will match against the supplied `ref`
    pub fn Str(comptime str: []const u8) Parser {
        return .{ .str = StringParser{ .ref = str } };
    }

    pub fn Capture(comptime terminator: []const u8) Parser {
        return .{ .cap = .{ .terminator = terminator, .optional = false } };
    }

    pub fn MaybeCapture(comptime terminator: []const u8) Parser {
        return .{ .cap = .{ .terminator = terminator, .optional = true } };
    }

    pub fn Seq(comptime parsers: []const Parser) Parser {
        return .{ .seq = .{ .inner = parsers } };
    }

    pub fn Select(comptime parsers: []const Parser) Parser {
        return .{ .sel = SelectParser.buildFrom(parsers) };
    }

    pub fn Repeat(comptime parser: Parser, times: usize) Parser {
        return .{ .rep = .{ .inner = &parser, .times = times } };
    }

    pub fn Zip(comptime parsers: []const Parser) Parser {
        switch (parsers.len) {
            0 => return ParserError.ParserBuildError,
            1 => return parsers[0],
            else => {},
        }

        return comptime init: {
            const headPrefix: []const u8 = switch (parsers[0]) {
                .seq => |seq| seq.headRef(),
                .str => |str| str.ref,
                inline else => @compileError("Unsupported"),
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
                @compileError("empty prefix");
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
                newParsers[ix] = dropPrefix(p, commonPrefix);
                ix += 1;
            }

            const select = Parser.Select(&newParsers);
            const seq: Parser = .{ .seq = .{ .inner = &.{ Parser.Str(commonPrefix), select }, .handler = .last } };

            break :init seq;
        };
    }
};

fn dropPrefix(comptime parser: Parser, comptime prefix: []const u8) Parser {
    return comptime build: {
        switch (parser) {
            .str => |str| {
                if (prefix.len > str.ref.len) {
                    @compileError("Prefix is bigger than reference");
                }
                for (prefix, 0..) |p, ix| {
                    if (str.ref[ix] != p) {
                        @compileError("Cannot drop prefix");
                    }
                }

                break :build .{ .str = .{ .ref = str.ref[prefix.len..], .handler = str.handler } };
            },
            .seq => |seq| {
                var next: [seq.inner.len]Parser = undefined;
                var cursor = prefix;

                var outerIndex: usize = 0;
                var innerIndex: usize = 0;

                while (cursor.len > 0) : (outerIndex += 1) {
                    switch (seq.inner[outerIndex]) {
                        .str => |istr| {
                            const innerLen = istr.ref.len;

                            if (innerLen > prefix.len) {
                                next[innerIndex] = dropPrefix(seq.inner[outerIndex], prefix);
                                innerIndex += 1;
                                outerIndex += 1;
                                break;
                            } else if (innerLen <= prefix.len) {
                                cursor = cursor[innerLen..];
                            }
                        },
                        inline else => @compileError("Unsupported splitting on inner parser type"),
                    }
                }

                for (outerIndex..seq.inner.len) |ix| {
                    next[innerIndex] = seq.inner[ix];
                    innerIndex += 1;
                }

                const final = next[0..innerIndex].*;
                return .{ .seq = .{ .inner = &final, .handler = seq.handler } };
            },
            inline else => @compileError("Unsupported parser type"),
        }
    };
}

test "Basic parser testing" {
    const testing = std.testing;
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

test "Capture parser" {
    const testing = std.testing;
    const capture = Parser.Capture(";");
    const result = try capture.parseRaw("Hello;");
    try switch (result.output) {
        .bin => |data| testing.expectEqualStrings("Hello", data),
        else => testing.expect(false),
    };
}

test "select parser" {
    const testing = std.testing;
    const p1 = comptime Parser.Str("alpha");
    const p2 = comptime Parser.Str("beta");
    const p3 = comptime Parser.Str("gamma");
    const p4 = comptime Parser.Str("delta");
    const p5 = comptime Parser.Str("eta");
    const p6 = comptime Parser.Str("tau");

    const select = Parser.Select(&.{ p1, p2, p3, p4, p5, p6 });

    const res1 = try select.parseRaw("alpha");
    try testing.expectEqualStrings("alpha", res1.output.bin);

    const res2 = try select.parseRaw("beta");
    try testing.expectEqualStrings("beta", res2.output.bin);

    try testing.expectError(ParserError.Mismatch, select.parseRaw("epsylon"));
}

test "zip parsers" {
    const testing = std.testing;
    const p1 = comptime Parser.Str("era");
    const p2 = comptime Parser.Str("engine");

    const glued = comptime Parser.Zip(&.{ p1, p2 });
    const zipped = comptime glued.map(.identity);

    var result = try zipped.parseRaw("engine");
    try testing.expectEqualDeep(ParseResult{ .seq = &.{ .{ .bin = "e" }, .{ .bin = "ngine" } } }, result.output);
    result = try zipped.parseRaw("era");
    try testing.expectEqualDeep(ParseResult{ .seq = &.{ .{ .bin = "e" }, .{ .bin = "ra" } } }, result.output);
}

test "putting it all together" {
    const testing = std.testing;
    const p1 = comptime Parser.Str("era");
    const p2 = comptime Parser.Str("engine");

    const zipped = Parser.Zip(&.{ p1, p2 });
    const updated: Parser = build: switch (zipped) {
        .seq => |s| {
            const lambda = struct {
                fn handler(state: ParseResult) ParserError!ParseResult {
                    return .{ .hls = .{ .Version = .{ .number = @intCast(state.seq[1].bin.len) } } };
                }
            };
            break :build .{ .seq = .{ .inner = s.inner, .handler = .{ .map = lambda.handler } } };
        },
        inline else => unreachable,
    };

    var result = try updated.parseRaw("engine");
    try testing.expectEqualDeep(ParseResult{ .hls = .{ .Version = .{ .number = 5 } } }, result.output);
    result = try updated.parseRaw("era");
    try testing.expectEqualDeep(ParseResult{ .hls = .{ .Version = .{ .number = 2 } } }, result.output);
}

test "repeating parser" {
    const testing = std.testing;
    const base = comptime Parser.Str("Yes!");
    const repeat = comptime Parser.Repeat(base, 3);

    const result = try repeat.parseRaw("Yes!Yes!Yes!");

    try testing.expectEqual(0, result.buffer.len);
    var ix: usize = 0;
    for (result.output.seq) |partial| {
        try testing.expectEqualStrings("Yes!", partial.bin);
        ix += 1;
    }

    try testing.expectEqual(3, ix);
}
