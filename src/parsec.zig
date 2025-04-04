// This is the first attempt and will be discarded in favor of parsers.zig
const std = @import("std");
const mem = std.mem;
const math = std.math;
const testing = std.testing;
const time = std.time;

const HLSFieldSpec = struct {
    field: HLSField,
    spec: []const u8,
};

const HLSField = enum {
    EXTM3U,
    EXT_X_VERSION,
    EXTINF,
};

const m3u8 = HLSFieldSpec{ .field = HLSField.EXTM3U, .spec = "#EXTM3U" };
const version = HLSFieldSpec{ .field = HLSField.EXT_X_VERSION, .spec = "#EXT-X-VERSION:<n>" };

/// Errors that might happen during parsing
const ParseError = error{
    /// Returned when the input doesn't match the parser expectations
    Mismatch,
    Unfinished,
    Unsupported,
};

const ParserBuildError = error{CannotFindDifference};

const ParseResult = union(enum) {
    data: HLSField,
    input: []const u8,
};

/// Matches on a constant string
const ConstParser = struct {
    data: []const u8,
    field: ?HLSField = null,

    fn parse(self: *const ConstParser, input: []const u8) ParseError!ParseResult {
        if (self.data.len > input.len) {
            return ParseError.Mismatch;
        }

        const ref = input[0..self.data.len];

        if (!mem.eql(u8, self.data, ref)) {
            return ParseError.Mismatch;
        } else {
            const remaining = input[self.data.len..input.len];
            if (remaining.len == 0) {
                const field = self.field orelse return ParseError.Unfinished;
                return ParseResult{ .data = field };
            } else {
                return ParseResult{ .input = remaining };
            }
        }
    }

    pub fn parser(self: ConstParser) Parser {
        return .{ .cnst = self };
    }
};

/// Tries different parsers until it finds one that matches
const OptionParser = struct {
    options: []const Parser,

    fn parse(self: *const OptionParser, input: []const u8) ParseError!ParseResult {
        for (self.options) |p| {
            return p.parse(input) catch |err| {
                switch (err) {
                    ParseError.Mismatch => continue,
                    else => return err,
                }
            };
        }
        return ParseError.Mismatch;
    }

    pub fn parser(self: OptionParser) Parser {
        return .{ .option = self };
    }
};

fn resolve_data(parser: *const Parser) ParseError![]const u8 {
    return switch (parser.*) {
        .cnst => |p| p.data,
        .sequence => |p| resolve_data(&p.sequence[0]),
        else => ParseError.Unsupported,
    };
}

const MappedOptionParser = struct {
    alloc: *const std.mem.Allocator,
    options: []const ?*const Parser,
    mask: u8,
    shift: u3,

    fn parse(self: *const MappedOptionParser, input: []const u8) ParseError!ParseResult {
        if (input.len == 0) {
            return ParseError.Mismatch;
        }

        const index = (input[0] >> self.shift) & self.mask;
        const p = self.options[index] orelse return ParseError.Mismatch;
        std.debug.print("Got index {} and parser at {*} \n", .{ index, p });

        return p.parse(input);
    }

    fn deinit(self: *const MappedOptionParser) void {
        self.alloc.free(self.options);
    }

    fn build(allocator: *const std.mem.Allocator, comptime parsers: []const *const Parser) !MappedOptionParser {
        const window = comptime math.log2_int(usize, parsers.len);
        const mask: u8 = (1 << window) - 1;
        comptime var shift: u3 = 0;

        maskshift: inline while (shift <= (8 - window)) : (shift += 1) {
            inline for (0..parsers.len - 1) |i| {
                inner: {
                    const data_i = try comptime resolve_data(parsers[i]);
                    const i_key = (data_i[0] >> shift) & mask;
                    inline for (i + 1..parsers.len) |j| {
                        const data_j = try comptime resolve_data(parsers[j]);
                        const j_key = (data_j[0] >> shift) & mask;
                        if (i_key == j_key) {
                            break :inner;
                        }
                    }

                    break :maskshift;
                }
            }
        }

        const parsermap = try allocator.alloc(?*const Parser, mask + 1);
        @memset(parsermap, null);

        inline for (parsers) |p| {
            const key = try resolve_data(p);
            const index = (key[0] >> shift) & mask;
            std.debug.print("Assigning {s} parser to index {X}\n", .{ key, index });
            const holder: ?*const Parser = p;
            parsermap[index] = holder;
        }

        return .{ .alloc = allocator, .options = parsermap, .mask = mask, .shift = shift };
    }

    pub fn parser(self: MappedOptionParser) Parser {
        return .{ .mapped = self };
    }
};

/// Combines multiple parsers by executing them in sequence
const SequenceParser = struct {
    sequence: []const Parser,

    fn parse(self: *const SequenceParser, input: []const u8) ParseError!ParseResult {
        var data = input;
        for (self.sequence) |p| {
            const parsed = try p.parse(data);
            switch (parsed) {
                .data => return parsed,
                .input => |remaining| data = remaining,
            }
        }

        if (data.len == 0) {
            return ParseError.Unfinished;
        } else {
            return ParseResult{ .input = data };
        }
    }

    pub fn parser(self: SequenceParser) Parser {
        return .{ .sequence = self };
    }
};

const Parser = union(enum) {
    cnst: ConstParser,
    option: OptionParser,
    mapped: MappedOptionParser,
    sequence: SequenceParser,

    pub fn parse(self: Parser, input: []const u8) ParseError!ParseResult {
        switch (self) {
            inline else => |impl| return impl.parse(input),
        }
    }
};

test "Basic const parser" {
    const parser = ConstParser{ .data = "#EXTM3U", .field = HLSField.EXTM3U };
    const remaining = try parser.parse("#EXTM3U");
    switch (remaining) {
        .data => |field| try testing.expectEqual(HLSField.EXTM3U, field),
        .input => try testing.expect(false),
    }
}

test "Remaining parsing" {
    const ext = ConstParser{ .data = "#EXT" };
    const remaining = try ext.parse("#EXTM3U");
    switch (remaining) {
        .data => try testing.expect(false),
        .input => |input| try std.testing.expect(input.len == 3),
    }
}

test "Compound parser" {
    const prefix = ConstParser{ .data = "#EXT" };
    const m3u = ConstParser{ .data = "M3U", .field = HLSField.EXTM3U };
    const inf = ConstParser{ .data = "INF", .field = HLSField.EXTINF };

    const select = OptionParser{ .options = &.{ m3u.parser(), inf.parser() } };
    const complete = SequenceParser{ .sequence = &.{ prefix.parser(), select.parser() } };

    var parsed = try complete.parse("#EXTM3U");
    try switch (parsed) {
        .data => |field| testing.expectEqual(HLSField.EXTM3U, field),
        else => testing.expect(false),
    };

    parsed = try complete.parse("#EXTINF");
    try switch (parsed) {
        .data => |field| testing.expectEqual(HLSField.EXTINF, field),
        else => testing.expect(false),
    };
}

test "Mapped options parser" {
    const prefix = ConstParser{ .data = "#EXT" };
    const m3u = ConstParser{ .data = "M3U", .field = HLSField.EXTM3U };
    const inf = ConstParser{ .data = "INF", .field = HLSField.EXTINF };

    std.debug.print("Parser addresses are {*} and {*}\n", .{ &m3u, &inf });
    const select = try MappedOptionParser.build(&testing.allocator, &.{ &m3u.parser(), &inf.parser() });
    defer select.deinit();
    const complete = SequenceParser{ .sequence = &.{ prefix.parser(), select.parser() } };

    var parsed = try complete.parse("#EXTM3U");
    try switch (parsed) {
        .data => |field| testing.expectEqual(HLSField.EXTM3U, field),
        else => testing.expect(false),
    };

    parsed = try complete.parse("#EXTINF");
    try switch (parsed) {
        .data => |field| testing.expectEqual(HLSField.EXTINF, field),
        else => testing.expect(false),
    };
}

test "Error cases" {
    const ext = ConstParser{ .data = "#EXT" };
    try testing.expectError(ParseError.Mismatch, ext.parse("#EX"));
    try testing.expectError(ParseError.Mismatch, ext.parse("#RXT"));
    try testing.expectError(ParseError.Unfinished, ext.parse("#EXT"));
}

fn bench_parser(name: []const u8, parser: *const Parser, data: []const u8, comptime max_loops: usize) !void {
    var timer = try time.Timer.start();
    var measurements: [max_loops]u64 = [_]u64{0} ** max_loops;

    var loops: usize = 0;

    while (loops < max_loops) : (loops += 1) {
        defer measurements[loops] = timer.lap();
        _ = try parser.parse(data);
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

test "Benchmark parsers" {
    const prefix = ConstParser{ .data = "#EXT" };
    const m3u = ConstParser{ .data = "M3U", .field = HLSField.EXTM3U };
    const inf = ConstParser{ .data = "INF", .field = HLSField.EXTINF };
    const x = ConstParser{ .data = "-X-", .field = HLSField.EXTINF };
    const discontinuity = ConstParser{ .data = "DISCONTINUITY", .field = HLSField.EXTINF };
    const byterange = ConstParser{ .data = "BYTERANGE", .field = HLSField.EXTINF };
    const vers = ConstParser{ .data = "VERSION", .field = HLSField.EXTINF };

    const suffix_options = OptionParser{ .options = &.{ discontinuity.parser(), byterange.parser(), vers.parser() } };
    const suffix = SequenceParser{ .sequence = &.{ x.parser(), suffix_options.parser() } };
    const options = OptionParser{ .options = &.{ m3u.parser(), inf.parser(), suffix.parser() } };

    const option = SequenceParser{ .sequence = &.{ prefix.parser(), options.parser() } };

    const m_suffix_options = try MappedOptionParser.build(&testing.allocator, &.{ &discontinuity.parser(), &byterange.parser(), &vers.parser() });
    defer m_suffix_options.deinit();
    const m_suffix = SequenceParser{ .sequence = &.{ x.parser(), m_suffix_options.parser() } };
    const m_options = try MappedOptionParser.build(&testing.allocator, &.{ &m3u.parser(), &inf.parser(), &m_suffix.parser() });
    defer m_options.deinit();

    const mapped_option = SequenceParser{ .sequence = &.{ prefix.parser(), m_options.parser() } };

    try bench_parser("select", &option.parser(), "#EXT-X-VERSION", 10_000);
    try bench_parser("multi_select", &mapped_option.parser(), "#EXT-X-VERSION", 10_000);
}

// select: 10000 loops
//  min   360 ns/op
//  max   11882 ns/op
//  avg   430 ns/op
//  p50   421 ns/op
//  p90   451 ns/op
//  p99   491 ns/op
//  total 43 us
