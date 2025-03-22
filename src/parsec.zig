const std = @import("std");
const mem = std.mem;
const testing = std.testing;

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
};

const ParseResult = union(enum) {
    data: HLSField,
    input: []const u8,
};

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

test "Error cases" {
    const ext = ConstParser{ .data = "#EXT" };
    try testing.expectError(ParseError.Mismatch, ext.parse("#EX"));
    try testing.expectError(ParseError.Mismatch, ext.parse("#RXT"));
    try testing.expectError(ParseError.Unfinished, ext.parse("#EXT"));
}

