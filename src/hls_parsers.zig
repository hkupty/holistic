const std = @import("std");
const hls = @import("hls.zig");
const parsers = @import("parsers.zig");

const P = parsers.Parser;
const ParserError = parsers.ParserError;
const ParserResult = parsers.ParseResult;

const handlers = struct {
    const extinf: parsers.ParseResultMapper = .{ .map = struct {
        fn extinf(state: ParserResult) ParserError!ParserResult {
            const duration = std.fmt.parseFloat(f32, state.seq[1].bin) catch |err| {
                std.debug.print("Failed to parse duration: {!}", .{err});
                return ParserError.Mismatch;
            };
            const title: ?[]const u8 = set: {
                if (std.mem.eql(u8, "", state.seq[2].bin)) {
                    break :set null;
                } else {
                    break :set state.seq[2].bin;
                }
            };
            return .{
                .hls = .{ .Inf = .{
                    .duration = duration,
                    .title = title,
                } },
            };
        }
    }.extinf };

    const keyattribute: parsers.ParseResultMapper = .{
        .map = struct {
            fn keyattr(state: ParserResult) ParserError!ParserResult {
                const attr: hls.KeyAttribute = build: {
                    const key = state.seq[0].bin;
                    const val = state.seq[1].bin;

                    if (std.mem.eql(u8, "METHOD", key)) {
                        const value = v: {
                            if (std.mem.eql(u8, "NONE", val)) {
                                break :v hls.KeyAttributeMethod.NONE;
                            }
                            if (std.mem.eql(u8, "AES-128", val)) {
                                break :v hls.KeyAttributeMethod.AES_128;
                            }
                            if (std.mem.eql(u8, "SAMPLE-AES", val)) {
                                break :v hls.KeyAttributeMethod.SAMPLE_AES;
                            }
                            return ParserError.Mismatch;
                        };
                        break :build .{ .Method = value };
                    }

                    if (std.mem.eql(u8, "IV", key)) {
                        const iv = std.fmt.parseInt(u128, state.seq[1].bin, 0) catch |err| {
                            std.debug.print("Failed to parse u128: {!}\n", .{err});
                            return ParserError.Mismatch;
                        };
                        break :build .{ .IV = iv };
                    }
                    return ParserError.Mismatch;
                };

                return .{ .hls = .{ .KeyAttribute = attr } };
            }
        }.keyattr,
    };

    const extkey: parsers.ParseResultMapper = .{
        .map = struct {
            fn xk(state: ParserResult) ParserError!ParserResult {
                var dummy = [_]hls.KeyAttribute{.{ .Method = hls.KeyAttributeMethod.NONE }} ** 5;
                var ix: usize = 0;
                for (state.seq[1].seq) |result| {
                    dummy[ix] = result.hls.KeyAttribute;
                    ix += 1;
                }
                const key: hls.EncriptionKey = .{ .attributeList = dummy[0..ix] };
                return .{ .hls = .{ .Key = key } };
            }
        }.xk,
    };

    const extversion: parsers.ParseResultMapper = .{ .map = struct {
        fn handler(state: ParserResult) ParserError!ParserResult {
            const version = std.fmt.parseInt(u8, state.seq[1].bin, 10) catch |err| {
                std.debug.print("Failed to parse: {!}", .{err});
                return ParserError.Mismatch;
            };
            return .{ .hls = .{ .Version = .{ .number = version } } };
        }
    }.handler };

    const extm3u: parsers.ParseResultMapper = .{ .map = struct {
        fn handler(_: parsers.ParseResult) ParserError!ParserResult {
            return .{ .hls = hls.HLSField.EXTM3U };
        }
    }.handler };
};

const extm3u = P.Str("#EXTM3U").map(handlers.extm3u);

const extversion = P.Seq(&.{ P.Str("#EXT-X-VERSION:"), P.Capture("") }).map(handlers.extversion);

const kv = P.Seq(&.{ P.Capture("="), P.MaybeCapture(",") });

const keyattribute = kv.map(handlers.keyattribute);

const extxkey = P.Seq(&.{
    P.Str("#EXT-X-KEY:"),
    P.Repeat(keyattribute, 5),
}).map(handlers.extkey);

const extinf = P.Seq(&.{
    P.Str("#EXTINF:"),
    P.Capture(","),
    P.Capture(""),
}).map(handlers.extinf);

const basic_tags = P.Zip(&.{
    extm3u,
    extversion,
});

test "ensure hls parsers work" {
    const testing = std.testing;
    var data = try extm3u.parseRaw("#EXTM3U");

    try testing.expectEqualStrings("", data.buffer);
    try testing.expectEqual(hls.HLSField.EXTM3U, data.output.hls);

    try testing.expectError(parsers.ParserError.Mismatch, extm3u.parseRaw("#EXT-M3U"));

    data = try extversion.parseRaw("#EXT-X-VERSION:5");

    try testing.expectEqualStrings("", data.buffer);
    var ref: hls.HLSField = .{ .Version = .{ .number = 5 } };
    try testing.expectEqual(ref, data.output.hls);

    data = try basic_tags.parseRaw("#EXTM3U");
    try testing.expectEqual(hls.HLSField.EXTM3U, data.output.hls);
    data = try basic_tags.parseRaw("#EXT-X-VERSION:5");
    try testing.expectEqual(ref, data.output.hls);

    data = try extinf.parseRaw("#EXTINF:2.00,");
    ref = .{ .Inf = .{ .duration = 2.0, .title = null } };
    try testing.expectEqualDeep(ref, data.output.hls);

    data = try extinf.parseRaw("#EXTINF:2.01,With title");
    ref = .{ .Inf = .{ .duration = 2.01, .title = "With title" } };
    try testing.expectEqualDeep(ref, data.output.hls);

    data = try extxkey.parseRaw("#EXT-X-KEY:METHOD=AES-128");
    ref = .{ .Key = .{ .attributeList = &.{
        .{ .Method = hls.KeyAttributeMethod.AES_128 },
    } } };

    try testing.expectEqualDeep(ref, data.output.hls);

    data = try extxkey.parseRaw("#EXT-X-KEY:METHOD=AES-128,IV=0x0123456789abcdef0123456789abcdef");
    ref = .{ .Key = .{ .attributeList = &.{
        .{ .Method = hls.KeyAttributeMethod.AES_128 },
        .{ .IV = 1512366075204170929049582354406559215 },
    } } };

    try testing.expectEqualDeep(ref, data.output.hls);
}
