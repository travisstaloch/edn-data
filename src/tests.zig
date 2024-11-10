const std = @import("std");
const testing = std.testing;
const talloc = testing.allocator;

const edn = @import("extensible-data-notation");

fn expectTag(src: []const u8, tag: edn.Value.Tag) !void {
    var res = try edn.parseFromSlice(talloc, src);
    defer res.deinit(talloc);
    try testing.expectEqual(1, res.top_level_values.len);
    const tlv = res.top_level_values[0];
    try testing.expectEqual(tag, std.meta.activeTag(res.values[tlv.index]));
}

test "basic" {
    try expectTag("nil", .nil);
    try expectTag("true", .true);
    try expectTag("false", .false);
    try expectTag("1", .integer);
    try expectTag("1.0", .float);
    try expectTag("\\c", .character);
    try expectTag("ns/name", .symbol);
    try expectTag("{:a 1 :b 2 :c 3}", .map);
    try expectTag("#{:a 1 \"foo\"}", .set);
}

test "map" {
    const src =
        \\; Maps are associative data structures that associate the key with its value
        \\{:eggs        2
        \\ :lemon-juice 3.5
        \\ :butter      1}
        \\        
        \\; You're not restricted to using keywords as keys
        \\
    ;
    var result = try edn.parseFromSlice(talloc, src);
    defer result.deinit(talloc);
    try testing.expectEqual(7, result.values.len);
    try testing.expectEqual(.keyword, std.meta.activeTag(result.values[0]));
    try testing.expectEqual(.integer, std.meta.activeTag(result.values[1]));
    try testing.expectEqual(.keyword, std.meta.activeTag(result.values[2]));
    try testing.expectEqual(.float, std.meta.activeTag(result.values[3]));
    try testing.expectEqual(.keyword, std.meta.activeTag(result.values[4]));
    try testing.expectEqual(.integer, std.meta.activeTag(result.values[5]));
    try testing.expectEqual(.map, std.meta.activeTag(result.values[6]));

    try testing.expectFmt(src, "{}", .{edn.fmtParseResult(result, src)});
}

test "list" {
    const src =
        \\(defrecord
        \\  MenuItem
        \\  [name rating])
    ;
    var result = try edn.parseFromSlice(talloc, src);
    defer result.deinit(talloc);
    try testing.expectEqual(6, result.values.len);
    try testing.expectEqual(.symbol, std.meta.activeTag(result.values[0]));
    try testing.expectEqual(.symbol, std.meta.activeTag(result.values[1]));
    try testing.expectEqual(.symbol, std.meta.activeTag(result.values[2]));
    try testing.expectEqual(.symbol, std.meta.activeTag(result.values[3]));
    try testing.expectEqual(.vector, std.meta.activeTag(result.values[4]));
    try testing.expectEqual(.list, std.meta.activeTag(result.values[5]));

    try testing.expectEqual(1, result.top_level_values.len);
    try testing.expectEqualStrings("(", result.top_level_values[0].leading_ws.src(src));
    try testing.expectEqualStrings(")", result.top_level_values[0].trailing_ws.src(src));

    try testing.expectEqual(3, result.values[5].list.len);
    try testing.expectEqualStrings("", result.values[5].list[0].leading_ws.src(src));
    try testing.expectEqualStrings("\n  ", result.values[5].list[0].trailing_ws.src(src));
    try testing.expectEqualStrings("", result.values[5].list[1].leading_ws.src(src));
    try testing.expectEqualStrings("\n  ", result.values[5].list[1].trailing_ws.src(src));
    try testing.expectEqual(.vector, std.meta.activeTag(result.values[result.values[5].list[2].index]));
    try testing.expectEqualStrings("[", result.values[5].list[2].leading_ws.src(src));
    try testing.expectEqualStrings("]", result.values[5].list[2].trailing_ws.src(src));

    try testing.expectEqual(2, result.values[4].vector.len);
    try testing.expectEqualStrings("", result.values[4].vector[0].leading_ws.src(src));
    try testing.expectEqualStrings(" ", result.values[4].vector[0].trailing_ws.src(src));
    try testing.expectEqualStrings("", result.values[4].vector[1].leading_ws.src(src));
    try testing.expectEqualStrings("", result.values[4].vector[1].trailing_ws.src(src));

    try testing.expectFmt(src, "{}", .{edn.fmtParseResult(result, src)});
}

test "nested list" {
    const src = " ( [] {} ) ";
    var result = try edn.parseFromSlice(talloc, src);
    defer result.deinit(talloc);

    try testing.expectEqual(1, result.top_level_values.len);
    try testing.expectEqual(.list, std.meta.activeTag(result.values[result.top_level_values[0].index]));
    try testing.expectEqualStrings(" (", result.top_level_values[0].leading_ws.src(src));
    try testing.expectEqualStrings(") ", result.top_level_values[0].trailing_ws.src(src));

    try testing.expectEqual(3, result.values.len);
    try testing.expectEqual(.vector, std.meta.activeTag(result.values[0]));
    try testing.expectEqual(.map, std.meta.activeTag(result.values[1]));
    try testing.expectEqual(.list, std.meta.activeTag(result.values[2]));
    try testing.expectEqual(2, result.values[2].list.len);
    try testing.expectEqualStrings(" [", result.values[2].list[0].leading_ws.src(src));
    try testing.expectEqualStrings("] ", result.values[2].list[0].trailing_ws.src(src));
    try testing.expectEqualStrings("{", result.values[2].list[1].leading_ws.src(src));
    try testing.expectEqualStrings("} ", result.values[2].list[1].trailing_ws.src(src));
    // try testing.expectEqualStrings("} )", result.trailing.src(src));
    try testing.expectFmt(src, "{}", .{edn.fmtParseResult(result, src)});
}

test "comments" {
    const src =
        \\; Let me explain this with a Clojure example. Suppose I want to transform that
        \\; piece of EDN into a MenuItem record.
        \\
        \\(defrecord MenuItem [name rating])
        \\
        \\; defrecord defined, among other things, map->MenuItem which will take a map
    ;
    var result = try edn.parseFromSlice(talloc, src);
    defer result.deinit(talloc);
    try testing.expectEqual(6, result.values.len);
    try testing.expectEqual(.symbol, std.meta.activeTag(result.values[0]));
    try testing.expectEqual(.symbol, std.meta.activeTag(result.values[1]));
    try testing.expectEqual(.symbol, std.meta.activeTag(result.values[2]));
    try testing.expectEqual(.symbol, std.meta.activeTag(result.values[3]));
    try testing.expectEqual(.vector, std.meta.activeTag(result.values[4]));
    try testing.expectEqual(.list, std.meta.activeTag(result.values[5]));

    try testing.expectFmt(src, "{}", .{edn.fmtParseResult(result, src)});
}

fn testEql(asrc: []const u8, bsrc: []const u8, alen: u8, blen: u8) !void {
    var ares = try edn.parseFromSlice(talloc, asrc);
    defer ares.deinit(talloc);
    var bres = try edn.parseFromSlice(talloc, bsrc);
    defer bres.deinit(talloc);
    try testing.expectEqual(alen, ares.values.len);
    try testing.expectEqual(blen, bres.values.len);
    const a = ares.values[ares.values.len - 1];
    const b = bres.values[bres.values.len - 1];
    try testing.expect(a.comparable(asrc, ares.values).eql(b.comparable(bsrc, bres.values)));

    try testing.expectFmt(asrc, "{}", .{edn.fmtParseResult(ares, asrc)});
    try testing.expectFmt(bsrc, "{}", .{edn.fmtParseResult(bres, bsrc)});
}

fn testNotEql(asrc: []const u8, bsrc: []const u8, alen: u8, blen: u8) !void {
    try testing.expectError(error.TestUnexpectedResult, testEql(asrc, bsrc, alen, blen));
}

inline fn concatStrs(
    comptime prefix: []const u8,
    comptime scalars: []const struct { []const u8, u8 },
    comptime suffix: []const u8,
) struct { []const u8, u8 } {
    comptime {
        var res: []const u8 = prefix;
        var len: u8 = 1;
        for (scalars, 0..) |s, i| {
            res = res ++ (if (i == 0) "" else " ") ++ s[0];
            len += s[1];
        }
        res = res ++ suffix;
        return .{ res, len };
    }
}

test "eqluality" {
    try testNotEql("1", "1.0", 1, 1);
    try testEql("1.0", "1.0", 1, 1);
    try testNotEql("1.0", "1.00", 1, 1);
    const scalars = [_]struct { []const u8, u8 }{
        .{ "nil", 1 },
        .{ "true", 1 },
        .{ "false", 1 },
        .{ "1", 1 },
        .{ "1.0", 1 },
        .{ "\\c", 1 },
        .{ "ns/name", 1 },
        .{ "{:a 1 :b 2 :c 3}", 7 },
        .{ "#{:a 1 \"foo\"}", 4 },
    };
    const srcs = scalars ++ [_]struct { []const u8, u8 }{
        concatStrs("(", &scalars, ")"),
        concatStrs("[", &scalars, "]"),
    };
    inline for (srcs) |asrc| {
        inline for (srcs) |bsrc| {
            if (std.mem.eql(u8, asrc[0], bsrc[0]))
                try testEql(asrc[0], bsrc[0], asrc[1], bsrc[1])
            else
                try testNotEql(asrc[0], bsrc[0], asrc[1], bsrc[1]);
        }
    }
}
