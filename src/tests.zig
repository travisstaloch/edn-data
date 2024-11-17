const std = @import("std");
const testing = std.testing;
const talloc = testing.allocator;

const edn = @import("extensible-data-notation");

fn expectTag(src: []const u8, tag: edn.Value.Tag) !void {
    // std.debug.print("src '{s}'\n", .{src});
    const res = try edn.parseFromSliceAlloc(talloc, src, .{});
    defer res.deinit(talloc);
    const top_level_vs = res.values[0..res.top_level_values];
    const tlv = top_level_vs[top_level_vs.len - 1];
    try testing.expectEqual(tag, std.meta.activeTag(tlv));
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

    const result = try edn.parseFromSliceAlloc(talloc, src, .{});
    defer result.deinit(talloc);
    try testing.expectEqual(7, result.values.len);
    try testing.expectEqual(1, result.top_level_values);
    try testing.expectEqual(.map, std.meta.activeTag(result.values[0]));
    try testing.expectEqual(.keyword, std.meta.activeTag(result.values[1]));
    try testing.expectEqual(.keyword, std.meta.activeTag(result.values[2]));
    try testing.expectEqual(.keyword, std.meta.activeTag(result.values[3]));
    try testing.expectEqual(.integer, std.meta.activeTag(result.values[4]));
    try testing.expectEqual(.float, std.meta.activeTag(result.values[5]));
    try testing.expectEqual(.integer, std.meta.activeTag(result.values[6]));
    try testing.expectEqual(3, result.values[0].map.keys.len);

    try testing.expectFmt(src, "{}", .{edn.fmtParseResult(result, src)});
}

test "list basic" {
    const src =
        \\(defrecord
        \\  MenuItem
        \\  [name rating])
    ;

    const result = try edn.parseFromSliceAlloc(talloc, src, .{});
    defer result.deinit(talloc);
    try testing.expectEqual(1, result.top_level_values);
    try testing.expectEqual(6, result.values.len);
    try testing.expectEqual(.list, std.meta.activeTag(result.values[0]));
    try testing.expectEqual(.symbol, std.meta.activeTag(result.values[1]));
    try testing.expectEqual(.symbol, std.meta.activeTag(result.values[2]));
    try testing.expectEqual(.vector, std.meta.activeTag(result.values[3]));
    try testing.expectEqual(.symbol, std.meta.activeTag(result.values[4]));
    try testing.expectEqual(.symbol, std.meta.activeTag(result.values[5]));

    const top_list = result.values[0];
    try testing.expectEqual(3, top_list.list.len);
    try testing.expectEqualStrings("(", result.whitespaces[0][0].src(src));
    try testing.expectEqualStrings(")", result.whitespaces[0][1].src(src));

    try testing.expectEqualStrings("", result.whitespaces[1][0].src(src));
    try testing.expectEqualStrings("\n  ", result.whitespaces[1][1].src(src));
    try testing.expectEqualStrings("", result.whitespaces[2][0].src(src));
    try testing.expectEqualStrings("\n  ", result.whitespaces[2][1].src(src));

    const list2 = top_list.list[2];
    try testing.expectEqual(.vector, std.meta.activeTag(list2));
    try testing.expectEqualStrings("[", result.whitespaces[3][0].src(src));
    try testing.expectEqualStrings("]", result.whitespaces[3][1].src(src));

    const vec = list2.vector;
    try testing.expectEqual(2, vec.len);
    try testing.expectEqualStrings("", result.whitespaces[4][0].src(src));
    try testing.expectEqualStrings(" ", result.whitespaces[4][1].src(src));
    try testing.expectEqual(.symbol, std.meta.activeTag(vec[0]));
    try testing.expectEqual(.symbol, std.meta.activeTag(vec[1]));
    try testing.expectEqualStrings("", result.whitespaces[5][0].src(src));
    try testing.expectEqualStrings("", result.whitespaces[5][1].src(src));

    try testing.expectFmt(src, "{}", .{edn.fmtParseResult(result, src)});
}

test "nested list" {
    const src = " ( [] {} ) ";
    const result = try edn.parseFromSliceAlloc(talloc, src, .{});
    defer result.deinit(talloc);

    const list_vi = result.values[0];
    try testing.expectEqual(.list, std.meta.activeTag(list_vi));
    try testing.expectEqualStrings(" (", result.whitespaces[0][0].src(src));
    try testing.expectEqualStrings(") ", result.whitespaces[0][1].src(src));

    try testing.expectEqual(3, result.values.len);
    try testing.expectEqual(.vector, std.meta.activeTag(result.values[1]));
    try testing.expectEqual(.map, std.meta.activeTag(result.values[2]));
    try testing.expectEqual(2, result.values[0].list.len);
    try testing.expectEqualStrings(" [", result.whitespaces[1][0].src(src));
    try testing.expectEqualStrings("] ", result.whitespaces[1][1].src(src));
    try testing.expectEqualStrings("{", result.whitespaces[2][0].src(src));
    try testing.expectEqualStrings("} ", result.whitespaces[2][1].src(src));

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
    const result = try edn.parseFromSliceAlloc(talloc, src, .{});
    defer result.deinit(talloc);
    try testing.expectEqual(6, result.values.len);
    try testing.expectEqual(.list, std.meta.activeTag(result.values[0]));
    try testing.expectEqual(.symbol, std.meta.activeTag(result.values[1]));
    try testing.expectEqual(.symbol, std.meta.activeTag(result.values[2]));
    try testing.expectEqual(.vector, std.meta.activeTag(result.values[3]));
    try testing.expectEqual(.symbol, std.meta.activeTag(result.values[4]));
    try testing.expectEqual(.symbol, std.meta.activeTag(result.values[5]));

    try testing.expectFmt(src, "{}", .{edn.fmtParseResult(result, src)});
}

fn testParse(alloc: std.mem.Allocator, src: []const u8) !void {
    const result = try edn.parseFromSliceAlloc(alloc, src, .{});
    defer result.deinit(alloc);
    const result2 = try edn.parseFromSliceAlloc(alloc, src, .{ .whitespace = .exclude });
    defer result2.deinit(alloc);
}

test "allocation failures" {
    const f = try std.fs.cwd().openFile("examples/edn.edn", .{});
    defer f.close();
    const src = try f.readToEndAlloc(talloc, 100000);
    defer talloc.free(src);
    try testing.checkAllAllocationFailures(talloc, testParse, .{src});
}

fn testEql(asrc: []const u8, bsrc: []const u8, alen: u8, blen: u8) !void {
    const ares = try edn.parseFromSliceAlloc(talloc, asrc, .{});
    defer ares.deinit(talloc);
    const bres = try edn.parseFromSliceAlloc(talloc, bsrc, .{});
    defer bres.deinit(talloc);
    try testing.expectEqual(alen, ares.values.len);
    try testing.expectEqual(blen, bres.values.len);
    try testing.expectEqual(1, ares.top_level_values);
    try testing.expectEqual(1, bres.top_level_values);
    try testing.expect(ares.values[0].eql(asrc, ares.values, bres.values[0], bsrc, bres.values));

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

const test_scalars = [_]struct { []const u8, u8 }{
    .{ "nil", 1 },
    .{ "true", 1 },
    .{ "false", 1 },
    .{ "1", 1 },
    .{ "1.0", 1 },
    .{ "\\c", 1 },
    .{ "ns/name", 1 },
    .{ "{:a 1 :b 2 :c 3}", 7 },
    .{ "#{:a 1 \"foo\"}", 4 },
    .{ "(:a 1 \"foo\")", 4 },
};

const test_srcs = test_scalars ++ [_]struct { []const u8, u8 }{
    concatStrs("(", &test_scalars, ")"),
    concatStrs("[", &test_scalars, "]"),
    concatStrs("{", &test_scalars, "}"),
    concatStrs("#{", &test_scalars, "}"),
};

test "eqluality" {
    try testNotEql("1", "1.0", 1, 1);
    try testEql("1.0", "1.0", 1, 1);
    try testNotEql("1.0", "1.00", 1, 1);
    inline for (test_srcs) |asrc| {
        inline for (test_srcs) |bsrc| {
            if (std.mem.eql(u8, asrc[0], bsrc[0]))
                try testEql(asrc[0], bsrc[0], asrc[1], bsrc[1])
            else
                try testNotEql(asrc[0], bsrc[0], asrc[1], bsrc[1]);
        }
    }
}

test "measure" {
    const measured = try edn.measure("(a, b, c)", .{});
    try testing.expectEqual(4, measured.values);
    try testing.expectEqual(4, measured.whitespaces);
}

test "no ws" {
    // TODO - more coverage
    const res = try edn.parseFromSliceAlloc(talloc, "(a, b, c)", .{ .whitespace = .exclude });
    defer res.deinit(talloc);
}

test "comptime parse" {
    inline for (test_srcs) |src_len| {
        const src = src_len[0];
        comptime {
            const res = try edn.parseFromSliceComptime(src, .{}, .{ .eval_branch_quota = 4000 });
            try testing.expectEqualStrings(src, std.fmt.comptimePrint("{}", .{edn.fmtParseResult(res, src)}));
        }
    }
}
