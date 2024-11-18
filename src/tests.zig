const std = @import("std");
const testing = std.testing;
const talloc = testing.allocator;

const edn = @import("extensible-data-notation");

fn expectTag(src: []const u8, tag: edn.Value.Tag) !void {
    // std.debug.print("src '{s}'\n", .{src});
    const res = try edn.parseFromSliceAlloc(talloc, src, .{}, .{});
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

    const result = try edn.parseFromSliceAlloc(talloc, src, .{}, .{});
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

    const result = try edn.parseFromSliceAlloc(talloc, src, .{}, .{});
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
    const result = try edn.parseFromSliceAlloc(talloc, src, .{}, .{});
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
    const result = try edn.parseFromSliceAlloc(talloc, src, .{}, .{});
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
    const result = try edn.parseFromSliceAlloc(alloc, src, .{}, .{});
    defer result.deinit(alloc);
    const result2 = try edn.parseFromSliceAlloc(alloc, src, .{ .whitespace = .exclude }, .{});
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
    const ares = try edn.parseFromSliceAlloc(talloc, asrc, .{}, .{});
    defer ares.deinit(talloc);
    const bres = try edn.parseFromSliceAlloc(talloc, bsrc, .{}, .{});
    defer bres.deinit(talloc);
    try testing.expectEqual(alen, ares.values.len);
    try testing.expectEqual(blen, bres.values.len);
    try testing.expectEqual(1, ares.top_level_values);
    try testing.expectEqual(1, bres.top_level_values);
    var actx = edn.MapContext{ .src = asrc, .values = ares.values };
    var bctx = edn.MapContext{ .src = bsrc, .values = bres.values };
    if (actx.hash(ares.values[0]) != bctx.hash(bres.values[0])) {
        return error.TestExpectedEqual;
    }
    try testing.expect(ares.values[0].eql(asrc, ares.values, bres.values[0], bsrc, bres.values));

    try testing.expectFmt(asrc, "{}", .{edn.fmtParseResult(ares, asrc)});
    try testing.expectFmt(bsrc, "{}", .{edn.fmtParseResult(bres, bsrc)});
}

fn testNotEql(asrc: []const u8, bsrc: []const u8, alen: u8, blen: u8) !void {
    testEql(asrc, bsrc, alen, blen) catch |e| switch (e) {
        error.TestUnexpectedResult,
        error.TestExpectedEqual,
        => {},
        else => return e,
    };
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
    try testEql("1", "1", 1, 1);
    try testNotEql("1.0", "1.00", 1, 1);
    try testEql("#{1}", "#{1}", 2, 2);
    try testEql("{1 1}", "{1 1}", 3, 3);
    const s =
        \\(nil true false 1 1.0 \c ns/name {:a 1 :b 2 :c 3} #{:a 1 "foo"} (:a 1 "foo"))
    ;
    try testEql(s, s, 23, 23);
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
    const measured = try edn.measure("(a, b, c)", .{}, .{});
    try testing.expectEqual(4, measured.values);
    try testing.expectEqual(4, measured.whitespaces);
}

test "no ws" {
    // TODO - more coverage
    const res = try edn.parseFromSliceAlloc(talloc, "a b c", .{ .whitespace = .exclude }, .{});
    defer res.deinit(talloc);
}

test "ParseResult find()" {
    const src =
        \\(a b c {:a 1 :b 2 "foo" 3 ns/sym 4 5 5 \c 6})
    ;
    const res = try edn.parseFromSliceAlloc(talloc, src, .{ .whitespace = .exclude }, .{});
    defer res.deinit(talloc);
    try testing.expectError(error.PathNotFound, res.find("1", src));
    try testing.expectError(error.PathNotFound, res.find("0//3//missing", src));
    {
        const v = (try res.find("0", src)).?;
        try testing.expectEqual(.list, std.meta.activeTag(v));
    }
    {
        const v = (try res.find("0//0", src)).?;
        try testing.expectEqual(.symbol, std.meta.activeTag(v));
    }
    {
        const v = (try res.find("0//3//:a", src)).?;
        try testing.expectEqual(.integer, std.meta.activeTag(v));
        try testing.expectEqual(1, v.integer);
    }
    {
        const v = (try res.find("0//3//:b", src)).?;
        try testing.expectEqual(.integer, std.meta.activeTag(v));
        try testing.expectEqual(2, v.integer);
    }
    {
        const v = (try res.find("0//3//\"foo\"", src)).?;
        try testing.expectEqual(.integer, std.meta.activeTag(v));
        try testing.expectEqual(3, v.integer);
    }
    {
        const v = (try res.find("0//3//ns/sym", src)).?;
        try testing.expectEqual(.integer, std.meta.activeTag(v));
        try testing.expectEqual(4, v.integer);
    }
    { // 5 is an integer map key
        const v = (try res.find("0//3//5", src)).?;
        try testing.expectEqual(.integer, std.meta.activeTag(v));
        try testing.expectEqual(5, v.integer);
    }
    {
        const v = (try res.find("0//3//\\c", src)).?;
        try testing.expectEqual(.integer, std.meta.activeTag(v));
        try testing.expectEqual(6, v.integer);
    }
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

const TestCase = struct {
    n: i8,
    s: []const u8,
    e: enum { a, b },
    f: f32,
    b: bool,
    o: ?u8,
    o2: ?u8,
    a: [2]u8,
    v: @Vector(2, u8),
    u: union(enum) { a: u8, s: []const u8 },
    u2: union { a: u8 },
};

fn expectTestCase(s: TestCase) !void {
    try testing.expectEqual(-1, s.n);
    try testing.expectEqualStrings(
        \\"b"
    , s.s);
    try testing.expectEqual(.a, s.e);
    try testing.expectEqual(42, s.f);
    try testing.expectEqual(true, s.b);
    try testing.expectEqual(null, s.o);
    try testing.expectEqual(0, s.o2);
    try testing.expectEqual(.{ 1, 2 }, s.a);
    try testing.expectEqual(.{ 3, 4 }, s.v);
    try testing.expectEqual(.a, std.meta.activeTag(s.u));
    try testing.expectEqual(1, s.u.a);
    try testing.expectEqual(1, s.u2.a);
}

test "parse into struct" {
    const src =
        \\{
        \\ :n -1, :s "b", :e :a, :f 42, :b true, :o nil,
        \\ :o2 0, :a [1,2], :v [3,4], :u {:a 1}, :u2 {:a 1}
        \\}
    ;

    try expectTestCase(try edn.parseTypeFromSlice(TestCase, src));
    comptime {
        @setEvalBranchQuota(10000);
        try expectTestCase(try edn.parseTypeFromSlice(TestCase, src));
    }
}

test "missing fields and default values" {
    try testing.expectError(
        error.MissingFields,
        edn.parseTypeFromSlice(struct { n: i8 }, "{}"),
    );
    try testing.expectError(
        error.InvalidUnion,
        edn.parseTypeFromSlice(union { n: i8 }, "{}"),
    );
    const s = try edn.parseTypeFromSlice(struct { n: i8 = 42 }, "{}");
    try testing.expectEqual(42, s.n);
}

test "tagged handler" {
    const datasrc =
        \\"2020-04-13T08:01:14.261Z"
    ;
    const src =
        \\{:crux.tx/tx-id 2 :crux.tx/tx-time #inst 
    ++ datasrc ++ "}";
    const res = try edn.parseFromSliceAlloc(talloc, src, .{}, .{
        .handlers = &.{.{
            "#inst",
            struct {
                fn handleInst(
                    value: edn.Value,
                    value_src: []const u8,
                    whole_src: []const u8,
                ) edn.ParseError!edn.Value {
                    _ = whole_src;
                    _ = value;
                    std.debug.assert(std.mem.eql(u8, value_src, datasrc));
                    return .{ .integer = 0 };
                }
            }.handleInst,
        }},
    });
    defer res.deinit(talloc);
    try testing.expectEqual(2, res.values[0].map.keys.len);
    try testing.expectEqual(.integer, std.meta.activeTag(res.values[0].map.values[1]));
    try testing.expectEqual(0, res.values[0].map.values[1].integer);
}

const Expectation = struct { edn.Value.Tag, u32 };

fn expect(src: []const u8, expected: []const Expectation) !void {
    const res = try edn.parseFromSliceAlloc(talloc, src, .{}, .{});
    defer res.deinit(talloc);
    // std.debug.print("{any}\n", .{res.values[0..res.top_level_values]});
    try testing.expectEqual(expected.len, res.top_level_values);
    for (expected, res.values[0..res.top_level_values]) |ex, actual| {
        try testing.expectEqual(ex[0], std.meta.activeTag(actual));
        switch (actual) {
            .string,
            .keyword,
            .symbol,
            .float,
            => |token| try testing.expectEqual(ex[1], token.end - token.start),
            .character => |c| try testing.expectEqual(ex[1], c),
            else => {},
        }
    }
}

test "string parsing" {
    try expect(
        \\""
    , &.{.{ .string, 2 }});
    try expect(
        \\"hi"
    , &.{.{ .string, 4 }});
    try expect(
        \\"hi there"
    , &.{.{ .string, 10 }});
    try expect(
        \\"one\ntwo"
    , &.{.{ .string, 10 }});
    try expect(
        \\"one\ntwo"
    , &.{.{ .string, 10 }});
    try expect(
        \\"one\rtwo"
    , &.{.{ .string, 10 }});
    try expect(
        \\"one\ttwo"
    , &.{.{ .string, 10 }});
    try expect(
        \\"\\"
    , &.{.{ .string, 4 }});
    try expect(
        \\"\""
    , &.{.{ .string, 4 }});
    try expect(
        \\hi"hi"
    , &.{ .{ .symbol, 2 }, .{ .string, 4 } });
    try expect(
        \\true"hi"
    , &.{ .{ .true, 0 }, .{ .string, 4 } });
    try expect(
        \\"hi"true
    , &.{ .{ .string, 4 }, .{ .true, 0 } });
    try expect(
        \\123"hi"
    , &.{ .{ .integer, 0 }, .{ .string, 4 } });
    try expect(
        \\"hi"123
    , &.{ .{ .string, 4 }, .{ .integer, 0 } });
    try expect(
        \\"hi""hi"
    , &.{ .{ .string, 4 }, .{ .string, 4 } });
}

test "char parsing" {
    try expect(
        \\\a
    , &.{.{ .character, 'a' }});
    try expect(
        \\\u
    , &.{.{ .character, 'u' }});
    try expect(
        \\\space
    , &.{.{ .character, ' ' }});
    try expect(
        \\\newline
    , &.{.{ .character, '\n' }});
    try expect(
        \\\return
    , &.{.{ .character, '\r' }});
    try expect(
        \\\tab
    , &.{.{ .character, '\t' }});
    try expect(
        \\\\
    , &.{.{ .character, '\\' }});
    try expect(
        \\\u1F590
    , &.{.{ .character, '🖐' }});
    try expect(
        \\\u"hi"
    , &.{ .{ .character, 'u' }, .{ .string, 4 } });
    try expect(
        \\\u1F590"hi"
    , &.{ .{ .character, '🖐' }, .{ .string, 4 } });
}
