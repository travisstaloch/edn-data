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
    const result2 = try edn.parseFromSliceAlloc(alloc, src, .{ .flags = .{ .whitespace = .exclude } }, .{});
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
    for (test_srcs) |asrc| {
        for (test_srcs) |bsrc| {
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

test "exclude whitespace" {
    // TODO - more coverage
    const res = try edn.parseFromSliceAlloc(talloc, "a b c", .{ .flags = .{ .whitespace = .exclude } }, .{});
    defer res.deinit(talloc);
}

test "ParseResult find()" {
    const src =
        \\(a b c {:a 1 :b 2 "foo" 3 ns/sym 4 5 5 \c 6})
    ;
    const res = try edn.parseFromSliceAlloc(talloc, src, .{ .flags = .{ .whitespace = .exclude } }, .{});
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
        const fmt = comptime blk: {
            const res = edn.parseFromSliceComptime(src, .{}, .{ .eval_branch_quota = 4000 }) catch unreachable;
            const final = std.fmt.comptimePrint("{}", .{edn.fmtParseResult(res, src)});
            break :blk final[0..final.len].*;
        };
        try testing.expectEqualStrings(src, &fmt);
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

    try expectTestCase(try edn.parseTypeFromSlice(TestCase, src, .{}));
    comptime {
        @setEvalBranchQuota(10000);
        try expectTestCase(try edn.parseTypeFromSlice(TestCase, src, .{}));
    }
}

test "missing fields and default values" {
    try testing.expectError(
        error.MissingFields,
        edn.parseTypeFromSlice(struct { n: i8 }, "{}", .{}),
    );
    try testing.expectError(
        error.InvalidUnion,
        edn.parseTypeFromSlice(union { n: i8 }, "{}", .{}),
    );
    const s = try edn.parseTypeFromSlice(struct { n: i8 = 42 }, "{}", .{});
    try testing.expectEqual(42, s.n);
}

test "tagged handler" {
    const datasrc =
        \\"2020-04-13T08:01:14.261Z"
    ;
    const src =
        \\{:crux.tx/tx-id 2 :crux.tx/tx-time #inst 
    ++ datasrc ++ "}";
    const handleInst = struct {
        fn handleInst(
            value: edn.Value,
            value_src: []const u8,
            whole_src: []const u8,
            userdata: ?*anyopaque,
        ) edn.ParseError!edn.Value {
            _ = whole_src;
            _ = value;
            std.debug.assert(std.mem.eql(u8, value_src, datasrc));
            const data: *u8 = @ptrCast(userdata orelse unreachable);
            data.* = 10;
            return .{ .integer = 0 };
        }
    }.handleInst;
    var userdata1: u8 = 0;
    const res = try edn.parseFromSliceAlloc(talloc, src, .{ .userdata = &userdata1 }, .{
        .handlers = &.{.{ "#inst", handleInst }},
    });
    defer res.deinit(talloc);
    try testing.expectEqual(2, res.values[0].map.keys.len);
    try testing.expectEqual(.integer, std.meta.activeTag(res.values[0].map.values[1]));
    try testing.expectEqual(0, res.values[0].map.values[1].integer);
    try testing.expectEqual(10, userdata1);

    const DateTime = struct { int: i128 };
    const Crux = struct {
        @"crux.tx/tx-id": u32,
        @"crux.tx/tx-time": DateTime,

        pub fn ednTagHandler(
            T: type,
            tag_src: []const u8,
            value_src: []const u8,
            whole_src: []const u8,
            value: edn.Value,
            userdata: ?*anyopaque,
        ) edn.ParseError!T {
            _ = whole_src;
            _ = value_src;
            _ = value;
            switch (T) {
                DateTime => {
                    if (std.mem.eql(u8, tag_src, "#inst")) {
                        const data: *u8 = @ptrCast(userdata orelse unreachable);
                        data.* = 10;
                        return .{ .int = -1 };
                    }
                },
                else => {},
            }
            return error.HandlerParse;
        }
    };
    var userdata2: u8 = 0;
    const crux = try edn.parseTypeFromSlice(Crux, src, .{ .userdata = &userdata2 });
    try testing.expectEqual(2, crux.@"crux.tx/tx-id");
    try testing.expectEqual(-1, crux.@"crux.tx/tx-time".int);
    try testing.expectEqual(10, userdata2);
    const CruxNoHandler = struct {
        @"crux.tx/tx-id": u32,
        @"crux.tx/tx-time": DateTime,
    };
    try testing.expectError(error.MissingHandler, edn.parseTypeFromSlice(CruxNoHandler, src, .{}));
}

const Expectation = union(enum) {
    true,
    false,
    nil,
    string: []const u8,
    symbol: []const u8,
    keyword: []const u8,
    float: []const u8,
    character: u21,
    integer: i128,
    vector: []const Expectation,
    list: []const Expectation,
    set: []const Expectation,
    map: []const [2]Expectation,
};

fn expectOne(ex: Expectation, actual: edn.Value, src: []const u8) !void {
    try testing.expectEqualStrings(@tagName(ex), @tagName(actual));
    switch (actual) {
        inline .string,
        .symbol,
        .keyword,
        => |token, tag| try testing.expectEqualStrings(@field(ex, @tagName(tag)), token.src(src)),
        .float,
        => |float| try testing.expectEqualStrings(ex.float, float.src(src)),
        .nil,
        .true,
        .false,
        => {},
        inline .character,
        .integer,
        => |int, tag| try testing.expectEqual(@field(ex, @tagName(tag)), int),
        .vector => |v| {
            try testing.expectEqual(ex.vector.len, v.len);
            for (ex.vector, v) |e, a| {
                try expectOne(e, a, src);
            }
        },
        .list => |v| {
            try testing.expectEqual(ex.list.len, v.len);
            for (ex.list, v) |e, a| {
                try expectOne(e, a, src);
            }
        },
        .set => |v| {
            try testing.expectEqual(ex.set.len, v.len);
            for (ex.set, v) |e, a| {
                try expectOne(e, a, src);
            }
        },
        .map => |v| {
            try testing.expectEqual(ex.map.len, v.keys.len);
            try testing.expectEqual(ex.map.len, v.values.len);
            for (ex.map, 0..) |e, i| {
                try expectOne(e[0], v.keys[i], src);
                try expectOne(e[1], v.values[i], src);
            }
        },
    }
}

fn expect(src: []const u8, expected: []const Expectation) !void {
    const res = try edn.parseFromSliceAlloc(talloc, src, .{}, .{});
    defer res.deinit(talloc);
    // std.debug.print("{any}\n", .{res.values[0..res.top_level_values]});
    try testing.expectEqual(expected.len, res.top_level_values);
    for (expected, res.values[0..res.top_level_values]) |ex, actual| {
        try expectOne(ex, actual, src);
    }
}

inline fn quote(comptime s: []const u8) []const u8 {
    return comptime "\"" ++ s ++ "\"";
}

const str_hi = Expectation{ .string = quote("hi") };
const str_one = Expectation{ .string = quote("one") };
const str_two = Expectation{ .string = quote("two") };
const str_and_two = Expectation{ .string = quote("and two") };
const str_well = Expectation{ .string = quote("well, then.") };
const sym_hi = Expectation{ .symbol = "hi" };
const str_a = Expectation{ .string = quote("a") };
const str_b = Expectation{ .string = quote("b") };
const str_c = Expectation{ .string = quote("c") };

test "string parsing" {
    try expect(
        \\""
    , &.{.{ .string = "\"\"" }});
    try expect("\"hi\"", &.{str_hi});
    try expect(
        \\"hi there"
    , &.{.{ .string = quote("hi there") }});
    try expect(
        \\"one\ntwo"
    , &.{.{ .string = quote("one\\ntwo") }});
    try expect(
        \\"one\ntwo"
    , &.{.{ .string = quote("one\\ntwo") }});
    try expect(
        \\"one\rtwo"
    , &.{.{ .string = quote("one\\rtwo") }});
    try expect(
        \\"one\ttwo"
    , &.{.{ .string = quote("one\\ttwo") }});
    try expect(
        \\"\\"
    , &.{.{ .string = 
    \\"\\"
    }});
    try expect(
        \\"\""
    , &.{.{ .string = 
    \\"\""
    }});
    try testing.expectError(error.MissingWhitespaceBetweenValues, expect(
        \\hi"hi"
    , &.{}));
    try expect(
        \\hi "hi"
    , &.{ sym_hi, str_hi });
    try expect(
        \\true "hi"
    , &.{ .true, str_hi });
    try expect(
        \\"hi" true
    , &.{ str_hi, .true });
    try expect(
        \\123 "hi"
    , &.{ .{ .integer = 123 }, str_hi });
    try expect(
        \\"hi" 123
    , &.{ str_hi, .{ .integer = 123 } });
    try expect(
        \\"hi" "hi"
    , &.{ str_hi, str_hi });

    try testing.expectError(error.InvalidString, expect(
        \\"
    , &.{}));
    try testing.expectError(error.InvalidEscape, expect(
        \\"\
    , &.{}));
    try testing.expectError(error.InvalidString, expect(
        \\"\\
    , &.{}));
    try testing.expectError(error.MissingWhitespaceBetweenValues, expect(
        \\"\\""
    , &.{}));
}

test "char parsing" {
    try expect(
        \\\a
    , &.{.{ .character = 'a' }});
    try expect(
        \\\u
    , &.{.{ .character = 'u' }});
    try expect(
        \\\space
    , &.{.{ .character = ' ' }});
    try expect(
        \\\newline
    , &.{.{ .character = '\n' }});
    try expect(
        \\\return
    , &.{.{ .character = '\r' }});
    try expect(
        \\\tab
    , &.{.{ .character = '\t' }});
    try expect(
        \\\\
    , &.{.{ .character = '\\' }});
    try expect(
        \\\u1F590
    , &.{.{ .character = '🖐' }});
    try expect(
        \\\u "hi"
    , &.{ .{ .character = 'u' }, str_hi });
    try expect(
        \\\u1F590 "hi"
    , &.{ .{ .character = '🖐' }, str_hi });

    try testing.expectError(error.InvalidChar, expect(
        \\\
    , &.{}));
}

test "int parsing" {
    try expect("928764", &.{.{ .integer = 928764 }});
    try expect("1001", &.{.{ .integer = 1001 }});
    try expect("0", &.{.{ .integer = 0 }});
    try expect("+3", &.{.{ .integer = 3 }});
    try expect("-0", &.{.{ .integer = 0 }});
    try expect("-12", &.{.{ .integer = -12 }});
}

test "float parsing" {
    try expect("928.764", &.{.{ .float = "928.764" }});
    try expect("1001.1", &.{.{ .float = "1001.1" }});
    try expect("-8.74", &.{.{ .float = "-8.74" }});
    try expect("2.1e5", &.{.{ .float = "2.1e5" }});
    try expect("2.1E5", &.{.{ .float = "2.1E5" }});
    try expect("22.1e+2", &.{.{ .float = "22.1e+2" }});
    try expect("22.1E+2", &.{.{ .float = "22.1E+2" }});
    try expect("5.12e-3", &.{.{ .float = "5.12e-3" }});
    try expect("5.12E-3", &.{.{ .float = "5.12E-3" }});
    try expect("1001.00100e10", &.{.{ .float = "1001.00100e10" }});
}

test "symbol parsing" {
    try expect("=", &.{.{ .symbol = "=" }});
    try expect("even?", &.{.{ .symbol = "even?" }});
    try expect("even? ", &.{.{ .symbol = "even?" }});
    try expect("a#b", &.{.{ .symbol = "a#b" }});
    try expect("a#", &.{.{ .symbol = "a#" }});
    try expect("a:b", &.{.{ .symbol = "a:b" }});
    try expect("a:", &.{.{ .symbol = "a:" }});
    try expect("/", &.{.{ .symbol = "/" }});
    try expect("a/a#", &.{.{ .symbol = "a/a#" }});
    try expect("a/a:", &.{.{ .symbol = "a/a:" }});

    try testing.expectError(error.InvalidSymbol, expect("a/b/c", &.{}));
    try testing.expectError(error.InvalidSymbol, expect("a/", &.{}));
    try testing.expectError(error.InvalidSymbol, expect("/a", &.{}));
    try testing.expectError(error.InvalidSymbol, expect("a/#a", &.{}));
    try testing.expectError(error.InvalidSymbol, expect("a/:a", &.{}));
    try testing.expectError(error.MissingWhitespaceBetweenValues, expect("-0a", &.{}));
    try testing.expectError(error.MissingWhitespaceBetweenValues, expect("+0a", &.{}));
    try testing.expectError(error.InvalidSymbol, expect(".0a", &.{}));
    try testing.expectError(error.InvalidSymbol, expect("##a", &.{}));
    try testing.expectError(error.InvalidSymbol, expect("#:a", &.{}));
    try testing.expectError(error.InvalidSymbol, expect("::a", &.{}));
    try testing.expectError(error.InvalidSymbol, expect(":#a", &.{}));
}

test "keyword parsing" {
    try expect(":a", &.{.{ .keyword = ":a" }});
    try expect(":name", &.{.{ .keyword = ":name" }});
    try expect(":ns.nested/name ", &.{.{ .keyword = ":ns.nested/name" }});
}

test "vector parsing" {
    const empty_vec = Expectation{ .vector = &.{} };
    try expect("[]", &.{empty_vec});
    try expect("[  ]", &.{empty_vec});
    try expect("[\"one\"]", &.{.{ .vector = &.{str_one} }});
    try expect("[\"one\" \"and two\"]", &.{.{ .vector = &.{ str_one, str_and_two } }});
    try expect("[true true]", &.{.{ .vector = &.{ .true, .true } }});
    try expect("[true \"well, then.\"]", &.{.{ .vector = &.{ .true, str_well } }});
    try expect("[true  [\"one\" [\"two\", nil ]]]", &.{
        .{ .vector = &.{ .true, .{ .vector = &.{ str_one, .{ .vector = &.{ str_two, .nil } } } } } },
    });
    try expect("[[] [] ]", &.{.{ .vector = &.{ empty_vec, empty_vec } }});
    try expect("[[] []]", &.{.{ .vector = &.{ empty_vec, empty_vec } }});
    try expect("[hi []]", &.{.{ .vector = &.{ sym_hi, empty_vec } }});
    try expect("[[] hi]", &.{.{ .vector = &.{ empty_vec, sym_hi } }});
    try expect("[true []]", &.{.{ .vector = &.{ .true, empty_vec } }});
    try expect("[[] true]", &.{.{ .vector = &.{ empty_vec, .true } }});
    try expect("[\"hi\" []]", &.{.{ .vector = &.{ str_hi, empty_vec } }});
    try expect("[[] \"hi\"]", &.{.{ .vector = &.{ empty_vec, str_hi } }});
}

test "list parsing" {
    const empty_list = Expectation{ .list = &.{} };
    try expect("()", &.{empty_list});
    try expect("(  )", &.{empty_list});
    try expect("(\"one\")", &.{.{ .list = &.{str_one} }});
    try expect("(\"one\" \"and two\")", &.{.{ .list = &.{ str_one, str_and_two } }});
    try expect("(true true)", &.{.{ .list = &.{ .true, .true } }});
    try expect("(true \"well, then.\")", &.{.{ .list = &.{ .true, str_well } }});
    try expect("(true  (\"one\" (\"two\", nil )))", &.{
        .{ .list = &.{ .true, .{ .list = &.{ str_one, .{ .list = &.{ str_two, .nil } } } } } },
    });
    try expect("(hi ())", &.{.{ .list = &.{ sym_hi, empty_list } }});
    try expect("(() hi)", &.{.{ .list = &.{ empty_list, sym_hi } }});
    try expect("(false ())", &.{.{ .list = &.{ .false, empty_list } }});
    try expect("(() false)", &.{.{ .list = &.{ empty_list, .false } }});
    try expect("(\"hi\" ())", &.{.{ .list = &.{ str_hi, empty_list } }});
    try expect("(() \"hi\")", &.{.{ .list = &.{ empty_list, str_hi } }});
    try expect("(true  (\"one\" (\"two\", nil )))", &.{
        .{ .list = &.{ .true, .{ .list = &.{ str_one, .{ .list = &.{ str_two, .nil } } } } } },
    });
}

test "set parsing" {
    const empty_set = Expectation{ .set = &.{} };
    try expect("#{}", &.{empty_set});
    try expect("#{  }", &.{empty_set});
    try expect("#{\"one\"}", &.{.{ .set = &.{str_one} }});
    try expect("#{\"one\" \"and two\"}", &.{.{ .set = &.{ str_one, str_and_two } }});
    try expect("#{true true}", &.{.{ .set = &.{ .true, .true } }});
    try expect("#{true \"well, then.\"}", &.{.{ .set = &.{ .true, str_well } }});
    try expect("#{true  #{\"one\" #{\"two\", nil }}}", &.{
        .{ .set = &.{ .true, .{ .set = &.{ str_one, .{ .set = &.{ str_two, .nil } } } } } },
    });
    try expect("#{true  #{\"one\" #{\"two\", nil }}}", &.{
        .{ .set = &.{ .true, .{ .set = &.{ str_one, .{ .set = &.{ str_two, .nil } } } } } },
    });
    try expect("#{true  #{\"one\" #{\"two\", nil }}}", &.{
        .{ .set = &.{ .true, .{ .set = &.{ str_one, .{ .set = &.{ str_two, .nil } } } } } },
    });
    try expect("#{hi #{}}", &.{.{ .set = &.{ sym_hi, empty_set } }});
    try expect("#{#{} hi}", &.{.{ .set = &.{ empty_set, sym_hi } }});
    try expect("#{true #{}}", &.{.{ .set = &.{ .true, empty_set } }});
    try expect("#{#{} true}", &.{.{ .set = &.{ empty_set, .true } }});
    try expect("#{\"hi\" #{}}", &.{.{ .set = &.{ str_hi, empty_set } }});
    try expect("#{#{} \"hi\"}", &.{.{ .set = &.{ empty_set, str_hi } }});
}

test "map parsing" {
    const empty_map = Expectation{ .map = &.{} };
    try expect("{}", &.{empty_map});
    try expect("{  }", &.{empty_map});
    try expect("{\"one\" \"two\"}", &.{.{ .map = &.{.{ str_one, str_two }} }});
    try expect("{\"a\" true, \"b\" false }", &.{
        .{ .map = &.{ .{ str_a, .true }, .{ str_b, .false } } },
    });
    try expect("{true  {\"one\" {\"two\", nil }}}", &.{
        .{ .map = &.{.{ .true, .{ .map = &.{.{ str_one, .{ .map = &.{.{ str_two, .nil }} } }} } }} },
    });
    try expect("{\"a\"  {\"b\" {\"c\", 123}}}", &.{
        .{ .map = &.{.{ str_a, .{ .map = &.{.{ str_b, .{ .map = &.{.{ str_c, .{ .integer = 123 } }} } }} } }} },
    });
    try expect("{hi {}}", &.{.{ .map = &.{.{ sym_hi, empty_map }} }});
    try expect("{{} hi}", &.{.{ .map = &.{.{ empty_map, sym_hi }} }});
    try expect("{false {}}", &.{.{ .map = &.{.{ .false, empty_map }} }});
    try expect("{{} false}", &.{.{ .map = &.{.{ empty_map, .false }} }});
    try expect("{\"hi\" {}}", &.{.{ .map = &.{.{ str_hi, empty_map }} }});
    try expect("{{} \"hi\"}", &.{.{ .map = &.{.{ empty_map, str_hi }} }});
}

test "discard parsing" {
    try expect("#_\"hi\"", &.{});
    try expect("#_  \"hi\"", &.{});
    try expect("#_928764", &.{});
    try expect("#_  928764", &.{});
    try expect("#_even? ", &.{});
    try expect("#_  even? ", &.{});
    try expect("#_:a", &.{});
    try expect("#_  :a", &.{});
    try expect("#_[]", &.{});
    try expect("#_  []", &.{});
    try expect("#_()", &.{});
    try expect("#_  ()", &.{});
    try expect("#_#{}", &.{});
    try expect("#_  #{}", &.{});
    try expect("#_{}", &.{});
    try expect("#_  {}", &.{});
    const int1 = Expectation{ .integer = 1 };
    const int2 = Expectation{ .integer = 2 };
    const int3 = Expectation{ .integer = 3 };
    const vec13 = [_]Expectation{.{ .vector = &.{ int1, int3 } }};
    try expect("[1 #_2 3]", &vec13);
    try expect("[1 #_  2 3]", &vec13);
    const list13 = [_]Expectation{.{ .list = &.{ int1, int3 } }};
    try expect("(1 #_2 3)", &list13);
    try expect("(1 #_  2 3)", &list13);
    const set13 = [_]Expectation{.{ .set = &.{ int1, int3 } }};
    try expect("#{1 #_2 3}", &set13);
    try expect("#{1 #_  2 3}", &set13);
    const map13 = [_]Expectation{.{ .map = &.{.{ int1, int3 }} }};
    try expect("{1 #_2 3}", &map13);
    try expect("{1 #_  2 3}", &map13);
    const map12 = [_]Expectation{.{ .map = &.{.{ int1, int2 }} }};
    try expect("{1 2 #_3}", &map12);
    try expect("{1 2 #_  3}", &map12);
    try expect("#_  #ns.a/tag :key", &.{});
    try expect("#_#ns.a/tag :key", &.{});
    try expect("#_ #_ #_ 1 2 3", &.{});
}

test "no intermediate whitespace" {
    const srcs_no_ws = [_][]const u8{
        "a\"a\"",
        "\"a\"a",
        "1a",
        "1.0a",
        "a[]",
        "[]a",
        "a#{}",
        "#{}a",
        "a{}",
        "{}a",
        "\"a\":a",
        ":a\"a\"",
        "\"a\"1",
        "1\"a\"",
        "\"a\"1.0",
        "1.0\"a\"",
        "\"a\"[]",
        "[]\"a\"",
        "\"a\"#{}",
        "#{}\"a\"",
        "\"a\"{}",
        "{}\"a\"",
        "1:a",
        "1.0:a",
        ":a[]",
        "[]:a",
        ":a#{}",
        "#{}:a",
        ":a{}",
        "{}:a",
        "1[]",
        "[]1",
        "1#{}",
        "#{}1",
        "1{}",
        "{}1",
        "1.0[]",
        "[]1.0",
        "1.0#{}",
        "#{}1.0",
        "1.0{}",
        "{}1.0",
        "[]#{}",
        "#{}[]",
        "[]{}",
        "{}[]",
        "#{}{}",
        "{}#{}",
    };

    for (srcs_no_ws) |src| {
        const res = edn.parseFromSliceAlloc(talloc, src, .{}, .{});
        try testing.expectError(error.MissingWhitespaceBetweenValues, res);
    }
}
