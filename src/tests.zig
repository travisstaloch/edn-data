const std = @import("std");
const testing = std.testing;
const talloc = testing.allocator;

const edn = @import("extensible-data-notation");

fn expectTag(src: [:0]const u8, tag: edn.Value.Tag) !void {
    // std.debug.print("src '{s}'\n", .{src});
    const res = try edn.parseFromSliceAlloc(talloc, src, .{}, .{});
    defer res.deinit(talloc);
    const tlv = res.firstValue();
    try testing.expectEqual(tag, std.meta.activeTag(tlv.*));
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
    try expectTag("#foo 1", .tagged);
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

    try testParse(talloc, src);
}

test "fmt chars" {
    const src =
        \\WS #{\space \tab \return \newline \, \u0000}
        \\
    ;

    try testParse(talloc, src);
}

test "list" {
    const src =
        \\(defrecord
        \\  MenuItem
        \\  [name rating])
    ;

    try testParse(talloc, src);
}

test "nested list" {
    try testParse(talloc, " ( [] {} ) ");
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

    try testParse(talloc, src);
}

fn testParse(alloc: std.mem.Allocator, src: [:0]const u8) !void {
    try testParseOptions(alloc, src, .{});
    try testParseOptions(alloc, src, .{ .whitespace = false });
}

fn testParseOptions(alloc: std.mem.Allocator, src: [:0]const u8, options: edn.Options) !void {
    const result = try edn.parseFromSliceAlloc(alloc, src, options, .{});
    defer result.deinit(alloc);
    if (false and options.whitespace) {
        for (0..result.values.items.len) |i| {
            const ws = result.items(.whitespace, i).*;
            const v = result.items(.values, i).*;
            std.debug.print("{} {s} '{s}'", .{ i, @tagName(v), src[ws[0]..ws[1]] });
            switch (v) {
                inline .list, .vector, .set, .map => |p| {
                    std.debug.print(" trailing_ws '{s}'", .{src[p.trailing_ws[0]..p.trailing_ws[1]]});
                },
                else => {},
            }
            std.debug.print("\n", .{});
        }
    }
    const src1 = try std.fmt.allocPrintZ(alloc, "{}", .{result.formatter(src)});
    defer alloc.free(src1);
    if (options.whitespace) {
        try testing.expectEqualStrings(src, src1);
    } else if (false) {
        std.debug.print("src  '{s}'\n", .{src});
        std.debug.print("src1 '{s}'\n", .{src1});
    }
    const result1 = edn.parseFromSliceAlloc(alloc, src1, options, .{}) catch |e| {
        // std.debug.print("src1 '{s}'\n", .{src1});
        return e;
    };
    defer result1.deinit(alloc);
    const len = @min(result.values.items.len, result1.values.items.len);
    for (result.values.items[0..len], result1.values.items[0..len]) |*r, *r1| {
        try testing.expect(r.eql(src, &result, r1, src1, &result1));
    }
}

test "allocation failures" {
    const f = try std.fs.cwd().openFile("examples/edn.edn", .{});
    defer f.close();
    const src = try f.readToEndAllocOptions(talloc, 100000, null, .@"8", 0);
    defer talloc.free(src);
    try testParse(talloc, src);
    try testing.checkAllAllocationFailures(talloc, testParse, .{src});
}

fn testEql(asrc: [:0]const u8, bsrc: [:0]const u8, alen: u8, blen: u8) !void {
    const ares = try edn.parseFromSliceAlloc(talloc, asrc, .{}, .{});
    defer ares.deinit(talloc);
    const bres = try edn.parseFromSliceAlloc(talloc, bsrc, .{}, .{});
    defer bres.deinit(talloc);
    try testing.expectEqual(alen + 1, ares.values.items.len);
    try testing.expectEqual(blen + 1, bres.values.items.len);

    var actx = edn.MapContext{ .src = asrc, .result = &ares };
    var bctx = edn.MapContext{ .src = bsrc, .result = &bres };
    if (actx.hash(&ares.values.items[0]) != bctx.hash(&bres.values.items[0])) {
        return error.TestExpectedEqual;
    }
    try testing.expect(ares.values.items[0].eql(asrc, &ares, &bres.values.items[0], bsrc, &bres));

    try testing.expectFmt(asrc, "{}", .{ares.formatter(asrc)});
    try testing.expectFmt(bsrc, "{}", .{bres.formatter(bsrc)});
}

fn testNotEql(asrc: [:0]const u8, bsrc: [:0]const u8, alen: u8, blen: u8) !void {
    testEql(asrc, bsrc, alen, blen) catch |e| switch (e) {
        error.TestUnexpectedResult,
        error.TestExpectedEqual,
        => {},
        else => return e,
    };
}

// zig fmt: off
const test_srcs = [_]struct {u8,  [:0]const u8}{
   .{1,  \\nil
}, .{1,  \\true
}, .{1,  \\false
}, .{1,  \\1
}, .{1,  \\1.0
}, .{1,  \\\c
}, .{1,  \\ns/name
}, .{7,  \\{:a 1 :b 2 :c 3}
}, .{4,  \\#{:a 1 "foo"}
}, .{4,  \\(:a 1 "foo")
}, .{23, \\(nil true false 1 1.0 \c ns/name {:a 1 :b 2 :c 3} #{:a 1 "foo"} (:a 1 "foo"))
}, .{23, \\[nil true false 1 1.0 \c ns/name {:a 1 :b 2 :c 3} #{:a 1 "foo"} (:a 1 "foo")]
}, .{23, \\{nil true false 1 1.0 \c ns/name {:a 1 :b 2 :c 3} #{:a 1 "foo"} (:a 1 "foo")}
}, .{23, \\#{nil true false 1 1.0 \c ns/name {:a 1 :b 2 :c 3} #{:a 1 "foo"} (:a 1 "foo")}
}};
// zig fmt: on

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
            if (std.mem.eql(u8, asrc[1], bsrc[1]))
                try testEql(asrc[1], bsrc[1], asrc[0], bsrc[0])
            else
                try testNotEql(asrc[1], bsrc[1], asrc[0], bsrc[0]);
        }
    }
}

test "measure" {
    {
        const measured = try edn.measure("(a, b, c)", .{}, .{});
        try testing.expectEqual(5, measured.capacity);
        try testing.expectEqual(1, measured.top_level_values);
    }
    {
        const measured = try edn.measure("( [a] {:a a} )", .{}, .{});
        try testing.expectEqual(7, measured.capacity);
        try testing.expectEqual(1, measured.top_level_values);
    }
}

test "exclude whitespace" {
    const src = "a (a b c [] {:a :b :c :d})";
    const res = try edn.parseFromSliceAlloc(talloc, src, .{ .whitespace = false }, .{});
    defer res.deinit(talloc);
    try testing.expectFmt(src, "{}", .{res.formatter(src)});
}

test "ParseResult find()" {
    const src =
        \\(a b c {:a 1, :b 2, "foo" 3, ns/sym 4, 5 10, \c 6})
    ;
    const res = try edn.parseFromSliceAlloc(talloc, src, .{ .whitespace = false }, .{});
    defer res.deinit(talloc);
    try testing.expectError(error.PathNotFound, res.find("1", src));
    try testing.expectError(error.PathNotFound, res.find("0//3//missing", src));
    {
        const v = (try res.find("0", src)).*;
        try testing.expectEqual(.list, std.meta.activeTag(v));
    }
    {
        const v = (try res.find("0//0", src)).*;
        try testing.expectEqual(.symbol, std.meta.activeTag(v));
    }
    {
        const v = (try res.find("0//3//:a", src)).*;
        try testing.expectEqual(.integer, std.meta.activeTag(v));
        try testing.expectEqual(1, v.integer);
    }
    {
        const v = (try res.find("0//3//:b", src)).*;
        try testing.expectEqual(.integer, std.meta.activeTag(v));
        try testing.expectEqual(2, v.integer);
    }
    {
        const v = (try res.find("0//3//\"foo\"", src)).*;
        try testing.expectEqual(.integer, std.meta.activeTag(v));
        try testing.expectEqual(3, v.integer);
    }
    {
        const v = (try res.find("0//3//ns/sym", src)).*;
        try testing.expectEqual(.integer, std.meta.activeTag(v));
        try testing.expectEqual(4, v.integer);
    }
    { // 5 is an integer map key
        const v = (try res.find("0//3//5", src)).*;
        try testing.expectEqual(.integer, std.meta.activeTag(v));
        try testing.expectEqual(10, v.integer);
    }
    {
        const v = (try res.find("0//3//\\c", src)).*;
        try testing.expectEqual(.integer, std.meta.activeTag(v));
        try testing.expectEqual(6, v.integer);
    }
}

test "comptime parse" {
    inline for (test_srcs) |src_len| {
        const src = src_len[1];
        const fmt = comptime blk: {
            const res = try edn.parseFromSliceComptime(src, .{ .whitespace = false }, .{ .eval_branch_quota = 32000 });
            const final = std.fmt.comptimePrint("{}", .{res.formatter(src)});
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

const TestCase_src =
    \\{
    \\ :n -1, :s "b", :e :a, :f 42, :b true, :o nil,
    \\ :o2 0, :a [1,2], :v [3,4], :u {:a 1}, :u2 {:a 1}
    \\}
;

test "parse into struct" {
    try expectTestCase(try edn.parseTypeFromSlice(TestCase, TestCase_src, .{}));
    comptime {
        @setEvalBranchQuota(10000);
        try expectTestCase(try edn.parseTypeFromSlice(TestCase, TestCase_src, .{}));
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
        ) edn.Error!edn.Value {
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
    try testing.expectEqual(2, res.values.items[1].map.len);
    var iter = res.iterator(&res.values.items[1]);
    const id = iter.nth(3).?;
    try testing.expectEqual(.tagged, std.meta.activeTag(res.items(.values, id).*));
    try testing.expectEqual(.integer, std.meta.activeTag(res.items(.values, id).*.tagged.value.*));
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
        ) edn.Error!T {
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

const Expectation = union(edn.Value.Tag) {
    nil,
    true,
    false,
    string: []const u8,
    character: u21,
    keyword: []const u8,
    symbol: []const u8,
    tagged: struct { []const u8, *const Expectation },
    integer: i128,
    float: []const u8,
    list: []const Expectation,
    vector: []const Expectation,
    set: []const Expectation,
    map: []const [2]Expectation,
};

fn expectOne(ex: Expectation, actual: *const edn.Value, src: [:0]const u8, result: *const edn.Result) !void {
    try testing.expectEqualStrings(@tagName(ex), @tagName(actual.*));
    switch (actual.*) {
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
        inline .vector, .list, .set => |v, tag| {
            const ex_payload = @field(ex, @tagName(tag));
            try testing.expectEqual(ex_payload.len, v.len);
            var iter = result.iterator(actual);
            for (ex_payload) |e| {
                const a = iter.next().?;
                try expectOne(e, &result.items(.values, a).*, src, result);
            }
            try testing.expectEqual(null, iter.next());
        },
        .map => |v| {
            try testing.expectEqual(ex.map.len, v.len);
            var iter = result.iterator(actual);
            // std.debug.print("actual {}\n", .{actual.*});
            for (ex.map) |e| {
                // std.debug.print("{}\n", .{e[0]});
                try expectOne(e[0], &result.items(.values, iter.next().?).*, src, result);
                // std.debug.print("{}\n", .{e[1]});
                try expectOne(e[1], &result.items(.values, iter.next().?).*, src, result);
            }
            try testing.expectEqual(null, iter.next());
        },
        .tagged => |tagged| {
            try testing.expectEqualStrings(ex.tagged[0], tagged.tag.src(src));
            try expectOne(ex.tagged[1].*, tagged.value, src, result);
        },
        // else => std.debug.panic("TODO {s}", .{@tagName(actual.*)}),
    }
}

fn expect(src: [:0]const u8, expected: []const Expectation) !void {
    const res = try edn.parseFromSliceAlloc(talloc, src, .{}, .{});
    defer res.deinit(talloc);
    // std.debug.print("expected {any}\n", .{expected});
    // std.debug.print("got      {any}\n", .{res.items(.values, 0).*.list.len});
    // std.debug.print("got      {}\n", .{res.items(.values, 0).*.listAt(0, &res)});

    try testing.expectEqual(.list, std.meta.activeTag(res.values.items[0]));
    try testing.expectEqual(expected.len, res.values.items[0].list.len);

    var iter = res.iterator(&res.values.items[0]);
    for (0..res.values.items[0].list.len) |i| {
        const child_id = iter.next().?;
        const ex = expected[i];
        try expectOne(ex, &res.items(.values, child_id).*, src, &res);
    }
}

inline fn quote(comptime s: []const u8) []const u8 {
    return comptime "\"" ++ s ++ "\"";
}

const str_hi = Expectation{ .string = quote("hi") };
const str_wave = Expectation{ .string = quote("🖐") };
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

    try testing.expectError(error.InvalidToken, expect("\"", &.{}));
    try testing.expectError(error.InvalidToken, expect("\"\\", &.{}));
    try testing.expectError(error.InvalidToken, expect("\"\\\\", &.{}));
    try testing.expectError(error.InvalidToken, expect("s\x13mbol", &.{}));
    try testing.expectError(error.MissingWhitespaceBetweenValues, expect(
        \\"\\"""
    , &.{}));
}

test "char parsing" {
    try expect(
        \\\a \u \u0000 "🖐"
    , &.{
        .{ .character = 'a' },
        .{ .character = 'u' },
        .{ .character = 0 },
        str_wave,
    });
    try expect(
        \\\space \newline \return \tab \\
    , &.{
        .{ .character = ' ' },
        .{ .character = '\n' },
        .{ .character = '\r' },
        .{ .character = '\t' },
        .{ .character = '\\' },
    });
    try expect(
        \\\u "hi"
    , &.{ .{ .character = 'u' }, str_hi });
    try expect(
        \\"🖐"
    , &.{str_wave});
    try expect(
        \\\u000a
    , &.{.{ .character = '\n' }});

    try testing.expectError(error.InvalidToken, expect(
        \\\
    , &.{}));
    try testing.expectError(error.InvalidToken, expect(
        \\\\0
    , &.{}));
    try testParse(talloc, "\\u0000");
    try testParse(talloc, "\\u0001");
    try testing.expectError(error.InvalidToken, expect("\\u0", &.{}));
    try testing.expectError(error.InvalidToken, expect("\\u000", &.{}));
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
    try testing.expectError(error.InvalidToken, expect("/", &.{.{ .symbol = "/" }}));
    try expect("a/a#", &.{.{ .symbol = "a/a#" }});
    try expect("a/a:", &.{.{ .symbol = "a/a:" }});

    try testing.expectError(error.InvalidToken, expect("a/b/c", &.{}));
    try testing.expectError(error.InvalidToken, expect("a/", &.{}));
    try testing.expectError(error.InvalidToken, expect("/a", &.{}));
    try testing.expectError(error.InvalidToken, expect("a/#a", &.{}));
    try testing.expectError(error.InvalidToken, expect("a/:a", &.{}));
    try testing.expectError(error.MissingWhitespaceBetweenValues, expect("-0a", &.{}));
    try testing.expectError(error.MissingWhitespaceBetweenValues, expect("+0a", &.{}));
    try testing.expectError(error.MissingWhitespaceBetweenValues, expect(".0a", &.{}));
    try testing.expectError(error.InvalidToken, expect("##a", &.{}));
    try testing.expectError(error.InvalidToken, expect("#:a", &.{}));
    try testing.expectError(error.InvalidToken, expect("::a", &.{}));
    try testing.expectError(error.InvalidToken, expect(":#a", &.{}));
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

test "tagged parsing and formatting" {
    try testParse(talloc, "#foo 1");
    try testParse(talloc,
        \\#MyYelpClone/MenuItem {:name "eggs-benedict" :rating 10}
    );
    try expect("#foo 1", &.{.{ .tagged = .{ "#foo", &.{ .integer = 1 } } }});
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
    // TODO fold top level, first discard into whitespaces or list.trailing_ws somehow
    // TODO try testParse(talloc, "#_foo");
    try testParse(talloc, "#_foo 1");
    try testParse(talloc, "1 #_   foo");
    try testParse(talloc, "(  #_   foo  )");
}

test "no intermediate whitespace" {
    const srcs_no_ws = [_][:0]const u8{
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

test "ednParse()" {
    var userdata1: u8 = 0;
    const T = struct {
        a: u8,
        pub fn ednParse(p: *edn.Parser, userdata: ?*anyopaque, options: edn.Options) edn.Error!@This() {
            const data: *u8 = @ptrCast(userdata orelse unreachable);
            data.* = 10;

            if (p.tokenizer.next().tag != .lcurly) return error.Parse;

            const key = try edn.userParseValue(p, options);
            if (key != .keyword) return error.Parse;

            const value = try edn.userParseValue(p, options);
            if (value != .integer) return error.Parse;
            if (p.tokenizer.next().tag != .rcurly) return error.Parse;

            return .{ .a = @intCast(value.integer) };
        }
    };
    const src = "{:a 10}";
    const t = try edn.parseTypeFromSlice(T, src, .{ .userdata = &userdata1 });
    try testing.expectEqual(10, t.a);
}

test "Tokenizer.edn" {
    const f = try std.fs.cwd().openFile("examples/Tokenizer.edn", .{});
    defer f.close();
    const src = try f.readToEndAllocOptions(talloc, 100000, null, .@"8", 0);
    defer talloc.free(src);
    try testing.checkAllAllocationFailures(talloc, testParse, .{src});

    const result = try edn.parseFromSliceAlloc(talloc, src, .{}, .{});
    defer result.deinit(talloc);
    try testing.expectFmt(src, "{}", .{result.formatter(src)});
}

test "unclosed containers" {
    for (&[_][:0]const u8{
        "{",
        "#{",
        "(",
        "[",
        "{)",
        "{]",
        "#{)",
        "#{]",
        "(}",
        "[}",
        "{[",
        "([",
        "[{",
        "{:a 1",
        "(1 2 3",
        "[1 2 3",
        "#{1 2 3",
        "{[])",
        "([]}",
        "#tag {",
        "#_ {",
    }) |s| {
        try testing.expectError(error.UnclosedContainer, testParse(talloc, s));
    }
}

test "tagged exclude ws format" {
    try testParseOptions(talloc, "(#a 1 #b 2)", .{ .whitespace = false });
}

test "parseFromSliceAlloc demo with Diagnostic" {
    // const edn = @import("extensible-data-notation");
    const src = "a (a b c [1 2 3] {:a 1, :b 2})";
    // on error, Diagnostic line, column, and error_message will be populated.
    var diag: edn.Diagnostic = .{ .file_path = "<test-file>" };
    const result = edn.parseFromSliceAlloc(std.testing.allocator, src, .{ .diagnostic = &diag }, .{}) catch |e| {
        std.log.debug("{s}\n", .{diag.error_message});
        return e;
    };
    defer result.deinit(std.testing.allocator);
    if (!@import("builtin").is_test) { // use format helper
        std.debug.print("{}\n", .{result.formatter(src)});
    }
    try std.testing.expectFmt(src, "{}", .{result.formatter(src)});
}

test "parseFromSliceComptime demo" {
    const src = "{:eggs 2 :lemon-juice 3.5 :butter 1}";
    const result = comptime try edn.parseFromSliceComptime(src, .{}, .{ .eval_branch_quota = 2000 });
    const src2 = std.fmt.comptimePrint("{}", .{comptime result.formatter(src)});
    try std.testing.expectEqualStrings(src, src2);
}

test "parseFromSliceBuf demo - runtime no allocation" {
    const src = "{:eggs 2 :lemon-juice 3.5 :butter 1}";
    const shape = comptime try edn.measure(src, .{}, .{}); // src must be comptime known here
    var arrays: shape.Arrays() = undefined;
    const result = try edn.parseFromSliceBuf(src, shape, arrays.buffers(), .{}, .{});
    try std.testing.expectFmt(src, "{}", .{result.formatter(src)});
}

fn fuzzOne(src: [:0]const u8) !void {
    // std.debug.print("\n{s}\n{any}\n", .{ src, src });
    {
        const result = try edn.parseFromSliceAlloc(talloc, src, .{}, .{});
        defer result.deinit(talloc);
        const src1 = try std.fmt.allocPrintZ(talloc, "{}", .{result.formatter(src)});
        defer talloc.free(src1);
    }
    {
        const result = try edn.parseFromSliceAlloc(talloc, src, .{ .whitespace = false }, .{});
        defer result.deinit(talloc);
        const src1 = try std.fmt.allocPrintZ(talloc, "{}", .{result.formatter(src)});
        defer talloc.free(src1);
    }
}

// zig build test -Dtest-filters="fuzz parseFromSliceAlloc and formatter" --summary all --fuzz --port 38495
test "fuzz parseFromSliceAlloc and formatter" {
    const Context = struct {
        fn testOne(_: @This(), input: []const u8) anyerror!void {
            // std.debug.print("{s}\n", .{input});
            fuzzOne(@ptrCast(input)) catch |e| {
                if (@import("builtin").is_test and !@import("builtin").fuzz) return;
                // TODO log input to file
                std.debug.print("fuzzOne error: {s}\n", .{@errorName(e)});
                // std.debug.print("{s}\n", .{input});
                // std.debug.print("{any}\n", .{input});
                return;
            };
        }
    };
    const corpus = [_][]const u8{
        "a (a b c [1 2 3] {:a 1 :b 2})",
        "{:a 1 :b 2}",
        \\STR_ESC    #{\" \n \t \\ \r}
        ,
        \\KEYWORD    #statez/charset "[a-eg-mo-zA-Z0-9*+!_?%&=<>/.\\-]"
        ,
        \\\n       {\i {\l accept symbol_nil nil} symbol nil} ; TODO use #statez/stringset for these
        \\\t       {\r {\u {\e accept symbol_true nil} symbol nil} symbol nil}
        \\\f       {\a {\l {\s {\e accept symbol_false nil} symbol nil} symbol nil} symbol nil}
        ,
        \\{[1 2 3 4] "tell the people what she wore",
        \\ [5 6 7 8] "the more you see the more you hate"}
        ,
        \\#{:a :b 88 "huat"}
        ,
        \\#MyYelpClone/MenuItem {:name "eggs-benedict" :rating 10}
        ,
        \\(defrecord MenuItem [name rating])
        ,
        \\(clojure.edn/read-string "{:eggs 2 :butter 1 :flour 5}")
        ,
        \\42
        \\3.14159
        ,
        \\(:bun :beef-patty 9 "yum!")
        ,
    };
    try std.testing.fuzz(Context{}, Context.testOne, .{ .corpus = &corpus });
}

// zig build test -Dtest-filters="fuzz parseTypeFromSlice" --summary all --fuzz --port 38495
test "fuzz parseTypeFromSlice" {
    const Context = struct {
        fn testOne(_: @This(), input: []const u8) anyerror!void {
            const tc = edn.parseTypeFromSlice(TestCase, @ptrCast(input), .{}) catch |e| {
                if (@import("builtin").is_test) return;
                // TODO log input to file
                std.debug.print("{s}\n", .{@errorName(e)});
                std.debug.print("{s}\n", .{input});
                std.debug.print("{any}\n", .{input});
                return;
            };
            expectTestCase(tc) catch |e| {
                if (@import("builtin").is_test) return;
                // TODO log input to file
                std.debug.print("{s}\n", .{@errorName(e)});
                std.debug.print("{s}\n", .{input});
                std.debug.print("{any}\n", .{input});
                return;
            };
        }
    };
    try std.testing.fuzz(Context{}, Context.testOne, .{ .corpus = &.{TestCase_src} });
}
