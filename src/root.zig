//! https://github.com/edn-format/edn

const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const assert = std.debug.assert;

pub const Parser = @import("Parser.zig");

pub const ParseError = error{
    Parse,
    Eof,
    InvalidChar,
    InvalidString,
    InvalidSymbol,
    MapDuplicateKey,
    SetDuplicateKey,
    HandlerParse,
    InvalidEscape,
    EndOfMap,
    MissingWhitespaceBetweenValues,
} ||
    Allocator.Error ||
    std.fmt.ParseIntError ||
    std.fmt.ParseFloatError;

pub const Token = struct {
    start: u32,
    end: u32,
    pub fn init(start: u32, end: u32) Token {
        return .{ .start = start, .end = end };
    }
    pub fn src(t: Token, s: []const u8) []const u8 {
        return s[t.start..t.end];
    }
    pub fn leading(t: Token, s: []const u8, offset: u16) []const u8 {
        return Token.init(t.start - offset, t.start).src(s);
    }
    pub fn trailing(t: Token, s: []const u8, offset: u16) []const u8 {
        return Token.init(t.end, t.end + offset).src(s);
    }
};

pub const ValueMap = struct {
    keys: []Value,
    values: []Value,
    pub const init: ValueMap = .{ .keys = &.{}, .values = &.{} };
};

pub const Value = union(Value.Tag) {
    nil,
    true,
    false,
    string: Token,
    character: u21,
    keyword: Token,
    symbol: Token,
    integer: i128,
    float: Token,
    list: []Value,
    vector: []Value,
    map: ValueMap,
    set: []Value,

    pub const Tag = enum(u8) {
        nil = 0,
        true = 1,
        false = 2,
        string = 3,
        character = 4,
        keyword = 5,
        symbol = 6,
        integer = 7,
        float = 8,
        // containers
        list = 9,
        vector = 10,
        map = 11,
        set = 12,
    };

    pub fn eql(
        a: Value,
        a_src: []const u8,
        a_values: []const Value,
        b: Value,
        b_src: []const u8,
        b_values: []const Value,
    ) bool {
        return a == std.meta.activeTag(b) and
            switch (a) {
            .nil, .true, .false => true,
            .integer => a.integer == b.integer,
            inline .float, .keyword, .symbol, .string => |payload, tag| mem.eql(
                u8,
                payload.src(a_src),
                @field(b, @tagName(tag)).src(b_src),
            ),
            inline .vector, .list => |as, tag| blk: {
                const bs = @field(b, @tagName(tag));
                break :blk as.len == bs.len and for (as, bs) |aa, bb| {
                    if (!aa.eql(a_src, a_values, bb, b_src, b_values)) break false;
                } else true;
            },
            .character => |c| c == b.character,
            // TODO hashing doesn't guarantee uniqueness
            inline .map, .set => blk: {
                const actx = MapContext{ .src = a_src, .values = a_values };
                const ahash = actx.hash(a);
                const bctx = MapContext{ .src = b_src, .values = b_values };
                const bhash = bctx.hash(b);
                break :blk ahash == bhash;
            },
            // else => std.debug.panic("TODO {s}", .{@tagName(a)}),
        };
    }
};

pub const MapContext = struct {
    src: []const u8,
    values: []const Value,

    fn hashValue(self: MapContext, v: Value, hptr: anytype) void {
        hptr.update(mem.asBytes(&@intFromEnum(v)));
        switch (v) {
            .nil, .true, .false => {},
            .character, .integer => |p| hptr.update(mem.asBytes(&p)),
            .float, .keyword, .symbol, .string => |token| hptr.update(token.src(self.src)),
            .vector, .list => |l| {
                hptr.update(mem.asBytes(&l.len));
                for (l) |i| self.hashValue(i, hptr);
            },
            // maps are equal if they have the same number of entries, and for
            // every key/value entry in one map an equal key is present and
            // mapped to an equal value in the other.
            .map => |map| {
                hptr.update(mem.asBytes(&map.keys.len));
                for (map.keys, map.values) |k, subv| {
                    self.hashValue(k, hptr);
                    self.hashValue(subv, hptr);
                }
            },
            // sets are equal if they have the same count of elements and, for
            // every element in one set, an equal element is in the other.
            .set => |keys| {
                hptr.update(mem.asBytes(&keys.len));
                for (keys) |k| self.hashValue(k, hptr);
            },
            // else => std.debug.panic("TODO {s}", .{@tagName(v)}),
        }
    }

    pub fn hash(self: MapContext, v: Value) u32 {
        var h = std.hash.Wyhash.init(0);
        self.hashValue(v, &h);
        return @truncate(h.final());
    }

    pub fn eql(_: MapContext, _: Value, _: Value, _: usize) bool {
        unreachable;
    }
};

pub const TaggedElementHandler = struct {
    pub const Fn = *const fn (
        value: Value,
        value_src: []const u8,
        src: []const u8,
    ) ParseError!Value;

    /// initialization format: {tag_name, handler_fn}.
    /// tag_name must include leading '#'.
    pub const Data = struct { []const u8, Fn };

    pub const Map = std.StaticStringMap(Fn);
};

pub const ParseResult = struct {
    values: []const Value,
    whitespaces: []const [2]Token,
    top_level_values: u32,

    pub fn deinit(r: ParseResult, alloc: Allocator) void {
        alloc.free(r.values);
        alloc.free(r.whitespaces);
    }

    fn getList(list: []Value, n: u32) !Value {
        if (n >= list.len) return error.PathNotFound;
        return list[n];
    }

    /// path: search components joined by double slashes ('//'). i.e. '0//1//foo'
    pub fn find(r: ParseResult, path: []const u8, src: []const u8) !?Value {
        var it = mem.splitSequence(u8, path, "//");
        var cur: Value = .{ .list = @constCast(r.values[0..r.top_level_values]) };
        component: while (it.next()) |component| {
            switch (cur) {
                .map => |map| for (map.keys, 0..) |k, i| {
                    switch (k) {
                        .string,
                        .keyword,
                        .symbol,
                        .float,
                        => |t| if (mem.eql(u8, component, t.src(src))) {
                            cur = map.values[i];
                            continue :component;
                        },
                        .integer => |n| {
                            var buf: [std.math.log10(std.math.maxInt(i128)) + 1]u8 = undefined;
                            const s = try std.fmt.bufPrint(&buf, "{}", .{n});
                            if (mem.eql(u8, component, s)) {
                                cur = map.values[i];
                                continue :component;
                            }
                        },
                        .character => |n| {
                            var buf: [std.math.log10(std.math.maxInt(u21)) + 1]u8 = undefined;
                            const s = try std.fmt.bufPrint(&buf, "\\{u}", .{n});
                            if (mem.eql(u8, component, s)) {
                                cur = map.values[i];
                                continue :component;
                            }
                        },
                        else => {},
                    }
                },
                else => {},
            }

            if (std.fmt.parseUnsigned(u32, component, 10)) |n| {
                switch (cur) {
                    .list => cur = try getList(cur.list, n),
                    .vector => cur = try getList(cur.vector, n),
                    .set => cur = try getList(cur.set, n),
                    .map => cur = try getList(cur.map.values, n),
                    else => return error.PathNotFound,
                }
                continue :component;
            } else |_| {}

            return error.PathNotFound;
        }
        return cur;
    }
};

pub const Options = packed struct(u8) {
    whitespace: enum(u1) { include, exclude } = .include,
    mode: ParseMode = .allocate,
    _padding: u6 = undefined,
};

pub const ParseMode = enum(u1) { allocate, measure };

pub const Measured = struct {
    values: u32 = 0,
    whitespaces: u32 = 0,
    top_level_values: u32 = 0,
};

pub fn measure(src: []const u8, options: Options, comptime comptime_options: ComptimeOptions) !Measured {
    var opts = options;
    opts.mode = .measure;
    var p = Parser.init(src, opts, undefined, undefined, undefined, undefined, comptime_options.handlers);
    try parseValues(&p, .measure);
    return p.measured;
}

pub fn parseFromSliceBuf(
    src: []const u8,
    options: Options,
    values: []Value,
    ws: [][2]Token,
    measured: Measured,
    comptime comptime_options: ComptimeOptions,
) !ParseResult {
    var p = Parser.init(
        src,
        options,
        values.ptr,
        values.ptr + measured.top_level_values,
        ws.ptr,
        ws.ptr + measured.top_level_values,
        comptime_options.handlers,
    );
    p.measured = measured;
    try parseValues(&p, .allocate);

    assert(p.values == p.values_start + measured.values);
    if (options.whitespace == .include) assert(p.ws == p.ws_start + measured.whitespaces);
    assert(p.values_start == values.ptr);
    assert(p.ws_start == ws.ptr);

    return .{
        .values = p.values_start[0..measured.values],
        .whitespaces = p.ws_start[0..measured.whitespaces],
        .top_level_values = measured.top_level_values,
    };
}

pub fn parseFromSliceAlloc(
    alloc: Allocator,
    src: []const u8,
    options: Options,
    comptime comptime_options: ComptimeOptions,
) !ParseResult {
    const measured = try measure(src, options, comptime_options);
    if (options.mode == .measure) {
        var r: ParseResult = undefined;
        r.values.len = measured.values;
        r.whitespaces.len = measured.whitespaces;
        r.top_level_values = measured.top_level_values;
        return r;
    }

    const values = try alloc.alloc(Value, measured.values);
    errdefer alloc.free(values);
    const ws = try alloc.alloc([2]Token, measured.whitespaces);
    errdefer alloc.free(ws);

    return parseFromSliceBuf(src, options, values, ws, measured, comptime_options);
}

pub const ComptimeOptions = struct {
    eval_branch_quota: u32 = 1000,
    handlers: []const TaggedElementHandler.Data = &.{},
};

pub inline fn parseFromSliceComptime(
    comptime src: []const u8,
    comptime options: Options,
    comptime comptime_options: ComptimeOptions,
) !ParseResult {
    comptime {
        @setEvalBranchQuota(comptime_options.eval_branch_quota);
        const measured = try measure(src, options, comptime_options);
        var values: [measured.values]Value = undefined;
        var ws: [measured.whitespaces][2]Token = undefined;
        return parseFromSliceBuf(src, options, &values, &ws, measured, comptime_options);
    }
}

// TODO support tagged element handlers
pub fn parseTypeFromSlice(
    comptime T: type,
    src: []const u8,
) !T {
    var p = Parser.init(src, .{}, undefined, undefined, undefined, undefined, &.{});
    const t = try parseType(T, &p);
    p.skipWsAndComments();
    if (!p.eos()) return error.InvalidType;
    return t;
}

inline fn unsupportedType(comptime T: type) noreturn {
    @compileError("unsupported type '" ++ @typeName(T) ++ "'");
}

fn parseType(
    comptime T: type,
    p: *Parser,
) !T {
    // TODO check for user defined ednParse()
    return switch (@typeInfo(T)) {
        .@"struct" => parseStruct(T, p),
        .@"union" => u: {
            const Fe = std.meta.FieldEnum(T);
            if (p.peek(0) != '{') return error.InvalidUnion;
            p.index += 1;
            p.skipWsAndComments();
            const c = p.peek(0) orelse return error.InvalidUnion;
            const field = switch (c) {
                ':' => blk: {
                    const key = try parseKeyword(p);
                    break :blk std.meta.stringToEnum(Fe, key.keyword.src(p.src)[1..]) orelse
                        return error.InvalidStructField;
                },
                else => {
                    err(p, "unexpected '{c}'", .{c}) catch {};
                    return error.InvalidUnion;
                },
            };
            debug("{s: <[1]}parseType union key {2s}", .{ "", p.depth * 2, @tagName(field) });
            p.skipWsAndComments();
            const u = switch (field) {
                inline else => |tag| @unionInit(
                    T,
                    @tagName(tag),
                    try parseType(std.meta.FieldType(T, tag), p),
                ),
            };
            if (p.peek(0) != '}') return error.InvalidUnion;
            p.index += 1;
            break :u u;
        },
        .bool => blk: {
            const rest = p.src[p.index..];
            if (mem.startsWith(u8, rest, "true")) {
                p.index += 4;
                break :blk true;
            } else if (mem.startsWith(u8, rest, "false")) {
                p.index += 5;
                break :blk false;
            } else break :blk error.InvaidBool;
        },
        .int => blk: {
            const n = try parseNum(p);
            if (n != .integer) return error.InvalidType;
            break :blk std.math.cast(T, n.integer) orelse error.Overflow;
        },
        .float => blk: {
            const n = try parseNum(p);
            break :blk switch (n) {
                .integer => break :blk @floatFromInt(n.integer),
                .float => break :blk std.fmt.parseFloat(T, n.float.src(p.src)),
                else => return error.InvalidType,
            };
        },
        .pointer => |i| blk: switch (i.size) {
            .Slice => switch (i.child) {
                u8 => {
                    const s = try parseString(p);
                    break :blk s.string.src(p.src);
                },
                else => unsupportedType(T),
            },
            else => unsupportedType(T),
        },
        .optional => |i| blk: {
            const rest = p.src[p.index..];
            if (mem.startsWith(u8, rest, "nil")) {
                p.index += 3;
                break :blk null;
            }
            break :blk try parseType(i.child, p);
        },
        .@"enum" => blk: {
            const key = try parseKeyword(p);
            debug("enum key {s}", .{key.keyword.src(p.src)});
            break :blk std.meta.stringToEnum(T, key.keyword.src(p.src)[1..]) orelse
                error.InvalidEnum;
        },
        inline .array, .vector => |i| blk: {
            var a: [i.len]i.child = undefined;
            p.skipWsAndComments();
            if (p.peek(0) != '[') return error.InvalidArray;
            p.index += 1;
            for (&a) |*ele| {
                p.skipWsAndComments();
                ele.* = try parseType(i.child, p);
            }
            p.skipWsAndComments();
            if (p.peek(0) != ']') return error.InvalidArray;
            p.index += 1;
            break :blk a;
        },
        else => unsupportedType(T),
    };
}

fn parseStruct(
    comptime T: type,
    p: *Parser,
) !T {
    const Fe = std.meta.FieldEnum(T);
    var seen_fields = std.enums.EnumSet(Fe).initEmpty();

    debug("{s: <[1]}parseStruct({2s})", .{ "", p.depth * 2, @typeName(T) });
    defer debug("{s: <[1]}<parseStruct({2s})", .{ "", p.depth * 2, @typeName(T) });
    p.depth += 1;
    defer p.depth -= 1;
    if (p.peek(0) != '{') return error.InvalidStruct;
    p.index += 1;
    var t: T = undefined;

    while (true) {
        p.skipWsAndComments();
        const c = p.peek(0) orelse return error.Eof;
        debug("{s: <[1]}parseStruct '{2?c}'", .{ "", p.depth * 2, c });
        if (c == '}') {
            p.index += 1;
            break;
        }

        const field = switch (c) {
            ':' => blk: {
                const key = try parseKeyword(p);
                break :blk std.meta.stringToEnum(Fe, key.keyword.src(p.src)[1..]) orelse
                    return error.InvalidStructField;
            },
            else => {
                err(p, "unexpected '{c}'", .{c}) catch {};
                return error.InvalidStruct;
            },
        };
        debug("{s: <[1]}parseStruct key {2s}", .{ "", p.depth * 2, @tagName(field) });
        p.skipWsAndComments();
        switch (field) {
            inline else => |tag| {
                @field(t, @tagName(tag)) = try parseType(std.meta.FieldType(T, tag), p);
            },
        }
        seen_fields.insert(field);
    }

    var unseen_iter = seen_fields.complement().iterator();
    while (unseen_iter.next()) |field| switch (field) {
        inline else => |tag| {
            const info = std.meta.fieldInfo(T, tag);
            if (info.default_value) |default| {
                const d: *const info.type = @ptrCast(default);
                @field(t, @tagName(tag)) = d.*;
                seen_fields.insert(tag);
            } else {
                err(p, "missing field '{s}'", .{@tagName(tag)}) catch {};
            }
        },
    };
    if (seen_fields.complement().count() != 0) return error.MissingFields;
    return t;
}

pub fn fmtParseResult(
    parse_result: ParseResult,
    src: []const u8,
) std.fmt.Formatter(formatParseResult) {
    return .{ .data = .{ .src = src, .parse_result = parse_result } };
}

fn formatParseResult(
    data: struct { src: []const u8, parse_result: ParseResult },
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    for (data.parse_result.values[0..data.parse_result.top_level_values], 0..) |v, ws_index| {
        try formatValue(.{
            .value = v,
            .ws_index = @intCast(ws_index),
            .src = data.src,
            .parse_result = data.parse_result,
        }, fmt, options, writer);
    }
}

pub fn fmtValue(
    value: Value,
    ws_index: u32,
    src: []const u8,
    parse_result: ParseResult,
) std.fmt.Formatter(formatValue) {
    return .{ .data = .{
        .value = value,
        .ws_index = ws_index,
        .src = src,
        .parse_result = parse_result,
    } };
}

fn formatValue(
    data: struct {
        value: Value,
        /// the offset of value within parse_result.values
        ws_index: u32,
        src: []const u8,
        parse_result: ParseResult,
    },
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    const v = data.value;

    if (data.ws_index < data.parse_result.whitespaces.len)
        try writer.writeAll(data.parse_result.whitespaces[data.ws_index][0].src(data.src));

    switch (v) {
        .true,
        .false,
        .nil,
        => try writer.writeAll(@tagName(v)),
        .string,
        .keyword,
        .symbol,
        .float,
        => |t| try writer.writeAll(t.src(data.src)),
        .character => |c| try writer.print("\\{u}", .{c}),
        .integer => |x| try std.fmt.formatType(x, fmt, options, writer, 0),
        .list => |l| for (l) |*item| {
            const index: u32 = @intCast(item - data.parse_result.values.ptr);
            try std.fmt.formatType(fmtValue(item.*, index, data.src, data.parse_result), fmt, options, writer, 0);
        },
        .vector => |l| for (l) |*item| {
            const index: u32 = @intCast(item - data.parse_result.values.ptr);
            try std.fmt.formatType(fmtValue(item.*, index, data.src, data.parse_result), fmt, options, writer, 0);
        },
        .map => |m| for (m.keys, m.values) |*k, *sv| {
            const kindex: u32 = @intCast(k - data.parse_result.values.ptr);
            try std.fmt.formatType(fmtValue(k.*, kindex, data.src, data.parse_result), fmt, options, writer, 0);
            const vindex: u32 = @intCast(sv - data.parse_result.values.ptr);
            try std.fmt.formatType(fmtValue(sv.*, vindex, data.src, data.parse_result), fmt, options, writer, 0);
        },
        .set => |keys| for (keys) |*k| {
            const index: u32 = @intCast(k - data.parse_result.values.ptr);
            try std.fmt.formatType(fmtValue(k.*, index, data.src, data.parse_result), fmt, options, writer, 0);
        },
    }

    if (data.ws_index < data.parse_result.whitespaces.len)
        try writer.writeAll(data.parse_result.whitespaces[data.ws_index][1].src(data.src));
}

fn err(_: *const Parser, comptime fmt: []const u8, args: anytype) ParseError {
    if (@inComptime()) {
        @compileError(std.fmt.comptimePrint(fmt, args));
    } else if (!@import("builtin").is_test) log.err(fmt, args);
    return error.Parse;
}

fn isSymChar(c: u8) bool {
    return switch (c) {
        'a'...'z', 'A'...'Z', '0'...'9' => true,
        '/', '.', '*', '+', '!', '-', '_', '?', '$', '%', '&', '=', '<', '>', '#', ':' => true,
        else => false,
    };
}

fn validateSym(slice: []const u8) !void {
    switch (slice[0]) {
        '0'...'9' => return error.InvalidSymbol,
        '-', '+', '.' => if (slice.len > 1 and std.ascii.isDigit(slice[1]))
            return error.InvalidSymbol,
        '#', ':' => return error.InvalidSymbol,
        // '\'' => unreachable,
        else => {},
    }
}

fn parseSym(p: *Parser, prefix: ?u8) !Value {
    debug("{s: <[1]}parseSym()", .{ "", p.depth * 2 });

    const tagged = prefix == '#';
    const start = p.index;
    p.index += @intFromBool(tagged);
    // Symbols are used to represent identifiers, and should map to something
    // other than strings, if possible.
    //
    // Symbols begin with a non-numeric character and can contain alphanumeric
    // characters and . * + ! - _ ? $ % & = < >. If -, + or . are the first character,
    // the second character (if any) must be non-numeric. Additionally, : # are
    // allowed as constituent characters in symbols other than as the first
    // character.
    //

    // / has special meaning in symbols. It can be used once only in the middle of a
    // symbol to separate the prefix (often a namespace) from the name, e.g.
    // my-namespace/foo. / by itself is a legal symbol, but otherwise neither the
    // prefix nor the name part can be empty when the symbol contains /.
    var slash_index: ?u32 = null;
    while (p.peek(0)) |c| {
        if (!isSymChar(c)) break;
        if (c == '/') {
            if (slash_index != null) return error.InvalidSymbol;
            slash_index = p.index - start;
        }
        p.index += 1;
    }

    const slice = p.src[start..p.index];
    if (slice.len == 0) return error.InvalidSymbol;

    if (std.meta.stringToEnum(Value.Tag, slice)) |v| {
        switch (v) {
            inline .nil, .true, .false => |tag| {
                return @unionInit(Value, @tagName(tag), undefined);
            },
            else => {},
        }
    }

    if (slash_index) |i| blk: {
        assert(slice[i] == '/');
        if (slice.len == 1) break :blk;
        if (i == 0 or i + 1 == slice.len) return error.InvalidSymbol;
        try validateSym(slice[0..i][@intFromBool(tagged)..]);
        // If a symbol has a prefix and /, the following name component should follow the
        // first-character restrictions for symbols as a whole. This is to avoid ambiguity
        // in reading contexts where prefixes might be presumed as implicitly included
        // namespaces and elided thereafter.
        if (i + 1 == slice.len) return error.InvalidSymbol;
        try validateSym(slice[i + 1 ..]);
    } else try validateSym(slice[@intFromBool(tagged)..]);
    return .{ .symbol = .init(start, p.index) };
}

fn parseNum(p: *Parser) !Value {
    const start = p.index;
    p.skipWhileFn(Parser.isIntOrFloatDigit);
    const src = p.src[start..p.index];
    debug("{s: <[1]}parseNum() '{2s}'", .{ "", p.depth * 2, src });
    return if (mem.indexOfScalar(u8, src, '.')) |_|
        .{ .float = .init(start, p.index) }
    else
        .{ .integer = try std.fmt.parseInt(i128, src, 10) };
}

fn parseString(p: *Parser) !Value {
    debug("{s: <[1]}parseString()", .{ "", p.depth * 2 });
    const start = p.index;
    p.index += 1;
    while (true) : (p.index += 1) {
        const c = p.peek(0) orelse return error.InvalidString;
        switch (c) {
            '"' => break,
            '\\' => {
                const c2 = p.peek(1) orelse return error.InvalidEscape;
                switch (c2) {
                    'n', 'r', 't', '\\', '"' => p.index += 1,
                    else => return error.InvalidEscape,
                }
            },
            else => {},
        }
    }
    p.index += 1;
    return .{ .string = .init(start, p.index) };
}

// Characters are preceded by a backslash: \c, \newline, \return, \space
// and \tab yield the corresponding characters. Unicode characters are
// represented with \uNNNN as in Java. Backslash cannot be followed by
// whitespace.
fn parseChar(p: *Parser) !Value {
    debug("{s: <[1]}parseChar()", .{ "", p.depth * 2 });
    assert(p.peek(0) == '\\');
    p.index += 1;

    const rest = p.src[p.index..];
    inline for ([_]struct { []const u8, u21 }{
        .{ "space", ' ' },
        .{ "newline", '\n' },
        .{ "return", '\r' },
        .{ "tab", '\t' },
        .{ "\\", '\\' },
    }) |char_escape| {
        if (std.mem.startsWith(u8, rest, char_escape[0])) {
            p.index += char_escape[0].len;
            return .{ .character = char_escape[1] };
        }
    }

    if (p.peek(0) == 'u') {
        p.index += 1;
        const start = p.index;
        const end = @min(start + 5, p.src.len);
        while (p.index < end) : (p.index += 1) {
            switch (p.src[p.index]) {
                'a'...'f', 'A'...'F', '0'...'9' => {},
                else => break,
            }
        }
        if (p.index == start) return .{ .character = 'u' };
        const c = try std.fmt.parseUnsigned(u21, p.src[start..p.index], 16);
        return .{ .character = c };
    }

    return .{ .character = p.next() orelse return error.InvalidChar };
}

// Keywords are identifiers that typically designate themselves. They are
// semantically akin to enumeration values. Keywords follow the rules of
// symbols, except they can (and must) begin with :, e.g. :fred or :my/fred.
fn parseKeyword(p: *Parser) !Value {
    debug("{s: <[1]}parseKeyword()", .{ "", p.depth * 2 });
    assert(p.peek(0) == ':');
    const start = p.index;
    p.index += 1;
    var sym = try parseSym(p, null);
    sym.symbol.start = start;
    return .{ .keyword = sym.symbol };
}

fn matchingEndChar(c: u8) u8 {
    return switch (c) {
        '(' => ')',
        '[' => ']',
        '{' => '}',
        else => unreachable,
    };
}

fn parseOrMeasureList(p: *Parser, c: u8, comptime mode: ParseMode) ![]Value {
    const index = p.index;
    const len = try parseList(p, c, .measure, undefined, undefined);
    if (mode == .measure) {
        var measured: []Value = undefined;
        measured.len = len;
        return measured;
    }
    // allocate space for values and whitespace
    const vs = p.values[0..len];
    const ws = p.ws[0..len];
    p.index = index;
    p.values = p.values[len..];
    p.ws = p.ws[len..];
    _ = try parseList(p, c, .allocate, vs, ws);
    return vs;
}

fn parseList(
    p: *Parser,
    prefix: u8,
    comptime mode: ParseMode,
    result: []Value,
    result_ws: [][2]Token,
) ParseError!u32 {
    debug("{s: <[1]}parseList('{2?c}', {3s})", .{ "", p.depth * 2, prefix, @tagName(mode) });
    assert(prefix == '(' or prefix == '[');
    p.depth += 1;
    defer p.depth -= 1;
    assert(p.peek(0) == prefix);
    p.index += 1;

    var i: u32 = 0;
    const end_char = matchingEndChar(prefix);
    while (true) : (i += 1) {
        var next_non_ws = p.index;
        const c = while (next_non_ws < p.src.len) : (next_non_ws += 1) {
            if (!p.isWs(next_non_ws)) break p.src[next_non_ws];
        } else return error.Eof;
        debug("{s: <[1]}parseList('{2?c}')", .{ "", p.depth * 2, c });
        if (c == end_char) {
            p.index = next_non_ws;
            break;
        }
        if (mode == .allocate) {
            try parseValue(p, mode, result[i..].ptr, result_ws[i..].ptr);
        } else {
            try parseValue(p, mode, undefined, undefined);
        }
    }

    return i;
}

fn parseOrMeasureMap(p: *Parser, comptime mode: ParseMode) !ValueMap {
    const index = p.index;
    const len = try parseMap(p, .measure, undefined, undefined);
    if (mode == .measure) {
        var measured: ValueMap = undefined;
        measured.keys.len = len;
        measured.values.len = len;
        return measured;
    }

    const vs = p.values[0 .. len * 2];
    const ws = p.ws[0 .. len * 2];
    const map: ValueMap = .{ .keys = vs[0..len], .values = vs[len..] };
    p.index = index;
    p.values = p.values[len * 2 ..];
    p.ws = p.ws[len * 2 ..];
    _ = try parseMap(p, .allocate, map, .{ ws[0..len], ws[len..] });
    return map;
}

fn parseMap(p: *Parser, comptime mode: ParseMode, result: ValueMap, result_wss: [2][][2]Token) ParseError!u32 {
    debug("{s: <[1]}parseMap({2s})", .{ "", p.depth * 2, @tagName(mode) });
    defer debug("{s: <[1]}<parseMap({2s})", .{ "", p.depth * 2, @tagName(mode) });
    p.depth += 1;
    defer p.depth -= 1;
    assert(p.peek(0) == '{');
    p.index += 1;

    var i: u32 = 0;
    while (true) : (i += 1) {
        var next_non_ws = p.index;
        const c = while (next_non_ws < p.src.len) : (next_non_ws += 1) {
            if (!p.isWs(next_non_ws)) break p.src[next_non_ws];
        } else return error.Eof;
        debug("{s: <[1]}parseMap '{2?c}'", .{ "", p.depth * 2, c });
        if (c == '}') {
            p.index = next_non_ws;
            break;
        }

        if (mode == .allocate) {
            parseValue(p, mode, result.keys[i..].ptr, result_wss[0][i..].ptr) catch |e| switch (e) {
                error.EndOfMap => break,
                else => return e,
            };
            try parseValue(p, mode, result.values[i..].ptr, result_wss[1][i..].ptr);
        } else {
            parseValue(p, mode, undefined, undefined) catch |e| switch (e) {
                error.EndOfMap => break,
                else => return e,
            };
            try parseValue(p, mode, undefined, undefined);
        }
    }
    debug("{s: <[1]}parseMap done len {2}", .{ "", p.depth * 2, i });
    return i;
}

fn parseOrMeasureSet(p: *Parser, comptime mode: ParseMode) ![]Value {
    const index = p.index;
    const len = try parseSet(p, .measure, undefined, undefined);
    if (mode == .measure) {
        var measured: []Value = &.{};
        measured.len = len;
        return measured;
    }

    const vs = p.values[0..len];
    const ws = p.ws[0..len];
    p.index = index;
    p.values = p.values[len..];
    p.ws = p.ws[len..];
    _ = try parseSet(p, .allocate, vs, ws);
    return vs;
}

fn parseSet(
    p: *Parser,
    comptime mode: ParseMode,
    result: []Value,
    result_ws: [][2]Token,
) ParseError!u32 {
    debug("{s: <[1]}parseSet({2s})", .{ "", p.depth * 2, @tagName(mode) });
    p.depth += 1;
    defer p.depth -= 1;
    assert(p.peek(0) == '#' and p.peek(1) == '{');
    p.index += 2;
    var i: u32 = 0;
    while (true) : (i += 1) {
        var next_non_ws = p.index;
        const c = while (next_non_ws < p.src.len) : (next_non_ws += 1) {
            if (!p.isWs(next_non_ws)) break p.src[next_non_ws];
        } else return error.Eof;
        debug("{s: <[1]}parseSet '{2?c}'", .{ "", p.depth * 2, c });
        if (c == '}') {
            p.index = next_non_ws;
            break;
        }

        if (mode == .allocate) {
            try parseValue(p, mode, result[i..].ptr, result_ws[i..].ptr);
        } else {
            try parseValue(p, mode, undefined, undefined);
        }
    }

    return i;
}

fn parseDiscard(p: *Parser, comptime mode: ParseMode, leading_ws: *Token) ParseError!Value {
    p.depth += 1;
    defer p.depth -= 1;
    assert(p.peek(0) == '#');
    assert(p.peek(1) == '_');
    p.index += 2;
    p.skipWsAndComments();
    const ignored = try parseValueInner(p, mode, leading_ws);
    debug("{s: <[1]}parseDiscard() ignored {2s}", .{ "", p.depth * 2, @tagName(ignored) });
    const is_container = @intFromBool(@intFromEnum(ignored) >= @intFromEnum(Value.Tag.list));
    p.index += is_container;

    p.skipWsAndComments();
    leading_ws.end = p.index;
    var next_dummy_leading_ws = Token.init(p.index, undefined);
    if (p.eos()) return error.Eof;
    debug("{s: <[1]}parseDiscard() no eos", .{ "", p.depth * 2 });
    const v = try parseValueInner(p, mode, &next_dummy_leading_ws);
    return v;
}

fn parseValueInner(p: *Parser, comptime mode: ParseMode, leading_ws: *Token) !Value {
    const c = p.peek(0).?;
    debug("{s: <[1]}parseValueInner() '{2?c}'", .{ "", p.depth * 2, c });
    return switch (c) {
        '0'...'9', '-', '+' => try parseNum(p),
        '"' => try parseString(p),
        '\\' => try parseChar(p),
        ':' => try parseKeyword(p),
        '(' => .{ .list = try parseOrMeasureList(p, c, mode) },
        '[' => .{ .vector = try parseOrMeasureList(p, c, mode) },
        '{' => .{ .map = try parseOrMeasureMap(p, mode) },
        '}' => return error.EndOfMap,
        '#' => if (p.peek(1)) |d| switch (d) {
            '{' => .{ .set = try parseOrMeasureSet(p, mode) },
            '_' => try parseDiscard(p, mode, leading_ws),
            else => blk: {
                const sym = try parseSym(p, c);
                // Upon encountering a tag, the reader will first read the
                // next element (which may itself be or comprise other
                // tagged elements), then pass the result to the
                // corresponding handler for further interpretation, and
                // the result of the handler will be the data value yielded
                // by the tag + tagged element, i.e. reading a tag and
                // tagged element yields one value.

                // If a reader encounters a tag for which no handler is
                // registered, the implementation can either report an error,
                // call a designated 'unknown element' handler, or create a
                // well-known generic representation that contains both the
                // tag and the tagged element, as it sees fit. Note that the
                // non-error strategies allow for readers which are capable of
                // reading any and all edn, in spite of being unaware of the
                // details of any extensions present.

                // Current strategy is to ignore the tagged symbol, adding it
                // to leading ws and fall back on default value parsing when
                // no handler is present.
                p.skipWsAndComments();
                leading_ws.end = p.index;
                const next_start = p.index;
                var next_dummy_leading_ws = Token.init(p.index, undefined);
                const next = try parseValueInner(p, mode, &next_dummy_leading_ws);
                const v = if (p.handlers.get(sym.symbol.src(p.src))) |handler| v: {
                    const next_end = p.index;
                    const v = try handler(next, p.src[next_start..next_end], p.src);
                    break :v v;
                } else v: {
                    break :v next;
                };
                break :blk v;
            },
        } else return err(p, "unexpected end of file '{c}'", .{c}),
        else => return if (isSymChar(c))
            try parseSym(p, null)
        else
            err(p, "unexpected character '{c}'", .{c}),
    };
}
fn parseValue(
    p: *Parser,
    comptime mode: ParseMode,
    result: [*]Value,
    whitespace_result: [*][2]Token,
) ParseError!void {
    // track leading ws
    var leading_ws = Token.init(p.index, undefined);
    p.skipWsAndComments();
    leading_ws.end = p.index;
    if (p.last_value_end == p.index) return error.MissingWhitespaceBetweenValues;

    const v = try parseValueInner(p, mode, &leading_ws);

    // leading ws includes any container opening chars such as '(', '#{'
    const is_container = @intFromBool(@intFromEnum(v) >= @intFromEnum(Value.Tag.list));
    leading_ws.end += is_container;
    leading_ws.end += @intFromBool(v == .set);

    // trailing ws indludes any container closing chars such as ')', '}'
    var trailing_ws = Token.init(p.index, undefined);
    p.index += is_container;
    p.last_value_end = p.index;
    p.skipWsAndComments();
    trailing_ws.end = p.index;
    if (p.options.whitespace == .include) {
        if (mode == .measure) {
            p.measured.whitespaces += 1;
        } else {
            whitespace_result[0] = .{ leading_ws, trailing_ws };
        }
    }

    debug(
        "{s: <[1]}parseValue({2s}) leadingws '{3s}' trailingws '{4s}'",
        .{ "", p.depth * 2, @tagName(mode), leading_ws.src(p.src), trailing_ws.src(p.src) },
    );

    if (mode == .measure) {
        p.measured.values += 1;
    } else {
        result[0] = v;
    }
}

fn parseValues(p: *Parser, comptime mode: ParseMode) !void {
    // top level values are stored at the beginning of values/whitespaces
    var top_level_vs: []Value = p.values_start[0..p.measured.top_level_values];
    var top_level_ws: [][2]Token = p.ws_start[0..p.measured.top_level_values];
    debug("{s: <[1]}parseValues({2s}) {3}", .{ "", p.depth * 2, @tagName(mode), p.measured });

    var i: u32 = 0;
    while (!p.eos()) : (i += 1) {
        if (mode == .allocate) {
            parseValue(p, mode, top_level_vs.ptr, top_level_ws.ptr) catch |e| switch (e) {
                error.Eof => break,
                else => return e,
            };
            top_level_vs = top_level_vs[1..];
            top_level_ws = top_level_ws[1..];
        } else {
            parseValue(p, mode, undefined, undefined) catch |e| switch (e) {
                error.Eof => break,
                else => return e,
            };
            p.measured.top_level_values += 1;
        }
    }

    if (mode == .allocate) {
        assert(top_level_vs.len == 0);
        assert(top_level_ws.len == 0);
    }
}

const log = std.log.scoped(.edn);

fn debug(comptime fmt: []const u8, args: anytype) void {
    if (!@inComptime())
        log.debug(fmt, args);
}
