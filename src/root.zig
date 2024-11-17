//! https://github.com/edn-format/edn

const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const assert = std.debug.assert;

const Parser = @import("Parser.zig");

pub const ParseError = error{
    Parse,
    Eof,
    CharInvalid,
    SymEmpty,
    SymInvalidFirstChar,
    SymIsDigit,
    MapDuplicateKey,
    SetDuplicateKey,
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
    integer: isize,
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
        a_values: []Value,
        b: Value,
        b_src: []const u8,
        b_values: []Value,
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
            inline .vector, .list => |payload, tag| for (payload, @field(b, @tagName(tag))) |aa, bb| {
                if (!aa.eql(a_src, a_values, bb, b_src, b_values)) break false;
            } else true,
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

pub const ParseResult = struct {
    values: []Value,
    whitespace: [][2]Token,
    top_level_values_len: u32,

    pub fn deinit(r: ParseResult, alloc: Allocator) void {
        alloc.free(r.values);
        alloc.free(r.whitespace);
    }
};

pub const Options = packed struct(u8) {
    whitespace: enum(u1) { include, exclude } = .include,
    mode: ParseMode = .allocate,
    _padding: u6 = undefined,
};

pub const ParseMode = enum(u1) { allocate, measure };

pub fn parseFromSlice(
    alloc: Allocator,
    src: []const u8,
    options: Options,
) !ParseResult {
    var p0 = Parser.init(src, options, undefined, undefined, undefined, undefined);
    try parseValues(&p0, .measure);

    if (options.mode == .measure) {
        var result: ParseResult = undefined;
        result.values.len = p0.measured.values_len;
        result.whitespace.len = p0.measured.whitespace_len;
        result.top_level_values_len = p0.measured.top_level_values_len;
        return result;
    }

    const values = try alloc.alloc(Value, p0.measured.values_len);
    errdefer alloc.free(values);
    const ws = try alloc.alloc([2]Token, p0.measured.whitespace_len);
    errdefer alloc.free(ws);
    var p = Parser.init(
        src,
        options,
        values.ptr,
        values.ptr + p0.measured.top_level_values_len,
        ws.ptr,
        ws.ptr + p0.measured.top_level_values_len,
    );
    p.measured = p0.measured;
    try parseValues(&p, .allocate);

    assert(p.values == p.values_start + p0.measured.values_len);
    if (options.whitespace == .include)
        assert(p.ws == p.ws_start + p0.measured.whitespace_len);
    assert(p.values_start == values.ptr);
    assert(p.ws_start == ws.ptr);

    return .{
        .values = p.values_start[0..p0.measured.values_len],
        .whitespace = p.ws_start[0..p0.measured.whitespace_len],
        .top_level_values_len = p0.measured.top_level_values_len,
    };
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
    for (data.parse_result.values[0..data.parse_result.top_level_values_len], 0..) |v, ws_index| {
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

    if (data.ws_index < data.parse_result.whitespace.len)
        try writer.writeAll(data.parse_result.whitespace[data.ws_index][0].src(data.src));

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

    if (data.ws_index < data.parse_result.whitespace.len)
        try writer.writeAll(data.parse_result.whitespace[data.ws_index][1].src(data.src));
}

fn err(_: *const Parser, comptime fmt: []const u8, args: anytype) ParseError {
    log.err(fmt, args);
    return error.Parse;
}

fn parseAlpha(p: *Parser, prefix: ?u8) !Value {
    log.debug("{s: <[1]}parseAlpha()", .{ "", p.depth * 2 });
    const rest = p.src[p.index..];
    if (mem.startsWith(u8, rest, "nil")) {
        p.index += 3;
        return .nil;
    } else if (mem.startsWith(u8, rest, "true")) {
        p.index += 4;
        return .true;
    } else if (mem.startsWith(u8, rest, "false")) {
        p.index += 5;
        return .false;
    } else {
        return .{ .symbol = try parseSym(p, prefix) };
    }
}

fn isSymChar(c: u8) bool {
    return switch (c) {
        'a'...'z', 'A'...'Z', '0'...'9' => true,
        '/', '.', '*', '+', '!', '-', '_', '?', '$', '%', '&', '=', '<', '>' => true,
        else => false,
    };
}

fn parseSym(p: *Parser, prefix: ?u8) !Value {
    log.debug("{s: <[1]}parseSym()", .{ "", p.depth * 2 });

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

    p.skipWhileFn(isSymChar);
    const slice = p.src[start..p.index];
    if (slice.len == 0) return error.SymEmpty;
    if (std.meta.stringToEnum(Value.Tag, slice)) |v| {
        switch (v) {
            inline .nil, .true, .false => |tag| {
                return @unionInit(Value, @tagName(tag), undefined);
            },
            else => {},
        }
    }
    switch (slice[@intFromBool(tagged)]) {
        '0'...'9' => return error.SymIsDigit,
        '-', '+', '.' => if (slice.len > 1 and std.ascii.isDigit(slice[1]))
            return error.SymIsDigit,
        '#', ':' => return error.SymInvalidFirstChar,
        '\'' => unreachable,
        else => {},
    }
    // TODO
    //
    // / has special meaning in symbols. It can be used once only in the middle of a
    // symbol to separate the prefix (often a namespace) from the name, e.g.
    // my-namespace/foo. / by itself is a legal symbol, but otherwise neither the
    // prefix nor the name part can be empty when the symbol contains /.
    //
    // If a symbol has a prefix and /, the following name component should follow the
    // first-character restrictions for symbols as a whole. This is to avoid ambiguity
    // in reading contexts where prefixes might be presumed as implicitly included
    // namespaces and elided thereafter.
    return .{ .symbol = .init(start, p.index) };
}

fn parseNum(p: *Parser) !Value {
    const start = p.index;
    p.skipWhileFn(Parser.isIntOrFloatDigit);
    const src = p.src[start..p.index];
    log.debug("{s: <[1]}parseNum() '{2s}'", .{ "", p.depth * 2, src });
    return if (mem.indexOfScalar(u8, src, '.')) |_|
        .{ .float = .init(start, p.index) }
    else
        .{ .integer = try std.fmt.parseInt(isize, src, 10) };
}

fn parseString(p: *Parser) !Value {
    log.debug("{s: <[1]}parseString()", .{ "", p.depth * 2 });
    const start = p.index;
    p.index += 1;
    // FIXME real parsing
    p.skipUntilChar('"');
    p.index += 1;
    return .{ .string = .init(start, p.index) };
}

fn parseChar(p: *Parser) !Value {
    log.debug("{s: <[1]}parseChar()", .{ "", p.depth * 2 });
    // FIXME real parsing
    assert(p.peek(0) == '\\');
    p.index += 1;
    const res: Value = .{ .character = p.next() orelse
        return error.CharInvalid };
    return res;
}

fn parseKeyword(p: *Parser) !Value {
    log.debug("{s: <[1]}parseKeyword()", .{ "", p.depth * 2 });
    assert(p.peek(0) == ':');
    const start = p.index;
    p.skipUntilWs();
    return .{ .keyword = .init(start, p.index) };
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
    log.debug("{s: <[1]}parseList('{2?c}', {3s})", .{ "", p.depth * 2, prefix, @tagName(mode) });
    assert(prefix == '(' or prefix == '[');
    p.depth += 1;
    defer p.depth -= 1;
    assert(p.peek(0) == prefix);
    p.index += 1;

    var i: u32 = 0;
    const end_char = matchingEndChar(prefix);
    while (true) : (i += 1) {
        const c = p.peek(0) orelse return error.Eof;
        log.debug("{s: <[1]}parseList('{2?c}')", .{ "", p.depth * 2, c });
        if (c == end_char) break;
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
    const map = ValueMap{ .keys = vs[0..len], .values = vs[len..] };
    p.index = index;
    p.values = p.values[len * 2 ..];
    p.ws = p.ws[len * 2 ..];
    _ = try parseMap(p, .allocate, map, .{ ws[0..len], ws[len..] });
    return map;
}

fn parseMap(p: *Parser, comptime mode: ParseMode, result: ValueMap, result_wss: [2][][2]Token) ParseError!u32 {
    log.debug("{s: <[1]}parseMap({2s})", .{ "", p.depth * 2, @tagName(mode) });
    defer log.debug("{s: <[1]}<parseMap({2s})", .{ "", p.depth * 2, @tagName(mode) });
    p.depth += 1;
    defer p.depth -= 1;
    assert(p.peek(0) == '{');
    p.index += 1;

    var i: u32 = 0;
    while (true) : (i += 1) {
        const c = p.peek(0) orelse return error.Eof;
        log.debug("{s: <[1]}parseMap '{2?c}'", .{ "", p.depth * 2, c });
        if (c == '}') break;

        if (mode == .allocate) {
            try parseValue(p, mode, result.keys[i..].ptr, result_wss[0][i..].ptr);
            try parseValue(p, mode, result.values[i..].ptr, result_wss[1][i..].ptr);
        } else {
            try parseValue(p, mode, undefined, undefined);
            try parseValue(p, mode, undefined, undefined);
        }
    }
    log.debug("{s: <[1]}parseMap done len {2}", .{ "", p.depth * 2, i });
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
    log.debug("{s: <[1]}parseSet({2s})", .{ "", p.depth * 2, @tagName(mode) });
    p.depth += 1;
    defer p.depth -= 1;
    assert(p.peek(0) == '#' and p.peek(1) == '{');
    p.index += 2;
    var i: u32 = 0;
    while (true) : (i += 1) {
        const c = p.peek(0) orelse return error.Eof;
        log.debug("{s: <[1]}parseSet '{2?c}'", .{ "", p.depth * 2, c });
        if (c == '}') break;

        if (mode == .allocate) {
            try parseValue(p, mode, result[i..].ptr, result_ws[i..].ptr);
        } else {
            try parseValue(p, mode, undefined, undefined);
        }
    }

    return i;
}

// TODO
fn parseDiscard(p: *Parser) !Value {
    p.depth += 1;
    defer p.depth -= 1;
    unreachable;
}

fn parseValue(p: *Parser, comptime mode: ParseMode, result: [*]Value, wsresult: [*][2]Token) !void {
    // track leading ws
    var leading_ws = Token.init(p.index, undefined);
    p.skipWsAndComments();
    leading_ws.end = p.index;

    const c = p.peek(0).?;
    log.debug("{s: <[1]}parseValue() '{2?c}'", .{ "", p.depth * 2, c });
    const v: Value = switch (c) {
        'a'...'z', 'A'...'Z' => try parseSym(p, null),
        '0'...'9', '-' => try parseNum(p),
        '"' => try parseString(p),
        '\\' => try parseChar(p),
        ':' => try parseKeyword(p),
        '(' => .{ .list = try parseOrMeasureList(p, c, mode) },
        '[' => .{ .vector = try parseOrMeasureList(p, c, mode) },
        '{' => .{ .map = try parseOrMeasureMap(p, mode) },
        '#' => if (p.peek(1)) |d| switch (d) {
            '{' => .{ .set = try parseOrMeasureSet(p, mode) },
            '_' => try parseDiscard(p),
            else => try parseSym(p, c),
        } else return err(p, "unexpected end of file '{c}'", .{c}),
        else => return err(p, "unexpected character '{c}'", .{c}),
    };
    // leading ws includes any container opening chars such as '(', '#{'
    const is_container = @intFromBool(@intFromEnum(v) >= @intFromEnum(Value.Tag.list));
    leading_ws.end += is_container;
    leading_ws.end += @intFromBool(v == .set);

    // trailing ws indludes any container closing chars such as ')', '}'
    var trailing_ws = Token.init(p.index, undefined);
    p.index += is_container;
    p.skipWsAndComments();
    trailing_ws.end = p.index;
    if (p.options.whitespace == .include) {
        if (mode == .measure) {
            p.measured.whitespace_len += 1;
        } else {
            wsresult[0] = .{ leading_ws, trailing_ws };
        }
    }

    log.debug(
        "{s: <[1]}parseValue({2s}) leadingws '{3s}' trailingws '{4s}'",
        .{ "", p.depth * 2, @tagName(mode), leading_ws.src(p.src), trailing_ws.src(p.src) },
    );

    if (mode == .measure) {
        p.measured.values_len += 1;
    } else {
        result[0] = v;
    }
}

pub fn parseValues(p: *Parser, comptime mode: ParseMode) !void {
    // top_level_values are stored at the beginning of value_indexes
    var top_level_vs = p.values_start[0..p.measured.top_level_values_len];
    var top_level_ws = p.ws_start[0..p.measured.top_level_values_len];
    log.debug("{s: <[1]}parseValues({2s}) {3}", .{ "", p.depth * 2, @tagName(mode), p.measured });

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
            p.measured.top_level_values_len += 1;
        }
    }
    if (mode == .allocate) assert(top_level_vs.len == 0);
}

const log = std.log.scoped(.edn);
