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

pub const ValueIndex = struct {
    /// index into values
    index: u32,

    pub fn init(index: u32) ValueIndex {
        return .{ .index = index };
    }
};

pub const ValueIndexSet = []ValueIndex;
pub const ValueIndexMap = struct { keys: []ValueIndex, values: []ValueIndex };

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
    list: []ValueIndex,
    vector: []ValueIndex,
    map: ValueIndexMap,
    set: ValueIndexSet,

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
        list = 9,
        vector = 10,
        map = 11,
        set = 12,
    };

    pub fn deinit(value: *Value, alloc: Allocator, values: []Value) void {
        switch (value.*) {
            .true,
            .false,
            .nil,
            .string,
            .keyword,
            .symbol,
            .character,
            .integer,
            .float,
            => {},
            .list, .vector => |l| {
                for (l) |*item| values[item.index].deinit(alloc, values);
                alloc.free(l);
            },
            .map => |m| {
                deinitMap(alloc, m.keys, m.values, values);
                alloc.free(m.keys);
                alloc.free(m.values);
            },
            .set => |s| {
                deinitSet(alloc, s, values);
                alloc.free(s);
            },
        }
    }

    fn deinitMap(alloc: Allocator, ks: []ValueIndex, vs: []ValueIndex, values: []Value) void {
        for (ks, vs) |k, v| {
            values[k.index].deinit(alloc, values);
            values[v.index].deinit(alloc, values);
        }
    }
    fn deinitSet(alloc: Allocator, ks: []ValueIndex, values: []Value) void {
        for (ks) |k| {
            values[k.index].deinit(alloc, values);
        }
    }
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
                const aval = a_values[aa.index];
                const bval = b_values[bb.index];
                if (!aval.eql(a_src, a_values, bval, b_src, b_values)) return false;
            } else true,
            .character => |c| c == b.character,
            // TODO hashing doesn't guarantee uniqueness
            inline .map, .set => blk: {
                const actx = MapContext{ .src = a_src, .values = a_values };
                const ahash = actx.hashValue(a);
                const bctx = MapContext{ .src = b_src, .values = b_values };
                const bhash = bctx.hashValue(b);
                break :blk ahash == bhash;
            },
            // else => std.debug.panic("TODO {s}", .{@tagName(a)}),
        };
    }
};

pub const MapContext = struct {
    src: []const u8,
    values: []const Value,

    fn hashByIndex(self: MapContext, vi: ValueIndex, hptr: anytype) void {
        self.hashByValue(self.values[vi.index], hptr);
    }

    fn hashByValue(self: MapContext, v: Value, hptr: anytype) void {
        hptr.update(mem.asBytes(&@intFromEnum(v)));
        switch (v) {
            .nil, .true, .false => {},
            .character, .integer => |p| hptr.update(mem.asBytes(&p)),
            .float, .keyword, .symbol, .string => |token| hptr.update(token.src(self.src)),
            .vector, .list => |l| {
                hptr.update(mem.asBytes(&l.len));
                for (l) |i| self.hashByIndex(i, hptr);
            },
            // maps are equal if they have the same number of entries, and for
            // every key/value entry in one map an equal key is present and
            // mapped to an equal value in the other.
            .map => |map| {
                hptr.update(mem.asBytes(&map.keys.len));
                for (map.keys, map.values) |k, subv| {
                    self.hashByIndex(k, hptr);
                    self.hashByIndex(subv, hptr);
                }
            },
            // sets are equal if they have the same count of elements and, for
            // every element in one set, an equal element is in the other.
            .set => |keys| {
                hptr.update(mem.asBytes(&keys.len));
                for (keys) |k| self.hashByIndex(k, hptr);
            },
            // else => std.debug.panic("TODO {s}", .{@tagName(v)}),
        }
    }

    pub fn hash(self: MapContext, v: ValueIndex) u32 {
        var h = std.hash.Wyhash.init(0);
        self.hashByIndex(v, &h);
        return @truncate(h.final());
    }

    pub fn hashValue(self: MapContext, v: Value) u32 {
        var h = std.hash.Wyhash.init(0);
        self.hashByValue(v, &h);
        return @truncate(h.final());
    }

    pub fn eql(_: MapContext, _: ValueIndex, _: ValueIndex, _: usize) bool {
        unreachable;
    }
};

pub const ParseResult = struct {
    values: []Value,
    top_level_values: []ValueIndex,
    whitespace: [][2]Token,

    pub fn deinit(r: *ParseResult, alloc: Allocator) void {
        for (r.top_level_values) |v| r.values[v.index].deinit(alloc, r.values);
        alloc.free(r.values);
        alloc.free(r.top_level_values);
        alloc.free(r.whitespace);
    }
};

pub const Options = packed struct(u8) {
    whitespace: enum(u1) { include, exclude } = .include,
    mode: enum(u1) { allocate, measure } = .allocate,
    _padding: u6 = undefined,
};

pub fn parseFromSlice(
    alloc: Allocator,
    src: []const u8,
    options: Options,
) !ParseResult {
    if (options.mode == .allocate) {
        var p = Parser.init(alloc, src, options);
        errdefer p.deinit();
        try parseValues(&p);
        return .{
            .values = try p.values.toOwnedSlice(alloc),
            .top_level_values = try p.top_level_values.toOwnedSlice(alloc),
            .whitespace = try p.whitespace.toOwnedSlice(alloc),
        };
    } else {
        unreachable;
    }
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
    var cw = std.io.countingWriter(writer);
    for (data.parse_result.top_level_values) |vo| {
        try formatValue(.{
            .value = vo,
            .src = data.src,
            .parse_result = data.parse_result,
        }, fmt, options, cw.writer());
    }
    try writer.writeAll(data.src[cw.bytes_written..]);
}

pub fn fmtValue(
    value: ValueIndex,
    src: []const u8,
    parse_result: ParseResult,
) std.fmt.Formatter(formatValue) {
    return .{ .data = .{
        .value = value,
        .src = src,
        .parse_result = parse_result,
    } };
}

fn formatValue(
    data: struct {
        value: ValueIndex,
        src: []const u8,
        parse_result: ParseResult,
    },
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    const v = data.parse_result.values[data.value.index];
    if (data.value.index < data.parse_result.whitespace.len)
        try writer.writeAll(data.parse_result.whitespace[data.value.index][0].src(data.src));

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
        .list => |l| for (l) |item| {
            try std.fmt.formatType(fmtValue(item, data.src, data.parse_result), fmt, options, writer, 0);
        },
        .vector => |l| for (l) |item| {
            try std.fmt.formatType(fmtValue(item, data.src, data.parse_result), fmt, options, writer, 0);
        },
        .map => |m| for (m.keys, m.values) |k, sv| {
            try std.fmt.formatType(fmtValue(k, data.src, data.parse_result), fmt, options, writer, 0);
            try std.fmt.formatType(fmtValue(sv, data.src, data.parse_result), fmt, options, writer, 0);
        },
        .set => |keys| for (keys) |k| {
            try std.fmt.formatType(fmtValue(k, data.src, data.parse_result), fmt, options, writer, 0);
        },
    }

    if (data.value.index < data.parse_result.whitespace.len)
        try writer.writeAll(data.parse_result.whitespace[data.value.index][1].src(data.src));
}

fn err(_: *const Parser, comptime fmt: []const u8, args: anytype) ParseError {
    log.err(fmt, args);
    return error.Parse;
}

fn parseAlpha(p: *Parser, prefix: ?u8) !ValueIndex {
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

fn parseList(p: *Parser, prefix: u8) ParseError!Value {
    log.debug("{s: <[1]}parseList('{2?c}')", .{ "", p.depth * 2, prefix });
    assert(prefix == '(' or prefix == '[');
    p.depth += 1;
    defer p.depth -= 1;
    assert(p.peek(0) == prefix);
    p.index += 1;

    var result = Parser.ValueIndexList{};
    defer result.deinit(p.alloc);
    const end_char = matchingEndChar(prefix);
    while (true) {
        const c = p.peek(0) orelse return error.Eof;
        log.debug("{s: <[1]}parseList('{2?c}')", .{ "", p.depth * 2, c });
        if (c == end_char) break;

        var v, const vi = try parseValue(p);
        errdefer v.deinit(p.alloc, p.values.items);
        try result.append(p.alloc, vi);
        try p.values.append(p.alloc, v);
    }

    return if (prefix == '(')
        .{ .list = try result.toOwnedSlice(p.alloc) }
    else
        .{ .vector = try result.toOwnedSlice(p.alloc) };
}

fn parseMap(p: *Parser) ParseError!Value {
    log.debug("{s: <[1]}parseMap()", .{ "", p.depth * 2 });
    defer log.debug("{s: <[1]}<parseMap()", .{ "", p.depth * 2 });
    p.depth += 1;
    defer p.depth -= 1;
    assert(p.peek(0) == '{');
    p.index += 1;

    var result = Parser.Map(ValueIndex){};
    defer result.deinit(p.alloc);
    errdefer Value.deinitMap(p.alloc, result.keys(), result.values(), p.values.items);

    while (true) {
        const c = p.peek(0) orelse return error.Eof;
        log.debug("{s: <[1]}parseMap '{2?c}'", .{ "", p.depth * 2, c });
        if (c == '}') break;

        const gop = blk: {
            var key, const key_vi = try parseValue(p);
            errdefer key.deinit(p.alloc, p.values.items);
            try p.values.append(p.alloc, key);
            const gop = try result.getOrPutContext(p.alloc, key_vi, .{
                .src = p.src,
                .values = p.values.items,
            });
            if (gop.found_existing) return error.MapDuplicateKey;
            break :blk gop;
        };

        var value, const value_vi = try parseValue(p);
        errdefer value.deinit(p.alloc, p.values.items);
        gop.value_ptr.* = value_vi;
        try p.values.append(p.alloc, value);
    }
    const keys = try p.alloc.dupe(ValueIndex, result.keys());
    errdefer p.alloc.free(keys);
    return .{ .map = .{
        .keys = keys,
        .values = try p.alloc.dupe(ValueIndex, result.values()),
    } };
}

fn parseSet(p: *Parser) ParseError!Value {
    log.debug("{s: <[1]}parseSet()", .{ "", p.depth * 2 });
    p.depth += 1;
    defer p.depth -= 1;
    assert(p.peek(0) == '#' and p.peek(1) == '{');
    p.index += 2;

    var result = Parser.Map(void){};
    defer result.deinit(p.alloc);
    errdefer Value.deinitSet(p.alloc, result.keys(), p.values.items);

    while (true) {
        const c = p.peek(0) orelse return error.Eof;
        log.debug("{s: <[1]}parseSet '{2?c}'", .{ "", p.depth * 2, c });
        if (c == '}') break;

        var v, const vi = try parseValue(p);
        errdefer v.deinit(p.alloc, p.values.items);
        try p.values.append(p.alloc, v);
        const gop = try result.getOrPutContext(p.alloc, vi, .{
            .src = p.src,
            .values = p.values.items,
        });
        if (gop.found_existing) return error.SetDuplicateKey;
    }
    return .{ .set = try p.alloc.dupe(ValueIndex, result.keys()) };
}

// TODO
fn parseDiscard(p: *Parser) !Value {
    p.depth += 1;
    defer p.depth -= 1;
    unreachable;
}

fn parseValue(p: *Parser) !struct { Value, ValueIndex } {
    // track leading ws
    var leading_ws = Token.init(p.index, undefined);
    p.skipWsAndComments();
    leading_ws.end = p.index;

    const c = p.peek(0).?;
    log.debug("{s: <[1]}parseValue() '{2?c}'", .{ "", p.depth * 2, c });
    const v = switch (c) {
        'a'...'z', 'A'...'Z' => try parseSym(p, null),
        '0'...'9', '-' => try parseNum(p),
        '"' => try parseString(p),
        '\\' => try parseChar(p),
        ':' => try parseKeyword(p),
        '(', '[' => try parseList(p, c),
        '{' => try parseMap(p),
        '#' => if (p.peek(1)) |d| switch (d) {
            '{' => try parseSet(p),
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
        try p.whitespace.append(p.alloc, .{ leading_ws, trailing_ws });
    }

    const vi = ValueIndex.init(@intCast(p.values.items.len));
    log.debug(
        "{s: <[1]}parseValue() leadingws '{2s}' trailingws '{3s}'",
        .{ "", p.depth * 2, leading_ws.src(p.src), trailing_ws.src(p.src) },
    );
    return .{ v, vi };
}

pub fn parseValues(p: *Parser) !void {
    while (!p.eos()) {
        var v, const vi = parseValue(p) catch |e| switch (e) {
            error.Eof => break,
            else => return e,
        };
        errdefer v.deinit(p.alloc, p.values.items);
        try p.top_level_values.append(p.alloc, vi);
        try p.values.append(p.alloc, v);
    }
}

const log = std.log.scoped(.edn);
