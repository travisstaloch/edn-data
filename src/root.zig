//! https://github.com/edn-format/edn

pub const ParseError = error{
    Parse,
    Eof,
    InvalidToken,
    InvalidChar,
    InvalidString,
    HandlerParse,
    MissingHandler,
    EndOfMap,
    MissingWhitespaceBetweenValues,
    InvalidStruct,
    InvalidStructField,
    MissingFields,
    InvalidUnion,
    InvalidBool,
    InvalidArray,
    InvalidEnum,
    InvalidInt,
    InvalidFloat,
} ||
    Allocator.Error ||
    std.fmt.ParseIntError ||
    std.fmt.ParseFloatError;

pub const Token = Tokenizer.Token;

pub const ValueList = struct {
    values: []Value,
    wss: [][2]u32,
    leading_ws: [2]u32,
    trailing_ws: [2]u32,
};

pub const ValueMap = struct {
    leading_ws: [2]u32,
    trailing_ws: [2]u32,
    keys: []Value,
    values: []Value,
    key_wss: [][2]u32,
    value_wss: [][2]u32,
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
    tagged: struct { Token, *Value }, // TODO remove Token. don't think its used for formatting or anything.
    integer: i128,
    float: Token,
    list: ValueList,
    vector: ValueList,
    map: ValueMap,
    set: ValueList,

    pub const Tag = enum(u8) {
        nil = 0,
        true = 1,
        false = 2,
        string = 3,
        character = 4,
        keyword = 5,
        symbol = 6,
        tagged = 7,
        integer = 8,
        float = 9,
        // containers
        list = 10,
        vector = 11,
        map = 12,
        set = 13,
    };

    pub fn eql(
        a: Value,
        a_src: [:0]const u8,
        a_values: []const Value,
        b: Value,
        b_src: [:0]const u8,
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
                    break :blk as.values.len == bs.values.len and for (as.values, bs.values) |aa, bb| {
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
                .tagged => std.debug.panic("TODO {s}", .{@tagName(a)}),
            };
    }
};

pub const MapContext = struct {
    src: [:0]const u8,
    values: []const Value,

    fn hashValue(self: MapContext, v: Value, hptr: anytype) void {
        hptr.update(mem.asBytes(&@intFromEnum(v)));
        switch (v) {
            .nil, .true, .false => {},
            .character, .integer => |p| hptr.update(mem.asBytes(&p)),
            .float, .keyword, .symbol, .string => |token| hptr.update(token.src(self.src)),
            .vector, .list => |l| {
                hptr.update(mem.asBytes(&l.values.len));
                for (l.values) |i| self.hashValue(i, hptr);
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
            .set => |set| {
                hptr.update(mem.asBytes(&set.values.len));
                for (set.values) |k| self.hashValue(k, hptr);
            },
            .tagged => std.debug.panic("TODO {s}", .{@tagName(v)}),
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
        src: [:0]const u8,
        userdata: ?*anyopaque,
    ) ParseError!Value;

    /// initialization format: {tag_name, handler_fn}.
    /// tag_name must include leading '#'.
    pub const Data = struct { []const u8, Fn };

    pub const Map = std.StaticStringMap(Fn);
};

pub const ParseResult = struct {
    values: []const Value,
    wss: []const [2]u32,
    final_ws: [2]u32,
    top_level_values: u32,

    pub fn deinit(r: ParseResult, alloc: Allocator) void {
        alloc.free(r.values);
        alloc.free(r.wss);
    }

    fn getList(list: []Value, n: u32) !Value {
        if (n >= list.len) return error.PathNotFound;
        return list[n];
    }

    /// path: search components joined by double slashes ('//'). i.e. '0//1//foo'
    pub fn find(r: ParseResult, path: []const u8, src: [:0]const u8) !?Value {
        var it = mem.splitSequence(u8, path, "//");
        var cur: Value = .{ .list = .{
            .values = @constCast(r.values[0..r.top_level_values]),
            .wss = if (r.wss.len != 0) @constCast(r.wss[0..r.top_level_values]) else &.{},
            .leading_ws = undefined,
            .trailing_ws = undefined,
        } };
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
                    .list => cur = try getList(cur.list.values, n),
                    .vector => cur = try getList(cur.vector.values, n),
                    .set => cur = try getList(cur.set.values, n),
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

pub const Options = packed struct {
    userdata: ?*anyopaque = null,
    whitespace: enum(u1) { include, exclude } = .include,
};

pub const ParseMode = enum(u1) {
    // TODO skip any expensive parsing such as parsing integers during measure.
    /// only update Parser.measured fields (values, whitespace, and top level).
    /// don't allocate or write to any memory.
    measure,
    /// write values and whitespace to Parser.values and Parser.ws.
    /// parseFromSliceAlloc() will parse twice, the first to measure and second
    /// to allocate write to allocate memory.
    allocate,
};

pub const Measured = struct {
    /// total number of values found during parsing
    values: u32 = 0,
    /// total number of values found at top level
    top_level_values: u32 = 0,
};

pub fn measure(
    src: [:0]const u8,
    options: Options,
    comptime comptime_options: ComptimeOptions,
) !Measured {
    var p = Parser.init(src, options, undefined, undefined, undefined, undefined, comptime_options.handlers);
    _ = try parseValues(&p, .measure);
    return p.measured;
}

/// assumes that values and wss are large enough to hold the parse results.
/// users may call measure() to get the necessary sizes.
pub fn parseFromSliceBuf(
    src: [:0]const u8,
    options: Options,
    values: []Value,
    wss: [][2]u32,
    measured: Measured,
    comptime comptime_options: ComptimeOptions,
) !ParseResult {
    var p = Parser.init(
        src,
        options,
        values.ptr,
        values.ptr + measured.top_level_values,
        wss.ptr,
        wss.ptr + measured.top_level_values,
        comptime_options.handlers,
    );
    p.measured = measured;
    const final_ws = try parseValues(&p, .allocate);

    debug("measured.values {} parsed values count {}", .{ p.measured.values, p.values - p.values_start });

    // TODO verify that all containers are closed.  currently we can trip this assertion with
    assert(p.values == p.values_start + measured.values);
    assert(p.values_start == values.ptr);
    if (p.options.whitespace == .include) assert(p.wss == p.wss_start + measured.values);
    assert(p.wss_start == wss.ptr);

    return .{
        .values = p.values_start[0..measured.values],
        .wss = if (options.whitespace == .include)
            p.wss_start[0..measured.values]
        else
            &.{},
        .top_level_values = measured.top_level_values,
        .final_ws = final_ws,
    };
}

/// performs two parsing passes. the first, measure, determines how many values
/// and whitespace entries are needed.  and the second allocates and write to
/// those entries using parseFromSliceBuf()
pub fn parseFromSliceAlloc(
    alloc: Allocator,
    src: [:0]const u8,
    options: Options,
    comptime comptime_options: ComptimeOptions,
) !ParseResult {
    const measured = try measure(src, options, comptime_options);

    const values = try alloc.alloc(Value, measured.values);
    errdefer alloc.free(values);
    const wss: [][2]u32 = if (options.whitespace == .include)
        try alloc.alloc([2]u32, measured.values)
    else
        &.{};
    errdefer alloc.free(wss);

    return parseFromSliceBuf(src, options, values, wss, measured, comptime_options);
}

pub const ComptimeOptions = struct {
    eval_branch_quota: u32 = 1000,
    handlers: []const TaggedElementHandler.Data = &.{},
};

pub inline fn parseFromSliceComptime(
    comptime src: [:0]const u8,
    comptime options: Options,
    comptime comptime_options: ComptimeOptions,
) !ParseResult {
    comptime {
        @setEvalBranchQuota(comptime_options.eval_branch_quota);
        const measured = try measure(src, options, comptime_options);
        var values: [measured.values]Value = undefined;
        var ws: [measured.values][2]u32 = undefined;
        return parseFromSliceBuf(src, options, &values, &ws, measured, comptime_options);
    }
}

pub fn parseTypeFromSlice(comptime T: type, src: [:0]const u8, options: Options) !T {
    var p = Parser.init(src, options, undefined, undefined, undefined, undefined, &.{});
    const t = try parseType(T, null, &p);
    if (!p.tokenizer.isTag(.eof)) return error.InvalidType;
    return t;
}

inline fn unsupportedType(comptime T: type) noreturn {
    @compileError("unsupported type '" ++ @typeName(T) ++ "'");
}

inline fn isContainer(T: type) bool {
    comptime {
        const info = @typeInfo(T);
        return info == .@"struct" or info == .@"union" or info == .@"enum" or info == .@"opaque";
    }
}

fn parseType(
    comptime T: type,
    comptime P: ?type, // Parent type
    p: *Parser,
) ParseError!T {
    if (isContainer(T) and @hasDecl(T, "ednParse")) {
        return T.ednParse(p, p.options.userdata);
    }

    if (p.tokenizer.peek().tag == .tagged) {
        // support tagged element handlers
        // TODO skip discards
        const tag = p.tokenizer.next();

        const tagged = p.tokenizer.next();
        const next = try parseValueInner(p, tagged, .measure);
        if (P) |Parent| {
            comptime assert(isContainer(Parent));
            if (@hasDecl(Parent, "ednTagHandler")) {
                return Parent.ednTagHandler(
                    T,
                    tag.src(p.tokenizer.src),
                    tagged.src(p.tokenizer.src),
                    p.tokenizer.src,
                    next,
                    p.options.userdata,
                );
            }
        }
        return err(p, "no handler for tag '{s}'", .{tag.src(p.tokenizer.src)}, error.MissingHandler);
    }

    return switch (@typeInfo(T)) {
        .@"struct" => parseStruct(T, p),
        .@"union" => u: {
            const Fe = std.meta.FieldEnum(T);
            _ = try expectNext(.lbrace, error.InvalidUnion, p);
            const c = p.tokenizer.next();
            const field = switch (c.tag) {
                .keyword => blk: {
                    const key = Value{ .keyword = c };
                    break :blk std.meta.stringToEnum(Fe, key.keyword.src(p.tokenizer.src)[1..]) orelse
                        return error.InvalidStructField;
                },
                else => {
                    return err(p, "unexpected '{c}'", .{c}, error.InvalidUnion);
                },
            };
            debug("{s: <[1]}parseType union key {2s}", .{ "", p.depth * 2, @tagName(field) });
            const u = switch (field) {
                inline else => |tag| @unionInit(
                    T,
                    @tagName(tag),
                    try parseType(@FieldType(T, @tagName(tag)), T, p),
                ),
            };
            if (consume(.rbrace, p) == null) return error.InvalidUnion;
            break :u u;
        },
        .bool => blk: {
            const t = p.tokenizer.next();
            if (t.tag == .true) {
                break :blk true;
            } else if (t.tag == .false) {
                break :blk false;
            } else break :blk error.InvalidBool;
        },
        .int => blk: {
            const t = try expectNext(.int, error.InvalidInt, p);
            break :blk try std.fmt.parseInt(T, t.src(p.tokenizer.src), 0);
        },
        .float => blk: {
            const t = p.tokenizer.next();
            if (t.tag != .float and t.tag != .int) return error.InvalidFloat;
            break :blk std.fmt.parseFloat(T, t.src(p.tokenizer.src));
        },
        .pointer => |i| blk: switch (i.size) {
            .slice => switch (i.child) {
                u8 => {
                    const t = try expectNext(.str, error.InvalidString, p);
                    const s = Value{ .string = t };
                    break :blk s.string.src(p.tokenizer.src);
                },
                else => unsupportedType(T),
            },
            else => unsupportedType(T),
        },
        .optional => |i| blk: {
            const t = p.tokenizer.peek();
            break :blk switch (t.tag) {
                .nil => {
                    _ = p.tokenizer.next();
                    break :blk null;
                },
                else => try parseType(i.child, P, p),
            };
        },
        .@"enum" => blk: {
            const t = try expectNext(.keyword, error.InvalidEnum, p);
            const key = Value{ .keyword = t };
            debug("enum key {s}", .{key.keyword.src(p.tokenizer.src)});
            break :blk std.meta.stringToEnum(T, key.keyword.src(p.tokenizer.src)[1..]) orelse
                error.InvalidEnum;
        },
        inline .array, .vector => |i| blk: {
            var a: [i.len]i.child = undefined;
            _ = try expectNext(.lbracket, error.InvalidArray, p);
            for (&a) |*ele| {
                ele.* = try parseType(i.child, P, p);
            }
            _ = try expectNext(.rbracket, error.InvalidArray, p);
            break :blk a;
        },
        else => unsupportedType(T),
    };
}

fn parseStruct(comptime T: type, p: *Parser) !T {
    const Fe = std.meta.FieldEnum(T);
    var seen_fields = std.enums.EnumSet(Fe).initEmpty();

    debug("{s: <[1]}parseStruct({2s})", .{ "", p.depth * 2, @typeName(T) });
    defer debug("{s: <[1]}<parseStruct({2s})", .{ "", p.depth * 2, @typeName(T) });
    p.depth += 1;
    defer p.depth -= 1;
    _ = try expectNext(.lbrace, error.InvalidStruct, p);

    var t: T = undefined;

    while (true) {
        const c = p.tokenizer.next();
        debug("{s: <[1]}parseStruct '{2s}'", .{ "", p.depth * 2, @tagName(c.tag) });
        if (c.tag == .rbrace) break;

        const field = switch (c.tag) {
            .keyword => blk: {
                const key = Value{ .keyword = c };
                break :blk std.meta.stringToEnum(Fe, key.keyword.src(p.tokenizer.src)[1..]) orelse
                    return error.InvalidStructField;
            },
            else => return err(p, "unexpected '{c}'", .{c}, error.InvalidStruct),
        };
        debug("{s: <[1]}parseStruct key {2s}", .{ "", p.depth * 2, @tagName(field) });

        switch (field) {
            inline else => |tag| {
                @field(t, @tagName(tag)) = try parseType(@FieldType(T, @tagName(tag)), T, p);
            },
        }
        seen_fields.insert(field);
    }

    var unseen_iter = seen_fields.complement().iterator();
    while (unseen_iter.next()) |field| switch (field) {
        inline else => |tag| {
            const info = std.meta.fieldInfo(T, tag);
            if (info.defaultValue()) |default| {
                @field(t, @tagName(tag)) = default;
                seen_fields.insert(tag);
            } else {
                err(p, "missing field '{s}'", .{@tagName(tag)}, error.MissingFields) catch {};
            }
        },
    };
    if (seen_fields.complement().count() != 0) return error.MissingFields;
    return t;
}

pub fn fmtParseResult(
    parse_result: ParseResult,
    src: [:0]const u8,
) std.fmt.Formatter(formatParseResult) {
    return .{ .data = .{ .src = src, .parse_result = parse_result, .parent = null } };
}

fn formatParseResult(
    data: struct { src: [:0]const u8, parse_result: ParseResult, parent: ?Value },
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    for (data.parse_result.values[0..data.parse_result.top_level_values], 0..) |v, value_id| {
        try formatValue(.{
            .value = v,
            .value_id = @intCast(value_id),
            .src = data.src,
            .parse_result = data.parse_result,
            .parent = data.parent,
        }, fmt, options, writer);
    }
    const ws = data.parse_result.final_ws;
    try writer.writeAll(data.src[ws[0]..ws[1]]);
}

pub fn fmtValue(
    value: Value,
    value_id: u32,
    src: [:0]const u8,
    parse_result: ParseResult,
    parent: ?Value,
) std.fmt.Formatter(formatValue) {
    return .{ .data = .{
        .value = value,
        .value_id = value_id,
        .src = src,
        .parse_result = parse_result,
        .parent = parent,
    } };
}

fn formatValue(
    data: struct {
        value: Value,
        /// the offset of value within parse_result.values
        value_id: u32,
        src: [:0]const u8,
        parse_result: ParseResult,
        parent: ?Value,
    },
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    const v = data.value;

    // print leading whitespace
    if (data.parse_result.wss.len > 0) {
        const ws = data.parse_result.wss[data.value_id];
        try writer.writeAll(data.src[ws[0]..ws[1]]);
    } else if (data.parent == null) { // skip if inside list, vec, map or set. those are handled in loops below
        // print a single space between top level values except first one
        try writer.writeAll(" "[0..@intFromBool(data.value_id > 0)]);
    }

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
        .character => |c| switch (c) {
            ' ' => try writer.writeAll("\\space"),
            '\n' => try writer.writeAll("\\newline"),
            '\t' => try writer.writeAll("\\tab"),
            '\r' => try writer.writeAll("\\return"),
            else => if (c < 256 and !std.ascii.isPrint(@truncate(c)))
                try writer.print("\\u{u}", .{c + '0'})
            else if (c >= 256)
                try writer.print("\\u{u}", .{c})
            else
                try writer.print("\\{u}", .{c}),
        },
        .integer => |x| try std.fmt.formatType(x, fmt, options, writer, 0),
        .tagged => |tag| {
            try writer.writeAll(tag[0].src(data.src));
            const value_id: u32 = @intCast(tag[1] - data.parse_result.values.ptr);
            try std.fmt.formatType(fmtValue(tag[1].*, value_id, data.src, data.parse_result, data.parent), fmt, options, writer, 0);
        },
        .list => |l| {
            try writer.writeAll(data.src[l.leading_ws[0]..l.leading_ws[1]]);
            for (l.values, 0..) |*item, i| {
                if (data.parse_result.wss.len == 0 and i > 0) try writer.writeByte(' ');
                const value_id: u32 = @intCast(item - data.parse_result.values.ptr);
                try std.fmt.formatType(fmtValue(item.*, value_id, data.src, data.parse_result, v), fmt, options, writer, 0);
            }
            try writer.writeAll(data.src[l.trailing_ws[0]..l.trailing_ws[1]]);
        },
        .vector => |l| {
            try writer.writeAll(data.src[l.leading_ws[0]..l.leading_ws[1]]);
            for (l.values, 0..) |*item, i| {
                if (data.parse_result.wss.len == 0 and i > 0) try writer.writeByte(' ');
                const value_id: u32 = @intCast(item - data.parse_result.values.ptr);
                try std.fmt.formatType(fmtValue(item.*, value_id, data.src, data.parse_result, v), fmt, options, writer, 0);
            }
            try writer.writeAll(data.src[l.trailing_ws[0]..l.trailing_ws[1]]);
        },
        .map => |m| {
            try writer.writeAll(data.src[m.leading_ws[0]..m.leading_ws[1]]);
            for (m.keys, m.values, 0..) |*k, *sv, i| {
                if (data.parse_result.wss.len == 0 and i > 0) try writer.writeAll(", ");
                const kvalue_id: u32 = @intCast(k - data.parse_result.values.ptr);
                try std.fmt.formatType(fmtValue(k.*, kvalue_id, data.src, data.parse_result, v), fmt, options, writer, 0);
                if (data.parse_result.wss.len == 0) try writer.writeByte(' ');
                const vvalue_id: u32 = @intCast(sv - data.parse_result.values.ptr);
                try std.fmt.formatType(fmtValue(sv.*, vvalue_id, data.src, data.parse_result, v), fmt, options, writer, 0);
            }
            try writer.writeAll(data.src[m.trailing_ws[0]..m.trailing_ws[1]]);
        },
        .set => |set| {
            try writer.writeAll(data.src[set.leading_ws[0]..set.leading_ws[1]]);
            for (set.values, 0..) |*k, i| {
                if (data.parse_result.wss.len == 0 and i > 0) try writer.writeByte(' ');
                const value_id: u32 = @intCast(k - data.parse_result.values.ptr);
                try std.fmt.formatType(fmtValue(k.*, value_id, data.src, data.parse_result, v), fmt, options, writer, 0);
            }
            try writer.writeAll(data.src[set.trailing_ws[0]..set.trailing_ws[1]]);
        },
    }
}

fn err(_: *const Parser, comptime fmt: []const u8, args: anytype, e: ParseError) ParseError {
    if (@inComptime()) {
        @compileError(std.fmt.comptimePrint(fmt, args));
    } else if (!@import("builtin").is_test) log.err(fmt, args);
    // TODO report error location as line/column
    return e;
}

fn isSymChar(c: u8) bool {
    return switch (c) {
        'a'...'z', 'A'...'Z', '0'...'9' => true,
        '/', '.', '*', '+', '!', '-', '_', '?', '$', '%', '&', '=', '<', '>', '#', ':' => true,
        else => false,
    };
}

fn validateSym(slice: []const u8) !void {
    debug("validateSym({s})", .{slice});
    switch (slice[0]) {
        '0'...'9' => return error.InvalidSymbol,
        '-', '+', '.' => if (slice.len > 1 and std.ascii.isDigit(slice[1]))
            return error.InvalidSymbol,
        '#', ':' => return error.InvalidSymbol,
        else => {},
    }
}

fn consume(tag: Token.Tag, p: *Parser) ?Token {
    if (p.tokenizer.isTag(tag)) {
        return p.tokenizer.next();
    }
    return null;
}

fn expectNext(tag: Token.Tag, e: ParseError, p: *Parser) ParseError!Token {
    const t = p.tokenizer.next();
    if (t.tag != tag) {
        debug("expectNext wanted '{s}' got '{s}'", .{ @tagName(tag), @tagName(t.tag) });
        return e;
    }
    return t;
}

fn parseInt(p: *Parser, t: Token) !Value {
    debug("{s: <[1]}parseInt('{2s}')", .{ "", p.depth * 2, t.src(p.tokenizer.src) });
    assert(t.tag == .int);
    return;
}

// Characters are preceded by a backslash: \c, \newline, \return, \space
// and \tab yield the corresponding characters. Unicode characters are
// represented with \uNNNN as in Java. Backslash cannot be followed by
// whitespace.
fn parseChar(p: *Parser, t: Token) !Value {
    debug("{s: <[1]}parseChar()", .{ "", p.depth * 2 });
    assert(t.tag == .char);
    const src0 = t.loc.src(p.tokenizer.src);
    assert(src0[0] == '\\');
    const src = src0[1..];
    const char: u21 = if (src.len == 1) src[0] else switch (src[0]) {
        'u' => try std.fmt.parseInt(u21, src[1..], 16),
        'n' => if (std.mem.eql(u8, "newline", src)) '\n' else return error.InvalidChar,
        'r' => if (std.mem.eql(u8, "return", src)) '\r' else return error.InvalidChar,
        's' => if (std.mem.eql(u8, "space", src)) ' ' else return error.InvalidChar,
        't' => if (std.mem.eql(u8, "tab", src)) '\t' else return error.InvalidChar,
        else => return error.InvalidChar,
    };
    debug("{s: <[1]}parseChar() result {2}:'{2u}'", .{ "", p.depth * 2, char });

    return .{ .character = char };
}

fn pairedTag(tag: Token.Tag) Token.Tag {
    return switch (tag) {
        .lparen => .rparen,
        .lbrace => .rbrace,
        .lbracket => .rbracket,
        .set => .rbrace,
        else => std.debug.panic("unexpected tag '{s}'", .{@tagName(tag)}),
    };
}

fn parseOrMeasureList(p: *Parser, t: Token, comptime mode: ParseMode) !ValueList {
    const tokenizer = p.tokenizer;
    const len, _ = try parseList(p, t, .measure, undefined, undefined);
    if (mode == .measure) {
        var measured: ValueList = undefined;
        measured.values.len = len;
        return measured;
    }
    p.tokenizer = tokenizer;
    // allocate space for values, wss
    const vs = p.values[0..len];
    p.values = p.values[len..];
    const wss = p.wss[0..len];
    p.wss = p.wss[len..];
    _, const trailing_ws = try parseList(p, t, .allocate, vs, wss);
    return .{
        .values = vs,
        .wss = wss,
        .leading_ws = .{ t.loc.start, t.loc.end },
        .trailing_ws = trailing_ws,
    };
}

fn parseList(
    p: *Parser,
    prefix: Token,
    comptime mode: ParseMode,
    result: []Value,
    result_wss: [][2]u32,
) ParseError!struct { u32, [2]u32 } {
    debug("{s: <[1]}parseList('{2s}', {3s})", .{ "", p.depth * 2, @tagName(prefix.tag), @tagName(mode) });
    assert(prefix.tag == .lparen or prefix.tag == .lbracket);
    p.depth += 1;
    defer p.depth -= 1;

    var i: u32 = 0;
    const end_tag = pairedTag(prefix.tag);
    var trailing_ws: [2]u32 = undefined;
    while (true) : (i += 1) {
        const c = p.tokenizer.next();
        debug("{s: <[1]}parseList('{2s}')", .{ "", p.depth * 2, @tagName(c.tag) });
        if (c.tag == end_tag) {
            trailing_ws = .{ c.loc.ws_start, c.loc.end };
            p.last_token = c;
            break;
        }
        if (mode == .allocate) {
            try parseValue(p, c, mode, result[i..].ptr, result_wss[i..].ptr);
        } else {
            try parseValue(p, c, mode, undefined, undefined);
        }
    }

    return .{ i, trailing_ws };
}

fn parseOrMeasureMap(p: *Parser, prefix: Token, comptime mode: ParseMode) !ValueMap {
    const tokenizer = p.tokenizer;
    const len = try parseMap(p, prefix, .measure, undefined);
    if (mode == .measure) {
        var measured: ValueMap = undefined;
        measured.keys.len = len;
        measured.values.len = len;
        return measured;
    }
    p.tokenizer = tokenizer;
    const vs = p.values[0 .. len * 2];
    const wss = p.wss[0 .. len * 2];
    var map: ValueMap = .{
        .keys = vs[0..len],
        .key_wss = wss[0..len],
        .values = vs[len..],
        .value_wss = wss[len..],
        .leading_ws = .{ prefix.loc.start, prefix.loc.end },
        .trailing_ws = undefined,
    };
    p.values = p.values[len * 2 ..];
    p.wss = p.wss[len * 2 ..];
    _ = try parseMap(p, prefix, .allocate, &map);
    return map;
}

fn parseMap(p: *Parser, prefix: Token, comptime mode: ParseMode, result: *ValueMap) ParseError!u32 {
    assert(prefix.tag == .lbrace);
    debug("{s: <[1]}parseMap({2s})", .{ "", p.depth * 2, @tagName(mode) });
    defer debug("{s: <[1]}<parseMap({2s})", .{ "", p.depth * 2, @tagName(mode) });
    p.depth += 1;
    defer p.depth -= 1;

    var i: u32 = 0;
    while (true) : (i += 1) {
        const c = p.tokenizer.next();
        debug("{s: <[1]}parseMap '{2s}'", .{ "", p.depth * 2, @tagName(c.tag) });
        if (c.tag == .rbrace) {
            if (mode == .allocate) result.trailing_ws = .{ c.loc.ws_start, c.loc.end };
            p.last_token = c;
            break;
        }

        if (mode == .allocate) {
            parseValue(p, c, mode, result.keys[i..].ptr, result.key_wss[i..].ptr) catch |e| switch (e) {
                error.EndOfMap => break,
                else => return e,
            };
            try parseValue(p, p.tokenizer.next(), mode, result.values[i..].ptr, result.value_wss[i..].ptr);
        } else {
            parseValue(p, c, mode, undefined, undefined) catch |e| switch (e) {
                error.EndOfMap => break,
                else => return e,
            };
            try parseValue(p, p.tokenizer.next(), mode, undefined, undefined);
        }
    }
    debug("{s: <[1]}parseMap done len {2}", .{ "", p.depth * 2, i });
    return i;
}

fn parseOrMeasureSet(p: *Parser, prefix: Token, comptime mode: ParseMode) !ValueList {
    const tokenizer = p.tokenizer;
    assert(prefix.tag == .set);
    const len, _ = try parseSet(p, prefix, .measure, undefined, undefined);
    if (mode == .measure) {
        var measured: ValueList = undefined;
        measured.values.len = len;
        return measured;
    }
    p.tokenizer = tokenizer;

    const vs = p.values[0..len];
    p.values = p.values[len..];
    const wss = p.wss[0..len];
    p.wss = p.wss[len..];
    _, const trailing_ws = try parseSet(p, prefix, .allocate, vs, wss);
    return .{
        .values = vs,
        .wss = wss,
        .leading_ws = .{ prefix.loc.start, prefix.loc.end },
        .trailing_ws = trailing_ws,
    };
}

fn parseSet(
    p: *Parser,
    prefix: Token,
    comptime mode: ParseMode,
    result: []Value,
    result_wss: [][2]u32,
) ParseError!struct { u32, [2]u32 } {
    assert(prefix.tag == .set);
    debug("{s: <[1]}parseSet({2s})", .{ "", p.depth * 2, @tagName(mode) });
    p.depth += 1;
    defer p.depth -= 1;
    var trailing_ws: [2]u32 = undefined;
    var i: u32 = 0;
    while (true) : (i += 1) {
        const c = p.tokenizer.next();
        debug("{s: <[1]}parseSet '{2s}' '{3s}'", .{ "", p.depth * 2, @tagName(c.tag), c.loc.src(p.tokenizer.src) });
        if (c.tag == .rbrace) {
            trailing_ws = .{ c.loc.ws_start, c.loc.end };
            p.last_token = c;
            break;
        }

        if (mode == .allocate) {
            try parseValue(p, c, mode, result[i..].ptr, result_wss[i..].ptr);
        } else {
            try parseValue(p, c, mode, undefined, undefined);
        }
    }

    return .{ i, trailing_ws };
}

fn parseDiscard(p: *Parser, t: Token, comptime mode: ParseMode) ParseError!Value {
    assert(t.tag == .discard);
    p.depth += 1;
    defer p.depth -= 1;

    const t2 = p.tokenizer.next();
    const discard = try parseValueInner(p, t2, mode);
    debug("{s: <[1]}parseDiscard() ignored {2s}", .{ "", p.depth * 2, @tagName(discard) });
    // TODO merge ws. pretty sure discards won't format correctly

    const t3 = p.tokenizer.next();
    return try parseValueInner(p, t3, mode);
}

fn parseTagged(p: *Parser, t: Token, comptime mode: ParseMode) ParseError!Value {
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

    const t2 = p.tokenizer.next();
    const next = try parseValueInner(p, t2, mode);
    const v = if (p.handlers.get(t.src(p.tokenizer.src))) |handler| v: {
        const cur = p.tokenizer.peek();
        const v = try handler(
            next,
            p.tokenizer.src[t2.loc.start..cur.loc.ws_start],
            p.tokenizer.src,
            p.options.userdata,
        );
        break :v v;
    } else next;

    if (mode == .measure) {
        p.measured.values += 2;
    } else {
        p.values[0] = .{ .tagged = .{ t, &p.values[1] } };
        p.values[1] = v;
        defer p.values = p.values[2..];
        if (p.options.whitespace == .include) {
            p.wss[0] = .{ t.loc.ws_start, t.loc.start };
            p.wss[1] = .{ t2.loc.ws_start, t2.loc.start };
            p.wss = p.wss[2..];
        }
        return p.values[0];
    }
    return v;
}

// TODO allow users to skip expensive parsing such as parsing integers
pub fn userParseValue(p: *Parser) !Value {
    return parseValueInner(p, p.tokenizer.next(), .measure);
}

fn parseValueInner(p: *Parser, t: Token, comptime mode: ParseMode) !Value {
    // std.debug.print("{s: <[1]}{2s}:\n  ws '{3s}'\n  content '{4s}'\n", .{ "", p.depth * 2, @tagName(t.tag), p.tokenizer.src[t.loc.ws_start..t.loc.start], t.src(p.tokenizer.src) });
    debug("{s: <[1]}parseValueInner() '{2s}'", .{ "", p.depth * 2, @tagName(t.tag) });
    if (p.last_token.loc.end == t.loc.start and
        !t.tag.isClosing() and
        !p.last_token.tag.isOpening())
    {
        return error.MissingWhitespaceBetweenValues;
    }
    p.last_token = t;

    return switch (t.tag) {
        .int => .{ .integer = try std.fmt.parseInt(i128, t.src(p.tokenizer.src), 0) },
        .str => .{ .string = t },
        .char => try parseChar(p, t),
        .keyword => .{ .keyword = t },
        .lparen => .{ .list = try parseOrMeasureList(p, t, mode) },
        .lbracket => .{ .vector = try parseOrMeasureList(p, t, mode) },
        .lbrace => .{ .map = try parseOrMeasureMap(p, t, mode) },
        .rbrace => return error.EndOfMap,
        .set => .{ .set = try parseOrMeasureSet(p, t, mode) },
        .discard => try parseDiscard(p, t, mode),
        .tagged => try parseTagged(p, t, mode),
        .eof => return err(p, "unexpected end of file", .{}, error.Eof),
        .symbol => .{ .symbol = t },
        .invalid => return err(p, "invalid token '{s}'", .{t.src(p.tokenizer.src)}, error.InvalidToken),
        .float => .{ .float = t },
        .nil => .nil,
        .true => .true,
        .false => .false,
        .rparen => unreachable,
        .rbracket => unreachable,
    };
}

fn parseValue(p: *Parser, t: Token, comptime mode: ParseMode, result: [*]Value, result_wss: [*][2]u32) ParseError!void {
    const v = try parseValueInner(p, t, mode);

    debug("{s: <[1]}parseValue({2s})", .{ "", p.depth * 2, @tagName(mode) });

    if (mode == .measure) {
        p.measured.values += 1;
    } else {
        result[0] = v;
        if (p.options.whitespace == .include) result_wss[0] = .{ t.loc.ws_start, t.loc.start };
    }
}

/// returns the final whitespace at eof
fn parseValues(p: *Parser, comptime mode: ParseMode) ![2]u32 {
    // top level values are stored at the beginning of values/whitespaces
    var top_level_vs: []Value = p.values_start[0..p.measured.top_level_values];
    var top_level_wss: [][2]u32 = p.wss_start[0..p.measured.top_level_values];
    debug("{s: <[1]}parseValues({2s}) {3}", .{ "", p.depth * 2, @tagName(mode), p.measured });

    var final_ws: [2]u32 = undefined;
    while (true) {
        const t = p.tokenizer.next();
        if (t.tag == .eof) {
            final_ws = .{ t.loc.ws_start, t.loc.end };
            break;
        }
        if (mode == .allocate) {
            parseValue(p, t, mode, top_level_vs.ptr, top_level_wss.ptr) catch |e| switch (e) {
                error.Eof => break,
                else => return e,
            };
            top_level_vs = top_level_vs[1..];
            top_level_wss = top_level_wss[1..];
        } else {
            parseValue(p, t, mode, undefined, undefined) catch |e| switch (e) {
                error.Eof => break,
                else => return e,
            };
            p.measured.top_level_values += 1;
        }
    }

    if (mode == .allocate) assert(top_level_vs.len == 0);

    return final_ws;
}

const log = std.log.scoped(.edn);

fn debug(comptime fmt: []const u8, args: anytype) void {
    _ = fmt; // autofix
    _ = args; // autofix
    if (!@inComptime()) {
        // log.debug(fmt, args);
        // std.debug.print(fmt ++ "\n", args);
    }
}

const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const assert = std.debug.assert;

pub const Parser = @import("Parser.zig");
pub const Tokenizer = @import("Tokenizer.zig");
