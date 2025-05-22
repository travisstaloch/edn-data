//! https://github.com/edn-format/edn

pub const Error = error{
    Parse,
    Eof,
    HandlerParse,
    MissingHandler,
    EndOfMap,
    MissingWhitespaceBetweenValues,
    InvalidStruct,
    InvalidStructField,
    MissingFields,
    InvalidToken,
    InvalidChar,
    InvalidString,
    InvalidUnion,
    InvalidBool,
    InvalidArray,
    InvalidEnum,
    InvalidInt,
    InvalidFloat,
    UnclosedContainer,
} ||
    Allocator.Error ||
    std.fmt.ParseIntError ||
    std.fmt.ParseFloatError;

pub const Token = Tokenizer.Token;

pub const Diagnostic = struct {
    /// zero based
    line: u32 = 0,
    column: u32 = 0,
    /// user is responsible for setting file_path
    file_path: ?[]const u8 = null,
    /// on error, this will be populated
    error_message: [256]u8 = [1]u8{0} ** 256,
    // TODO user defined printFn?
};

pub const Span = [2]u32;

pub const Value = union(Value.Tag) {
    nil,
    true,
    false,
    string: Token,
    character: u21,
    keyword: Token,
    symbol: Token,
    tagged: struct { tag: Token, value: *Value },
    integer: i128,
    float: Token,
    list: List,
    vector: List,
    set: List,
    map: List,

    pub const Tag = enum(u8) {
        nil,
        true,
        false,
        string,
        character,
        keyword,
        symbol,
        tagged,
        integer,
        float,
        // containers
        list,
        vector,
        set,
        map,
    };

    pub const List = struct {
        len: u32 = 0,
        trailing_ws: Span = .{ 0, 0 },
    };

    pub const Id = usize; // TODO use enum
    pub const Ids = std.ArrayListUnmanaged(Id);
    pub const value_id_max = std.math.maxInt(Id); // TODO delete. replace with enum field.

    pub fn eql(
        a: *const Value,
        a_src: [:0]const u8,
        a_result: *const Result,
        b: *const Value,
        b_src: [:0]const u8,
        b_result: *const Result,
    ) bool {
        return a.* == std.meta.activeTag(b.*) and
            switch (a.*) {
                .nil, .true, .false => true,
                .integer => a.integer == b.integer,
                .character => |c| c == b.character,
                .tagged => |tagged| mem.eql(u8, tagged.tag.src(a_src), b.tagged.tag.src(b_src)) and
                    tagged.value.eql(a_src, a_result, b.tagged.value, b_src, b_result),
                inline .float, .keyword, .symbol, .string => |payload, tag| mem.eql(
                    u8,
                    payload.src(a_src),
                    @field(b, @tagName(tag)).src(b_src),
                ),
                inline .vector, .list => |as, tag| blk: {
                    const bs = @field(b, @tagName(tag));
                    var aiter = a_result.iterator(a);
                    var biter = b_result.iterator(b);
                    break :blk as.len == bs.len and for (0..as.len) |_| {
                        const aa = &a_result.values.items[aiter.next() orelse break false];
                        const bb = &b_result.values.items[biter.next() orelse break false];
                        if (!eql(aa, a_src, a_result, bb, b_src, b_result)) break false;
                    } else true;
                },
                .set, .map => blk: {
                    const actx = MapContext{ .src = a_src, .result = a_result };
                    const ahash = actx.hash(a);
                    const bctx = MapContext{ .src = b_src, .result = b_result };
                    const bhash = bctx.hash(b);
                    break :blk ahash == bhash;
                },
                // else => std.debug.panic("TODO {s}", .{@tagName(a.*)}),
            };
    }

    pub fn formatter(value: *const Value, src: [:0]const u8, result: *const Result) std.fmt.Formatter(formatValue) {
        return .{ .data = .{ .value = value, .src = src, .result = result } };
    }

    pub fn isContainer(v: Value) bool {
        return @intFromEnum(@as(Tag, v)) >= @intFromEnum(Tag.list);
    }

    pub fn listAt(v: *const Value, index: usize, result: *const Result) *const Value {
        return &result.values.items[v - result.values.items.ptr + index];
    }
};

pub const MapContext = struct {
    src: [:0]const u8,
    result: *const Result,

    fn hashValue(self: MapContext, v: *const Value, hptr: anytype) void {
        hptr.update(mem.asBytes(&@intFromEnum(v.*)));
        switch (v.*) {
            .nil, .true, .false => {},
            .character, .integer => |p| hptr.update(mem.asBytes(&p)),
            .float, .keyword, .symbol, .string => |token| hptr.update(token.src(self.src)),
            // these are equal if they have the same count of elements and, for
            // every element in one set, an equal element is in the other.
            .list, .vector, .set, .map => |*l| {
                hptr.update(mem.asBytes(&l.len));
                var iter = self.result.iterator(v);
                while (iter.next()) |cidx| self.hashValue(&self.result.values.items[cidx], hptr);
            },
            // maps are equal if they have the same number of entries, and for
            // every key/value entry in one map an equal key is present and
            // mapped to an equal value in the other.
            .tagged => |tagged| {
                hptr.update(tagged.tag.src(self.src));
                self.hashValue(tagged.value, hptr);
            },
            // else => std.debug.panic("TODO {s}", .{@tagName(v.*)}),
        }
    }

    pub fn hash(self: MapContext, v: *const Value) u32 {
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
    ) Error!Value;

    /// initialization format: {tag_name, handler_fn}.
    /// tag_name must include leading '#'.
    pub const Data = struct { []const u8, Fn };

    pub const Map = std.StaticStringMap(Fn);
};

pub const Result = struct {
    values: Values,
    whitespaces: Whitespaces,
    children: Value.Ids,
    siblings: Value.Ids,

    pub const init: Result = .{
        .values = .{},
        .whitespaces = .{},
        .children = .{},
        .siblings = .{},
    };

    pub fn deinit(r: Result, alloc: Allocator) void {
        const rr: *Result = @constCast(&r);
        rr.values.deinit(alloc);
        rr.whitespaces.deinit(alloc);
        rr.children.deinit(alloc);
        rr.siblings.deinit(alloc);
    }

    fn findOne(component: []const u8, r: *const Result, src: [:0]const u8, value: *const Value) !*const Value {
        switch (value.*) {
            inline .list, .vector, .set => |payload| {
                const i = try std.fmt.parseInt(usize, component, 0);
                var iter = r.iterator(value);
                for (0..payload.len) |j| {
                    const child_id = iter.next() orelse return error.PathNotFound;
                    if (i == j) return &r.values.items[child_id];
                } else return error.PathNotFound;
            },
            .map => |map| {
                var iter = r.iterator(value);
                for (0..map.len) |_| {
                    const kid = iter.next() orelse return error.PathNotFound;
                    const vid = iter.next() orelse return error.PathNotFound;
                    const k = &r.values.items[kid];
                    const v = &r.values.items[vid];
                    switch (k.*) {
                        .string,
                        .keyword,
                        .symbol,
                        => |t| if (mem.eql(u8, component, t.src(src))) return v,
                        .integer => {
                            var buf: [std.math.log10(std.math.maxInt(i128)) + 1]u8 = undefined;
                            const s = try std.fmt.bufPrint(&buf, "{}", .{k.formatter(src, r)});
                            if (mem.eql(u8, component, s)) {
                                return v;
                            }
                        },
                        .character => {
                            var buf: [std.math.log(usize, 2, std.math.maxInt(u21)) + 1]u8 = undefined;
                            const s = try std.fmt.bufPrint(&buf, "{}", .{k.formatter(src, r)});
                            if (mem.eql(u8, component, s)) {
                                return v;
                            }
                        },
                        else => return error.PathNotFound,
                    }
                }
            },
            else => unreachable,
        }
        return error.PathNotFound;
    }

    /// path: search components joined by double slashes ('//'). i.e. '0//1//foo'
    pub fn find(r: *const Result, path: []const u8, src: [:0]const u8) !*const Value {
        var it = mem.splitSequence(u8, path, "//");
        const component0 = it.next() orelse return error.InvalidPath;
        var cur = try findOne(component0, r, src, &r.values.items[0]);
        while (it.next()) |component| {
            cur = try findOne(component, r, src, cur);
        }
        return if (it.next() != null) error.PathNotFound else cur;
    }

    pub fn formatter(result: *const Result, src: [:0]const u8) std.fmt.Formatter(formatParseResult) {
        return .{ .data = .{ .src = src, .result = result } };
    }

    pub const Iterator = struct {
        result: *const Result,
        id: Value.Id,

        pub fn next(iter: *Iterator) ?Value.Id {
            if (iter.id == Value.value_id_max) return null;
            defer iter.id = iter.result.siblings.items[iter.id];
            return iter.id;
        }

        pub fn nth(iter: *Iterator, n: usize) ?Value.Id {
            for (1..n) |_| _ = iter.next();
            return iter.next();
        }
    };

    /// visits each child of parent.  does not descend to grand children.
    pub fn iterator(result: *const Result, parent: *const Value) Iterator {
        return .{ .result = result, .id = result.children.items[parent - result.values.items.ptr] };
    }

    pub fn firstValue(r: *const Result) *const Value {
        return &r.values.items[1];
    }
};

pub const Options = struct {
    userdata: ?*anyopaque = null,
    /// when set line and column will be populated on error and an error message
    /// with format <file_path>:<line>:<column> will be printed to diagnostic.error_message.
    /// user is responsible for setting Diagnostic.file_path.
    diagnostic: ?*Diagnostic = null,
    /// whether to save whitespace.  when false ParseResult.whitespaces will be
    /// empty and each whitespace will be formatted as a single space.
    whitespace: bool = true,
    // TODO possibly merge store and allocate?
    /// when false discard by parsing without storing values.
    store: bool = true,
    /// false to measure without allocating
    allocate: bool = true,
    /// stride = 2 is used to parse maps
    stride: u6 = 1,

    /// returns a copy of 'self' with fields from 'other' which don't have their default value.
    pub fn with(self: Options, other: Options) Options {
        var r = self;
        inline for (@typeInfo(Options).@"struct".fields) |f| {
            if (@field(other, f.name) != f.defaultValue().?) {
                @field(r, f.name) = @field(other, f.name);
            }
        }
        return r;
    }
};

pub const Slices = struct {
    values: []Value,
    whitespaces: []Span,
    children: []Value.Id,
    siblings: []Value.Id,
};

const Shape = struct {
    /// total number of values in Result
    capacity: u32 = 0,
    /// total number of values found at top level
    top_level_values: u32 = 0,

    pub inline fn Buffers(comptime s: Shape) type {
        return struct {
            values: [s.capacity]Value,
            whitespace: [s.capacity]Span,
            children: [s.capacity]Value.Id,
            siblings: [s.capacity]Value.Id,

            pub fn slices(arrays: *@This()) Slices {
                return .{
                    .values = &arrays.values,
                    .whitespaces = &arrays.whitespace,
                    .children = &arrays.children,
                    .siblings = &arrays.siblings,
                };
            }
        };
    }
};

pub fn measure(
    src: [:0]const u8,
    options: Options,
    comptime comptime_options: ComptimeOptions,
) !Shape {
    var p = try Parser.init(src, comptime_options.handlers);
    const list = try parseList(&p, .eof, &.{ .rparen, .rbracket, .rcurly }, 0, options.with(.{ .allocate = false }));
    return .{
        .capacity = @intCast(p.res.values.items.len + 1),
        .top_level_values = @intCast(list.len),
    };
}

pub const Values = std.ArrayListUnmanaged(Value);
pub const Whitespaces = std.ArrayListUnmanaged(Span);

/// assumes that values and whitespaces are large enough to hold the parse results.
/// users may call measure() to get the necessary sizes.
pub fn parseFromSliceBuf(
    src: [:0]const u8,
    shape: Shape,
    slices: Slices,
    options: Options,
    comptime comptime_options: ComptimeOptions,
) !Result {
    var p = try Parser.initFixed(src, slices.values, slices.whitespaces, slices.children, slices.siblings, comptime_options.handlers);
    const id = storeValue(&p, .zero, .{ .list = .{} }, null, options);
    const list = try parseList(&p, .eof, &.{ .rparen, .rbracket, .rcurly }, id, options);
    if (options.allocate) p.res.values.items[id] = .{ .list = list };

    debug("shape.values {} parsed values count {} top level count {}", .{
        shape.capacity,
        p.res.values.items.len,
        list.len,
    });

    // TODO remove this after early catching when a container isn't closed.  currently errors on unclosed map
    if (p.depth != 0) return error.UnclosedContainer;
    if (p.tokenizer.index != 0 and p.tokenizer.peek().tag != .eof) return error.IncompleteParse;

    return .{
        .values = p.res.values,
        .whitespaces = if (options.whitespace)
            p.res.whitespaces
        else
            .{},
        .children = p.res.children,
        .siblings = p.res.siblings,
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
) !Result {
    const shape = try measure(src, options, comptime_options);

    const values = try alloc.alloc(Value, shape.capacity);
    errdefer alloc.free(values);
    const whitespaces: []Span = if (options.whitespace)
        try alloc.alloc(Span, shape.capacity)
    else
        &.{};
    errdefer alloc.free(whitespaces);
    const children = try alloc.alloc(Value.Id, shape.capacity);
    errdefer alloc.free(children);
    const siblings = try alloc.alloc(Value.Id, shape.capacity);
    errdefer alloc.free(siblings);

    return try parseFromSliceBuf(
        src,
        shape,
        .{
            .values = values,
            .whitespaces = whitespaces,
            .children = children,
            .siblings = siblings,
        },
        options,
        comptime_options,
    );
}

pub const ComptimeOptions = struct {
    eval_branch_quota: u32 = 1000,
    handlers: []const TaggedElementHandler.Data = &.{},
};

pub inline fn parseFromSliceComptime(
    comptime src: [:0]const u8,
    comptime options: Options,
    comptime comptime_options: ComptimeOptions,
) !Result {
    comptime {
        @setEvalBranchQuota(comptime_options.eval_branch_quota);
        const shape = try measure(src, options, comptime_options);
        debug("parseFromSliceComptime cap {}", .{shape.capacity});
        var buffers: shape.Buffers() = undefined;
        return try parseFromSliceBuf(src, shape, buffers.slices(), options, comptime_options);
    }
}

pub fn parseTypeFromSlice(comptime T: type, src: [:0]const u8, options: Options) !T {
    var p = try Parser.init(src, &.{});
    const t = try parseType(T, null, &p, options.with(.{ .allocate = false }));
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
    options: Options,
) Error!T {
    if (isContainer(T) and @hasDecl(T, "ednParse")) {
        return T.ednParse(p, options.userdata, options);
    }

    if (p.tokenizer.index != 0 and p.tokenizer.isTag(.tagged)) {
        // support tagged element handlers
        // TODO skip discards
        const tag = p.tokenizer.next();

        const tagged = p.tokenizer.next();
        _, const next = try parseValue(p, tagged, null, options);
        if (P) |Parent| {
            comptime assert(isContainer(Parent));
            if (@hasDecl(Parent, "ednTagHandler")) {
                return Parent.ednTagHandler(
                    T,
                    tag.src(p.tokenizer.src),
                    tagged.src(p.tokenizer.src),
                    p.tokenizer.src,
                    next,
                    options.userdata,
                );
            }
        }
        return error.MissingHandler;
    }

    return switch (@typeInfo(T)) {
        .@"struct" => parseStruct(T, p, options),
        .@"union" => u: {
            const Fe = std.meta.FieldEnum(T);
            _ = try expectNext(.lcurly, error.InvalidUnion, p);
            const c = p.tokenizer.next();
            const field = switch (c.tag) {
                .keyword => blk: {
                    const key = Value{ .keyword = c };
                    break :blk std.meta.stringToEnum(Fe, key.keyword.src(p.tokenizer.src)[1..]) orelse
                        return error.InvalidStructField;
                },
                else => {
                    return error.InvalidUnion;
                },
            };
            debug("{s: <[1]}parseType union key {2s}", .{ "", p.depth * 2, @tagName(field) });
            const u = switch (field) {
                inline else => |tag| @unionInit(
                    T,
                    @tagName(tag),
                    try parseType(@FieldType(T, @tagName(tag)), T, p, options),
                ),
            };
            if (consume(.rcurly, p) == null) return error.InvalidUnion;
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
                else => try parseType(i.child, P, p, options),
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
                ele.* = try parseType(i.child, P, p, options);
            }
            _ = try expectNext(.rbracket, error.InvalidArray, p);
            break :blk a;
        },
        else => unsupportedType(T),
    };
}

fn parseStruct(comptime T: type, p: *Parser, options: Options) !T {
    const Fe = std.meta.FieldEnum(T);
    var seen_fields = std.enums.EnumSet(Fe).initEmpty();

    debug("{s: <[1]}parseStruct({2s})", .{ "", p.depth * 2, @typeName(T) });
    defer debug("{s: <[1]}<parseStruct({2s})", .{ "", p.depth * 2, @typeName(T) });
    p.depth += 1;
    defer p.depth -= 1;
    _ = try expectNext(.lcurly, error.InvalidStruct, p);

    var t: T = undefined;

    while (true) {
        const c = p.tokenizer.next();
        debug("{s: <[1]}parseStruct '{2s}'", .{ "", p.depth * 2, @tagName(c.tag) });
        if (c.tag == .rcurly) break;

        const field = switch (c.tag) {
            .keyword => blk: {
                const key = Value{ .keyword = c };
                break :blk std.meta.stringToEnum(Fe, key.keyword.src(p.tokenizer.src)[1..]) orelse
                    return error.InvalidStructField;
            },
            else => return error.InvalidStruct,
        };
        debug("{s: <[1]}parseStruct key {2s}", .{ "", p.depth * 2, @tagName(field) });

        switch (field) {
            inline else => |tag| {
                @field(t, @tagName(tag)) = try parseType(@FieldType(T, @tagName(tag)), T, p, options);
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
                return error.MissingFields;
            }
        },
    };
    if (seen_fields.complement().count() != 0) return error.MissingFields;
    return t;
}

fn formatParseResult(
    data: struct { src: [:0]const u8, result: *const Result },
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    try formatValue(.{
        .value = &data.result.values.items[0],
        .src = data.src,
        .result = data.result,
    }, fmt, options, writer);
}

fn formatValue(
    data: struct {
        value: *const Value,
        src: [:0]const u8,
        result: *const Result,
    },
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    const have_ws = data.result.whitespaces.items.len > 0;
    const index = data.value - data.result.values.items.ptr;
    // std.debug.print("{s} index {} data.value {*}\n", .{ @tagName(data.value.*), index, data.value });
    if (index >= data.result.values.items.len) return error.CorruptData;
    if (have_ws and index < data.result.whitespaces.items.len) {
        // print leading whitespace
        const ws = data.result.whitespaces.items[index];
        try writer.writeAll(data.src[ws[0]..ws[1]]);
    }
    const v = data.value.*;
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
        .tagged => |*tagged| {
            const src = tagged.tag.src(data.src);
            try writer.writeAll(src);
            // FIXME - this hack allows '#foo 1' to print correctly instead of '#foo  1' when .whitespace = false.
            if (!have_ws and (src.len == 0 or !std.ascii.isWhitespace(src[src.len - 1]))) {
                try writer.writeByte(' ');
            }
            try std.fmt.formatType(tagged.value.formatter(data.src, data.result), fmt, options, writer, 0);
        },
        inline .list, .vector, .set, .map => |l, tag| {
            // std.debug.print("{} {*} {*}\n", .{ index, data.value, data.result.values.items.ptr });
            if (!have_ws and index != 0) {
                switch (tag) {
                    .list => try writer.writeByte('('),
                    .vector => try writer.writeByte('['),
                    .set => try writer.writeAll("#{"),
                    .map => try writer.writeByte('{'),
                    else => {},
                }
            }

            // visit children
            var iter = data.result.iterator(data.value);
            var i: usize = 0;
            while (iter.next()) |child_id| : (i += 1) {
                const child = &data.result.values.items[child_id];
                if (!have_ws and i > 0) try writer.writeByte(' ');
                try std.fmt.formatType(child.formatter(data.src, data.result), fmt, options, writer, 0);
            }
            try writer.writeAll(data.src[l.trailing_ws[0]..l.trailing_ws[1]]);
        },
    }
}

fn consume(tag: Token.Tag, p: *Parser) ?Token {
    if (p.tokenizer.isTag(tag)) {
        return p.tokenizer.next();
    }
    return null;
}

fn expectNext(tag: Token.Tag, e: Error, p: *Parser) Error!Token {
    const t = p.tokenizer.next();
    if (t.tag != tag) {
        debug("expectNext wanted '{s}' got '{s}'", .{ @tagName(tag), t.tag.lexeme() });
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
    const char: u21 = if (src.len == 0)
        return error.InvalidChar
    else if (src.len == 1)
        src[0]
    else switch (src[0]) {
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

fn storeValue(p: *Parser, loc: Token.Loc, v: Value, mparent: ?Value.Id, options: Options) Value.Id {
    const id = p.res.values.items.len;
    if (options.allocate) {
        debug("{s: <[1]}storeValue('{2s}') id {3}", .{ "", p.depth * 2, loc.src(p.tokenizer.src), id });
        p.res.values.appendAssumeCapacity(v);
        if (options.whitespace) p.res.whitespaces.appendAssumeCapacity(.{ loc.ws_start, loc.start });
        p.res.children.appendAssumeCapacity(Value.value_id_max);
        p.res.siblings.appendAssumeCapacity(Value.value_id_max);
        if (mparent) |parent| {
            const pv = &p.res.values.items[parent];
            switch (pv.*) {
                inline .list, .vector, .set, .map => |_, tag| {
                    @field(pv, @tagName(tag)).len += 1;
                    // TODO maybe use an iterator.  TODO Parser.result field
                    var child_id = p.res.children.items[parent];
                    if (child_id == Value.value_id_max) {
                        // If parent has no children yet, this is the first child
                        p.res.children.items[parent] = id;
                    } else {
                        // Otherwise, find the last sibling and update its next_sibling
                        while (p.res.siblings.items[child_id] != Value.value_id_max)
                            child_id = p.res.siblings.items[child_id];
                        p.res.siblings.items[child_id] = id;
                    }
                },
                .tagged => {
                    assert(p.res.children.items[parent] == Value.value_id_max);
                    p.res.children.items[parent] = id;
                },

                else => unreachable,
            }
        }
    } else {
        p.res.values.items.len += 1;
        p.res.whitespaces.items.len += 1;
        p.res.children.items.len += 1;
        p.res.siblings.items.len += 1;
    }
    if (options.whitespace) assert(p.res.values.items.len == p.res.whitespaces.items.len);

    assert(p.res.values.items.len == p.res.children.items.len);
    assert(p.res.values.items.len == p.res.siblings.items.len);
    return id;
}

fn tokenToValueTag(tag: Token.Tag) Value.Tag {
    return switch (tag) {
        .lparen, .eof => .list,
        .lbracket => .vector,
        .set => .set,
        else => unreachable,
    };
}

/// handles lists, vectors, sets and maps.
/// prefix.tag == .eof is used at top level which is treated as an implicit
/// list with no parens. maps are parsed with options.stride = 2.  discards with
/// options.store = false.
fn parseList(
    p: *Parser,
    end_tag: Token.Tag,
    invalid_tags: []const Token.Tag,
    parent: Value.Id,
    options: Options,
) Error!Value.List {
    p.depth += 1;
    defer p.depth -= 1;

    var list: Value.List = .{};
    outer: while (true) : (list.len += 1) {
        for (0..options.stride) |_| {
            const t = p.tokenizer.next();
            // debug("{s: <[1]}parseList('{2s}') {3} {4} {5any}", .{ "", p.depth * 2, t.src(p.tokenizer.src), t.tag, end_tag, invalid_tags });
            if (t.tag == end_tag) {
                list.trailing_ws = .{ t.loc.ws_start, t.loc.end };
                p.last_token = t;
                break :outer;
            } else if (mem.indexOfScalar(Token.Tag, invalid_tags, t.tag)) |_| {
                list.trailing_ws = .{ t.loc.ws_start, t.loc.end };
                p.last_token = t;
                return error.UnclosedContainer;
            }
            _ = parseValue(p, t, parent, options) catch |e| switch (e) {
                error.EndOfMap, error.Eof => {
                    // exit(p, t, &list);
                    // if (true) unreachable;
                    list.trailing_ws = .{ t.loc.ws_start, t.loc.end };
                    p.last_token = t;
                    break :outer;
                },
                else => return e,
            };
        }
    }

    return list;
}

fn parseTagged(p: *Parser, t: Token, parent: Value.Id, options: Options) Error!Value {
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
    const next_id, const next = try parseValue(p, t2, parent, options);
    const v = if (p.handlers.get(t.src(p.tokenizer.src))) |handler| v: {
        const cur = p.tokenizer.peek();
        const v = try handler(
            next,
            p.tokenizer.src[t2.loc.start..cur.loc.ws_start],
            p.tokenizer.src,
            options.userdata,
        );
        break :v v;
    } else next;

    if (options.allocate and options.store) {
        p.res.values.items[parent].tagged = .{ .tag = t, .value = &p.res.values.items[next_id] };
        p.res.values.items[next_id] = v;
    }
    return v;
}

// TODO allow users to skip expensive parsing such as parsing integers
pub fn userParseValue(p: *Parser, options: Options) !Value {
    _, const val = try parseValue(p, p.tokenizer.next(), null, options);
    return val;
}

fn parseValue(p: *Parser, t: Token, parent: ?Value.Id, options: Options) !struct { Value.Id, Value } {
    // std.debug.print("{s: <[1]}{2s}:\n  ws '{3s}'\n  content '{4s}'\n", .{ "", p.depth * 2, t.tag.lexeme(), p.tokenizer.src[t.loc.ws_start..t.loc.start], t.src(p.tokenizer.src) });
    debug("{s: <[1]}parseValueInner('{2s}')", .{
        "",
        p.depth * 2,
        t.src(p.tokenizer.src),
    });
    if (p.last_token.loc.end == t.loc.start and
        !t.tag.isClosing() and
        !p.last_token.tag.isOpening())
    {
        return error.MissingWhitespaceBetweenValues;
    }
    p.last_token = t;

    const ws = Token.Loc{ .ws_start = t.loc.ws_start, .start = t.loc.end, .end = t.loc.end };
    const value: Value = switch (t.tag) {
        .nil => .nil,
        .true => .true,
        .false => .false,
        .int => .{ .integer = try std.fmt.parseInt(i128, t.src(p.tokenizer.src), 0) },
        .float => .{ .float = t },
        .str => .{ .string = t },
        .char => try parseChar(p, t),
        .discard => {
            const t2 = p.tokenizer.next();
            _, const discard = try parseValue(p, t2, parent, options.with(.{ .store = false }));
            debug("{s: <[1]}discarded {2s} '{3s}'", .{ "", p.depth * 2, @tagName(discard), t2.src(p.tokenizer.src) });
            // TODO merge tokens. include discard in whitespace somehow.  TODO add test for discard format
            const t3 = p.tokenizer.next();
            return try parseValue(p, t3, parent, options);
        },
        .tagged => {
            const id = storeValue(p, t.loc, .{ .tagged = undefined }, parent, options);
            return .{ id, try parseTagged(p, t, id, options) };
        },
        .keyword => .{ .keyword = t },
        .symbol => .{ .symbol = t },
        .lparen => {
            const id = storeValue(p, ws, .{ .list = .{} }, parent, options);
            const v = Value{ .list = try parseList(p, .rparen, &.{ .rbracket, .rcurly, .eof }, id, options) };
            if (options.allocate) p.res.values.items[id] = v;
            return .{ id, v };
        },
        .lbracket => {
            const id = storeValue(p, ws, .{ .vector = .{} }, parent, options);
            const v = Value{ .vector = try parseList(p, .rbracket, &.{ .rparen, .rcurly, .eof }, id, options) };
            if (options.allocate) p.res.values.items[id] = v;
            return .{ id, v };
        },
        .set => {
            const id = storeValue(p, ws, .{ .set = .{} }, parent, options);
            const v = Value{ .set = try parseList(p, .rcurly, &.{ .rparen, .rbracket, .eof }, id, options) };
            if (options.allocate) p.res.values.items[id] = v;
            return .{ id, v };
        },
        .lcurly => {
            const id = storeValue(p, ws, .{ .map = .{} }, parent, options);
            const v = Value{ .map = try parseList(p, .rcurly, &.{ .rparen, .rbracket, .eof }, id, options.with(.{ .stride = 2 })) };
            if (options.allocate) p.res.values.items[id] = v;
            return .{ id, v };
        },
        .rcurly => return error.EndOfMap,
        .eof => return error.Eof,
        .invalid, .rparen, .rbracket => return error.InvalidToken,
    };

    return .{
        if (options.store) storeValue(p, t.loc, value, parent, options) else p.res.values.items.len,
        value,
    };
}

fn printDiagnostic(p: *const Parser, e: Error, t: Token, options: Options) void {
    if (options.diagnostic) |diag| { // user wants a diagnostic
        diag.* = .{ .line = 0, .column = 0 };
        for (p.tokenizer.src[0..t.loc.start]) |c| {
            if (c == '\n') {
                diag.line += 1;
                diag.column = 0;
            } else {
                diag.column += 1;
            }
        }
        var fbs = std.io.fixedBufferStream(&diag.error_message);
        if (diag.file_path) |file_path| {
            fbs.writer().print("error: {s}:{}:{} {s}. source '{s}'.\n", .{
                file_path,
                diag.line,
                diag.column,
                @errorName(e),
                t.src(p.tokenizer.src),
            }) catch {};
        } else {
            fbs.writer().print("error: {}:{} {s}. source '{s}'.\n", .{
                diag.line,
                diag.column,
                @errorName(e),
                t.src(p.tokenizer.src),
            }) catch {};
        }
        fbs.writer().writeByte(0) catch {};
    }
}

const log = std.log.scoped(.edn);
fn debug(comptime fmt: []const u8, args: anytype) void {
    _ = fmt; // autofix
    _ = args; // autofix
    if (@inComptime()) {
        // @compileLog(std.fmt.comptimePrint(fmt, args));
    } else {
        // log.debug(fmt, args);
        // std.debug.print(fmt ++ "\n", args);
    }
}

const assert = std.debug.assert;

pub const Tokenizer = @import("Tokenizer.zig");

pub const Parser = @This();
tokenizer: Tokenizer,
res: Result,
depth: u8, // only used when logging
handlers: TaggedElementHandler.Map,
/// the last token seen.  used to track required whitespace between tokens.
last_token: Token = .{
    .tag = undefined,
    .loc = .{ .ws_start = 0, .start = 0, .end = std.math.maxInt(u32) },
},

pub fn init(
    src: [:0]const u8,
    comptime handlers: []const TaggedElementHandler.Data,
) !Parser {
    return .{
        .tokenizer = try .init(src),
        .depth = 0,
        .res = .init,
        .handlers = .initComptime(handlers),
    };
}

pub fn initFixed(
    src: [:0]const u8,
    values: []Value,
    whitespaces: []Span,
    children: []Value.Id,
    siblings: []Value.Id,
    comptime handlers: []const TaggedElementHandler.Data,
) !Parser {
    return .{
        .tokenizer = try .init(src),
        .depth = 0,
        .res = .{
            .values = .initBuffer(values),
            .whitespaces = .initBuffer(whitespaces),
            .children = .initBuffer(children),
            .siblings = .initBuffer(siblings),
        },
        .handlers = .initComptime(handlers),
    };
}

const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
