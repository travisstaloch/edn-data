/// a multi array list for tagged union types which stores payloads for each
/// field of `T` in its own list.  this is a contrast to std.MultiArrayList
/// which stores the payloads together in a single list of bare (untagged)
/// union.  the idea here is to use less memory and optimize for iterating over
/// items of with same field and payload type.
///
/// this type also keeps a list of unique ids (Id) across all payload.
pub fn UnionMultiArrayList(T: type) type {
    const info = @typeInfo(T);
    if (info != .@"union" or info.@"union".tag_type == null)
        @compileError("UnionMultiArrayList only supports tagged unions");
    const uinfo = info.@"union";

    return struct {
        ids: Ids = .{},
        /// a struct with a field of List(Payload) for each field type from T.
        payloads: Payloads = .{},

        /// a type safe u32 representing for indexing into self.ids
        pub const Id = enum(u32) {
            _,
            pub fn int(id: Id) u32 {
                return @intFromEnum(id);
            }
        };
        pub const Ids = std.ArrayListUnmanaged(FieldId);

        fn List(U: type) type {
            return std.ArrayListUnmanaged(U);
        }

        /// a struct with a field of List(Payload) for each field of T
        pub const Payloads = blk: {
            const StructField = std.builtin.Type.StructField;
            var fields: []const StructField = &.{};
            for (uinfo.fields) |f| {
                fields = fields ++ [1]StructField{.{
                    .name = f.name,
                    .type = List(f.type),
                    .default_value_ptr = &List(f.type){},
                    .alignment = @alignOf(List(f.type)),
                    .is_comptime = false,
                }};
            }
            break :blk @Type(.{ .@"struct" = .{
                .fields = fields,
                .layout = .auto,
                .decls = &.{},
                .is_tuple = false,
            } });
        };

        /// std.meta.FieldEnum but with tag_type set to allow use in packed struct FieldId
        pub const Field = @Type(.{
            .@"enum" = .{
                .fields = @typeInfo(std.meta.FieldEnum(T)).@"enum".fields,
                .decls = &.{},
                .is_exhaustive = true,
                .tag_type = std.meta.Int(.unsigned, std.math.log2_int_ceil(usize, uinfo.fields.len)),
            },
        });

        /// a combined field and id.  id can be used to index self.payloads.<field>.
        pub const FieldId = packed struct(u32) {
            /// index to to a List(Payload)
            id: @Type(.{ .int = .{ .bits = 32 - @bitSizeOf(Field), .signedness = .unsigned } }),
            field: Field,

            pub fn int(self: @This()) u32 {
                return @bitCast(self);
            }
        };

        inline fn payloadField(Payload: type) Field {
            inline for (uinfo.fields, 0..) |f, i| {
                if (f.type == Payload) return @enumFromInt(i);
            }

            @compileError("unexpected type. expected a tag Payload from '" ++ @typeName(T) ++ "'. got '" ++ @typeName(Payload) ++ "'");
        }

        fn PayloadPtr(SelfType: type, comptime field: Field) type {
            return if (@typeInfo(SelfType).pointer.is_const)
                *const @FieldType(T, @tagName(field))
            else
                *@FieldType(T, @tagName(field));
        }

        fn ListPtr(SelfType: type, comptime field: Field) type {
            return if (@typeInfo(SelfType).pointer.is_const)
                *const List(@FieldType(T, @tagName(field)))
            else
                *List(@FieldType(T, @tagName(field)));
        }

        const Self = @This();

        // /// Initialize with externally-managed memory. The buffer determines the
        // /// capacity, and the length is set to zero.
        // /// When initialized this way, all functions that accept an Allocator
        // /// argument cause illegal behavior.
        // pub fn initBuffer(ids: []FieldId, payloads: Payloads) Self {
        //     return .{
        //         .ids = Ids.initBuffer(ids),
        //         .payloads = payloads,
        //     };
        // }

        pub fn deinit(self: *Self, allocator: mem.Allocator) void {
            inline for (uinfo.fields) |f| {
                @field(self.payloads, f.name).deinit(allocator);
            }
            self.ids.deinit(allocator);
        }

        pub fn ensureTotalCapacity(self: *Self, allocator: mem.Allocator, n: usize) !void {
            try self.ids.ensureTotalCapacity(allocator, n);
        }

        /// return an opaque pointer to a *Payload
        pub fn ptrErased(self: anytype, field_id: FieldId) ?*anyopaque {
            switch (field_id.field) {
                inline else => |t| {
                    const list = getList(self, t);
                    return if (field_id.id < list.items.len) &list.items[field_id.id] else null;
                },
            }
            unreachable;
        }

        /// (field_id, tag) => *Payload
        pub fn ptr(self: anytype, field_id: FieldId, comptime field: Field) ?PayloadPtr(@TypeOf(self), field) {
            assert(field_id.field == field);
            return @ptrCast(@alignCast(self.ptrErased(field_id)));
        }

        pub fn getList(self: anytype, comptime field: Field) ListPtr(@TypeOf(self), field) {
            return &@field(self.payloads, @tagName(field));
        }

        /// payload => (id, field_id)
        /// append payload to the appropriate payload list.
        /// returns an (id, field_id) pair which can each be used to address the payload.
        pub fn append(self: *Self, allocator: mem.Allocator, payload: anytype) !struct { Id, FieldId } {
            const tag = payloadField(@TypeOf(payload));
            const field_id = FieldId{
                .field = tag,
                .id = @intCast(self.getList(tag).items.len),
            };
            try self.getList(tag).append(allocator, payload);
            const id: Id = @enumFromInt(self.ids.items.len);
            try self.ids.append(allocator, field_id);
            return .{ id, field_id };
        }

        /// (id, tag) => *Payload
        pub fn ptrById(
            self: anytype,
            id: Id,
            comptime field: Field,
        ) ?*@FieldType(T, @tagName(field)) {
            if (id.int() >= self.ids.items.len) return null;
            const field_id = self.ids.items[id.int()];
            assert(field_id.field == field);

            const list = self.getList(field);
            return if (field_id.id < list.items.len) &list.items[field_id.id] else null;
        }

        /// (id, tag) => *Payload
        pub fn ptrErasedById(self: anytype, id: Id) ?*anyopaque {
            if (id.int() >= self.ids.items.len) return null;
            const field_id = self.ids.items[id.int()];

            switch (field_id.field) {
                inline else => |tag| {
                    const list = self.getList(tag);
                    return if (field_id.id < list.items.len) &list.items[field_id.id] else null;
                },
            }
            unreachable;
        }

        /// add n new ids to be later assigned. can be assigned with appendAtId()
        pub fn addIds(self: *Self, allocator: mem.Allocator, n: u32) ![]FieldId {
            try self.ensureTotalCapacity(allocator, n);
            return self.addIdsAssumeCapacity(n);
        }

        pub fn addIdsAssumeCapacity(self: *Self, n: u32) []FieldId {
            return self.ids.addManyAsSliceAssumeCapacity(n);
        }

        pub fn appendAtId(self: *Self, allocator: mem.Allocator, id: Id, payload: anytype) !FieldId {
            if (id.int() >= self.ids.items.len) return error.OutOfBoundsId;
            const tag = payloadField(@TypeOf(payload));
            const list = self.getList(tag);
            const field_id = FieldId{ .field = tag, .id = @intCast(list.items.len) };
            try list.append(allocator, payload);
            self.ids.items[id.int()] = field_id;
            return field_id;
        }
    };
}

test UnionMultiArrayList {
    const Function = struct { id: i32 };
    const Variable = struct { id: enum(u32) { _ } };
    const Expression = struct { id: u32 };

    const Node = union(enum) {
        function: Function,
        variable: Variable,
        expression: Expression,
    };

    const L = UnionMultiArrayList(Node);
    var l: L = .{};
    defer l.deinit(talloc);
    var prng = std.Random.DefaultPrng.init(0);
    const random = prng.random();
    for (0..100) |_| {
        switch (random.enumValue(L.Field)) {
            inline else => |tag_static| {
                // init payload
                const payload = switch (tag_static) {
                    .function => Function{ .id = random.int(i32) },
                    .variable => Variable{ .id = @enumFromInt(random.int(u32)) },
                    .expression => Expression{ .id = random.int(u32) },
                };
                const P = @TypeOf(payload);
                // Payload => (id, field_id)
                const id, const field_id = try l.append(talloc, payload);
                // Payload => tag
                try std.testing.expectEqual(tag_static, L.payloadField(P));
                // field_id => ?*anyopaque
                const payload2: *P = @ptrCast(@alignCast(l.ptrErased(field_id)));
                try std.testing.expectEqual(payload, payload2.*);
                // (tag, Id) => *Payload
                try std.testing.expectEqual(payload, l.ptrById(id, tag_static).?.*);
            },
        }

        // use case: reserve n consecutive ids then assign them
        const N = 100;
        const start_id = l.ids.items.len;
        try l.ids.resize(talloc, start_id + N);

        for (start_id..start_id + N) |i| {
            const id: L.Id = @enumFromInt(i);
            switch (random.enumValue(L.Field)) {
                inline else => |field_static| {
                    const payload = switch (field_static) {
                        .function => Function{ .id = random.int(i32) },
                        .variable => Variable{ .id = @enumFromInt(random.int(u32)) },
                        .expression => Expression{ .id = random.int(u32) },
                    };
                    const P = @TypeOf(payload);
                    const field_id = try l.appendAtId(talloc, id, payload);
                    { // field_id -> ?*anyopaque
                        const payload2: *P = @ptrCast(@alignCast(l.ptrErased(field_id)));
                        try std.testing.expectEqual(payload, payload2.*);
                    }
                    // (field_id, field) => ?*Payload
                    try std.testing.expectEqual(payload, l.ptr(field_id, field_static).?.*);

                    { // id -> ?*anyopaque
                        const payload2: *P = @ptrCast(@alignCast(l.ptrErasedById(id)));
                        try std.testing.expectEqual(payload, payload2.*);
                    }
                    // (id, field) -> ?*Payload
                    try std.testing.expectEqual(payload, l.ptrById(id, field_static).?.*);
                },
            }
        }
    }
}

const std = @import("std");
const talloc = std.testing.allocator;
const assert = std.debug.assert;
const mem = std.mem;
