const std = @import("std");

const edn = @import("extensible-data-notation");

pub const std_options = std.Options{ .log_level = .warn };

fn usage() void {
    std.debug.print(
        \\usage: <options?> <input-file>
        \\
        \\  parse <input-file> and print formatted edn to stdout.
        \\
        \\options:
        \\  --json-to-edn: convert json <input-file> to edn
        \\
        \\
    , .{});
}

pub fn main() !void {
    mainInner() catch |e| {
        std.debug.print("\nerror: {s}\n\n", .{@errorName(e)});
        usage();
    };
}

fn mainInner() !void {
    const gpa = std.heap.smp_allocator;
    const args = try std.process.argsAlloc(gpa);
    defer std.process.argsFree(gpa, args);

    if (args.len < 2) return error.MissingArgs;
    var buf: [std.heap.pageSize()]u8 = undefined;
    var stdoutw = std.fs.File.stdout().writer(&buf);
    const stdout = &stdoutw.interface;

    if (std.mem.eql(u8, args[1], "--json-to-edn")) {
        if (args.len < 3) return error.Args;
        const src = try std.fs.cwd().readFileAlloc(args[2], gpa, .unlimited);
        defer gpa.free(src);
        const json = try std.json.parseFromSlice(std.json.Value, gpa, src, .{});
        defer json.deinit();
        try edn.printFromJson(json.value, stdout, .{ .whitespace = .indent_2 });
        try stdout.writeAll("\n");
        try stdout.flush();
        return;
    }

    if (std.mem.eql(u8, args[1], "-")) @panic("TODO support reading stdin");
    const src = try std.fs.cwd().readFileAllocOptions(args[1], gpa, .unlimited, .fromByteUnits(1), 0);
    defer gpa.free(src);
    var diag: edn.Diagnostic = .{ .file_path = args[1] };
    const result = edn.parseFromSlice(edn.Result, src, .{ .diagnostic = &diag, .allocator = gpa }, .{}) catch {
        std.debug.print("{s}\n", .{diag.error_message});
        return;
    };
    defer result.deinit(gpa);
    try stdout.print("{f}\n", .{result.formatter(src)});
    try stdout.flush();
}
