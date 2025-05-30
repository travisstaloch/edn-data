const std = @import("std");

const edn = @import("extensible-data-notation");

pub const std_options = std.Options{ .log_level = .warn };

fn usage() void {
    std.debug.print(
        \\usage <options?> <file>
        \\
        \\options:
        \\  --json-to-edn: convert json file to edn
        \\
    , .{});
}

pub fn main() !void {
    mainInner() catch |e| {
        std.debug.print("{s}\n", .{@errorName(e)});
        usage();
    };
}

fn mainInner() !void {
    const alloc = std.heap.smp_allocator;
    const args = try std.process.argsAlloc(alloc);
    defer std.process.argsFree(alloc, args);

    if (args.len < 2) return error.ArgsLen;
    var bw = std.io.bufferedWriter(std.io.getStdOut().writer());
    const stdout = bw.writer();

    if (std.mem.eql(u8, args[1], "--json-to-edn")) {
        if (args.len < 3) return error.Args;
        const file = try std.fs.cwd().openFile(args[2], .{});
        defer file.close();
        const src = try file.readToEndAllocOptions(alloc, 1024 * 1024, null, 8, 0);
        defer alloc.free(src);
        const json = try std.json.parseFromSlice(std.json.Value, alloc, src, .{});
        defer json.deinit();
        try edn.writeFromJson(json.value, stdout, .{ .whitespace = .indent_2 });
        try stdout.writeAll("\n");
        try bw.flush();
        return;
    }

    const file = if (std.mem.eql(u8, args[1], "-")) std.io.getStdIn() else try std.fs.cwd().openFile(args[1], .{});
    defer file.close();
    const src = try file.readToEndAllocOptions(alloc, 1024 * 1024, null, 8, 0);
    defer alloc.free(src);
    var diag: edn.Diagnostic = .{ .file_path = args[1] };
    const result = edn.parseFromSlice(edn.Result, src, .{ .diagnostic = &diag, .allocator = alloc }, .{}) catch {
        try std.io.getStdErr().writer().print("{s}\n", .{diag.error_message});
        return;
    };
    defer result.deinit(alloc);
    try stdout.print("{}\n", .{result.formatter(src)});
    try bw.flush();
}
