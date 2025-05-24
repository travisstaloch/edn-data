const std = @import("std");

const edn = @import("extensible-data-notation");

pub const std_options = std.Options{
    .log_level = .warn,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();
    const args = try std.process.argsAlloc(alloc);
    defer std.process.argsFree(alloc, args);
    const file = try std.fs.cwd().openFile(args[1], .{});
    const src = try file.readToEndAllocOptions(alloc, 1024 * 1024, null, 8, 0);
    defer alloc.free(src);
    var diag: edn.Diagnostic = .{ .file_path = args[1] };
    const result = edn.parseFromSlice(edn.Result, src, .{ .diagnostic = &diag, .allocator = alloc }, .{}) catch {
        try std.io.getStdErr().writer().print("{s}\n", .{diag.error_message});
        return;
    };
    defer result.deinit(alloc);
    try std.io.getStdOut().writer().print("{}\n", .{result.formatter(src)});
}
