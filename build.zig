const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const mod = b.addModule("extensible-data-notation", .{
        .root_source_file = b.path("src/root.zig"),
        .imports = &.{},
    });

    const exe = b.addExecutable(.{
        .name = "edn-parse",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    exe.root_module.addImport("extensible-data-notation", mod);
    b.installArtifact(exe);
    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const use_llvm = b.option(bool, "llvm", "use llvm. true by default.") orelse true;

    // tests
    const test_step = b.step("test", "Run unit tests");
    const filters = b.option([]const []const u8, "test-filters", "") orelse &.{};
    const main_tests = try addTest("src/tests.zig", "tests", b, target, optimize, filters, test_step, use_llvm);
    main_tests.root_module.addImport("extensible-data-notation", mod);
    _ = try addTest("src/ringbuffer.zig", "test-ringbuffer", b, target, optimize, filters, test_step, use_llvm);
    _ = try addTest("src/Tokenizer.zig", "test-Tokenizer", b, target, optimize, filters, test_step, use_llvm);
    _ = try addTest("src/root.zig", "test-root", b, target, optimize, filters, test_step, use_llvm);

    // bench
    const bench = b.addExecutable(.{
        .name = "edn-bench",
        .root_source_file = b.path("src/bench.zig"),
        .target = target,
        .optimize = optimize,
    });
    bench.root_module.addImport("extensible-data-notation", mod);
    b.installArtifact(bench);

    // fuzz
    const fuzz = b.addStaticLibrary(.{
        .name = "fuzz",
        .root_source_file = b.path("src/tests.zig"),
        .target = target,
        .optimize = optimize,
    });
    fuzz.root_module.addImport("extensible-data-notation", mod);
    fuzz.bundle_compiler_rt = true;
    fuzz.want_lto = true;
    fuzz.pie = true;
    b.installArtifact(fuzz);
}

fn addTest(file_path: []const u8, name: []const u8, b: *std.Build, target: anytype, optimize: anytype, filters: anytype, test_step: anytype, use_llvm: bool) !*std.Build.Step.Compile {
    const tests = b.addTest(.{
        .root_source_file = b.path(file_path),
        .target = target,
        .optimize = optimize,
        .name = name,
    });
    tests.filters = filters;
    tests.use_lld = use_llvm;
    tests.use_llvm = use_llvm;
    const run_tests = b.addRunArtifact(tests);
    run_tests.has_side_effects = true;
    test_step.dependOn(&run_tests.step);
    b.installArtifact(tests);
    return tests;
}
