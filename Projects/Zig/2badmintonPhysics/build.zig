const std = @import("std");
const rlz = @import("raylib_zig");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const raylib_dep = b.dependency("raylib_zig", .{
        .target = target,
        .optimize = optimize,
        .platform = rlz.PlatformBackend.glfw,
    });

    const app_module = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    app_module.addImport("raylib", raylib_dep.module("raylib"));

    const exe = b.addExecutable(.{
        .name = "badminton-simulator",
        .root_module = app_module,
    });
    b.installArtifact(exe);

    const run = b.addRunArtifact(exe);
    run.step.dependOn(b.getInstallStep());
    if (b.args) |args| run.addArgs(args);
    const run_step = b.step("run", "Run the badminton simulator");
    run_step.dependOn(&run.step);

    const simulation_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/simulation.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    const physics_loop_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/physics_loop.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    const test_step = b.step("test", "Run simulation tests");
    test_step.dependOn(&b.addRunArtifact(simulation_tests).step);
    test_step.dependOn(&b.addRunArtifact(physics_loop_tests).step);
}
