const std = @import("std");
const rlz = @import("raylib_zig");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const raylib_dep = b.dependency("raylib_zig", .{
        .target = target,
        .optimize = optimize,
        .platform = rlz.PlatformBackend.glfw,
    });
    const raylib = raylib_dep.module("raylib");
    const raygui = raylib_dep.module("raygui");

    const exe_module = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    exe_module.addImport("raylib", raylib);
    exe_module.addImport("raygui", raygui);

    const exe = b.addExecutable(.{
        .name = "badminton-sound",
        .root_module = exe_module,
    });
    exe.root_module.linkLibrary(raylib_dep.artifact("raylib"));
    b.installArtifact(exe);

    const run_command = b.addRunArtifact(exe);
    run_command.step.dependOn(b.getInstallStep());
    if (b.args) |args| run_command.addArgs(args);

    const run_step = b.step("run", "Build and run the badminton sound laboratory");
    run_step.dependOn(&run_command.step);

    const tests_module = b.createModule(.{
        .root_source_file = b.path("src/tests.zig"),
        .target = target,
        .optimize = optimize,
    });
    tests_module.addImport(
        "badminton_audio_runtime",
        b.createModule(.{
            .root_source_file = b.path("runtime/src/engine.zig"),
            .target = target,
            .optimize = optimize,
        }),
    );
    const tests = b.addTest(.{ .root_module = tests_module });
    const run_tests = b.addRunArtifact(tests);
    const test_step = b.step("test", "Run deterministic headless tests");
    test_step.dependOn(&run_tests.step);

    const benchmark_module = b.createModule(.{
        .root_source_file = b.path("tools/runtime_benchmark.zig"),
        .target = target,
        .optimize = optimize,
    });
    benchmark_module.addImport(
        "badminton_audio_runtime",
        b.createModule(.{
            .root_source_file = b.path("runtime/src/engine.zig"),
            .target = target,
            .optimize = optimize,
        }),
    );
    benchmark_module.addImport(
        "lab_dsp",
        b.createModule(.{
            .root_source_file = b.path("src/dsp.zig"),
            .target = target,
            .optimize = optimize,
        }),
    );
    const benchmark = b.addExecutable(.{
        .name = "badminton-runtime-benchmark",
        .root_module = benchmark_module,
    });
    const run_benchmark = b.addRunArtifact(benchmark);
    const benchmark_step = b.step(
        "runtime-benchmark",
        "Benchmark the allocation-free continuous runtime synthesizer",
    );
    benchmark_step.dependOn(&run_benchmark.step);

    const runtime_c_module = b.createModule(.{
        .root_source_file = b.path("runtime/src/c_api.zig"),
        .target = target,
        .optimize = optimize,
        .pic = true,
    });
    const runtime_library = b.addLibrary(.{
        .name = "badminton_audio",
        .linkage = .static,
        .root_module = runtime_c_module,
    });
    runtime_library.installHeader(
        b.path("runtime/include/badminton_audio.h"),
        "badminton_audio.h",
    );
    const install_runtime = b.addInstallArtifact(runtime_library, .{});
    const runtime_step = b.step(
        "runtime",
        "Build the standalone UI-free runtime static library",
    );
    runtime_step.dependOn(&install_runtime.step);

    const calibrate = b.addExecutable(.{
        .name = "badminton-calibrate",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/calibrate.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    const run_calibrate = b.addRunArtifact(calibrate);
    const calibrate_step = b.step("calibrate", "Render and report the default reference hit");
    calibrate_step.dependOn(&run_calibrate.step);

    const reference_render_module = b.createModule(.{
        .root_source_file = b.path("src/reference_render.zig"),
        .target = target,
        .optimize = optimize,
    });
    reference_render_module.addImport("raylib", raylib);
    const reference_render = b.addExecutable(.{
        .name = "badminton-reference-render",
        .root_module = reference_render_module,
    });
    reference_render.root_module.linkLibrary(raylib_dep.artifact("raylib"));
    const run_reference_render = b.addRunArtifact(reference_render);
    const reference_render_step = b.step(
        "reference-render",
        "Render central 20/24/27/30 lb WAVs for reference comparison",
    );
    reference_render_step.dependOn(&run_reference_render.step);

    const shuttle_reference_module = b.createModule(.{
        .root_source_file = b.path("src/shuttle_reference_render.zig"),
        .target = target,
        .optimize = optimize,
    });
    shuttle_reference_module.addImport("raylib", raylib);
    const shuttle_reference = b.addExecutable(.{
        .name = "badminton-shuttle-reference-render",
        .root_module = shuttle_reference_module,
    });
    shuttle_reference.root_module.linkLibrary(raylib_dep.artifact("raylib"));
    const run_shuttle_reference = b.addRunArtifact(shuttle_reference);
    const shuttle_reference_step = b.step(
        "shuttle-reference-render",
        "Render 30 lb low-speed WAVs for the real-shuttle comparison",
    );
    shuttle_reference_step.dependOn(&run_shuttle_reference.step);
}
