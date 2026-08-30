const std = @import("std");
const rl = @import("raylib");
const model = @import("model.zig");
const simulator = @import("simulator.zig");
const dsp = @import("dsp.zig");

const tensions_lbf = [_]f64{ 20.0, 24.0, 27.0, 30.0 };
const output_paths = [_][:0]const u8{
    "reference/generated/model_20lb.wav",
    "reference/generated/model_24lb.wav",
    "reference/generated/model_27lb.wav",
    "reference/generated/model_30lb.wav",
};

pub fn main() !void {
    const allocator = std.heap.smp_allocator;

    for (tensions_lbf, output_paths) |tension, output_path| {
        var profile = model.Profile{};
        profile.hit.main_tension_lbf = tension;
        profile.hit.cross_tension_lbf = tension;
        profile.hit.relative_normal_speed_mps = 60.0;
        profile.hit.x_mm = 0.0;
        profile.hit.y_mm = 0.0;
        profile.model.duration_s = 0.18;
        profile.model.visualization_duration_ms = 1.0;

        var result = try simulator.simulate(allocator, profile);
        defer result.deinit();
        const wav = try dsp.buildPcm16Wav(
            allocator,
            result.audio,
            result.output_sample_rate_hz,
        );
        defer allocator.free(wav);

        if (!rl.saveFileData(output_path, wav)) return error.CouldNotSaveReferenceWav;
        std.debug.print(
            "{d:.0} lb -> {s}: {d:.1} Hz, {d:.3} ms contact, peak {d:.4}\n",
            .{
                tension,
                output_path,
                result.diagnostics.dominant_frequency_hz,
                result.diagnostics.contact_duration_ms,
                result.diagnostics.peak_before_clamp,
            },
        );
    }
}
