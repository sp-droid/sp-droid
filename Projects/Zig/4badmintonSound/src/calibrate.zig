const std = @import("std");
const model = @import("model.zig");
const simulator = @import("simulator.zig");

pub fn main(init: std.process.Init) !void {
    const allocator = std.heap.smp_allocator;
    var reference = model.Profile{};
    reference.hit.main_tension_lbf = 23.0;
    reference.hit.cross_tension_lbf = 23.0;
    try report(init.io, allocator, "23/23 lb, 5 m/s touch reference", reference);
    try report(init.io, allocator, "27/28 lb, 5 m/s default", model.Profile{});
}

fn report(
    io: std.Io,
    allocator: std.mem.Allocator,
    label: []const u8,
    profile: model.Profile,
) !void {
    const started = std.Io.Clock.Timestamp.now(io, .awake);
    var result = try simulator.simulate(allocator, profile);
    defer result.deinit();
    const finished = std.Io.Clock.Timestamp.now(io, .awake);
    const elapsed_ns = started.durationTo(finished).raw.nanoseconds;
    const peak_dbfs = if (result.diagnostics.peak_before_clamp > 0.0)
        20.0 * std.math.log10(result.diagnostics.peak_before_clamp)
    else
        -std.math.inf(f64);
    std.debug.print(
        \\{s}
        \\  nodes: {d}
        \\  substeps: {d}
        \\  contact: {d:.3} ms
        \\  peak force: {d:.2} N
        \\  max deflection: {d:.3} mm
        \\  dominant peak: {d:.1} Hz
        \\  apparent restitution: {d:.3}
        \\  RMS: {d:.7}
        \\  raw fixed-gain peak: {d:.7}
        \\  peak: {d:.7} ({d:.2} dBFS)
        \\  compressor reduction: {d:.2} dB
        \\  limiter samples: {d}
        \\  clipped samples: {d}
        \\  render: {d:.2} ms
        \\
    , .{
        label,
        result.diagnostics.node_count,
        result.diagnostics.numerical_substeps,
        result.diagnostics.contact_duration_ms,
        result.diagnostics.peak_force_n,
        result.diagnostics.maximum_deflection_mm,
        result.diagnostics.dominant_frequency_hz,
        result.diagnostics.apparent_restitution,
        result.diagnostics.rms,
        result.diagnostics.peak_before_dynamics,
        result.diagnostics.peak_before_clamp,
        peak_dbfs,
        result.diagnostics.maximum_gain_reduction_db,
        result.diagnostics.limited_samples,
        result.diagnostics.clipped_samples,
        @as(f64, @floatFromInt(elapsed_ns)) / 1_000_000.0,
    });
    reportTonalPeaks(result.spectrum_db, result.output_sample_rate_hz);
}

fn reportTonalPeaks(spectrum: []const f32, sample_rate: u32) void {
    if (spectrum.len < 3) return;
    var selected_bins: [6]usize = @splat(0);
    var selected_db: [6]f32 = @splat(-std.math.inf(f32));
    const fft_size = (spectrum.len - 1) * 2;
    const bin_hz = @as(f64, @floatFromInt(sample_rate)) /
        @as(f64, @floatFromInt(fft_size));
    for (spectrum[1 .. spectrum.len - 1], 1..) |level, bin| {
        const hz = @as(f64, @floatFromInt(bin)) * bin_hz;
        if (hz < 300.0 or hz > 5000.0 or
            level <= spectrum[bin - 1] or level < spectrum[bin + 1]) continue;
        var insertion: usize = selected_db.len;
        for (selected_db, 0..) |existing, index| {
            if (level > existing) {
                insertion = index;
                break;
            }
        }
        if (insertion == selected_db.len) continue;
        var move_index = selected_db.len - 1;
        while (move_index > insertion) : (move_index -= 1) {
            selected_db[move_index] = selected_db[move_index - 1];
            selected_bins[move_index] = selected_bins[move_index - 1];
        }
        selected_db[insertion] = level;
        selected_bins[insertion] = bin;
    }
    std.debug.print("  tonal peaks:", .{});
    for (selected_bins, selected_db) |bin, level| {
        if (!std.math.isFinite(level)) continue;
        std.debug.print(
            " {d:.1} Hz/{d:.1} dB",
            .{ @as(f64, @floatFromInt(bin)) * bin_hz, level },
        );
    }
    std.debug.print("\n", .{});
}
