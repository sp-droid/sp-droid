const std = @import("std");
const rl = @import("raylib");
const model = @import("model.zig");
const simulator = @import("simulator.zig");
const dsp = @import("dsp.zig");
const live_swoosh = @import("live_swoosh.zig");

const default_strike_count = 3;
const max_strikes = 8;
const strike_spacing_s = 1.2;
const lead_in_s = 0.3;
const tail_s = 1.6;

const Scenario = struct {
    name: [:0]const u8,
    closing_mps: f64,
    racket_mps: f64,
    x_mm: f64 = 0.0,
    // When set, one strike per tension (lbf) instead of three repeats.
    tensions: ?[]const f64 = null,
};

const tension_series = [_]f64{ 32, 30, 28, 26, 24, 22 };
const tight_net = [_]f64{ 30, 30, 30 };

/// Renders what the desktop stream plays: several successive strikes, each
/// a fresh variation, mixed with the continuous swoosh
/// and limited exactly like the live audio engine.
pub fn main() !void {
    const allocator = std.heap.smp_allocator;
    const frame_x = 0.5 * 188.0 + 0.5 * 9.0;
    const scenarios = [_]Scenario{
        .{ .name = "demo/touch_5mps.wav", .closing_mps = 5.0, .racket_mps = 5.0 },
        .{ .name = "demo/drive_18mps.wav", .closing_mps = 18.0, .racket_mps = 20.0 },
        .{ .name = "demo/clear_30mps.wav", .closing_mps = 30.0, .racket_mps = 30.0 },
        .{ .name = "demo/smash_60mps.wav", .closing_mps = 60.0, .racket_mps = 50.0 },
        .{ .name = "demo/frame_hit_20mps.wav", .closing_mps = 20.0, .racket_mps = 15.0, .x_mm = frame_x },
        // Soft net shots on a tight bed: the crisp case of reference/net.mp3.
        .{ .name = "demo/net_shot_30lb_4mps.wav", .closing_mps = 4.0, .racket_mps = 4.0, .tensions = &tight_net },
        // Steps through the chart-fitted tension range (22-32 lbf).
        .{ .name = "demo/tension_series_32to22lbs.wav", .closing_mps = 15.0, .racket_mps = 12.0, .tensions = &tension_series },
    };
    for (scenarios) |scenario| try renderScenario(allocator, scenario);
}

fn renderScenario(allocator: std.mem.Allocator, scenario: Scenario) !void {
    var profile = model.Profile{};
    profile.hit.relative_normal_speed_mps = scenario.closing_mps;
    profile.hit.racket_speed_mps = scenario.racket_mps;
    profile.hit.x_mm = scenario.x_mm;
    profile.model.visualization_duration_ms = 1.0;
    const strike_count: usize = if (scenario.tensions) |list| list.len else default_strike_count;

    const rate: usize = live_swoosh.sample_rate_hz;
    const total_s = lead_in_s +
        @as(f64, @floatFromInt(strike_count - 1)) * strike_spacing_s +
        profile.model.duration_s + tail_s;
    const output = try allocator.alloc(f32, @intFromFloat(@ceil(total_s * @as(f64, @floatFromInt(rate)))));
    defer allocator.free(output);
    @memset(output, 0.0);

    var impact_times_buffer: [max_strikes]f64 = undefined;
    const impact_times_s = impact_times_buffer[0..strike_count];
    var peak_dry: f64 = 0.0;
    for (0..strike_count) |strike| {
        var strike_profile = profile;
        strike_profile.hit.variation_seed = @intCast(strike + 1);
        if (scenario.tensions) |list| {
            strike_profile.hit.main_tension_lbf = list[strike];
            strike_profile.hit.cross_tension_lbf = list[strike];
        }
        var result = try simulator.simulate(allocator, strike_profile);
        defer result.deinit();
        if (result.output_sample_rate_hz != rate) return error.UnexpectedRate;
        const start_s = lead_in_s + @as(f64, @floatFromInt(strike)) * strike_spacing_s;
        const start: usize = @intFromFloat(@round(start_s * @as(f64, @floatFromInt(rate))));
        impact_times_s[strike] = start_s + result.impactTimeSeconds();
        for (result.impact_only_audio, 0..) |sample, index| {
            if (start + index < output.len) output[start + index] += sample;
            peak_dry = @max(peak_dry, @abs(@as(f64, sample)));
        }
    }

    var swoosh = live_swoosh.Generator.init();
    const swoosh_control = live_swoosh.Control.fromProfile(profile);

    const ceiling: f32 = @floatCast(std.math.pow(f64, 10.0, profile.model.limiter_ceiling_dbfs / 20.0));
    var limited: usize = 0;
    var block_start: usize = 0;
    while (block_start < output.len) : (block_start += 512) {
        const block_end = @min(output.len, block_start + 512);
        const block = output[block_start..block_end];
        // Same swing envelope the live engine applies, per 512-frame block.
        var live_control = swoosh_control;
        if (profile.model.swing_build_up_ms > 0.0) {
            const centre_s = @as(f64, @floatFromInt(block_start + block.len / 2)) /
                @as(f64, @floatFromInt(rate));
            var envelope: f64 = 0.0;
            for (impact_times_s) |impact_s| {
                envelope = @max(envelope, model.swingEnvelope(profile.model, centre_s - impact_s));
            }
            live_control.racket_speed_mps *= envelope;
        }
        swoosh.setControl(live_control);
        var wind: [512]f32 = @splat(0.0);
        swoosh.addTo(wind[0..block.len]);
        for (block, wind[0..block.len]) |*sample, gust| sample.* += gust;
        for (block) |*sample| {
            if (@abs(sample.*) > ceiling) limited += 1;
            sample.* = std.math.clamp(sample.*, -ceiling, ceiling);
        }
    }

    const wav = try dsp.buildPcm16Wav(allocator, output, @intCast(rate));
    defer allocator.free(wav);
    if (!rl.saveFileData(scenario.name, wav)) return error.CouldNotSaveDemoWav;
    std.debug.print("{s}: dry hit peak {d:.1} dBFS, limited samples {d}\n", .{
        scenario.name,
        20.0 * std.math.log10(@max(1.0e-9, peak_dry)),
        limited,
    });
}
