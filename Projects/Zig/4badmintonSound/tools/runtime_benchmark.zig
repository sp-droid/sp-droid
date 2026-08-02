const std = @import("std");
const runtime = @import("badminton_audio_runtime");
const dsp = @import("lab_dsp");

const benchmark_block_frames: usize = 128;

const Scenario = enum {
    idle,
    typical_hits,
    eight_voice_stress,
    fast_swoosh,
};

pub fn main(init: std.process.Init) !void {
    const io = init.io;
    std.debug.print(
        "Badminton runtime benchmark (48 kHz mono, ReleaseFast)\n" ++
            "  engine bytes: {d}\n" ++
            "  engine alignment: {d}\n" ++
            "  voices / queue: {d} / {d}\n",
        .{
            @sizeOf(runtime.Engine),
            @alignOf(runtime.Engine),
            runtime.max_voices,
            runtime.event_queue_capacity,
        },
    );

    try runScenario(io, "idle continuous stream", .idle, 300.0);
    try runScenario(io, "typical: 2 hits/s", .typical_hits, 120.0);
    try runScenario(io, "stress: 8 overlapping voices", .eight_voice_stress, 30.0);
    try runScenario(io, "60 m/s continuous swoosh", .fast_swoosh, 60.0);
    try callbackDistribution(io);
    try onsetCheck();
    try qualityCheck();
}

fn runScenario(
    io: std.Io,
    label: []const u8,
    scenario: Scenario,
    audio_duration_s: f64,
) !void {
    var engine = try runtime.Engine.init(.{});
    if (scenario == .fast_swoosh) engine.setRacketSpeed(60.0);
    const total_frames: usize = @intFromFloat(
        audio_duration_s * runtime.preferred_sample_rate_hz,
    );
    var block: [benchmark_block_frames]f32 = undefined;
    var frame_cursor: usize = 0;
    var next_hit_frame: usize = 0;
    var checksum: f64 = 0.0;
    const started = std.Io.Clock.Timestamp.now(io, .awake);
    while (frame_cursor < total_frames) {
        if (scenario == .typical_hits and frame_cursor >= next_hit_frame) {
            var hit = runtime.HitEvent{};
            hit.relative_normal_speed_mps = if ((next_hit_frame / 24_000) % 5 == 4)
                30.0
            else
                5.0;
            hit.x_mm = if ((next_hit_frame / 24_000) % 3 == 0) 18.0 else 0.0;
            _ = engine.submitHit(hit);
            next_hit_frame += 24_000;
        } else if (scenario == .eight_voice_stress and
            frame_cursor >= next_hit_frame)
        {
            var hit = runtime.HitEvent{};
            hit.relative_normal_speed_mps = 5.0;
            hit.x_mm = @as(f32, @floatFromInt((next_hit_frame / 2400) % 8)) *
                8.0 - 28.0;
            _ = engine.submitHit(hit);
            // Twenty impacts per second keeps all eight voices occupied and
            // repeatedly exercises voice setup/replacement.
            next_hit_frame += 2400;
        }
        const count = @min(block.len, total_frames - frame_cursor);
        engine.renderMono(block[0..count]);
        checksum += block[frame_cursor % count];
        frame_cursor += count;
    }
    const finished = std.Io.Clock.Timestamp.now(io, .awake);
    const elapsed_ns = started.durationTo(finished).raw.nanoseconds;
    const elapsed_ms = @as(f64, @floatFromInt(elapsed_ns)) / 1_000_000.0;
    const realtime_percent = elapsed_ms / (audio_duration_s * 1000.0) * 100.0;
    const realtime_factor = audio_duration_s * 1_000_000_000.0 /
        @as(f64, @floatFromInt(@max(1, elapsed_ns)));
    const stats = engine.stats();
    std.debug.print(
        "  {s}: {d:.2} ms for {d:.0} s audio, {d:.4}% of one core, " ++
            "{d:.0}x real-time, hits {d}, checksum {e:.3}\n",
        .{
            label,
            elapsed_ms,
            audio_duration_s,
            realtime_percent,
            realtime_factor,
            stats.rendered_hits,
            checksum,
        },
    );
}

fn callbackDistribution(io: std.Io) !void {
    const allocator = std.heap.smp_allocator;
    const callback_count: usize = 20_000;
    const durations = try allocator.alloc(u64, callback_count);
    defer allocator.free(durations);
    var engine = try runtime.Engine.init(.{});
    var block: [benchmark_block_frames]f32 = undefined;
    var checksum: f64 = 0.0;
    for (0..runtime.max_voices) |index| {
        var hit = runtime.HitEvent{};
        hit.relative_normal_speed_mps = 60.0;
        hit.x_mm = @as(f32, @floatFromInt(index)) * 6.0 - 21.0;
        _ = engine.submitHit(hit);
    }
    for (durations, 0..) |*duration, index| {
        var hit = runtime.HitEvent{};
        hit.relative_normal_speed_mps = 40.0;
        hit.x_mm = @as(f32, @floatFromInt(index % 7)) * 9.0 - 27.0;
        _ = engine.submitHit(hit);
        const started = std.Io.Clock.Timestamp.now(io, .awake);
        engine.renderMono(&block);
        const finished = std.Io.Clock.Timestamp.now(io, .awake);
        duration.* = @intCast(started.durationTo(finished).raw.nanoseconds);
        checksum += block[index % block.len];
    }
    std.sort.heap(u64, durations, {}, std.sort.asc(u64));
    const median = durations[durations.len / 2];
    const p99 = durations[durations.len * 99 / 100];
    const maximum = durations[durations.len - 1];
    const callback_budget_us = @as(f64, @floatFromInt(benchmark_block_frames)) /
        runtime.preferred_sample_rate_hz * 1_000_000.0;
    std.debug.print(
        "  128-frame callback: p50 {d:.2} us, p99 {d:.2} us, max {d:.2} us, " ++
            "budget {d:.2} us, checksum {e:.3}\n",
        .{
            @as(f64, @floatFromInt(median)) / 1000.0,
            @as(f64, @floatFromInt(p99)) / 1000.0,
            @as(f64, @floatFromInt(maximum)) / 1000.0,
            callback_budget_us,
            checksum,
        },
    );
}

fn onsetCheck() !void {
    var engine = try runtime.Engine.init(.{});
    try std.testing.expectEqual(runtime.SubmitResult.accepted, engine.submitHit(.{}));
    var block: [benchmark_block_frames]f32 = undefined;
    engine.renderMono(&block);
    var first_nonzero: ?usize = null;
    for (block, 0..) |sample, index| {
        if (sample != 0.0) {
            first_nonzero = index;
            break;
        }
    }
    const sample = first_nonzero orelse return error.NoImpactOnset;
    std.debug.print(
        "  internal event-to-sample onset: {d} frames ({d:.3} ms); " ++
            "host scheduling bound at 128 frames: {d:.3} ms\n",
        .{
            sample,
            @as(f64, @floatFromInt(sample)) /
                runtime.preferred_sample_rate_hz * 1000.0,
            @as(f64, @floatFromInt(benchmark_block_frames)) /
                runtime.preferred_sample_rate_hz * 1000.0,
        },
    );
}

fn qualityCheck() !void {
    const allocator = std.heap.smp_allocator;
    const sample_count: usize = 12_000;
    const audio = try allocator.alloc(f32, sample_count);
    defer allocator.free(audio);
    const config = runtime.Config{};
    const cases = [_]struct {
        label: []const u8,
        hit: runtime.HitEvent,
        minimum_hz: f64,
        maximum_hz: f64,
    }{
        .{
            .label = "27/28 lb string touch",
            .hit = .{},
            .minimum_hz = 500.0,
            .maximum_hz = 1800.0,
        },
        .{
            .label = "30 lb side-frame touch",
            .hit = .{
                .main_tension_lbf = 30.0,
                .cross_tension_lbf = 30.0,
                .x_mm = 0.5 * config.stringed_width_mm +
                    0.5 * config.frame_radial_width_mm,
            },
            .minimum_hz = 2000.0,
            .maximum_hz = 18_000.0,
        },
    };
    for (cases) |case| {
        var engine = try runtime.Engine.init(config);
        _ = engine.submitHit(case.hit);
        engine.renderMono(audio);
        var peak: f64 = 0.0;
        var energy: f64 = 0.0;
        for (audio) |sample| {
            peak = @max(peak, @abs(@as(f64, sample)));
            energy += @as(f64, sample) * @as(f64, sample);
        }
        const frequency = try dsp.dominantFrequency(
            allocator,
            audio,
            config.sample_rate_hz,
            case.minimum_hz,
            case.maximum_hz,
        );
        std.debug.print(
            "  {s}: peak {d:.4}, RMS {d:.5}, dominant {d:.1} Hz\n",
            .{
                case.label,
                peak,
                @sqrt(energy / @as(f64, @floatFromInt(audio.len))),
                frequency,
            },
        );
    }
    var swoosh_engine = try runtime.Engine.init(config);
    swoosh_engine.setRacketSpeed(60.0);
    swoosh_engine.renderMono(audio);
    var swoosh_peak: f64 = 0.0;
    var swoosh_energy: f64 = 0.0;
    for (audio) |sample| {
        swoosh_peak = @max(swoosh_peak, @abs(@as(f64, sample)));
        swoosh_energy += @as(f64, sample) * @as(f64, sample);
    }
    std.debug.print(
        "  60 m/s swoosh: peak {d:.4}, RMS {d:.5}\n",
        .{
            swoosh_peak,
            @sqrt(swoosh_energy / @as(f64, @floatFromInt(audio.len))),
        },
    );
}
