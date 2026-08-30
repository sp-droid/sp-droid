const std = @import("std");
const dsp = @import("dsp.zig");
const runtime = @import("badminton_audio_runtime");

test "runtime classifier keeps string frame and outside regimes" {
    const config = runtime.Config{};
    try std.testing.expectEqual(
        runtime.HitRegion.strings,
        runtime.classifyHit(config, .{}).?,
    );
    var frame = runtime.HitEvent{};
    frame.x_mm = 0.5 * config.stringed_width_mm + 2.0;
    try std.testing.expectEqual(
        runtime.HitRegion.frame,
        runtime.classifyHit(config, frame).?,
    );
    frame.x_mm = config.stringed_width_mm;
    try std.testing.expectEqual(
        @as(?runtime.HitRegion, null),
        runtime.classifyHit(config, frame),
    );
}

test "runtime continuously emits exact idle silence without allocation" {
    var engine = try runtime.Engine.init(.{});
    var block: [257]f32 = undefined;
    for (0..100) |_| {
        engine.renderMono(&block);
        for (block) |sample| try std.testing.expectEqual(@as(f32, 0.0), sample);
    }
    const stats = engine.stats();
    try std.testing.expectEqual(@as(u64, block.len * 100), stats.rendered_frames);
    try std.testing.expectEqual(@as(u64, 0), stats.rendered_hits);
}

test "runtime swoosh runs continuously from velocity without hit events" {
    var engine = try runtime.Engine.init(.{});
    engine.setRacketSpeed(60.0);
    var block: [256]f32 = undefined;
    var energy: f64 = 0.0;
    for (0..100) |_| {
        engine.renderMono(&block);
        for (block) |sample| energy += @as(f64, sample) * sample;
    }
    try std.testing.expect(energy > 0.01);
    try std.testing.expectEqual(@as(u64, 0), engine.stats().rendered_hits);

    engine.setRacketSpeed(0.0);
    for (0..100) |_| engine.renderMono(&block);
    engine.renderMono(&block);
    for (block) |sample| try std.testing.expectEqual(@as(f32, 0.0), sample);
}

test "runtime block partitioning is bit stable" {
    var whole_engine = try runtime.Engine.init(.{});
    var split_engine = try runtime.Engine.init(.{});
    const hit = runtime.HitEvent{};
    try std.testing.expectEqual(runtime.SubmitResult.accepted, whole_engine.submitHit(hit));
    try std.testing.expectEqual(runtime.SubmitResult.accepted, split_engine.submitHit(hit));

    var whole: [8192]f32 = undefined;
    var split: [8192]f32 = undefined;
    whole_engine.renderMono(&whole);
    var cursor: usize = 0;
    const sizes = [_]usize{ 17, 128, 193, 64, 511, 31, 256 };
    var size_index: usize = 0;
    while (cursor < split.len) {
        const count = @min(sizes[size_index % sizes.len], split.len - cursor);
        split_engine.renderMono(split[cursor .. cursor + count]);
        cursor += count;
        size_index += 1;
    }
    try std.testing.expectEqualSlices(f32, &whole, &split);
}

test "runtime string hit is finite audible and decays to digital silence" {
    var engine = try runtime.Engine.init(.{});
    try std.testing.expectEqual(
        runtime.SubmitResult.accepted,
        engine.submitHit(.{}),
    );
    var block: [256]f32 = undefined;
    var peak: f32 = 0.0;
    var nonzero_samples: usize = 0;
    for (0..150) |_| {
        engine.renderMono(&block);
        for (block) |sample| {
            try std.testing.expect(std.math.isFinite(sample));
            peak = @max(peak, @abs(sample));
            if (sample != 0.0) nonzero_samples += 1;
        }
    }
    try std.testing.expect(peak > 0.02);
    try std.testing.expect(peak <= 0.891_251);
    try std.testing.expect(nonzero_samples > 1000);
    engine.renderMono(&block);
    for (block) |sample| try std.testing.expectEqual(@as(f32, 0.0), sample);
}

test "runtime tension changes the low-speed string pitch" {
    const allocator = std.testing.allocator;
    const frame_count: usize = 12_000;
    const low_audio = try allocator.alloc(f32, frame_count);
    defer allocator.free(low_audio);
    const high_audio = try allocator.alloc(f32, frame_count);
    defer allocator.free(high_audio);
    var low_engine = try runtime.Engine.init(.{});
    var high_engine = try runtime.Engine.init(.{});
    var low_hit = runtime.HitEvent{};
    low_hit.main_tension_lbf = 20.0;
    low_hit.cross_tension_lbf = 20.0;
    var high_hit = low_hit;
    high_hit.main_tension_lbf = 30.0;
    high_hit.cross_tension_lbf = 30.0;
    _ = low_engine.submitHit(low_hit);
    _ = high_engine.submitHit(high_hit);
    low_engine.renderMono(low_audio);
    high_engine.renderMono(high_audio);
    const low_peak = try dsp.dominantFrequency(allocator, low_audio, 48_000, 700.0, 1600.0);
    const high_peak = try dsp.dominantFrequency(allocator, high_audio, 48_000, 700.0, 1600.0);
    try std.testing.expectApproxEqRel(@sqrt(30.0 / 20.0), high_peak / low_peak, 0.03);
}

test "runtime frame hit is short bright and bounded" {
    const allocator = std.testing.allocator;
    const frame_count: usize = 8192;
    const audio = try allocator.alloc(f32, frame_count);
    defer allocator.free(audio);
    var engine = try runtime.Engine.init(.{});
    var hit = runtime.HitEvent{};
    const config = runtime.Config{};
    hit.x_mm = 0.5 * config.stringed_width_mm +
        0.5 * config.frame_radial_width_mm;
    try std.testing.expectEqual(runtime.SubmitResult.accepted, engine.submitHit(hit));
    engine.renderMono(audio);
    const peak = try dsp.dominantFrequency(allocator, audio, 48_000, 2000.0, 18_000.0);
    try std.testing.expect(peak >= 4000.0);
    try std.testing.expect(peak <= 9000.0);
    for (audio) |sample| {
        try std.testing.expect(std.math.isFinite(sample));
        try std.testing.expect(@abs(sample) <= 0.891_251);
    }
}

test "runtime queue is bounded and reports overload" {
    var engine = try runtime.Engine.init(.{});
    for (0..runtime.event_queue_capacity) |_| {
        try std.testing.expectEqual(runtime.SubmitResult.accepted, engine.submitHit(.{}));
    }
    try std.testing.expectEqual(runtime.SubmitResult.queue_full, engine.submitHit(.{}));
    var block: [64]f32 = undefined;
    engine.renderMono(&block);
    const stats = engine.stats();
    try std.testing.expectEqual(@as(u64, runtime.event_queue_capacity), stats.submitted_hits);
    try std.testing.expectEqual(@as(u64, 1), stats.dropped_hits);
    try std.testing.expectEqual(@as(u64, runtime.event_queue_capacity), stats.rendered_hits);
    try std.testing.expect(stats.stolen_voices > 0);
}

test "runtime SPSC queue passes concurrent game and audio threads" {
    const event_count: usize = 2000;
    var engine = try runtime.Engine.init(.{});
    const Producer = struct {
        fn run(synth: *runtime.Engine) void {
            for (0..event_count) |index| {
                var hit = runtime.HitEvent{};
                hit.x_mm = @as(f32, @floatFromInt(index % 9)) * 6.0 - 24.0;
                while (synth.submitHit(hit) != .accepted) {
                    std.atomic.spinLoopHint();
                }
            }
        }
    };
    const producer = try std.Thread.spawn(.{}, Producer.run, .{&engine});
    var block: [64]f32 = undefined;
    while (engine.stats().rendered_hits < event_count) {
        engine.renderMono(&block);
    }
    producer.join();
    const stats = engine.stats();
    try std.testing.expectEqual(@as(u64, event_count), stats.submitted_hits);
    try std.testing.expectEqual(@as(u64, event_count), stats.rendered_hits);
}
