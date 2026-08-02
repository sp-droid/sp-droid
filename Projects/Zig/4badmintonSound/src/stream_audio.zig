const std = @import("std");

/// Converts a completed mono render to the persistent device rate. This is
/// done once per strike by the audio owner thread, never by the device callback.
pub fn resampleWindowedSinc(
    allocator: std.mem.Allocator,
    input: []const f32,
    input_rate_hz: u32,
    output_rate_hz: u32,
) ![]f32 {
    if (input_rate_hz == 0 or output_rate_hz == 0) {
        return error.InvalidSampleRate;
    }
    if (input_rate_hz == output_rate_hz) return allocator.dupe(f32, input);
    if (input.len == 0) return allocator.alloc(f32, 0);

    const output_length: usize = @intFromFloat(@ceil(
        @as(f64, @floatFromInt(input.len)) *
            @as(f64, @floatFromInt(output_rate_hz)) /
            @as(f64, @floatFromInt(input_rate_hz)),
    ));
    const output = try allocator.alloc(f32, output_length);
    errdefer allocator.free(output);

    const input_per_output =
        @as(f64, @floatFromInt(input_rate_hz)) /
        @as(f64, @floatFromInt(output_rate_hz));
    const rate_ratio =
        @as(f64, @floatFromInt(output_rate_hz)) /
        @as(f64, @floatFromInt(input_rate_hz));
    const cutoff = 0.45 * @min(1.0, rate_ratio);
    const radius: isize = 16;
    const radius_f: f64 = @floatFromInt(radius);

    for (output, 0..) |*destination, output_index| {
        const source_position =
            @as(f64, @floatFromInt(output_index)) * input_per_output;
        const centre: isize = @intFromFloat(@floor(source_position));
        var weighted_sum: f64 = 0.0;
        var weight_sum: f64 = 0.0;
        var tap: isize = -radius;
        while (tap <= radius) : (tap += 1) {
            const source_index = centre + tap;
            if (source_index < 0 or source_index >= input.len) continue;
            const offset =
                @as(f64, @floatFromInt(source_index)) - source_position;
            const normalized = 2.0 * cutoff * offset;
            const sinc = if (@abs(normalized) < 1.0e-12)
                1.0
            else
                @sin(std.math.pi * normalized) /
                    (std.math.pi * normalized);
            const window = 0.42 +
                0.5 * @cos(std.math.pi * offset / radius_f) +
                0.08 * @cos(2.0 * std.math.pi * offset / radius_f);
            const weight = 2.0 * cutoff * sinc * window;
            weighted_sum += @as(f64, input[@intCast(source_index)]) * weight;
            weight_sum += weight;
        }
        const sample = if (@abs(weight_sum) > 1.0e-12)
            weighted_sum / weight_sum
        else
            0.0;
        destination.* = @floatCast(std.math.clamp(sample, -1.0, 1.0));
    }
    return output;
}

pub const ClipCursor = struct {
    samples: []const f32,
    position: usize = 0,

    /// Copies as much contiguous audio as fits. The caller may immediately
    /// continue with another cursor in the same output block, avoiding gaps.
    pub fn copyInto(self: *ClipCursor, destination: []f32) usize {
        const remaining = self.samples.len - self.position;
        const copied = @min(remaining, destination.len);
        @memcpy(
            destination[0..copied],
            self.samples[self.position .. self.position + copied],
        );
        self.position += copied;
        return copied;
    }

    pub fn finished(self: ClipCursor) bool {
        return self.position >= self.samples.len;
    }
};

test "windowed-sinc resampler preserves duration and a pass-band sinusoid" {
    const allocator = std.testing.allocator;
    const input_rate: u32 = 96_000;
    const output_rate: u32 = 48_000;
    const input = try allocator.alloc(f32, 9600);
    defer allocator.free(input);
    for (input, 0..) |*sample, index| {
        sample.* = @floatCast(0.4 * @sin(
            2.0 * std.math.pi * 1000.0 *
                @as(f64, @floatFromInt(index)) /
                @as(f64, @floatFromInt(input_rate)),
        ));
    }
    const output = try resampleWindowedSinc(
        allocator,
        input,
        input_rate,
        output_rate,
    );
    defer allocator.free(output);
    try std.testing.expectEqual(@as(usize, 4800), output.len);
    for (output[100 .. output.len - 100], 100..) |sample, index| {
        const expected: f32 = @floatCast(0.4 * @sin(
            2.0 * std.math.pi * 1000.0 *
                @as(f64, @floatFromInt(index)) /
                @as(f64, @floatFromInt(output_rate)),
        ));
        try std.testing.expectApproxEqAbs(expected, sample, 2.0e-3);
    }
}

test "resampler up-samples DC without changing its level" {
    const allocator = std.testing.allocator;
    const input = try allocator.alloc(f32, 1200);
    defer allocator.free(input);
    @memset(input, 0.25);
    const output = try resampleWindowedSinc(allocator, input, 24_000, 48_000);
    defer allocator.free(output);
    try std.testing.expectEqual(@as(usize, 2400), output.len);
    for (output[64 .. output.len - 64]) |sample| {
        try std.testing.expectApproxEqAbs(@as(f32, 0.25), sample, 1.0e-5);
    }
}

test "down-sampling rejects content above the device Nyquist limit" {
    const allocator = std.testing.allocator;
    const input_rate: u32 = 96_000;
    const input = try allocator.alloc(f32, 9600);
    defer allocator.free(input);
    for (input, 0..) |*sample, index| {
        sample.* = @floatCast(0.4 * @sin(
            2.0 * std.math.pi * 30_000.0 *
                @as(f64, @floatFromInt(index)) /
                @as(f64, @floatFromInt(input_rate)),
        ));
    }
    const output = try resampleWindowedSinc(allocator, input, input_rate, 48_000);
    defer allocator.free(output);
    var energy: f64 = 0.0;
    const interior = output[100 .. output.len - 100];
    for (interior) |sample| {
        energy += @as(f64, sample) * @as(f64, sample);
    }
    const rms = @sqrt(energy / @as(f64, @floatFromInt(interior.len)));
    try std.testing.expect(rms < 0.02);
}

test "clip cursors concatenate without adding block gaps" {
    const first = [_]f32{ 1.0, 2.0, 3.0 };
    const second = [_]f32{ 4.0, 5.0 };
    var first_cursor = ClipCursor{ .samples = &first };
    var second_cursor = ClipCursor{ .samples = &second };
    var block: [8]f32 = @splat(0.0);
    var written: usize = 0;
    written += first_cursor.copyInto(block[written..]);
    written += second_cursor.copyInto(block[written..]);
    try std.testing.expectEqual(@as(usize, 5), written);
    try std.testing.expectEqualSlices(
        f32,
        &.{ 1.0, 2.0, 3.0, 4.0, 5.0, 0.0, 0.0, 0.0 },
        &block,
    );
}
