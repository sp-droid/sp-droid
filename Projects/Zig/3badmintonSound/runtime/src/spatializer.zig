const std = @import("std");

pub const maximum_sample_rate_hz: u32 = 96_000;
const delay_capacity: usize = 128;

pub const RelativePosition = extern struct {
    right_m: f32 = 0.0,
    up_m: f32 = 0.0,
    forward_m: f32 = 0.5,
};

const Targets = struct {
    left_gain: f32,
    right_gain: f32,
    left_delay: f32,
    right_delay: f32,
    left_shadow: f32,
    right_shadow: f32,
    spectral_gain: f32,
};

/// Fixed-memory, allocation-free mono-to-stereo near-field spatializer.
/// The host supplies a racket-centre position already transformed into the
/// listener's right/up/forward coordinate frame.
pub const Spatializer = struct {
    sample_rate_hz: u32,
    delay_line: [delay_capacity]f32 = @splat(0.0),
    write_index: usize = 0,
    left_gain: f32 = 0.70710677,
    right_gain: f32 = 0.70710677,
    left_delay: f32 = 0.0,
    right_delay: f32 = 0.0,
    left_shadow: f32 = 0.0,
    right_shadow: f32 = 0.0,
    spectral_gain: f32 = 1.0,
    left_shadow_state: f32 = 0.0,
    right_shadow_state: f32 = 0.0,
    left_spectral_state: f32 = 0.0,
    right_spectral_state: f32 = 0.0,
    smoothing: f32,
    spectral_lowpass: f32,
    initialized: bool = false,

    pub fn init(sample_rate_hz: u32) error{InvalidSampleRate}!Spatializer {
        if (sample_rate_hz < 24_000 or sample_rate_hz > maximum_sample_rate_hz)
            return error.InvalidSampleRate;
        const rate: f32 = @floatFromInt(sample_rate_hz);
        return .{
            .sample_rate_hz = sample_rate_hz,
            .smoothing = 1.0 - @exp(-1.0 / (0.010 * rate)),
            .spectral_lowpass = @exp(-2.0 * std.math.pi * 3500.0 / rate),
        };
    }

    pub fn reset(self: *Spatializer) void {
        const rate = self.sample_rate_hz;
        self.* = Spatializer.init(rate) catch unreachable;
    }

    /// Writes interleaved stereo. Input and output may not overlap.
    pub fn render(
        self: *Spatializer,
        mono: []const f32,
        stereo: []f32,
        relative: RelativePosition,
    ) void {
        std.debug.assert(stereo.len >= mono.len * 2);
        const target_values = self.targets(relative);
        if (!self.initialized) {
            self.left_gain = target_values.left_gain;
            self.right_gain = target_values.right_gain;
            self.left_delay = target_values.left_delay;
            self.right_delay = target_values.right_delay;
            self.left_shadow = target_values.left_shadow;
            self.right_shadow = target_values.right_shadow;
            self.spectral_gain = target_values.spectral_gain;
            self.initialized = true;
        }

        for (mono, 0..) |input, frame| {
            self.left_gain += self.smoothing * (target_values.left_gain - self.left_gain);
            self.right_gain += self.smoothing * (target_values.right_gain - self.right_gain);
            self.left_delay += self.smoothing * (target_values.left_delay - self.left_delay);
            self.right_delay += self.smoothing * (target_values.right_delay - self.right_delay);
            self.left_shadow += self.smoothing * (target_values.left_shadow - self.left_shadow);
            self.right_shadow += self.smoothing * (target_values.right_shadow - self.right_shadow);
            self.spectral_gain += self.smoothing *
                (target_values.spectral_gain - self.spectral_gain);

            self.delay_line[self.write_index] = input;
            const left_delayed = self.readDelay(self.left_delay);
            const right_delayed = self.readDelay(self.right_delay);
            self.write_index = (self.write_index + 1) % delay_capacity;

            self.left_shadow_state =
                (1.0 - self.left_shadow) * left_delayed +
                self.left_shadow * self.left_shadow_state;
            self.right_shadow_state =
                (1.0 - self.right_shadow) * right_delayed +
                self.right_shadow * self.right_shadow_state;

            self.left_spectral_state =
                (1.0 - self.spectral_lowpass) * self.left_shadow_state +
                self.spectral_lowpass * self.left_spectral_state;
            self.right_spectral_state =
                (1.0 - self.spectral_lowpass) * self.right_shadow_state +
                self.spectral_lowpass * self.right_spectral_state;
            const left_shaped = self.left_spectral_state +
                (self.left_shadow_state - self.left_spectral_state) * self.spectral_gain;
            const right_shaped = self.right_spectral_state +
                (self.right_shadow_state - self.right_spectral_state) * self.spectral_gain;

            stereo[frame * 2] = std.math.clamp(
                left_shaped * self.left_gain,
                -1.0,
                1.0,
            );
            stereo[frame * 2 + 1] = std.math.clamp(
                right_shaped * self.right_gain,
                -1.0,
                1.0,
            );
        }
    }

    fn targets(self: Spatializer, relative: RelativePosition) Targets {
        const finite = std.math.isFinite(relative.right_m) and
            std.math.isFinite(relative.up_m) and
            std.math.isFinite(relative.forward_m);
        const right = if (finite) relative.right_m else 0.0;
        const up = if (finite) relative.up_m else 0.0;
        const forward = if (finite) relative.forward_m else 0.5;
        const distance = @max(0.001, @sqrt(right * right + up * up + forward * forward));
        const side = std.math.clamp(right / distance, -1.0, 1.0);
        const vertical = std.math.clamp(up / distance, -1.0, 1.0);
        const front = std.math.clamp(forward / distance, -1.0, 1.0);
        const distance_gain = if (distance <= 1.0) 1.0 else 1.0 / distance;

        // Preserve energy while keeping the far ear audible. Visual alignment
        // supplies the remaining front/back disambiguation in VR.
        const left_gain = distance_gain * @sqrt(0.5 * (1.0 - 0.75 * side));
        const right_gain = distance_gain * @sqrt(0.5 * (1.0 + 0.75 * side));
        const maximum_itd_samples = 0.00065 * @as(f32, @floatFromInt(self.sample_rate_hz));
        const signed_itd = maximum_itd_samples * side;
        const left_delay = if (signed_itd > 0.0) signed_itd else 0.0;
        const right_delay = if (signed_itd < 0.0) -signed_itd else 0.0;

        const far_amount = @abs(side);
        const near_cutoff: f32 = 19_000.0;
        const far_cutoff = near_cutoff + (4_800.0 - near_cutoff) * far_amount;
        const rate: f32 = @floatFromInt(self.sample_rate_hz);
        const near_coefficient = @exp(-2.0 * std.math.pi * near_cutoff / rate);
        const far_coefficient = @exp(-2.0 * std.math.pi * far_cutoff / rate);
        const left_shadow = if (side > 0.0) far_coefficient else near_coefficient;
        const right_shadow = if (side < 0.0) far_coefficient else near_coefficient;

        // A restrained pinna-like spectral tilt supplies an elevation cue and
        // gently darkens sources behind the listener without an HRTF table.
        const spectral_gain = std.math.clamp(
            1.0 + 0.12 * vertical - 0.06 * @max(0.0, -front),
            0.82,
            1.16,
        );
        return .{
            .left_gain = left_gain,
            .right_gain = right_gain,
            .left_delay = left_delay,
            .right_delay = right_delay,
            .left_shadow = left_shadow,
            .right_shadow = right_shadow,
            .spectral_gain = spectral_gain,
        };
    }

    fn readDelay(self: *const Spatializer, delay_samples: f32) f32 {
        const bounded = std.math.clamp(
            delay_samples,
            0.0,
            @as(f32, @floatFromInt(delay_capacity - 2)),
        );
        const whole: usize = @intFromFloat(@floor(bounded));
        const fraction = bounded - @as(f32, @floatFromInt(whole));
        const newest = (self.write_index + delay_capacity - whole) % delay_capacity;
        const older = (newest + delay_capacity - 1) % delay_capacity;
        return self.delay_line[newest] * (1.0 - fraction) +
            self.delay_line[older] * fraction;
    }
};

fn channelEnergy(stereo: []const f32, channel: usize) f64 {
    var energy: f64 = 0.0;
    var index: usize = channel;
    while (index < stereo.len) : (index += 2)
        energy += @as(f64, stereo[index]) * stereo[index];
    return energy;
}

test "spatializer mirrors left and right sources" {
    var right = try Spatializer.init(48_000);
    var left = try Spatializer.init(48_000);
    var mono: [2048]f32 = @splat(0.0);
    for (&mono, 0..) |*sample, index|
        sample.* = if (index % 17 == 0) 0.25 else 0.0;
    var right_output: [4096]f32 = undefined;
    var left_output: [4096]f32 = undefined;
    right.render(&mono, &right_output, .{ .right_m = 0.5, .forward_m = 0.5 });
    left.render(&mono, &left_output, .{ .right_m = -0.5, .forward_m = 0.5 });
    try std.testing.expect(channelEnergy(&right_output, 1) > channelEnergy(&right_output, 0));
    try std.testing.expect(channelEnergy(&left_output, 0) > channelEnergy(&left_output, 1));
    try std.testing.expectApproxEqRel(
        channelEnergy(&right_output, 1),
        channelEnergy(&left_output, 0),
        0.0001,
    );
}

test "spatializer attenuates only beyond one metre" {
    var near = try Spatializer.init(48_000);
    var far = try Spatializer.init(48_000);
    var mono: [1024]f32 = @splat(0.2);
    var near_output: [2048]f32 = undefined;
    var far_output: [2048]f32 = undefined;
    near.render(&mono, &near_output, .{ .forward_m = 0.5 });
    far.render(&mono, &far_output, .{ .forward_m = 2.0 });
    const near_energy = channelEnergy(&near_output, 0) + channelEnergy(&near_output, 1);
    const far_energy = channelEnergy(&far_output, 0) + channelEnergy(&far_output, 1);
    try std.testing.expect(near_energy > far_energy * 3.5);
}

test "spatializer is invariant to constant-position block partitioning" {
    var whole = try Spatializer.init(48_000);
    var split = try Spatializer.init(48_000);
    var mono: [1024]f32 = undefined;
    for (&mono, 0..) |*sample, index|
        sample.* = @floatCast(0.2 * @sin(2.0 * std.math.pi * 1200.0 *
            @as(f64, @floatFromInt(index)) / 48_000.0));
    var whole_output: [2048]f32 = undefined;
    var split_output: [2048]f32 = undefined;
    const position = RelativePosition{ .right_m = 0.31, .up_m = 0.17, .forward_m = 0.48 };
    whole.render(&mono, &whole_output, position);
    var cursor: usize = 0;
    const blocks = [_]usize{ 17, 63, 128, 5, 251 };
    var block_index: usize = 0;
    while (cursor < mono.len) {
        const count = @min(blocks[block_index % blocks.len], mono.len - cursor);
        split.render(
            mono[cursor .. cursor + count],
            split_output[cursor * 2 .. (cursor + count) * 2],
            position,
        );
        cursor += count;
        block_index += 1;
    }
    try std.testing.expectEqualSlices(f32, &whole_output, &split_output);
}

test "moving source direction is smoothed instead of stepping" {
    var spatializer = try Spatializer.init(48_000);
    const one = [_]f32{0.25};
    var first: [2]f32 = undefined;
    spatializer.render(&one, &first, .{ .forward_m = 0.5 });
    const centered_left = spatializer.left_gain;
    const centered_right = spatializer.right_gain;

    spatializer.render(
        &one,
        &first,
        .{ .right_m = 0.5, .forward_m = 0.5 },
    );
    try std.testing.expect(spatializer.right_gain > centered_right);
    try std.testing.expect(spatializer.left_gain < centered_left);
    // A single sample advances only a tiny fraction of the 10 ms response.
    try std.testing.expect(spatializer.right_gain - centered_right < 0.002);

    var settle_input: [960]f32 = @splat(0.25);
    var settle_output: [1920]f32 = undefined;
    spatializer.render(
        &settle_input,
        &settle_output,
        .{ .right_m = 0.5, .forward_m = 0.5 },
    );
    try std.testing.expect(spatializer.right_gain > spatializer.left_gain * 1.5);
}
