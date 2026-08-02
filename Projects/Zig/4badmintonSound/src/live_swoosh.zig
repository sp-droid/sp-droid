const std = @import("std");
const dsp = @import("dsp.zig");
const model = @import("model.zig");

pub const sample_rate_hz: u32 = 48_000;

/// A compact snapshot copied from the UI thread to the audio-owner thread.
/// There is deliberately no impact/contact field: airflow velocity is the
/// only excitation for this source.
pub const Control = struct {
    racket_speed_mps: f64 = 0.0,
    threshold_mps: f64 = 7.0,
    reference_speed_mps: f64 = 40.0,
    speed_exponent: f64 = 3.0,
    gain: f64 = 1.20,
    strouhal_number: f64 = 0.20,
    frame_diameter_mm: f64 = 10.0,
    string_diameter_mm: f64 = model.bg66_diameter_mm,
    air_density_kg_m3: f64 = 1.204,
    head_width_mm: f64 = 188.0,
    head_height_mm: f64 = 253.0,
    microphone_distance_m: f64 = 1.0,
    response_ms: f64 = 18.0,
    master_gain: f64 = 0.15,
    compressor_threshold_dbfs: f64 = -8.0,
    compressor_ratio: f64 = 4.0,
    compressor_knee_db: f64 = 6.0,
    compressor_release_ms: f64 = 40.0,
    limiter_ceiling_dbfs: f64 = -1.0,

    pub fn fromProfile(profile: model.Profile) Control {
        const params = profile.model;
        return .{
            .racket_speed_mps = profile.hit.racket_speed_mps,
            .threshold_mps = params.swoosh_threshold_mps,
            .reference_speed_mps = params.swoosh_reference_speed_mps,
            .speed_exponent = params.swoosh_speed_exponent,
            .gain = params.swoosh_gain,
            .strouhal_number = params.swoosh_strouhal_number,
            .frame_diameter_mm = params.swoosh_frame_diameter_mm,
            .string_diameter_mm = params.string_diameter_mm,
            .air_density_kg_m3 = params.air_density_kg_m3,
            .head_width_mm = params.head_width_mm,
            .head_height_mm = params.head_height_mm,
            .microphone_distance_m = params.microphone_distance_m,
            // Kept under the version-1 JSON field name for profile
            // compatibility. It is now velocity response time, not an
            // impact-triggered release envelope.
            .response_ms = params.swoosh_post_impact_decay_ms,
            .master_gain = params.master_gain,
            .compressor_threshold_dbfs = params.compressor_threshold_dbfs,
            .compressor_ratio = params.compressor_ratio,
            .compressor_knee_db = params.compressor_knee_db,
            .compressor_release_ms = params.compressor_release_ms,
            .limiter_ceiling_dbfs = params.limiter_ceiling_dbfs,
        };
    }
};

/// Stateful continuous aerodynamic source for the desktop audition stream.
/// Its filter and turbulence phases survive block boundaries, so changing the
/// speed slider changes one ongoing wind source instead of launching a clip.
pub const Generator = struct {
    control: Control = .{},
    current_speed_mps: f64 = 0.0,
    frame_band: dsp.Biquad,
    string_band: dsp.Biquad,
    previous_highpass_input: f64 = 0.0,
    previous_highpass_output: f64 = 0.0,
    compressor_gain: f64 = 1.0,
    sample_index: u64 = 0,
    filter_update_countdown: u32 = 0,

    pub fn init() Generator {
        return .{
            .frame_band = dsp.Biquad.bandPass(sample_rate_hz, 140.0, 0.38),
            .string_band = dsp.Biquad.bandPass(sample_rate_hz, 2121.0, 0.48),
        };
    }

    pub fn setControl(self: *Generator, control: Control) void {
        const next = sanitize(control);
        if (!std.meta.eql(self.control, next)) self.filter_update_countdown = 0;
        self.control = next;
    }

    /// Adds the wind source to an existing mono block, then applies the shared
    /// safety ceiling. No allocation or impact event is involved.
    pub fn addTo(self: *Generator, output: []f32) void {
        const control = self.control;
        const limiter_ceiling = std.math.pow(
            f64,
            10.0,
            control.limiter_ceiling_dbfs / 20.0,
        );
        if (control.racket_speed_mps <= control.threshold_mps and
            self.current_speed_mps <= control.threshold_mps)
        {
            self.current_speed_mps = control.racket_speed_mps;
            self.clearSignalState();
            for (output) |*sample| {
                sample.* = @floatCast(std.math.clamp(
                    @as(f64, sample.*),
                    -limiter_ceiling,
                    limiter_ceiling,
                ));
            }
            return;
        }

        const rate: f64 = @floatFromInt(sample_rate_hz);
        const response_coefficient = 1.0 -
            @exp(-1.0 / (control.response_ms * 0.001 * rate));
        const compressor_release =
            @exp(-1.0 / (control.compressor_release_ms * 0.001 * rate));
        const highpass_radius = @exp(-2.0 * std.math.pi * 30.0 / rate);
        const reference_face_area = std.math.pi * 0.188 * 0.253 * 0.25;
        const face_area = std.math.pi *
            control.head_width_mm * 0.001 *
            control.head_height_mm * 0.001 * 0.25;
        const propagation_scale =
            (control.air_density_kg_m3 / 1.204) *
            (face_area / reference_face_area) /
            control.microphone_distance_m;

        for (output) |*destination| {
            self.current_speed_mps += response_coefficient *
                (control.racket_speed_mps - self.current_speed_mps);
            if (self.filter_update_countdown == 0) {
                self.retuneFilters(@max(
                    control.threshold_mps + 0.01,
                    self.current_speed_mps,
                ));
                self.filter_update_countdown = 64;
            }
            self.filter_update_countdown -= 1;
            const speed_amount =
                (self.current_speed_mps - control.threshold_mps) /
                (control.reference_speed_mps - control.threshold_mps);
            const velocity_scale = if (speed_amount > 0.0)
                std.math.pow(f64, speed_amount, control.speed_exponent)
            else
                0.0;
            const frame_noise = self.frame_band.process(
                deterministicNoise(self.sample_index, 0xa24b_aed4_963e_e407),
            );
            const string_noise = self.string_band.process(
                deterministicNoise(self.sample_index, 0x9fb2_1c65_1e98_df25),
            );
            const time_s = @as(f64, @floatFromInt(self.sample_index)) / rate;
            const turbulent_flutter =
                1.0 + 0.10 * @sin(2.0 * std.math.pi * 31.0 * time_s) +
                0.06 * @sin(2.0 * std.math.pi * 73.0 * time_s + 0.7);
            const raw = control.gain * velocity_scale * propagation_scale *
                turbulent_flutter *
                (0.72 * frame_noise + 0.28 * string_noise);
            const highpassed = raw - self.previous_highpass_input +
                highpass_radius * self.previous_highpass_output;
            self.previous_highpass_input = raw;
            self.previous_highpass_output = highpassed;

            const conditioned = highpassed * control.master_gain;
            const target_gain = compressorTargetGain(conditioned, control);
            if (target_gain < self.compressor_gain) {
                self.compressor_gain = target_gain;
            } else {
                self.compressor_gain = target_gain + compressor_release *
                    (self.compressor_gain - target_gain);
            }
            const mixed = @as(f64, destination.*) +
                conditioned * self.compressor_gain;
            destination.* = @floatCast(std.math.clamp(
                mixed,
                -limiter_ceiling,
                limiter_ceiling,
            ));
            self.sample_index +%= 1;
        }
    }

    fn retuneFilters(self: *Generator, speed_mps: f64) void {
        const rate: f64 = @floatFromInt(sample_rate_hz);
        const frame_frequency_hz = std.math.clamp(
            self.control.strouhal_number * speed_mps /
                (self.control.frame_diameter_mm * 0.001),
            60.0,
            rate * 0.30,
        );
        const string_frequency_hz = std.math.clamp(
            self.control.strouhal_number * speed_mps /
                (self.control.string_diameter_mm * 0.001),
            600.0,
            rate * 0.42,
        );
        replaceCoefficientsPreservingState(
            &self.frame_band,
            dsp.Biquad.bandPass(rate, frame_frequency_hz, 0.38),
        );
        replaceCoefficientsPreservingState(
            &self.string_band,
            dsp.Biquad.bandPass(rate, string_frequency_hz, 0.48),
        );
    }

    fn clearSignalState(self: *Generator) void {
        clearFilterState(&self.frame_band);
        clearFilterState(&self.string_band);
        self.previous_highpass_input = 0.0;
        self.previous_highpass_output = 0.0;
        self.compressor_gain = 1.0;
        self.filter_update_countdown = 0;
    }
};

fn sanitize(input: Control) Control {
    var output = input;
    if (!std.math.isFinite(output.racket_speed_mps) or output.racket_speed_mps < 0.0)
        output.racket_speed_mps = 0.0;
    if (!std.math.isFinite(output.threshold_mps) or output.threshold_mps < 0.0)
        output.threshold_mps = 7.0;
    if (!std.math.isFinite(output.reference_speed_mps) or
        output.reference_speed_mps <= output.threshold_mps)
        output.reference_speed_mps = output.threshold_mps + 1.0;
    if (!finitePositive(output.speed_exponent)) output.speed_exponent = 3.0;
    if (!std.math.isFinite(output.gain) or output.gain < 0.0) output.gain = 0.0;
    if (!finitePositive(output.strouhal_number)) output.strouhal_number = 0.20;
    if (!finitePositive(output.frame_diameter_mm)) output.frame_diameter_mm = 10.0;
    if (!finitePositive(output.string_diameter_mm))
        output.string_diameter_mm = model.bg66_diameter_mm;
    if (!finitePositive(output.air_density_kg_m3)) output.air_density_kg_m3 = 1.204;
    if (!finitePositive(output.head_width_mm)) output.head_width_mm = 188.0;
    if (!finitePositive(output.head_height_mm)) output.head_height_mm = 253.0;
    if (!finitePositive(output.microphone_distance_m)) output.microphone_distance_m = 1.0;
    if (!finitePositive(output.response_ms)) output.response_ms = 18.0;
    if (!std.math.isFinite(output.master_gain) or output.master_gain < 0.0)
        output.master_gain = 0.0;
    if (!std.math.isFinite(output.compressor_threshold_dbfs))
        output.compressor_threshold_dbfs = -8.0;
    output.compressor_threshold_dbfs = std.math.clamp(
        output.compressor_threshold_dbfs,
        -80.0,
        0.0,
    );
    if (!finitePositive(output.compressor_ratio)) output.compressor_ratio = 4.0;
    if (!std.math.isFinite(output.compressor_knee_db) or output.compressor_knee_db < 0.0)
        output.compressor_knee_db = 0.0;
    if (!finitePositive(output.compressor_release_ms)) output.compressor_release_ms = 40.0;
    if (!std.math.isFinite(output.limiter_ceiling_dbfs)) output.limiter_ceiling_dbfs = -1.0;
    output.limiter_ceiling_dbfs = std.math.clamp(
        output.limiter_ceiling_dbfs,
        -24.0,
        0.0,
    );
    return output;
}

fn compressorTargetGain(sample: f64, control: Control) f64 {
    if (control.compressor_ratio <= 1.0) return 1.0;
    const peak = @abs(sample);
    if (peak <= 1.0e-20) return 1.0;
    const input_db = 20.0 * std.math.log10(peak);
    const over_db = input_db - control.compressor_threshold_dbfs;
    const slope = 1.0 / control.compressor_ratio - 1.0;
    const gain_db = if (control.compressor_knee_db <= 0.0)
        if (over_db > 0.0) slope * over_db else 0.0
    else blk: {
        const half_knee = 0.5 * control.compressor_knee_db;
        if (over_db <= -half_knee) break :blk 0.0;
        if (over_db >= half_knee) break :blk slope * over_db;
        const position = over_db + half_knee;
        break :blk slope * position * position /
            (2.0 * control.compressor_knee_db);
    };
    return std.math.pow(f64, 10.0, gain_db / 20.0);
}

fn replaceCoefficientsPreservingState(destination: *dsp.Biquad, source: dsp.Biquad) void {
    const x1 = destination.x1;
    const x2 = destination.x2;
    const y1 = destination.y1;
    const y2 = destination.y2;
    destination.* = source;
    destination.x1 = x1;
    destination.x2 = x2;
    destination.y1 = y1;
    destination.y2 = y2;
}

fn clearFilterState(filter: *dsp.Biquad) void {
    filter.x1 = 0.0;
    filter.x2 = 0.0;
    filter.y1 = 0.0;
    filter.y2 = 0.0;
}

fn finitePositive(value: f64) bool {
    return std.math.isFinite(value) and value > 0.0;
}

fn deterministicNoise(frame: u64, salt: u64) f64 {
    var bits = frame +% salt;
    bits = (bits ^ (bits >> 30)) *% 0xbf58_476d_1ce4_e5b9;
    bits = (bits ^ (bits >> 27)) *% 0x94d0_49bb_1331_11eb;
    bits ^= bits >> 31;
    const mantissa = bits >> 11;
    const unit = @as(f64, @floatFromInt(mantissa)) /
        @as(f64, @floatFromInt(@as(u64, 1) << 53));
    return unit * 2.0 - 1.0;
}

test "continuous swoosh is driven by velocity without an impact event" {
    var generator = Generator.init();
    var output: [4096]f32 = undefined;

    generator.setControl(.{ .racket_speed_mps = 7.0 });
    @memset(&output, 0.0);
    generator.addTo(&output);
    for (output) |sample| try std.testing.expectEqual(@as(f32, 0.0), sample);

    generator.setControl(.{ .racket_speed_mps = 40.0 });
    var energy: f64 = 0.0;
    for (0..8) |_| {
        @memset(&output, 0.0);
        generator.addTo(&output);
        for (output) |sample| energy += @as(f64, sample) * sample;
    }
    try std.testing.expect(energy > 0.01);

    generator.setControl(.{ .racket_speed_mps = 0.0 });
    for (0..8) |_| {
        @memset(&output, 0.0);
        generator.addTo(&output);
    }
    @memset(&output, 0.0);
    generator.addTo(&output);
    for (output) |sample| try std.testing.expectEqual(@as(f32, 0.0), sample);
}

test "continuous swoosh is independent of host block partitioning" {
    var whole_generator = Generator.init();
    var split_generator = Generator.init();
    const control = Control{ .racket_speed_mps = 40.0 };
    whole_generator.setControl(control);
    split_generator.setControl(control);

    var whole: [8192]f32 = @splat(0.0);
    var split: [8192]f32 = @splat(0.0);
    whole_generator.addTo(&whole);
    var cursor: usize = 0;
    const block_sizes = [_]usize{ 17, 128, 63, 511, 32, 257 };
    var block_index: usize = 0;
    while (cursor < split.len) {
        const count = @min(
            block_sizes[block_index % block_sizes.len],
            split.len - cursor,
        );
        split_generator.addTo(split[cursor .. cursor + count]);
        cursor += count;
        block_index += 1;
    }
    try std.testing.expectEqualSlices(f32, &whole, &split);
}
