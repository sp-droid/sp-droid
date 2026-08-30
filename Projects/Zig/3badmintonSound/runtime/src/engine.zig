const std = @import("std");

pub const preferred_sample_rate_hz: u32 = 48_000;
pub const max_voices: usize = 8;
pub const max_modes_per_voice: usize = 16;
pub const event_queue_capacity: usize = 32;

pub const HitRegion = enum(u32) {
    strings = 0,
    frame = 1,
};

pub const SubmitResult = enum(u32) {
    accepted = 0,
    invalid = 1,
    outside_racket = 2,
    queue_full = 3,
};

/// Immutable runtime configuration. Construct one Engine per independently
/// spatialized racket source. The host should spatialize the mono output.
pub const Config = extern struct {
    sample_rate_hz: u32 = preferred_sample_rate_hz,
    stringed_width_mm: f32 = 188.0,
    stringed_height_mm: f32 = 253.0,
    frame_radial_width_mm: f32 = 9.0,
    // One fixed calibration shared by every hit; never per-hit normalization.
    master_gain: f32 = 3.5,
    swoosh_gain: f32 = 1.0,
};

/// The only per-impact runtime data. Collision speed is the relative normal
/// closing speed; racket speed is supplied continuously with setRacketSpeed.
pub const HitEvent = extern struct {
    main_tension_lbf: f32 = 27.0,
    cross_tension_lbf: f32 = 28.0,
    relative_normal_speed_mps: f32 = 5.0,
    x_mm: f32 = 0.0,
    y_mm: f32 = 0.0,
};

pub const Stats = extern struct {
    submitted_hits: u64,
    dropped_hits: u64,
    rendered_hits: u64,
    stolen_voices: u64,
    rendered_frames: u64,
    active_voices: u32,
};

const ModalPreset = struct {
    frequency_hz_at_30lb: f32,
    relative_gain: f32,
    decay_scale: f32,
};

const string_mode_presets = [_]ModalPreset{
    .{ .frequency_hz_at_30lb = 1289.0, .relative_gain = 1.00, .decay_scale = 1.25 },
    .{ .frequency_hz_at_30lb = 1842.0, .relative_gain = 0.100, .decay_scale = 1.20 },
    .{ .frequency_hz_at_30lb = 2032.0, .relative_gain = 0.631, .decay_scale = 0.95 },
    .{ .frequency_hz_at_30lb = 2730.0, .relative_gain = 0.282, .decay_scale = 0.72 },
    .{ .frequency_hz_at_30lb = 2920.0, .relative_gain = 1.200, .decay_scale = 1.05 },
    .{ .frequency_hz_at_30lb = 3387.0, .relative_gain = 0.112, .decay_scale = 0.66 },
    .{ .frequency_hz_at_30lb = 3698.0, .relative_gain = 0.200, .decay_scale = 0.78 },
    .{ .frequency_hz_at_30lb = 4010.0, .relative_gain = 0.126, .decay_scale = 0.58 },
    .{ .frequency_hz_at_30lb = 4440.0, .relative_gain = 0.251, .decay_scale = 0.74 },
    .{ .frequency_hz_at_30lb = 4543.0, .relative_gain = 0.251, .decay_scale = 0.62 },
    .{ .frequency_hz_at_30lb = 5234.0, .relative_gain = 0.200, .decay_scale = 0.56 },
    .{ .frequency_hz_at_30lb = 6115.0, .relative_gain = 0.282, .decay_scale = 0.48 },
    .{ .frequency_hz_at_30lb = 6848.0, .relative_gain = 0.224, .decay_scale = 0.42 },
    .{ .frequency_hz_at_30lb = 7180.0, .relative_gain = 0.200, .decay_scale = 0.38 },
    .{ .frequency_hz_at_30lb = 9301.0, .relative_gain = 0.020, .decay_scale = 0.28 },
};

const FramePreset = struct {
    frequency_hz: f32,
    relative_gain: f32,
    decay_ms: f32,
    order: u32,
    phase_turns: f32,
};

const frame_mode_presets = [_]FramePreset{
    .{ .frequency_hz = 2350.0, .relative_gain = 0.24, .decay_ms = 15.0, .order = 0, .phase_turns = 0.00 },
    .{ .frequency_hz = 3370.0, .relative_gain = 0.48, .decay_ms = 19.0, .order = 2, .phase_turns = 0.08 },
    .{ .frequency_hz = 4680.0, .relative_gain = 1.00, .decay_ms = 22.0, .order = 3, .phase_turns = 0.17 },
    .{ .frequency_hz = 6120.0, .relative_gain = 0.78, .decay_ms = 17.0, .order = 4, .phase_turns = 0.04 },
    .{ .frequency_hz = 7930.0, .relative_gain = 0.60, .decay_ms = 13.0, .order = 5, .phase_turns = 0.23 },
    .{ .frequency_hz = 10_150.0, .relative_gain = 0.42, .decay_ms = 10.0, .order = 6, .phase_turns = 0.12 },
    .{ .frequency_hz = 12_850.0, .relative_gain = 0.25, .decay_ms = 7.0, .order = 7, .phase_turns = 0.31 },
    .{ .frequency_hz = 16_100.0, .relative_gain = 0.12, .decay_ms = 4.5, .order = 8, .phase_turns = 0.19 },
};

const EventQueue = struct {
    storage: [event_queue_capacity]HitEvent = undefined,
    write_index: std.atomic.Value(u32) align(std.atomic.cache_line) = .init(0),
    read_index: std.atomic.Value(u32) align(std.atomic.cache_line) = .init(0),

    fn push(self: *EventQueue, event: HitEvent) bool {
        const write = self.write_index.load(.monotonic);
        const read = self.read_index.load(.acquire);
        if (write -% read >= event_queue_capacity) return false;
        self.storage[write & (event_queue_capacity - 1)] = event;
        self.write_index.store(write +% 1, .release);
        return true;
    }

    fn pop(self: *EventQueue) ?HitEvent {
        const read = self.read_index.load(.monotonic);
        const write = self.write_index.load(.acquire);
        if (read == write) return null;
        const event = self.storage[read & (event_queue_capacity - 1)];
        self.read_index.store(read +% 1, .release);
        return event;
    }

    fn reset(self: *EventQueue) void {
        self.read_index.store(0, .monotonic);
        self.write_index.store(0, .monotonic);
    }
};

const Mode = struct {
    current: f32 = 0.0,
    previous: f32 = 0.0,
    coefficient: f32 = 0.0,
    radius_squared: f32 = 0.0,

    fn init(
        sample_rate: f32,
        frequency_hz: f32,
        decay_ms: f32,
        amplitude: f32,
        phase: f32,
    ) Mode {
        const frequency = std.math.clamp(frequency_hz, 20.0, sample_rate * 0.45);
        const omega = 2.0 * std.math.pi * frequency / sample_rate;
        const decay_samples = @max(1.0, decay_ms * 0.001 * sample_rate);
        const radius = @exp(-1.0 / decay_samples);
        return .{
            .current = amplitude * @sin(phase),
            .previous = amplitude / radius * @sin(phase - omega),
            .coefficient = 2.0 * radius * @cos(omega),
            .radius_squared = radius * radius,
        };
    }

    inline fn next(self: *Mode) f32 {
        const output = self.current;
        const following = self.coefficient * self.current -
            self.radius_squared * self.previous;
        self.previous = self.current;
        self.current = following;
        return output;
    }
};

const Biquad = struct {
    b0: f32 = 0.0,
    b1: f32 = 0.0,
    b2: f32 = 0.0,
    a1: f32 = 0.0,
    a2: f32 = 0.0,
    x1: f32 = 0.0,
    x2: f32 = 0.0,
    y1: f32 = 0.0,
    y2: f32 = 0.0,

    fn bandPass(sample_rate: f32, frequency_hz: f32, quality_factor: f32) Biquad {
        var result = Biquad{};
        result.setBandPass(sample_rate, frequency_hz, quality_factor);
        return result;
    }

    fn setBandPass(
        self: *Biquad,
        sample_rate: f32,
        frequency_hz: f32,
        quality_factor: f32,
    ) void {
        const frequency = std.math.clamp(frequency_hz, 20.0, sample_rate * 0.45);
        const q = @max(0.05, quality_factor);
        const omega = 2.0 * std.math.pi * frequency / sample_rate;
        const alpha = @sin(omega) / (2.0 * q);
        const a0 = 1.0 + alpha;
        self.b0 = alpha / a0;
        self.b1 = 0.0;
        self.b2 = -alpha / a0;
        self.a1 = -2.0 * @cos(omega) / a0;
        self.a2 = (1.0 - alpha) / a0;
    }

    inline fn process(self: *Biquad, input: f32) f32 {
        const output = self.b0 * input + self.b1 * self.x1 +
            self.b2 * self.x2 - self.a1 * self.y1 - self.a2 * self.y2;
        self.x2 = self.x1;
        self.x1 = input;
        self.y2 = self.y1;
        self.y1 = output;
        return output;
    }
};

const Voice = struct {
    active: bool = false,
    mode_count: u8 = 0,
    modes: [max_modes_per_voice]Mode = @splat(.{}),
    remaining_samples: u32 = 0,
    texture_envelope: f32 = 0.0,
    texture_decay: f32 = 0.0,
    texture_gain: f32 = 0.0,
    hard_envelope: f32 = 0.0,
    hard_decay: f32 = 0.0,
    hard_gain: f32 = 0.0,
    texture_filter: Biquad = .{},
    hard_filter: Biquad = .{},
    rng_state: u64 = 1,

    inline fn next(self: *Voice) f32 {
        var output: f32 = 0.0;
        for (self.modes[0..self.mode_count]) |*mode| output += mode.next();

        if (self.texture_envelope > 1.0e-6 or self.hard_envelope > 1.0e-6) {
            const noise_sample = self.noise();
            output += self.texture_filter.process(noise_sample) *
                self.texture_envelope * self.texture_gain;
            output += self.hard_filter.process(noise_sample) *
                self.hard_envelope * self.hard_gain;
            self.texture_envelope *= self.texture_decay;
            self.hard_envelope *= self.hard_decay;
        }

        self.remaining_samples -|= 1;
        if (self.remaining_samples == 0) self.active = false;
        return output;
    }

    inline fn noise(self: *Voice) f32 {
        var value = self.rng_state;
        value ^= value << 13;
        value ^= value >> 7;
        value ^= value << 17;
        self.rng_state = value;
        const upper: u32 = @truncate(value >> 32);
        return @as(f32, @floatFromInt(upper)) * (2.0 / 4_294_967_295.0) - 1.0;
    }
};

/// Fixed-size, allocation-free real-time synthesizer.
///
/// Threading contract:
/// - one game/physics producer may call submitHit and setRacketSpeed;
/// - one audio consumer calls renderMono;
/// - reset is only legal while both threads are stopped.
pub const Engine = struct {
    config: Config,
    queue: EventQueue = .{},
    voices: [max_voices]Voice = @splat(.{}),
    racket_speed_bits: std.atomic.Value(u32) = .init(0),
    submitted_hits: std.atomic.Value(u64) = .init(0),
    dropped_hits: std.atomic.Value(u64) = .init(0),
    rendered_hits: std.atomic.Value(u64) = .init(0),
    stolen_voices: std.atomic.Value(u64) = .init(0),
    rendered_frames: std.atomic.Value(u64) = .init(0),
    active_voice_count: std.atomic.Value(u32) = .init(0),
    sequence: u64 = 0,
    smoothed_racket_speed_mps: f32 = 0.0,
    swoosh_filter_speed_mps: f32 = 0.0,
    swoosh_frame_filter: Biquad,
    swoosh_string_filter: Biquad,
    swoosh_rng_state: u64 = 0x9e37_79b9_7f4a_7c15,
    compressor_envelope: f32 = 0.0,
    compressor_gain: f32 = 1.0,
    speed_smoothing_coefficient: f32,
    compressor_release_coefficient: f32,

    pub fn init(config: Config) error{InvalidConfig}!Engine {
        if (!validConfig(config)) return error.InvalidConfig;
        const rate: f32 = @floatFromInt(config.sample_rate_hz);
        return .{
            .config = config,
            .swoosh_frame_filter = Biquad.bandPass(rate, 800.0, 0.38),
            .swoosh_string_filter = Biquad.bandPass(rate, 10_000.0, 0.48),
            .speed_smoothing_coefficient = 1.0 - @exp(-1.0 / (0.010 * rate)),
            .compressor_release_coefficient = @exp(-1.0 / (0.040 * rate)),
        };
    }

    pub fn submitHit(self: *Engine, event: HitEvent) SubmitResult {
        if (!validHit(event)) return .invalid;
        if (classifyHit(self.config, event) == null) return .outside_racket;
        if (!self.queue.push(event)) {
            _ = self.dropped_hits.fetchAdd(1, .monotonic);
            return .queue_full;
        }
        _ = self.submitted_hits.fetchAdd(1, .monotonic);
        return .accepted;
    }

    pub fn setRacketSpeed(self: *Engine, speed_mps: f32) void {
        const speed = if (std.math.isFinite(speed_mps)) @max(0.0, speed_mps) else 0.0;
        self.racket_speed_bits.store(@bitCast(speed), .release);
    }

    /// Overwrites a caller-owned mono float buffer at the configured sample
    /// rate. It performs no allocation, locking, I/O, or waiting. Calling it
    /// for every host callback creates one continuous stream, including exact
    /// digital silence when no hit or audible swing is active.
    pub fn renderMono(self: *Engine, output: []f32) void {
        while (self.queue.pop()) |event| self.startVoice(event);

        var active_count = self.countActiveVoices();
        const target_speed: f32 = @bitCast(self.racket_speed_bits.load(.acquire));
        if (active_count == 0 and target_speed <= 7.0 and
            self.smoothed_racket_speed_mps <= 7.0)
        {
            @memset(output, 0.0);
            self.compressor_envelope = 0.0;
            self.compressor_gain = 1.0;
            _ = self.rendered_frames.fetchAdd(output.len, .monotonic);
            return;
        }

        const filter_speed = @max(target_speed, self.smoothed_racket_speed_mps);
        if (filter_speed > 7.0 and
            @abs(filter_speed - self.swoosh_filter_speed_mps) > 0.25)
        {
            const rate: f32 = @floatFromInt(self.config.sample_rate_hz);
            self.swoosh_frame_filter.setBandPass(
                rate,
                std.math.clamp(0.20 * filter_speed / 0.010, 60.0, rate * 0.30),
                0.38,
            );
            self.swoosh_string_filter.setBandPass(
                rate,
                std.math.clamp(0.20 * filter_speed / 0.00066, 600.0, rate * 0.42),
                0.48,
            );
            self.swoosh_filter_speed_mps = filter_speed;
        }

        for (output) |*destination| {
            self.smoothed_racket_speed_mps += self.speed_smoothing_coefficient *
                (target_speed - self.smoothed_racket_speed_mps);
            var sample = self.renderSwoosh();
            for (&self.voices) |*voice| {
                if (voice.active) sample += voice.next();
            }
            sample *= self.config.master_gain;
            destination.* = self.applyDynamics(sample);
        }

        active_count = self.countActiveVoices();
        self.active_voice_count.store(active_count, .release);
        _ = self.rendered_frames.fetchAdd(output.len, .monotonic);
    }

    pub fn reset(self: *Engine) void {
        self.queue.reset();
        self.voices = @splat(.{});
        self.racket_speed_bits.store(0, .monotonic);
        self.submitted_hits.store(0, .monotonic);
        self.dropped_hits.store(0, .monotonic);
        self.rendered_hits.store(0, .monotonic);
        self.stolen_voices.store(0, .monotonic);
        self.rendered_frames.store(0, .monotonic);
        self.active_voice_count.store(0, .monotonic);
        self.sequence = 0;
        self.smoothed_racket_speed_mps = 0.0;
        self.swoosh_filter_speed_mps = 0.0;
        self.swoosh_frame_filter = Biquad.bandPass(
            @floatFromInt(self.config.sample_rate_hz),
            800.0,
            0.38,
        );
        self.swoosh_string_filter = Biquad.bandPass(
            @floatFromInt(self.config.sample_rate_hz),
            10_000.0,
            0.48,
        );
        self.compressor_envelope = 0.0;
        self.compressor_gain = 1.0;
    }

    pub fn stats(self: *const Engine) Stats {
        return .{
            .submitted_hits = self.submitted_hits.load(.acquire),
            .dropped_hits = self.dropped_hits.load(.acquire),
            .rendered_hits = self.rendered_hits.load(.acquire),
            .stolen_voices = self.stolen_voices.load(.acquire),
            .rendered_frames = self.rendered_frames.load(.acquire),
            .active_voices = self.active_voice_count.load(.acquire),
        };
    }

    fn startVoice(self: *Engine, event: HitEvent) void {
        var selected: *Voice = &self.voices[0];
        var found_inactive = false;
        for (&self.voices) |*voice| {
            if (!voice.active) {
                selected = voice;
                found_inactive = true;
                break;
            }
            if (voice.remaining_samples < selected.remaining_samples) selected = voice;
        }
        if (!found_inactive) _ = self.stolen_voices.fetchAdd(1, .monotonic);

        self.sequence +%= 1;
        selected.* = .{
            .active = true,
            .rng_state = mixSeed(self.sequence),
        };
        if (event.relative_normal_speed_mps == 0.0) {
            selected.active = false;
            _ = self.rendered_hits.fetchAdd(1, .monotonic);
            return;
        }
        switch (classifyHit(self.config, event).?) {
            .strings => self.startStringVoice(selected, event),
            .frame => self.startFrameVoice(selected, event),
        }
        _ = self.rendered_hits.fetchAdd(1, .monotonic);
    }

    fn startStringVoice(self: *Engine, voice: *Voice, event: HitEvent) void {
        const rate: f32 = @floatFromInt(self.config.sample_rate_hz);
        const mean_tension = 0.5 * (event.main_tension_lbf + event.cross_tension_lbf);
        const frequency_scale = @sqrt(mean_tension / 30.0);
        const speed_scale = oneAndEighthPower(
            event.relative_normal_speed_mps / 5.0,
        );
        const hard_amount = smoothStep01(
            (event.relative_normal_speed_mps - 6.5) / (20.0 - 6.5),
        );
        const decay_scale = 1.0 - 0.65 * hard_amount;
        const nx = event.x_mm / (0.5 * self.config.stringed_width_mm);
        const ny = event.y_mm / (0.5 * self.config.stringed_height_mm);
        const radius_squared = nx * nx + ny * ny;
        const angle = std.math.atan2(ny, nx);
        var maximum_decay_ms: f32 = 1.0;

        for (string_mode_presets, 0..) |preset, index| {
            const frequency = preset.frequency_hz_at_30lb * frequency_scale;
            const decay_ms = 45.0 * decay_scale * preset.decay_scale *
                fourthRoot(3000.0 / @max(3000.0, frequency));
            maximum_decay_ms = @max(maximum_decay_ms, decay_ms);
            const location_gain = if (index == 0)
                @max(0.18, 1.0 - 0.70 * radius_squared)
            else blk: {
                const order: f32 = @floatFromInt(index % 6 + 1);
                break :blk 0.35 + 0.65 * @abs(@cos(order * angle +
                    @as(f32, @floatFromInt(index)) * 0.43));
            };
            var mode_gain = preset.relative_gain *
                oneAndEighthPower(frequency / 3000.0) * location_gain;
            if (index == 0) mode_gain *= 1.0 - 0.82 * hard_amount;
            const phase = @as(f32, @floatFromInt(index + 1)) * 1.731 +
                0.37 * nx - 0.29 * ny;
            voice.modes[index] = Mode.init(
                rate,
                frequency,
                decay_ms,
                0.040 * speed_scale * mode_gain,
                phase,
            );
        }
        voice.mode_count = string_mode_presets.len;
        voice.remaining_samples = lifetimeSamples(rate, maximum_decay_ms);
        voice.texture_envelope = 1.0;
        voice.texture_decay = @exp(-1.0 / (0.018 * rate));
        voice.texture_gain = 0.035 * speed_scale;
        voice.hard_envelope = hard_amount;
        voice.hard_decay = @exp(-1.0 / (0.004 * rate));
        voice.hard_gain = 0.18 *
            oneAndEighthPower(event.relative_normal_speed_mps / 20.0);
        voice.texture_filter = Biquad.bandPass(rate, 2766.0, 0.36);
        voice.hard_filter = Biquad.bandPass(rate, 5550.0, 0.47);
    }

    fn startFrameVoice(self: *Engine, voice: *Voice, event: HitEvent) void {
        const rate: f32 = @floatFromInt(self.config.sample_rate_hz);
        const speed_scale = oneAndSixteenthPower(
            event.relative_normal_speed_mps / 5.0,
        );
        const outer_nx = event.x_mm /
            (0.5 * (self.config.stringed_width_mm +
                2.0 * self.config.frame_radial_width_mm));
        const outer_ny = event.y_mm /
            (0.5 * (self.config.stringed_height_mm +
                2.0 * self.config.frame_radial_width_mm));
        const angle = std.math.atan2(outer_ny, outer_nx);
        var maximum_decay_ms: f32 = 1.0;
        for (frame_mode_presets, 0..) |preset, index| {
            maximum_decay_ms = @max(maximum_decay_ms, preset.decay_ms);
            const order: f32 = @floatFromInt(preset.order);
            const location_gain = if (preset.order == 0)
                1.0
            else
                @abs(@cos(order * angle + preset.phase_turns *
                    2.0 * std.math.pi));
            const radiation_gain = oneAndEighthPower(
                preset.frequency_hz / 4500.0,
            );
            voice.modes[index] = Mode.init(
                rate,
                preset.frequency_hz,
                preset.decay_ms,
                0.115 * speed_scale * preset.relative_gain *
                    location_gain * radiation_gain,
                @as(f32, @floatFromInt(index + 1)) * 1.217,
            );
        }
        voice.mode_count = frame_mode_presets.len;
        voice.remaining_samples = lifetimeSamples(rate, maximum_decay_ms);
        voice.texture_envelope = 0.0;
        voice.texture_decay = 0.0;
        voice.texture_gain = 0.0;
        voice.hard_envelope = 1.0;
        voice.hard_decay = @exp(-1.0 / (0.0018 * rate));
        voice.hard_gain = 0.18 * speed_scale;
        voice.texture_filter = Biquad.bandPass(rate, 3000.0, 0.40);
        voice.hard_filter = Biquad.bandPass(rate, 7350.0, 0.49);
    }

    inline fn renderSwoosh(self: *Engine) f32 {
        const speed = self.smoothed_racket_speed_mps;
        if (speed <= 7.0) return 0.0;
        const amount = @max(0.0, (speed - 7.0) / (40.0 - 7.0));
        const amplitude = 0.035 * self.config.swoosh_gain * amount * amount * amount;
        const noise = self.swooshNoise();
        return amplitude * (0.72 * self.swoosh_frame_filter.process(noise) +
            0.28 * self.swoosh_string_filter.process(noise));
    }

    inline fn swooshNoise(self: *Engine) f32 {
        var value = self.swoosh_rng_state;
        value ^= value << 13;
        value ^= value >> 7;
        value ^= value << 17;
        self.swoosh_rng_state = value;
        const upper: u32 = @truncate(value >> 32);
        return @as(f32, @floatFromInt(upper)) * (2.0 / 4_294_967_295.0) - 1.0;
    }

    inline fn applyDynamics(self: *Engine, input: f32) f32 {
        const magnitude = @abs(input);
        self.compressor_envelope = @max(
            magnitude,
            self.compressor_envelope * self.compressor_release_coefficient,
        );
        const threshold: f32 = 0.398_107_17; // -8 dBFS
        const target_gain = if (self.compressor_envelope > threshold)
            (threshold + (self.compressor_envelope - threshold) * 0.25) /
                self.compressor_envelope
        else
            1.0;
        if (target_gain < self.compressor_gain) {
            self.compressor_gain = target_gain;
        } else {
            self.compressor_gain = target_gain + self.compressor_release_coefficient *
                (self.compressor_gain - target_gain);
        }
        return std.math.clamp(input * self.compressor_gain, -0.891_250_9, 0.891_250_9);
    }

    fn countActiveVoices(self: *const Engine) u32 {
        var count: u32 = 0;
        for (self.voices) |voice| if (voice.active) {
            count += 1;
        };
        return count;
    }
};

pub fn classifyHit(config: Config, event: HitEvent) ?HitRegion {
    if (!validConfig(config) or !validHit(event)) return null;
    const inner_nx = event.x_mm / (0.5 * config.stringed_width_mm);
    const inner_ny = event.y_mm / (0.5 * config.stringed_height_mm);
    if (inner_nx * inner_nx + inner_ny * inner_ny < 1.0) return .strings;

    const outer_width = config.stringed_width_mm + 2.0 * config.frame_radial_width_mm;
    const outer_height = config.stringed_height_mm + 2.0 * config.frame_radial_width_mm;
    const outer_nx = event.x_mm / (0.5 * outer_width);
    const outer_ny = event.y_mm / (0.5 * outer_height);
    if (outer_nx * outer_nx + outer_ny * outer_ny <= 1.0) return .frame;
    return null;
}

fn validConfig(config: Config) bool {
    return config.sample_rate_hz >= 24_000 and config.sample_rate_hz <= 96_000 and
        finitePositive(config.stringed_width_mm) and
        finitePositive(config.stringed_height_mm) and
        finitePositive(config.frame_radial_width_mm) and
        std.math.isFinite(config.master_gain) and config.master_gain >= 0.0 and
        std.math.isFinite(config.swoosh_gain) and config.swoosh_gain >= 0.0;
}

fn validHit(event: HitEvent) bool {
    return finitePositive(event.main_tension_lbf) and
        finitePositive(event.cross_tension_lbf) and
        std.math.isFinite(event.relative_normal_speed_mps) and
        event.relative_normal_speed_mps >= 0.0 and
        std.math.isFinite(event.x_mm) and std.math.isFinite(event.y_mm);
}

fn finitePositive(value: f32) bool {
    return std.math.isFinite(value) and value > 0.0;
}

fn smoothStep01(value: f32) f32 {
    const amount = std.math.clamp(value, 0.0, 1.0);
    return amount * amount * (3.0 - 2.0 * amount);
}

// Runtime-friendly exponent approximations. These replace general log/exp
// powers in the event-start path with hardware square roots: 1.125 is used for
// calibrated 1.10-1.15 curves and 1.0625 for the frame's 1.05 curve.
fn fourthRoot(value: f32) f32 {
    return @sqrt(@sqrt(@max(0.0, value)));
}

fn oneAndEighthPower(value: f32) f32 {
    const positive = @max(0.0, value);
    return positive * @sqrt(fourthRoot(positive));
}

fn oneAndSixteenthPower(value: f32) f32 {
    const positive = @max(0.0, value);
    return positive * @sqrt(@sqrt(fourthRoot(positive)));
}

fn lifetimeSamples(sample_rate: f32, maximum_decay_ms: f32) u32 {
    return @intFromFloat(@ceil(@min(
        sample_rate * 0.75,
        sample_rate * maximum_decay_ms * 0.001 * 11.0,
    )));
}

fn mixSeed(sequence: u64) u64 {
    var bits = sequence +% 0x9e37_79b9_7f4a_7c15;
    bits = (bits ^ (bits >> 30)) *% 0xbf58_476d_1ce4_e5b9;
    bits = (bits ^ (bits >> 27)) *% 0x94d0_49bb_1331_11eb;
    bits ^= bits >> 31;
    return if (bits == 0) 1 else bits;
}
