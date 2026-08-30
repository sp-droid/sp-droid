const std = @import("std");

pub const Complex = struct {
    re: f64,
    im: f64,

    fn add(a: Complex, b: Complex) Complex {
        return .{ .re = a.re + b.re, .im = a.im + b.im };
    }

    fn sub(a: Complex, b: Complex) Complex {
        return .{ .re = a.re - b.re, .im = a.im - b.im };
    }

    fn mul(a: Complex, b: Complex) Complex {
        return .{
            .re = a.re * b.re - a.im * b.im,
            .im = a.re * b.im + a.im * b.re,
        };
    }
};

pub const Biquad = struct {
    b0: f64,
    b1: f64,
    b2: f64,
    a1: f64,
    a2: f64,
    x1: f64 = 0.0,
    x2: f64 = 0.0,
    y1: f64 = 0.0,
    y2: f64 = 0.0,

    pub fn bandPass(sample_rate: f64, frequency_hz: f64, quality_factor: f64) Biquad {
        const frequency = std.math.clamp(frequency_hz, 20.0, sample_rate * 0.45);
        const q = @max(0.05, quality_factor);
        const omega = 2.0 * std.math.pi * frequency / sample_rate;
        const sine = @sin(omega);
        const cosine = @cos(omega);
        const alpha = sine / (2.0 * q);
        const a0 = 1.0 + alpha;
        return .{
            .b0 = alpha / a0,
            .b1 = 0.0,
            .b2 = -alpha / a0,
            .a1 = (-2.0 * cosine) / a0,
            .a2 = (1.0 - alpha) / a0,
        };
    }

    pub fn lowPass(sample_rate: f64, frequency_hz: f64, quality_factor: f64) Biquad {
        const frequency = std.math.clamp(frequency_hz, 20.0, sample_rate * 0.45);
        const q = @max(0.05, quality_factor);
        const omega = 2.0 * std.math.pi * frequency / sample_rate;
        const sine = @sin(omega);
        const cosine = @cos(omega);
        const alpha = sine / (2.0 * q);
        const a0 = 1.0 + alpha;
        return .{
            .b0 = 0.5 * (1.0 - cosine) / a0,
            .b1 = (1.0 - cosine) / a0,
            .b2 = 0.5 * (1.0 - cosine) / a0,
            .a1 = (-2.0 * cosine) / a0,
            .a2 = (1.0 - alpha) / a0,
        };
    }

    pub fn highPass(sample_rate: f64, frequency_hz: f64, quality_factor: f64) Biquad {
        const frequency = std.math.clamp(frequency_hz, 20.0, sample_rate * 0.45);
        const q = @max(0.05, quality_factor);
        const omega = 2.0 * std.math.pi * frequency / sample_rate;
        const sine = @sin(omega);
        const cosine = @cos(omega);
        const alpha = sine / (2.0 * q);
        const a0 = 1.0 + alpha;
        return .{
            .b0 = 0.5 * (1.0 + cosine) / a0,
            .b1 = -(1.0 + cosine) / a0,
            .b2 = 0.5 * (1.0 + cosine) / a0,
            .a1 = (-2.0 * cosine) / a0,
            .a2 = (1.0 - alpha) / a0,
        };
    }

    pub fn process(self: *Biquad, input: f64) f64 {
        const output = self.b0 * input +
            self.b1 * self.x1 +
            self.b2 * self.x2 -
            self.a1 * self.y1 -
            self.a2 * self.y2;
        self.x2 = self.x1;
        self.x1 = input;
        self.y2 = self.y1;
        self.y1 = output;
        return output;
    }
};

pub fn decimateLowPass(
    allocator: std.mem.Allocator,
    input: []const f64,
    input_rate: u32,
    output_rate: u32,
    output_frames: usize,
) ![]f32 {
    if (input_rate % output_rate != 0) return error.NonIntegralSampleRateRatio;
    const ratio: usize = input_rate / output_rate;
    if (ratio == 0) return error.NonIntegralSampleRateRatio;

    const tap_count: usize = 63;
    const half: isize = @intCast(tap_count / 2);
    var taps: [tap_count]f64 = undefined;
    const cutoff = 0.45 / @as(f64, @floatFromInt(ratio));
    var tap_sum: f64 = 0.0;
    for (&taps, 0..) |*tap, index| {
        const n: isize = @as(isize, @intCast(index)) - half;
        const x = @as(f64, @floatFromInt(n));
        const sinc = if (n == 0)
            2.0 * cutoff
        else
            @sin(2.0 * std.math.pi * cutoff * x) / (std.math.pi * x);
        const phase = @as(f64, @floatFromInt(index)) /
            @as(f64, @floatFromInt(tap_count - 1));
        const blackman = 0.42 -
            0.5 * @cos(2.0 * std.math.pi * phase) +
            0.08 * @cos(4.0 * std.math.pi * phase);
        tap.* = sinc * blackman;
        tap_sum += tap.*;
    }
    for (&taps) |*tap| tap.* /= tap_sum;

    const output = try allocator.alloc(f32, output_frames);
    errdefer allocator.free(output);
    for (output, 0..) |*sample, output_index| {
        const center: isize = @intCast(output_index * ratio);
        var sum: f64 = 0.0;
        for (taps, 0..) |tap, tap_index| {
            const source_index = center + @as(isize, @intCast(tap_index)) - half;
            if (source_index >= 0 and source_index < input.len) {
                sum += input[@intCast(source_index)] * tap;
            }
        }
        sample.* = @floatCast(sum);
    }
    return output;
}

pub fn fftInPlace(values: []Complex) !void {
    const n = values.len;
    if (n == 0 or !std.math.isPowerOfTwo(n)) return error.InvalidFftSize;

    var j: usize = 0;
    var i: usize = 1;
    while (i < n) : (i += 1) {
        var bit = n >> 1;
        while ((j & bit) != 0) {
            j ^= bit;
            bit >>= 1;
        }
        j ^= bit;
        if (i < j) std.mem.swap(Complex, &values[i], &values[j]);
    }

    var length: usize = 2;
    while (length <= n) : (length *= 2) {
        const angle = -2.0 * std.math.pi / @as(f64, @floatFromInt(length));
        const root = Complex{ .re = @cos(angle), .im = @sin(angle) };
        var block: usize = 0;
        while (block < n) : (block += length) {
            var w = Complex{ .re = 1.0, .im = 0.0 };
            for (0..length / 2) |offset| {
                const even = values[block + offset];
                const odd = Complex.mul(values[block + offset + length / 2], w);
                values[block + offset] = Complex.add(even, odd);
                values[block + offset + length / 2] = Complex.sub(even, odd);
                w = Complex.mul(w, root);
            }
        }
    }
}

pub fn magnitudeSpectrumDb(
    allocator: std.mem.Allocator,
    samples: []const f32,
    fft_size: usize,
) ![]f32 {
    if (fft_size == 0 or !std.math.isPowerOfTwo(fft_size)) return error.InvalidFftSize;
    var values = try allocator.alloc(Complex, fft_size);
    defer allocator.free(values);

    for (values, 0..) |*value, index| {
        const sample: f64 = if (index < samples.len) samples[index] else 0.0;
        const phase = if (fft_size == 1)
            0.0
        else
            @as(f64, @floatFromInt(index)) / @as(f64, @floatFromInt(fft_size - 1));
        const hann = 0.5 - 0.5 * @cos(2.0 * std.math.pi * phase);
        value.* = .{ .re = sample * hann, .im = 0.0 };
    }
    try fftInPlace(values);

    const magnitudes = try allocator.alloc(f32, fft_size / 2 + 1);
    errdefer allocator.free(magnitudes);
    const scale = 2.0 / @as(f64, @floatFromInt(fft_size));
    for (magnitudes, values[0 .. fft_size / 2 + 1]) |*magnitude, value| {
        const linear = @sqrt(value.re * value.re + value.im * value.im) * scale;
        magnitude.* = @floatCast(20.0 * std.math.log10(@max(linear, 1.0e-12)));
    }
    return magnitudes;
}

pub fn dominantFrequency(
    allocator: std.mem.Allocator,
    samples: []const f32,
    sample_rate: u32,
    min_hz: f64,
    max_hz: f64,
) !f64 {
    var fft_size: usize = 1;
    const desired = @min(samples.len, 16_384);
    while (fft_size * 2 <= desired) fft_size *= 2;
    if (fft_size < 256) return 0.0;
    const spectrum = try magnitudeSpectrumDb(allocator, samples, fft_size);
    defer allocator.free(spectrum);

    const bin_hz = @as(f64, @floatFromInt(sample_rate)) /
        @as(f64, @floatFromInt(fft_size));
    const first_bin: usize = @intFromFloat(@max(1.0, @ceil(min_hz / bin_hz)));
    const last_bin: usize = @min(spectrum.len - 1, @as(usize, @intFromFloat(@floor(max_hz / bin_hz))));
    if (first_bin > last_bin) return 0.0;

    var peak_bin = first_bin;
    var peak_value = spectrum[first_bin];
    for (spectrum[first_bin .. last_bin + 1], first_bin..) |value, bin| {
        if (value > peak_value) {
            peak_value = value;
            peak_bin = bin;
        }
    }
    return @as(f64, @floatFromInt(peak_bin)) * bin_hz;
}

pub fn buildPcm16Wav(
    allocator: std.mem.Allocator,
    samples: []const f32,
    sample_rate: u32,
) ![]u8 {
    const data_size = std.math.mul(usize, samples.len, 2) catch return error.WavTooLarge;
    if (data_size > std.math.maxInt(u32) - 36) return error.WavTooLarge;
    const bytes = try allocator.alloc(u8, 44 + data_size);
    errdefer allocator.free(bytes);

    @memcpy(bytes[0..4], "RIFF");
    putU32(bytes[4..8], @intCast(36 + data_size));
    @memcpy(bytes[8..12], "WAVE");
    @memcpy(bytes[12..16], "fmt ");
    putU32(bytes[16..20], 16);
    putU16(bytes[20..22], 1);
    putU16(bytes[22..24], 1);
    putU32(bytes[24..28], sample_rate);
    putU32(bytes[28..32], sample_rate * 2);
    putU16(bytes[32..34], 2);
    putU16(bytes[34..36], 16);
    @memcpy(bytes[36..40], "data");
    putU32(bytes[40..44], @intCast(data_size));

    for (samples, 0..) |sample, index| {
        const clamped = std.math.clamp(@as(f64, sample), -1.0, 1.0);
        const quantized: i16 = @intFromFloat(@round(clamped * 32767.0));
        putU16(bytes[44 + index * 2 .. 46 + index * 2], @bitCast(quantized));
    }
    return bytes;
}

fn putU16(destination: []u8, value: u16) void {
    destination[0] = @truncate(value);
    destination[1] = @truncate(value >> 8);
}

fn putU32(destination: []u8, value: u32) void {
    destination[0] = @truncate(value);
    destination[1] = @truncate(value >> 8);
    destination[2] = @truncate(value >> 16);
    destination[3] = @truncate(value >> 24);
}

fn readU32(source: []const u8) u32 {
    return @as(u32, source[0]) |
        (@as(u32, source[1]) << 8) |
        (@as(u32, source[2]) << 16) |
        (@as(u32, source[3]) << 24);
}

test "radix two FFT locates a sinusoid" {
    const allocator = std.testing.allocator;
    const rate: u32 = 48_000;
    const count: usize = 4096;
    const samples = try allocator.alloc(f32, count);
    defer allocator.free(samples);
    for (samples, 0..) |*sample, index| {
        sample.* = @floatCast(@sin(2.0 * std.math.pi * 1125.0 *
            @as(f64, @floatFromInt(index)) / @as(f64, @floatFromInt(rate))));
    }
    const peak = try dominantFrequency(allocator, samples, rate, 300.0, 3000.0);
    try std.testing.expectApproxEqAbs(@as(f64, 1125.0), peak, 12.0);
}

test "PCM WAV writer emits a valid unnormalised header and payload" {
    const allocator = std.testing.allocator;
    const samples = [_]f32{ 0.0, 0.25, -0.5, 1.0 };
    const bytes = try buildPcm16Wav(allocator, &samples, 48_000);
    defer allocator.free(bytes);
    try std.testing.expectEqualStrings("RIFF", bytes[0..4]);
    try std.testing.expectEqualStrings("WAVE", bytes[8..12]);
    try std.testing.expectEqual(@as(u32, 48_000), readU32(bytes[24..28]));
    try std.testing.expectEqual(@as(u32, samples.len * 2), readU32(bytes[40..44]));
    try std.testing.expect(bytes[48] != 0 or bytes[49] != 0);
}

test "low-pass decimator preserves a DC signal away from the leading edge" {
    const allocator = std.testing.allocator;
    const input = try allocator.alloc(f64, 4096);
    defer allocator.free(input);
    @memset(input, 0.25);
    const output = try decimateLowPass(allocator, input, 192_000, 48_000, 1024);
    defer allocator.free(output);
    try std.testing.expectApproxEqAbs(@as(f32, 0.25), output[100], 1.0e-4);
}

test "biquad low-pass and high-pass separate DC from Nyquist alternation" {
    var low_pass_dc = Biquad.lowPass(48_000.0, 3_000.0, 0.707);
    var high_pass_dc = Biquad.highPass(48_000.0, 3_000.0, 0.707);
    var low_pass_alternating = Biquad.lowPass(48_000.0, 3_000.0, 0.707);
    var high_pass_alternating = Biquad.highPass(48_000.0, 3_000.0, 0.707);
    var low_dc: f64 = 0.0;
    var high_dc: f64 = 0.0;
    var low_alternating: f64 = 0.0;
    var high_alternating: f64 = 0.0;
    for (0..4096) |index| {
        low_dc = low_pass_dc.process(1.0);
        high_dc = high_pass_dc.process(1.0);
        const alternating: f64 = if (index % 2 == 0) 1.0 else -1.0;
        low_alternating = low_pass_alternating.process(alternating);
        high_alternating = high_pass_alternating.process(alternating);
    }
    try std.testing.expect(@abs(low_dc) > 0.9);
    try std.testing.expect(@abs(high_dc) < 1.0e-6);
    try std.testing.expect(@abs(low_alternating) < 0.1);
    try std.testing.expect(@abs(high_alternating) > 0.8);
}
