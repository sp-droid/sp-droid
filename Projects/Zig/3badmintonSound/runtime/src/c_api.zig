const engine = @import("engine.zig");
const spatializer = @import("spatializer.zig");

pub export fn badminton_audio_engine_size() usize {
    return @sizeOf(engine.Engine);
}

pub export fn badminton_audio_engine_alignment() usize {
    return @alignOf(engine.Engine);
}

pub export fn badminton_audio_default_config() engine.Config {
    return .{};
}

pub export fn badminton_audio_init(
    memory: ?*anyopaque,
    memory_size: usize,
    config: ?*const engine.Config,
) ?*engine.Engine {
    const raw = memory orelse return null;
    if (memory_size < @sizeOf(engine.Engine) or
        @intFromPtr(raw) % @alignOf(engine.Engine) != 0)
    {
        return null;
    }
    const instance: *engine.Engine = @ptrFromInt(@intFromPtr(raw));
    instance.initInPlace(if (config) |value| value.* else .{}) catch
        return null;
    return instance;
}

pub export fn badminton_audio_submit_hit(
    instance: ?*engine.Engine,
    event: ?*const engine.HitEvent,
) u32 {
    const synth = instance orelse return @intFromEnum(engine.SubmitResult.invalid);
    const hit = event orelse return @intFromEnum(engine.SubmitResult.invalid);
    return @intFromEnum(synth.submitHit(hit.*));
}

pub export fn badminton_audio_set_racket_speed(
    instance: ?*engine.Engine,
    speed_mps: f32,
) void {
    const synth = instance orelse return;
    synth.setRacketSpeed(speed_mps);
}

pub export fn badminton_audio_set_racket_geometry(
    instance: ?*engine.Engine,
    stringed_width_mm: f32,
    stringed_height_mm: f32,
) void {
    const synth = instance orelse return;
    synth.setRacketGeometry(stringed_width_mm, stringed_height_mm);
}

pub export fn badminton_audio_clear_transients(instance: ?*engine.Engine) u32 {
    const synth = instance orelse return 0;
    return @intFromBool(synth.requestClearTransients());
}

pub export fn badminton_audio_render_mono(
    instance: ?*engine.Engine,
    output: ?[*]f32,
    frame_count: usize,
) void {
    const synth = instance orelse return;
    const samples = output orelse return;
    synth.renderMono(samples[0..frame_count]);
}

pub export fn badminton_audio_get_stats(
    instance: ?*const engine.Engine,
    output: ?*engine.Stats,
) void {
    const synth = instance orelse return;
    const destination = output orelse return;
    destination.* = synth.stats();
}

pub export fn badminton_audio_reset(instance: ?*engine.Engine) void {
    const synth = instance orelse return;
    synth.reset();
}

pub export fn badminton_spatializer_size() usize {
    return @sizeOf(spatializer.Spatializer);
}

pub export fn badminton_spatializer_alignment() usize {
    return @alignOf(spatializer.Spatializer);
}

pub export fn badminton_spatializer_init(
    memory: ?*anyopaque,
    memory_size: usize,
    sample_rate_hz: u32,
) ?*spatializer.Spatializer {
    const raw = memory orelse return null;
    if (memory_size < @sizeOf(spatializer.Spatializer) or
        @intFromPtr(raw) % @alignOf(spatializer.Spatializer) != 0)
    {
        return null;
    }
    const instance: *spatializer.Spatializer = @ptrFromInt(@intFromPtr(raw));
    instance.* = spatializer.Spatializer.init(sample_rate_hz) catch return null;
    return instance;
}

pub export fn badminton_spatializer_render(
    instance: ?*spatializer.Spatializer,
    mono: ?[*]const f32,
    stereo_interleaved: ?[*]f32,
    frame_count: usize,
    relative: spatializer.RelativePosition,
) void {
    const renderer = instance orelse return;
    const input = mono orelse return;
    const output = stereo_interleaved orelse return;
    renderer.render(input[0..frame_count], output[0 .. frame_count * 2], relative);
}

pub export fn badminton_spatializer_reset(
    instance: ?*spatializer.Spatializer,
) void {
    const renderer = instance orelse return;
    renderer.reset();
}
