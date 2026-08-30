const engine = @import("engine.zig");

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
    instance.* = engine.Engine.init(if (config) |value| value.* else .{}) catch
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
