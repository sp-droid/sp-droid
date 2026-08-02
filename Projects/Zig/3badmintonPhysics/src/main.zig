const std = @import("std");
const rl = @import("raylib");
const sim = @import("simulation.zig");
const renderer = @import("renderer.zig");
const physics_runtime = @import("physics_loop.zig");

const sample_rate = 44_100;
const sample_count = sample_rate / 12;
var launch_samples: [sample_count]i16 = undefined;

pub fn main(init: std.process.Init) !void {
    const capture_reverse_contact = try hasArgument(init, "--screenshot-net-contact-reverse");
    const capture_net_contact = capture_reverse_contact or try hasArgument(init, "--screenshot-net-contact");
    const capture_preview = capture_net_contact or try hasArgument(init, "--screenshot");

    rl.setTraceLogLevel(.warning);
    rl.setConfigFlags(.{ .msaa_4x_hint = true, .window_highdpi = true });
    rl.initWindow(1, 1, "Badminton Shuttlecock Dynamics");
    defer rl.closeWindow();
    rl.toggleFullscreen();
    rl.setTargetFPS(1000);

    rl.initAudioDevice();
    defer rl.closeAudioDevice();
    const launch_sound = createLaunchSound();
    defer if (launch_sound) |sound| rl.unloadSound(sound);

    var view = try renderer.Renderer.init(rl.getScreenWidth(), rl.getScreenHeight());
    defer view.deinit();

    const seed_high: u64 = @intCast(rl.getRandomValue(1, 0x3fff_ffff));
    const seed_low: u64 = @intCast(rl.getRandomValue(1, 0x3fff_ffff));
    const seed = if (capture_net_contact) @as(u64, 81273) else (seed_high << 32) ^ seed_low;
    var initial_simulation = sim.Simulation.init(seed);
    if (capture_net_contact) seekNetContact(&initial_simulation);
    if (capture_net_contact) view.focusContactPreview(initial_simulation.shuttle.position, capture_reverse_contact);
    playLaunchSound(launch_sound);

    var physics = physics_runtime.PhysicsLoop.init(init.io, initial_simulation);
    try physics.start();
    defer physics.deinit();
    physics.setTimeScale(view.timeScale());

    var acknowledged_automatic_launch: u64 = 0;
    var captured_frames: u16 = 0;

    while (!rl.windowShouldClose()) {
        const frame_time = @min(rl.getFrameTime(), 0.05);
        if (view.updateControls(frame_time)) |shot| {
            acknowledged_automatic_launch = physics.launchShot(shot);
            playLaunchSound(launch_sound);
        }
        physics.setTimeScale(view.timeScale());

        const state = physics.snapshot();
        if (state.automatic_launch_serial != acknowledged_automatic_launch) {
            acknowledged_automatic_launch = state.automatic_launch_serial;
            playLaunchSound(launch_sound);
        }
        const visible_shuttle = state.simulation.predictedShuttle(state.pending_time);

        rl.beginDrawing();
        view.draw(&state.simulation, &visible_shuttle);
        rl.endDrawing();

        if (capture_preview) {
            captured_frames += 1;
            const contact_frame = capture_net_contact and state.simulation.net.contact_timer > 0;
            const regular_frame = !capture_net_contact and captured_frames == 300;
            const contact_timeout = capture_net_contact and captured_frames == 5000;
            if (contact_frame or regular_frame or contact_timeout) {
                rl.takeScreenshot("simulator-preview.png");
                break;
            }
        }
    }
}

fn seekNetContact(simulation: *sim.Simulation) void {
    for (0..40) |_| {
        simulation.launchShot(.net_roll);
        const start_x = simulation.shuttle.position.x;
        for (0..1500) |_| {
            _ = simulation.step(1.0 / 1000.0);
            if (simulation.net.contact_count > 0) return;
            if (start_x * simulation.shuttle.position.x < -0.15 or simulation.phase == .waiting) break;
        }
    }
}

fn hasArgument(init: std.process.Init, wanted: []const u8) !bool {
    var args = try std.process.Args.Iterator.initAllocator(init.minimal.args, init.gpa);
    defer args.deinit();
    _ = args.skip();
    while (args.next()) |argument| {
        if (std.mem.eql(u8, argument, wanted)) return true;
    }
    return false;
}

fn createLaunchSound() ?rl.Sound {
    if (!rl.isAudioDeviceReady()) return null;

    var phase: f32 = 0;
    for (&launch_samples, 0..) |*sample, index| {
        const progress = @as(f32, @floatFromInt(index)) / @as(f32, @floatFromInt(sample_count));
        const envelope = (1.0 - progress) * (1.0 - progress);
        const frequency = 780.0 - 260.0 * progress;
        phase += 2.0 * std.math.pi * frequency / sample_rate;
        const value = (@sin(phase) * 0.72 + @sin(phase * 2.03) * 0.16) * envelope;
        sample.* = @intFromFloat(@max(-1.0, @min(1.0, value)) * 22_000.0);
    }

    const wave = rl.Wave{
        .frameCount = sample_count,
        .sampleRate = sample_rate,
        .sampleSize = 16,
        .channels = 1,
        .data = @ptrCast(&launch_samples[0]),
    };
    return rl.loadSoundFromWave(wave);
}

fn playLaunchSound(sound: ?rl.Sound) void {
    if (sound) |ready_sound| rl.playSound(ready_sound);
}
