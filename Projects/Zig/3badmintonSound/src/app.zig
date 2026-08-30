const std = @import("std");
const rl = @import("raylib");
const gui = @import("raygui");
const model = @import("model.zig");
const dsp = @import("dsp.zig");
const simulator = @import("simulator.zig");
const worker_module = @import("worker.zig");
const audio_module = @import("audio_engine.zig");

const profile_path: [:0]const u8 = "badminton_profile.json";
const wav_path: [:0]const u8 = "current_hit.wav";

const palette = struct {
    const background = color(9, 13, 18, 255);
    const sidebar = color(15, 21, 29, 255);
    const panel = color(20, 28, 38, 255);
    const panel_alt = color(25, 35, 47, 255);
    const border = color(49, 65, 82, 255);
    const text = color(229, 236, 242, 255);
    const muted = color(137, 153, 169, 255);
    const cyan = color(64, 214, 220, 255);
    const blue = color(83, 153, 255, 255);
    const violet = color(169, 111, 255, 255);
    const amber = color(255, 190, 85, 255);
    const red = color(255, 94, 105, 255);
    const green = color(81, 215, 151, 255);
};

const SectionState = struct {
    hit: bool = true,
    geometry: bool = false,
    strings: bool = false,
    contact: bool = false,
    frame: bool = false,
    frame_hit: bool = false,
    upper_modes: bool = false,
    hard_impact: bool = false,
    swoosh: bool = false,
    acoustics: bool = false,
    numerics: bool = false,
};

const App = struct {
    allocator: std.mem.Allocator,
    worker: worker_module.RenderWorker,
    audio: audio_module.AudioEngine,
    profile: model.Profile = .{},
    result: ?*simulator.SimulationResult = null,
    sections: SectionState = .{},
    sidebar_scroll: f32 = 0.0,
    sidebar_content_height: f32 = 0.0,
    next_replay_time: f64 = 0.0,
    animation_started: f64 = 0.0,
    was_valid: bool = false,
    status_buffer: [192:0]u8 = undefined,
    status_length: usize = 0,

    fn init(io: std.Io) App {
        const allocator = std.heap.smp_allocator;
        var app = App{
            .allocator = allocator,
            .worker = worker_module.RenderWorker.init(io, allocator),
            .audio = audio_module.AudioEngine.init(io, allocator),
        };
        app.setStatus("Ready - continuous audio engine starting", .{});
        return app;
    }

    fn deinit(self: *App) void {
        self.worker.deinit();
        self.audio.deinit();
        if (self.result) |result| {
            result.deinit();
            self.allocator.destroy(result);
        }
    }

    fn update(self: *App) void {
        const now = rl.getTime();
        if (self.worker.takeCompleted()) |completed| {
            self.installResult(completed, now);
        }
        if (self.worker.takeError()) |message| {
            self.setStatus("Render failed: {s}", .{message});
        }
        if (self.audio.takeError()) |message| {
            self.setStatus("Continuous audio failed: {s}", .{message});
        }

        // Aerodynamic sound follows the current velocity every stream block,
        // even if contact coordinates are invalid or no strike is rendering.
        self.audio.setSwoosh(self.profile);

        if (validationError(self.profile)) |_| {
            self.was_valid = false;
            return;
        }
        if (!self.was_valid) {
            self.was_valid = true;
            self.next_replay_time = now;
        }
        if (now >= self.next_replay_time) {
            _ = self.worker.request(self.profile);
            self.next_replay_time = now + self.profile.model.replay_interval_s;
        }
    }

    fn installResult(
        self: *App,
        completed: *simulator.SimulationResult,
        now: f64,
    ) void {
        const queued = self.audio.submit(
            completed.impact_only_audio,
            completed.output_sample_rate_hz,
        ) catch |err| failed: {
            self.setStatus("Audio queue failed: {s}", .{@errorName(err)});
            break :failed false;
        };
        if (self.result) |old| {
            old.deinit();
            self.allocator.destroy(old);
        }
        self.result = completed;
        self.animation_started = now + completed.impactTimeSeconds();
        if (queued) {
            self.setStatus(
                "Stream queued; impact in {d:.0} ms, {d:.1} Hz, {d:.1} dB GR, rendered in {d:.0} ms",
                .{
                    completed.impactTimeSeconds() * 1000.0,
                    completed.diagnostics.dominant_frequency_hz,
                    completed.diagnostics.maximum_gain_reduction_db,
                    completed.diagnostics.render_time_ms,
                },
            );
        }
    }

    fn requestNow(self: *App) void {
        if (validationError(self.profile)) |err| {
            self.setStatus("Cannot render: {s}", .{@errorName(err)});
            return;
        }
        if (self.worker.request(self.profile)) {
            self.next_replay_time =
                rl.getTime() + self.profile.model.replay_interval_s;
            self.setStatus("Rendering a new strike...", .{});
        } else {
            self.setStatus("Render already in progress; request skipped", .{});
        }
    }

    fn saveProfile(self: *App) void {
        const json = model.profileToJson(self.allocator, self.profile) catch |err| {
            self.setStatus("Profile serialization failed: {s}", .{@errorName(err)});
            return;
        };
        defer self.allocator.free(json);
        const terminated = self.allocator.dupeZ(u8, json) catch {
            self.setStatus("Profile serialization failed: OutOfMemory", .{});
            return;
        };
        defer self.allocator.free(terminated);
        if (rl.saveFileText(profile_path, terminated)) {
            self.setStatus("Saved {s}", .{profile_path});
        } else {
            self.setStatus("Could not save {s}", .{profile_path});
        }
    }

    fn loadProfile(self: *App) void {
        if (!rl.fileExists(profile_path)) {
            self.setStatus("{s} does not exist yet", .{profile_path});
            return;
        }
        const text = rl.loadFileText(profile_path);
        defer rl.unloadFileText(text);
        var parsed = model.profileFromJson(self.allocator, text) catch |err| {
            self.setStatus("Profile parse failed: {s}", .{@errorName(err)});
            return;
        };
        defer parsed.deinit();
        model.validate(parsed.value) catch |err| {
            self.setStatus("Profile rejected: {s}", .{@errorName(err)});
            return;
        };
        self.profile = parsed.value;
        self.was_valid = false;
        self.setStatus("Loaded {s}", .{profile_path});
    }

    fn exportWav(self: *App) void {
        const result = self.result orelse {
            self.setStatus("Render a hit before exporting WAV", .{});
            return;
        };
        const bytes = dsp.buildPcm16Wav(
            self.allocator,
            result.audio,
            result.output_sample_rate_hz,
        ) catch |err| {
            self.setStatus("WAV export failed: {s}", .{@errorName(err)});
            return;
        };
        defer self.allocator.free(bytes);
        if (rl.saveFileData(wav_path, bytes)) {
            self.setStatus("Exported {s} without normalization", .{wav_path});
        } else {
            self.setStatus("Could not save {s}", .{wav_path});
        }
    }

    fn resetProfile(self: *App) void {
        self.profile = .{};
        self.was_valid = false;
        self.sidebar_scroll = 0.0;
        self.setStatus("Reset to calibrated BG66 defaults", .{});
    }

    fn setStatus(self: *App, comptime format: []const u8, args: anytype) void {
        const rendered_status = std.fmt.bufPrintZ(&self.status_buffer, format, args) catch {
            self.status_buffer[0] = 0;
            self.status_length = 0;
            return;
        };
        self.status_length = rendered_status.len;
    }

    fn status(self: *const App) [:0]const u8 {
        return self.status_buffer[0..self.status_length :0];
    }
};

pub const RunOptions = struct {
    smoke_test: bool = false,
};

pub fn run(io: std.Io, options: RunOptions) !void {
    rl.setTraceLogLevel(.warning);
    rl.setConfigFlags(.{
        .window_resizable = true,
        .window_highdpi = true,
        .msaa_4x_hint = true,
    });
    rl.initWindow(1440, 920, "Badminton Impact Lab - physics-based synthesis");
    if (!rl.isWindowReady()) return error.WindowInitializationFailed;
    defer rl.closeWindow();
    rl.setWindowMinSize(1100, 720);
    rl.setExitKey(.null);
    rl.setTargetFPS(60);

    configureGuiStyle();

    var app = App.init(io);
    try app.audio.start();
    errdefer app.audio.deinit();
    try app.worker.start();
    defer app.deinit();

    while (!rl.windowShouldClose()) {
        app.update();
        rl.beginDrawing();
        rl.clearBackground(palette.background);
        drawApplication(&app);
        rl.endDrawing();
        const audio_status = app.audio.status();
        if (options.smoke_test and app.result != null and
            (audio_status.ready or audio_status.failed) and
            rl.getTime() - app.animation_started >= 0.42)
        {
            rl.takeScreenshot("ui-smoke.png");
            break;
        }
    }
}

fn drawApplication(app: *App) void {
    const width: f32 = @floatFromInt(rl.getScreenWidth());
    const height: f32 = @floatFromInt(rl.getScreenHeight());
    const sidebar_width: f32 = 390.0;

    rl.drawRectangleRec(.{ .x = 0, .y = 0, .width = sidebar_width, .height = height }, palette.sidebar);
    rl.drawLineEx(
        .{ .x = sidebar_width, .y = 0 },
        .{ .x = sidebar_width, .y = height },
        1,
        palette.border,
    );

    drawSidebar(app, .{ .x = 0, .y = 0, .width = sidebar_width, .height = height });
    drawWorkspace(
        app,
        .{
            .x = sidebar_width,
            .y = 0,
            .width = width - sidebar_width,
            .height = height,
        },
    );
}

fn drawSidebar(app: *App, bounds: rl.Rectangle) void {
    rl.drawText("MODEL CONTROLS", 18, 17, 19, palette.text);
    rl.drawText("All values are part of the saved JSON profile", 18, 42, 10, palette.muted);

    const gap: f32 = 8;
    const button_width = (bounds.width - 36 - gap) * 0.5;
    if (gui.button(.{ .x = 18, .y = 64, .width = button_width, .height = 31 }, "SIMULATE NOW")) {
        app.requestNow();
    }
    if (gui.button(.{ .x = 18 + button_width + gap, .y = 64, .width = button_width, .height = 31 }, "EXPORT WAV")) {
        app.exportWav();
    }
    const third = (bounds.width - 36 - gap * 2) / 3;
    if (gui.button(.{ .x = 18, .y = 101, .width = third, .height = 27 }, "SAVE JSON")) {
        app.saveProfile();
    }
    if (gui.button(.{ .x = 18 + third + gap, .y = 101, .width = third, .height = 27 }, "LOAD JSON")) {
        app.loadProfile();
    }
    if (gui.button(.{ .x = 18 + (third + gap) * 2, .y = 101, .width = third, .height = 27 }, "RESET")) {
        app.resetProfile();
    }

    const viewport = rl.Rectangle{
        .x = 12,
        .y = 140,
        .width = bounds.width - 24,
        .height = @max(100.0, bounds.height - 188),
    };
    rl.drawRectangleRec(viewport, palette.background);
    rl.drawRectangleLinesEx(viewport, 1, palette.border);

    const mouse = rl.getMousePosition();
    if (rl.checkCollisionPointRec(mouse, viewport)) {
        app.sidebar_scroll += rl.getMouseWheelMove() * 48.0;
    }
    const maximum_scroll = @max(0.0, app.sidebar_content_height - viewport.height + 14.0);
    app.sidebar_scroll = std.math.clamp(app.sidebar_scroll, -maximum_scroll, 0.0);

    rl.beginScissorMode(
        @intFromFloat(viewport.x),
        @intFromFloat(viewport.y),
        @intFromFloat(viewport.width),
        @intFromFloat(viewport.height),
    );
    var controls = ControlEditor.init(viewport, app.sidebar_scroll);
    if (controls.section("HIT INPUT", &app.sections.hit)) {
        controls.slider("Main tension", "lbf", &app.profile.hit.main_tension_lbf, 1.0, 40.0);
        controls.slider("Cross tension", "lbf", &app.profile.hit.cross_tension_lbf, 1.0, 40.0);
        controls.slider("Normal collision", "m/s", &app.profile.hit.relative_normal_speed_mps, 0.0, 120.0);
        controls.slider("Racket-head speed", "m/s", &app.profile.hit.racket_speed_mps, 0.0, 80.0);
        controls.slider(
            "Contact x",
            "mm",
            &app.profile.hit.x_mm,
            -0.499 * model.frameOuterWidthMm(app.profile.model),
            0.499 * model.frameOuterWidthMm(app.profile.model),
        );
        controls.slider(
            "Contact y",
            "mm",
            &app.profile.hit.y_mm,
            -0.499 * model.frameOuterHeightMm(app.profile.model),
            0.499 * model.frameOuterHeightMm(app.profile.model),
        );
    }
    if (controls.section("GEOMETRY", &app.sections.geometry)) {
        controls.slider(
            "Stringed width",
            "mm",
            &app.profile.model.head_width_mm,
            150.0,
            model.bwf_max_stringed_width_mm,
        );
        controls.slider(
            "Stringed height",
            "mm",
            &app.profile.model.head_height_mm,
            180.0,
            model.bwf_max_stringed_length_mm,
        );
        controls.slider(
            "Frame radial width",
            "mm",
            &app.profile.model.frame_radial_width_mm,
            2.0,
            20.0,
        );
        controls.sliderU32("Mains", &app.profile.model.main_count, 4, 32);
        controls.sliderU32("Crosses", &app.profile.model.cross_count, 4, 32);
    }
    if (controls.section("STRING BED (BG66)", &app.sections.strings)) {
        controls.slider("Diameter", "mm", &app.profile.model.string_diameter_mm, 0.4, 1.2);
        controls.slider("Density", "kg/m3", &app.profile.model.string_density_kg_m3, 700.0, 1800.0);
        controls.slider("Young's modulus", "GPa", &app.profile.model.string_young_modulus_gpa, 1.0, 15.0);
        controls.logSlider("Small-signal damping", "1/s", &app.profile.model.string_damping_rate_s, 1.0, 1500.0);
        controls.slider("Nonlinear damping", "1/s", &app.profile.model.string_nonlinear_damping_rate_s, 0.0, 1200.0);
        controls.slider("Nonlinear onset", "m/s", &app.profile.model.string_nonlinear_damping_onset_mps, 0.0, 20.0);
        controls.slider("Nonlinear transition", "m/s", &app.profile.model.string_nonlinear_damping_transition_mps, 0.1, 40.0);
        controls.slider("Crossing friction", "", &app.profile.model.crossing_friction, 0.0, 1.0);
        controls.slider("Grommet loss", "x impedance", &app.profile.model.grommet_loss_factor, 0.0, 1.0);
        controls.slider("Transverse scale", "", &app.profile.model.transverse_stiffness_scale, 0.2, 1.5);
        controls.slider("Maximum strain", "", &app.profile.model.maximum_dynamic_strain, 0.005, 0.15);
    }
    if (controls.section("SHUTTLE + CONTACT", &app.sections.contact)) {
        controls.slider("Shuttle mass", "g", &app.profile.model.shuttle_mass_g, 1.0, 20.0);
        controls.slider("Cork diameter", "mm", &app.profile.model.cork_diameter_mm, 15.0, 40.0);
        controls.logSlider("Contact stiffness", "", &app.profile.model.contact_stiffness_n_m_pow, 1.0e3, 1.0e8);
        controls.slider("Contact exponent", "", &app.profile.model.contact_exponent, 1.0, 3.0);
        controls.slider("Restitution target", "", &app.profile.model.target_restitution, 0.05, 1.0);
        controls.logSlider(
            "Damping calibration",
            "",
            &app.profile.model.contact_damping_calibration,
            0.1,
            3.0,
        );
    }
    if (controls.section("FRAME MODES", &app.sections.frame)) {
        controls.slider("Frame mass", "g", &app.profile.model.frame_mass_g, 10.0, 120.0);
        controls.slider("Radiation asymmetry", "", &app.profile.model.frame_radiation_asymmetry, 0.0, 1.0);
        for (&app.profile.model.frame_modes, 0..) |*mode, index| {
            controls.modeLabel(index + 1);
            controls.logSlider("Frequency", "Hz", &mode.frequency_hz, 40.0, 5000.0);
            controls.logSlider("Quality factor", "", &mode.quality_factor, 2.0, 100.0);
            controls.slider("Force coupling", "", &mode.force_coupling, 0.0, 2.5);
            controls.logSlider("Radiating area", "m2", &mode.radiation_area_m2, 0.0001, 0.05);
        }
    }
    if (controls.section("DIRECT FRAME HIT", &app.sections.frame_hit)) {
        controls.logSlider(
            "Contact stiffness",
            "",
            &app.profile.model.frame_contact_stiffness_n_m_pow,
            1.0e5,
            1.0e9,
        );
        controls.slider(
            "Contact exponent",
            "",
            &app.profile.model.frame_contact_exponent,
            1.0,
            3.0,
        );
        controls.slider(
            "Restitution target",
            "",
            &app.profile.model.frame_contact_restitution,
            0.05,
            1.0,
        );
        controls.logSlider(
            "Damping calibration",
            "",
            &app.profile.model.frame_contact_damping_calibration,
            0.1,
            3.0,
        );
        controls.logSlider(
            "Mode-bank gain",
            "",
            &app.profile.model.frame_hit_mode_gain,
            0.001,
            30.0,
        );
        controls.slider(
            "Acceleration weighting",
            "",
            &app.profile.model.frame_hit_radiation_exponent,
            0.0,
            3.0,
        );
        controls.logSlider(
            "Transient gain",
            "",
            &app.profile.model.frame_hit_transient_gain,
            0.0001,
            1.0,
        );
        controls.slider(
            "Force-slew time",
            "ms",
            &app.profile.model.frame_hit_slew_time_ms,
            0.01,
            1.0,
        );
        controls.slider(
            "Transient decay",
            "ms",
            &app.profile.model.frame_hit_transient_decay_ms,
            0.2,
            20.0,
        );
        controls.logSlider(
            "High-pass",
            "Hz",
            &app.profile.model.frame_hit_highpass_hz,
            300.0,
            12_000.0,
        );
        controls.logSlider(
            "Low-pass",
            "Hz",
            &app.profile.model.frame_hit_lowpass_hz,
            3_000.0,
            22_000.0,
        );
        for (&app.profile.model.frame_hit_modes, 0..) |*mode, index| {
            controls.modeLabel(index + 1);
            controls.logSlider("Frequency", "Hz", &mode.frequency_hz, 500.0, 22_000.0);
            controls.logSlider("Relative gain", "", &mode.relative_gain, 0.001, 3.0);
            controls.slider("Decay", "ms", &mode.decay_ms, 1.0, 80.0);
            controls.sliderU32("Circumferential order", &mode.circumferential_order, 0, 16);
            controls.slider("Phase", "turn", &mode.phase_turns, -1.0, 1.0);
        }
    }
    if (controls.section("UPPER RADIATION MODES", &app.sections.upper_modes)) {
        controls.logSlider("Mode-bank gain", "", &app.profile.model.upper_mode_gain, 0.001, 30.0);
        controls.slider("Mode decay", "ms", &app.profile.model.upper_mode_decay_ms, 5.0, 100.0);
        controls.slider("Fast-hit decay scale", "", &app.profile.model.upper_mode_fast_decay_scale, 0.05, 1.0);
        controls.slider("Contact roughness", "", &app.profile.model.upper_mode_contact_roughness, 0.0, 2.0);
        controls.slider("Acceleration weighting", "", &app.profile.model.upper_mode_radiation_exponent, 0.0, 3.0);
        for (&app.profile.model.upper_modes, 0..) |*mode, index| {
            controls.modeLabel(index + 1);
            controls.logSlider("30 lb frequency", "Hz", &mode.frequency_hz_at_30lb, 500.0, 18_000.0);
            controls.logSlider("Relative gain", "", &mode.relative_gain, 0.001, 3.0);
            controls.slider("Decay scale", "", &mode.decay_scale, 0.1, 3.0);
        }
    }
    if (controls.section("HARD-IMPACT TRANSIENT", &app.sections.hard_impact)) {
        controls.slider("Silent below", "m/s", &app.profile.model.hard_impact_threshold_mps, 0.0, 30.0);
        controls.slider("Full by", "m/s", &app.profile.model.hard_impact_reference_speed_mps, 10.0, 100.0);
        controls.logSlider("Transient gain", "", &app.profile.model.hard_impact_gain, 0.0001, 1.0);
        controls.slider("Force-slew time", "ms", &app.profile.model.hard_impact_slew_time_ms, 0.01, 1.0);
        controls.slider("Texture decay", "ms", &app.profile.model.hard_impact_decay_ms, 0.2, 20.0);
        controls.logSlider("High-pass", "Hz", &app.profile.model.hard_impact_highpass_hz, 300.0, 12_000.0);
        controls.logSlider("Low-pass", "Hz", &app.profile.model.hard_impact_lowpass_hz, 3_000.0, 22_000.0);
    }
    if (controls.section("SWING + SWOOSH", &app.sections.swoosh)) {
        controls.slider("Silent below", "m/s", &app.profile.model.swoosh_threshold_mps, 0.0, 20.0);
        controls.slider("Reference speed", "m/s", &app.profile.model.swoosh_reference_speed_mps, 15.0, 80.0);
        controls.slider("Velocity exponent", "", &app.profile.model.swoosh_speed_exponent, 1.0, 4.0);
        controls.logSlider("Swoosh gain", "", &app.profile.model.swoosh_gain, 0.001, 10.0);
        controls.slider("Strouhal number", "", &app.profile.model.swoosh_strouhal_number, 0.10, 0.30);
        controls.slider("Frame diameter", "mm", &app.profile.model.swoosh_frame_diameter_mm, 3.0, 25.0);
        controls.slider("Velocity response", "ms", &app.profile.model.swoosh_post_impact_decay_ms, 2.0, 80.0);
    }
    if (controls.section("AIR + RADIATION", &app.sections.acoustics)) {
        controls.slider("Air density", "kg/m3", &app.profile.model.air_density_kg_m3, 0.5, 2.0);
        controls.slider("Sound speed", "m/s", &app.profile.model.sound_speed_mps, 300.0, 380.0);
        controls.logSlider("Microphone distance", "m", &app.profile.model.microphone_distance_m, 0.1, 10.0);
        controls.slider("Microphone horizontal", "m", &app.profile.model.microphone_x_m, -1.0, 1.0);
        controls.slider("Microphone vertical", "m", &app.profile.model.microphone_y_m, -1.0, 1.0);
        controls.slider("String radiation", "", &app.profile.model.string_radiation_efficiency, 0.0, 3.0);
        controls.slider("Frame radiation", "", &app.profile.model.frame_radiation_efficiency, 0.0, 3.0);
        controls.logSlider("Contact texture", "", &app.profile.model.contact_noise_gain, 1.0e-5, 1.0);
        controls.slider("Texture speed curve", "", &app.profile.model.contact_noise_speed_exponent, 0.0, 1.5);
        controls.slider("Texture decay", "ms", &app.profile.model.contact_noise_decay_ms, 0.0, 30.0);
        controls.slider("Micro-burst chance", "", &app.profile.model.contact_noise_burst_probability, 0.0, 0.10);
        controls.slider("Micro-burst gain", "", &app.profile.model.contact_noise_burst_gain, 0.0, 6.0);
        controls.logSlider("Texture high-pass", "Hz", &app.profile.model.contact_noise_highpass_hz, 100.0, 10_000.0);
        controls.logSlider("Texture low-pass", "Hz", &app.profile.model.contact_noise_lowpass_hz, 1000.0, 22_000.0);
        controls.logSlider("Fixed playback gain", "", &app.profile.model.master_gain, 0.0001, 2.0);
        controls.slider("Compressor threshold", "dBFS", &app.profile.model.compressor_threshold_dbfs, -40.0, 0.0);
        controls.slider("Compressor ratio", ":1", &app.profile.model.compressor_ratio, 1.0, 20.0);
        controls.slider("Compressor knee", "dB", &app.profile.model.compressor_knee_db, 0.0, 18.0);
        controls.slider("Lookahead", "ms", &app.profile.model.compressor_lookahead_ms, 0.0, 5.0);
        controls.slider("Release", "ms", &app.profile.model.compressor_release_ms, 5.0, 300.0);
        controls.slider("Limiter ceiling", "dBFS", &app.profile.model.limiter_ceiling_dbfs, -12.0, 0.0);
    }
    if (controls.section("NUMERICS + REPLAY", &app.sections.numerics)) {
        controls.rateChoice(
            "Internal rate",
            &app.profile.model.internal_sample_rate_hz,
            &.{ 96_000, 192_000, 384_000 },
        );
        controls.rateChoice(
            "Output rate",
            &app.profile.model.output_sample_rate_hz,
            &.{ 24_000, 48_000, 96_000 },
        );
        controls.slider("Duration", "s", &app.profile.model.duration_s, 0.08, 2.0);
        controls.slider("Pre-impact audio", "ms", &app.profile.model.impact_pre_roll_ms, 0.0, 250.0);
        controls.slider(
            "Animation capture",
            "ms",
            &app.profile.model.visualization_duration_ms,
            1.0,
            100.0,
        );
        controls.rateChoice(
            "Animation rate",
            &app.profile.model.visualization_rate_hz,
            &.{ 1_000, 2_000, 4_000, 8_000, 16_000, 32_000 },
        );
        controls.slider("Replay interval", "s", &app.profile.model.replay_interval_s, 0.25, 30.0);
    }
    app.sidebar_content_height = controls.contentHeight();
    rl.endScissorMode();

    if (maximum_scroll > 0.0) {
        const track = rl.Rectangle{
            .x = viewport.x + viewport.width - 4,
            .y = viewport.y + 4,
            .width = 2,
            .height = viewport.height - 8,
        };
        rl.drawRectangleRec(track, palette.border);
        const thumb_height = @max(28.0, track.height * viewport.height / app.sidebar_content_height);
        const amount = -app.sidebar_scroll / maximum_scroll;
        rl.drawRectangleRec(
            .{
                .x = track.x - 1,
                .y = track.y + amount * (track.height - thumb_height),
                .width = 4,
                .height = thumb_height,
            },
            palette.cyan,
        );
    }

    rl.drawText(app.status(), 18, @intFromFloat(bounds.height - 31), 10, palette.muted);
}

fn drawWorkspace(app: *App, bounds: rl.Rectangle) void {
    const margin: f32 = 18;
    drawHeader(app, .{
        .x = bounds.x + margin,
        .y = 12,
        .width = bounds.width - margin * 2,
        .height = 54,
    });
    drawMetrics(app, .{
        .x = bounds.x + margin,
        .y = 76,
        .width = bounds.width - margin * 2,
        .height = 64,
    });

    const rack_width = @max(360.0, bounds.width * 0.49);
    const top_y: f32 = 151;
    const top_height = @max(270.0, bounds.height * 0.48);
    const racket_bounds = rl.Rectangle{
        .x = bounds.x + margin,
        .y = top_y,
        .width = rack_width - margin,
        .height = top_height,
    };
    const spectrum_bounds = rl.Rectangle{
        .x = racket_bounds.x + racket_bounds.width + margin,
        .y = top_y,
        .width = bounds.x + bounds.width - margin -
            (racket_bounds.x + racket_bounds.width + margin),
        .height = top_height,
    };
    drawRacket(app, racket_bounds);
    drawSpectrum(app.result, spectrum_bounds);

    const waveform_y = top_y + top_height + margin;
    drawWaveform(app.result, .{
        .x = bounds.x + margin,
        .y = waveform_y,
        .width = bounds.width - margin * 2,
        .height = @max(120.0, bounds.height - waveform_y - margin),
    });
}

fn drawHeader(app: *App, bounds: rl.Rectangle) void {
    rl.drawText("BADMINTON IMPACT LAB", @intFromFloat(bounds.x), @intFromFloat(bounds.y), 25, palette.text);
    rl.drawText(
        "Coupled nonlinear bed + frame  |  dedicated continuous 48 kHz audio",
        @intFromFloat(bounds.x),
        @intFromFloat(bounds.y + 32),
        11,
        palette.muted,
    );
    const audio_status = app.audio.status();
    const audio_text: [:0]const u8 = if (audio_status.failed)
        "AUDIO STREAM ERROR"
    else if (!audio_status.ready)
        "AUDIO STREAM STARTING"
    else
        "AUDIO STREAM ONLINE";
    const audio_width: f32 = @floatFromInt(rl.measureText(audio_text, 10));
    rl.drawText(
        audio_text,
        @intFromFloat(bounds.x + bounds.width - audio_width),
        @intFromFloat(bounds.y + 42),
        10,
        if (audio_status.failed) palette.red else if (audio_status.ready) palette.green else palette.amber,
    );

    var state_buffer: [128:0]u8 = undefined;
    const state_text: [:0]const u8 = if (validationError(app.profile)) |err|
        std.fmt.bufPrintZ(&state_buffer, "REPLAY PAUSED  |  {s}", .{@errorName(err)}) catch "REPLAY PAUSED"
    else if (app.worker.isRunning())
        "SIMULATING ON WORKER THREAD"
    else blk: {
        const remaining = @max(0.0, app.next_replay_time - rl.getTime());
        break :blk std.fmt.bufPrintZ(
            &state_buffer,
            "NEXT STRIKE IN {d:.1} s",
            .{remaining},
        ) catch "REPLAY READY";
    };
    const text_width: f32 = @floatFromInt(rl.measureText(state_text, 15));
    const badge = rl.Rectangle{
        .x = bounds.x + bounds.width - text_width - 27,
        .y = bounds.y + 8,
        .width = text_width + 20,
        .height = 29,
    };
    rl.drawRectangleRounded(
        badge,
        0.35,
        8,
        if (validationError(app.profile) != null) color(75, 31, 38, 255) else palette.panel_alt,
    );
    rl.drawText(
        state_text,
        @intFromFloat(badge.x + 10),
        @intFromFloat(badge.y + 7),
        15,
        if (validationError(app.profile) != null) palette.red else palette.cyan,
    );
}

fn drawMetrics(app: *App, bounds: rl.Rectangle) void {
    const labels = [_][:0]const u8{
        "CONTACT",
        "PEAK FORCE",
        "DEFLECTION",
        "MAIN PEAK",
        "RENDER",
        "CLIPPING",
    };
    var values: [6][48:0]u8 = undefined;
    var value_text: [6][:0]const u8 = undefined;
    if (app.result) |result| {
        const d = result.diagnostics;
        value_text[0] = std.fmt.bufPrintZ(&values[0], "{d:.2} ms", .{d.contact_duration_ms}) catch "--";
        value_text[1] = std.fmt.bufPrintZ(&values[1], "{d:.0} N", .{d.peak_force_n}) catch "--";
        value_text[2] = std.fmt.bufPrintZ(&values[2], "{d:.2} mm", .{d.maximum_deflection_mm}) catch "--";
        value_text[3] = std.fmt.bufPrintZ(&values[3], "{d:.0} Hz", .{d.dominant_frequency_hz}) catch "--";
        value_text[4] = std.fmt.bufPrintZ(&values[4], "{d:.0} ms", .{d.render_time_ms}) catch "--";
        value_text[5] = if (d.clipped_samples > 0)
            std.fmt.bufPrintZ(&values[5], "{d} CLIPPED", .{d.clipped_samples}) catch "--"
        else if (d.limited_samples > 0)
            std.fmt.bufPrintZ(&values[5], "{d} LIMITED", .{d.limited_samples}) catch "--"
        else
            std.fmt.bufPrintZ(&values[5], "CLEAN", .{}) catch "--";
    } else {
        for (&value_text) |*text| text.* = "--";
    }
    const gap: f32 = 8;
    const cell_width = (bounds.width - gap * 5) / 6;
    for (labels, value_text, 0..) |label, value, index| {
        const rect = rl.Rectangle{
            .x = bounds.x + @as(f32, @floatFromInt(index)) * (cell_width + gap),
            .y = bounds.y,
            .width = cell_width,
            .height = bounds.height,
        };
        rl.drawRectangleRounded(rect, 0.18, 6, palette.panel);
        rl.drawRectangleRoundedLinesEx(rect, 0.18, 6, 1, palette.border);
        rl.drawText(label, @intFromFloat(rect.x + 11), @intFromFloat(rect.y + 10), 10, palette.muted);
        rl.drawText(
            value,
            @intFromFloat(rect.x + 11),
            @intFromFloat(rect.y + 30),
            18,
            if (index == 5 and app.result != null and app.result.?.diagnostics.clipped_samples > 0)
                palette.red
            else if (index == 5 and app.result != null and app.result.?.diagnostics.limited_samples > 0)
                palette.amber
            else
                palette.text,
        );
    }
}

fn drawRacket(app: *App, bounds: rl.Rectangle) void {
    drawPanel(bounds, "RACKET FACE  |  CLICK STRINGS OR FRAME");
    const inner = inset(bounds, 22, 43, 22, 18);
    const half_width_m = app.profile.model.head_width_mm * 0.0005;
    const half_height_m = app.profile.model.head_height_mm * 0.0005;
    const half_outer_width_m =
        model.frameOuterWidthMm(app.profile.model) * 0.0005;
    const half_outer_height_m =
        model.frameOuterHeightMm(app.profile.model) * 0.0005;
    const scale = 0.88 * @min(
        inner.width / @as(f32, @floatCast(2.0 * half_outer_width_m)),
        inner.height / @as(f32, @floatCast(2.0 * half_outer_height_m)),
    );
    const center = rl.Vector2{
        .x = inner.x + inner.width * 0.5,
        .y = inner.y + inner.height * 0.51,
    };

    const mouse = rl.getMousePosition();
    const local_x_m = @as(f64, @floatCast((mouse.x - center.x) / scale));
    const local_y_m = @as(f64, @floatCast((center.y - mouse.y) / scale));
    const outer_ellipse = square(local_x_m / half_outer_width_m) +
        square(local_y_m / half_outer_height_m);
    if (outer_ellipse <= 1.0 and rl.isMouseButtonPressed(.left)) {
        app.profile.hit.x_mm = local_x_m * 1000.0;
        app.profile.hit.y_mm = local_y_m * 1000.0;
    }

    const radius_x: f32 = @floatCast(half_width_m * scale);
    const radius_y: f32 = @floatCast(half_height_m * scale);
    const outer_radius_x: f32 = @floatCast(half_outer_width_m * scale);
    const outer_radius_y: f32 = @floatCast(half_outer_height_m * scale);
    const frame_pixel_width: usize = @max(
        2,
        @as(usize, @intFromFloat(@ceil(
            app.profile.model.frame_radial_width_mm * 0.001 *
                @as(f64, @floatCast(scale)),
        ))),
    );
    // The visible annulus is the same geometry used by model.hitRegion: the
    // inner ellipse terminates the strings and the outer ellipse rejects hits.
    for (0..frame_pixel_width + 1) |ring| {
        const amount = @as(f32, @floatFromInt(ring)) /
            @as(f32, @floatFromInt(frame_pixel_width));
        rl.drawEllipseLinesV(
            center,
            radius_x + (outer_radius_x - radius_x) * amount,
            radius_y + (outer_radius_y - radius_y) * amount,
            if (ring * 2 < frame_pixel_width)
                color(69, 126, 176, 255)
            else
                color(126, 91, 178, 255),
        );
    }
    rl.drawEllipseLinesV(center, radius_x, radius_y, palette.blue);
    rl.drawEllipseLinesV(center, outer_radius_x, outer_radius_y, palette.violet);

    var simulated_time_s: f64 = 0.0;
    var frame: []const f32 = &.{};
    var peak_displacement_mm: f64 = 0.0;
    var inner_sum_squares: f64 = 0.0;
    var outer_sum_squares: f64 = 0.0;
    var inner_count: usize = 0;
    var outer_count: usize = 0;
    if (app.result) |result| {
        const capture_s = result.params.visualization_duration_ms * 0.001;
        const elapsed_since_impact = rl.getTime() - app.animation_started;
        simulated_time_s = if (capture_s > 0.0 and elapsed_since_impact > 0.0)
            @mod(elapsed_since_impact / 200.0, capture_s)
        else
            0.0;
        const frame_index: usize = @intFromFloat(
            simulated_time_s * @as(f64, @floatFromInt(result.visual_frame_rate_hz)),
        );
        frame = result.visualFrame(frame_index);
        for (frame) |displacement| {
            peak_displacement_mm = @max(
                peak_displacement_mm,
                @abs(@as(f64, displacement)) * 1000.0,
            );
        }
        const envelope_frame_count: usize = @max(
            1,
            @as(usize, @intCast(result.visual_frame_rate_hz / 500)),
        );
        const envelope_start =
            frame_index - @min(frame_index, envelope_frame_count - 1);
        for (envelope_start..frame_index + 1) |envelope_frame_index| {
            const envelope_frame = result.visualFrame(envelope_frame_index);
            for (result.node_positions, envelope_frame) |position, displacement| {
                const radius_squared =
                    square(position.x / half_width_m) +
                    square(position.y / half_height_m);
                if (radius_squared <= 0.25) {
                    inner_sum_squares += square(@as(f64, displacement));
                    inner_count += 1;
                } else if (radius_squared >= 0.64) {
                    outer_sum_squares += square(@as(f64, displacement));
                    outer_count += 1;
                }
            }
        }

        for (result.segments) |segment| {
            const za: f64 = if (segment.a_node >= 0) frame[@intCast(segment.a_node)] else 0.0;
            const zb: f64 = if (segment.b_node >= 0) frame[@intCast(segment.b_node)] else 0.0;
            const a = projectBedPoint(segment.a_position, za, center, scale);
            const b = projectBedPoint(segment.b_position, zb, center, scale);
            const is_main = @abs(segment.a_position.x - segment.b_position.x) <
                @abs(segment.a_position.y - segment.b_position.y);
            rl.drawLineEx(
                a,
                b,
                1.05,
                if (is_main) color(81, 187, 224, 210) else color(169, 111, 255, 190),
            );
            if (segment.a_node < 0) rl.drawCircleV(a, 1.8, palette.blue);
            if (segment.b_node < 0) rl.drawCircleV(b, 1.8, palette.blue);
        }
    } else {
        rl.drawText(
            "Rendering the first deterministic strike...",
            @intFromFloat(inner.x + 20),
            @intFromFloat(inner.y + inner.height * 0.5),
            14,
            palette.muted,
        );
    }

    const hit_point = projectBedPoint(
        .{
            .x = app.profile.hit.x_mm * 0.001,
            .y = app.profile.hit.y_mm * 0.001,
        },
        0.0,
        center,
        scale,
    );
    const hit_region = model.hitRegion(app.profile);
    const hit_color = if (hit_region) |region|
        switch (region) {
            .strings => palette.amber,
            .frame => palette.red,
        }
    else
        palette.muted;
    const hit_target: [:0]const u8 = if (hit_region) |region|
        switch (region) {
            .strings => "STRINGS",
            .frame => "FRAME",
        }
    else
        "OUTSIDE";
    rl.drawCircleV(hit_point, 6.5, hit_color);
    rl.drawCircleLinesV(hit_point, 10.0, hit_color);

    const inner_rms = if (inner_count == 0)
        0.0
    else
        @sqrt(inner_sum_squares / @as(f64, @floatFromInt(inner_count)));
    const outer_rms = if (outer_count == 0)
        0.0
    else
        @sqrt(outer_sum_squares / @as(f64, @floatFromInt(outer_count)));
    const spatial_ratio = inner_rms / @max(1.0e-9, outer_rms);
    var footer_buffer: [288:0]u8 = undefined;
    const footer = std.fmt.bufPrintZ(
        &footer_buffer,
        "TARGET {s}  |  200x time / 5x frame-relative z  |  t {d:.2} ms  |  peak {d:.2} mm  |  2 ms centre/rim {d:.1}x",
        .{
            hit_target,
            simulated_time_s * 1000.0,
            peak_displacement_mm,
            spatial_ratio,
        },
    ) catch "200x slow motion";
    rl.drawText(
        footer,
        @intFromFloat(bounds.x + 16),
        @intFromFloat(bounds.y + bounds.height - 22),
        10,
        palette.muted,
    );
}

fn drawWaveform(result_optional: ?*simulator.SimulationResult, bounds: rl.Rectangle) void {
    drawPanel(bounds, "DRY MICROPHONE WAVEFORM  |  FIXED FULL SCALE");
    const plot = inset(bounds, 45, 42, 17, 28);
    drawPlotGrid(plot, false);
    const result = result_optional orelse return;
    if (result.audio.len == 0) return;

    const pixel_count: usize = @max(1, @as(usize, @intFromFloat(plot.width)));
    for (0..pixel_count) |pixel| {
        const start = pixel * result.audio.len / pixel_count;
        const end = @max(start + 1, (pixel + 1) * result.audio.len / pixel_count);
        var minimum: f32 = 1.0;
        var maximum: f32 = -1.0;
        for (result.audio[start..@min(end, result.audio.len)]) |sample| {
            minimum = @min(minimum, sample);
            maximum = @max(maximum, sample);
        }
        const x = plot.x + @as(f32, @floatFromInt(pixel));
        rl.drawLineEx(
            .{ .x = x, .y = plot.y + plot.height * (0.5 - maximum * 0.47) },
            .{ .x = x, .y = plot.y + plot.height * (0.5 - minimum * 0.47) },
            1,
            palette.cyan,
        );
    }
    var duration_buffer: [48:0]u8 = undefined;
    const duration = std.fmt.bufPrintZ(
        &duration_buffer,
        "{d:.0} ms",
        .{@as(f64, @floatFromInt(result.audio.len)) /
            @as(f64, @floatFromInt(result.output_sample_rate_hz)) * 1000.0},
    ) catch "";
    rl.drawText(duration, @intFromFloat(plot.x + plot.width - 42), @intFromFloat(plot.y + plot.height + 7), 10, palette.muted);
    rl.drawText("+1", @intFromFloat(bounds.x + 13), @intFromFloat(plot.y - 2), 10, palette.muted);
    rl.drawText("0", @intFromFloat(bounds.x + 22), @intFromFloat(plot.y + plot.height * 0.5 - 4), 10, palette.muted);
    rl.drawText("-1", @intFromFloat(bounds.x + 13), @intFromFloat(plot.y + plot.height - 8), 10, palette.muted);
    const impact_x = plot.x +
        @as(f32, @floatFromInt(result.impact_audio_frame)) /
            @as(f32, @floatFromInt(result.audio.len)) * plot.width;
    rl.drawLineEx(
        .{ .x = impact_x, .y = plot.y },
        .{ .x = impact_x, .y = plot.y + plot.height },
        1.0,
        palette.amber,
    );
    rl.drawText(
        "IMPACT",
        @intFromFloat(@min(impact_x + 4, plot.x + plot.width - 42)),
        @intFromFloat(plot.y + 4),
        9,
        palette.amber,
    );
}

fn drawSpectrum(result_optional: ?*simulator.SimulationResult, bounds: rl.Rectangle) void {
    drawPanel(bounds, "HANN FFT  |  LOG FREQUENCY");
    const plot = inset(bounds, 43, 43, 18, 31);
    drawPlotGrid(plot, true);
    const result = result_optional orelse return;
    if (result.spectrum_db.len < 2) return;

    const minimum_hz: f64 = 80.0;
    const maximum_hz: f64 = @min(20_000.0, @as(f64, @floatFromInt(result.output_sample_rate_hz)) * 0.5);
    const fft_size = (result.spectrum_db.len - 1) * 2;
    const bin_hz = @as(f64, @floatFromInt(result.output_sample_rate_hz)) /
        @as(f64, @floatFromInt(fft_size));
    var previous: ?rl.Vector2 = null;
    for (result.spectrum_db, 0..) |level, bin| {
        const frequency = @as(f64, @floatFromInt(bin)) * bin_hz;
        if (frequency < minimum_hz or frequency > maximum_hz) continue;
        const x_amount = std.math.log10(frequency / minimum_hz) /
            std.math.log10(maximum_hz / minimum_hz);
        const y_amount = std.math.clamp((@as(f64, level) + 140.0) / 140.0, 0.0, 1.0);
        const point = rl.Vector2{
            .x = plot.x + @as(f32, @floatCast(x_amount)) * plot.width,
            .y = plot.y + (1.0 - @as(f32, @floatCast(y_amount))) * plot.height,
        };
        if (previous) |last| rl.drawLineEx(last, point, 1.2, palette.violet);
        previous = point;
    }
    rl.drawText("80", @intFromFloat(plot.x - 4), @intFromFloat(plot.y + plot.height + 8), 10, palette.muted);
    rl.drawText("1k", @intFromFloat(plot.x + plot.width * 0.46), @intFromFloat(plot.y + plot.height + 8), 10, palette.muted);
    rl.drawText("20k Hz", @intFromFloat(plot.x + plot.width - 38), @intFromFloat(plot.y + plot.height + 8), 10, palette.muted);
    rl.drawText("0", @intFromFloat(bounds.x + 21), @intFromFloat(plot.y - 3), 10, palette.muted);
    rl.drawText("-70", @intFromFloat(bounds.x + 12), @intFromFloat(plot.y + plot.height * 0.5 - 4), 10, palette.muted);
    rl.drawText("-140", @intFromFloat(bounds.x + 7), @intFromFloat(plot.y + plot.height - 8), 10, palette.muted);
}

const ControlEditor = struct {
    viewport: rl.Rectangle,
    start_y: f32,
    y: f32,

    fn init(viewport: rl.Rectangle, scroll: f32) ControlEditor {
        const y = viewport.y + 8 + scroll;
        return .{ .viewport = viewport, .start_y = y, .y = y };
    }

    fn contentHeight(self: ControlEditor) f32 {
        return self.y - self.start_y + 16;
    }

    fn section(self: *ControlEditor, title: [:0]const u8, open: *bool) bool {
        var title_buffer: [96:0]u8 = undefined;
        const display = std.fmt.bufPrintZ(
            &title_buffer,
            "{s}  {s}",
            .{ if (open.*) "[-]" else "[+]", title },
        ) catch title;
        const rect = rl.Rectangle{
            .x = self.viewport.x + 8,
            .y = self.y,
            .width = self.viewport.width - 20,
            .height = 30,
        };
        if (self.visible(rect) and gui.button(rect, display)) open.* = !open.*;
        self.y += 37;
        return open.*;
    }

    fn modeLabel(self: *ControlEditor, mode_number: usize) void {
        var buffer: [40:0]u8 = undefined;
        const text = std.fmt.bufPrintZ(&buffer, "MODE {d}", .{mode_number}) catch "MODE";
        if (self.y >= self.viewport.y - 15 and self.y < self.viewport.y + self.viewport.height) {
            rl.drawText(text, @intFromFloat(self.viewport.x + 13), @intFromFloat(self.y), 10, palette.violet);
        }
        self.y += 18;
    }

    fn slider(
        self: *ControlEditor,
        label: [:0]const u8,
        unit: [:0]const u8,
        value: *f64,
        minimum: f64,
        maximum: f64,
    ) void {
        var display_buffer: [112:0]u8 = undefined;
        const display = std.fmt.bufPrintZ(
            &display_buffer,
            "{s}   {d:.4} {s}",
            .{ label, value.*, unit },
        ) catch label;
        const row = rl.Rectangle{
            .x = self.viewport.x + 12,
            .y = self.y,
            .width = self.viewport.width - 28,
            .height = 46,
        };
        if (self.visible(row)) {
            rl.drawText(display, @intFromFloat(row.x), @intFromFloat(row.y), 11, palette.text);
            var current: f32 = @floatCast(value.*);
            _ = gui.sliderBar(
                .{ .x = row.x, .y = row.y + 20, .width = row.width, .height = 17 },
                null,
                null,
                &current,
                @floatCast(minimum),
                @floatCast(maximum),
            );
            value.* = @floatCast(current);
        }
        self.y += 49;
    }

    fn logSlider(
        self: *ControlEditor,
        label: [:0]const u8,
        unit: [:0]const u8,
        value: *f64,
        minimum: f64,
        maximum: f64,
    ) void {
        var display_buffer: [112:0]u8 = undefined;
        const display = if (value.* < 0.001 or value.* >= 10_000.0)
            std.fmt.bufPrintZ(
                &display_buffer,
                "{s}   {e:.3} {s}",
                .{ label, value.*, unit },
            ) catch label
        else
            std.fmt.bufPrintZ(
                &display_buffer,
                "{s}   {d:.5} {s}",
                .{ label, value.*, unit },
            ) catch label;
        const row = rl.Rectangle{
            .x = self.viewport.x + 12,
            .y = self.y,
            .width = self.viewport.width - 28,
            .height = 46,
        };
        if (self.visible(row)) {
            rl.drawText(display, @intFromFloat(row.x), @intFromFloat(row.y), 11, palette.text);
            const minimum_log = std.math.log10(minimum);
            const maximum_log = std.math.log10(maximum);
            var current: f32 = @floatCast(std.math.log10(std.math.clamp(value.*, minimum, maximum)));
            _ = gui.sliderBar(
                .{ .x = row.x, .y = row.y + 20, .width = row.width, .height = 17 },
                null,
                null,
                &current,
                @floatCast(minimum_log),
                @floatCast(maximum_log),
            );
            value.* = std.math.pow(f64, 10.0, @as(f64, @floatCast(current)));
        }
        self.y += 49;
    }

    fn sliderU32(
        self: *ControlEditor,
        label: [:0]const u8,
        value: *u32,
        minimum: u32,
        maximum: u32,
    ) void {
        var as_float: f64 = @floatFromInt(value.*);
        self.slider(label, "", &as_float, @floatFromInt(minimum), @floatFromInt(maximum));
        value.* = @intFromFloat(@round(as_float));
    }

    fn rateChoice(
        self: *ControlEditor,
        label: [:0]const u8,
        value: *u32,
        choices: []const u32,
    ) void {
        var buffer: [96:0]u8 = undefined;
        const text = std.fmt.bufPrintZ(
            &buffer,
            "{s}   {d} Hz  |  click to change",
            .{ label, value.* },
        ) catch label;
        const rect = rl.Rectangle{
            .x = self.viewport.x + 12,
            .y = self.y,
            .width = self.viewport.width - 28,
            .height = 31,
        };
        if (self.visible(rect) and gui.button(rect, text)) {
            var next: usize = 0;
            for (choices, 0..) |choice, index| {
                if (choice == value.*) {
                    next = (index + 1) % choices.len;
                    break;
                }
            }
            value.* = choices[next];
        }
        self.y += 38;
    }

    fn visible(self: ControlEditor, rect: rl.Rectangle) bool {
        return rect.y + rect.height >= self.viewport.y and
            rect.y <= self.viewport.y + self.viewport.height;
    }
};

fn validationError(profile: model.Profile) ?model.ValidationError {
    model.validate(profile) catch |err| return err;
    return null;
}

fn projectBedPoint(
    position: simulator.Vec2,
    displacement_m: f64,
    center: rl.Vector2,
    scale: f32,
) rl.Vector2 {
    const z_pixels: f32 = @floatCast(displacement_m * @as(f64, @floatCast(scale)) * 5.0);
    return .{
        .x = center.x + @as(f32, @floatCast(position.x)) * scale + z_pixels * 0.32,
        .y = center.y - @as(f32, @floatCast(position.y)) * scale - z_pixels,
    };
}

fn drawPanel(bounds: rl.Rectangle, title: [:0]const u8) void {
    rl.drawRectangleRounded(bounds, 0.025, 6, palette.panel);
    rl.drawRectangleRoundedLinesEx(bounds, 0.025, 6, 1, palette.border);
    rl.drawText(title, @intFromFloat(bounds.x + 15), @intFromFloat(bounds.y + 14), 12, palette.muted);
}

fn drawPlotGrid(bounds: rl.Rectangle, logarithmic_x: bool) void {
    for (0..5) |index| {
        const amount = @as(f32, @floatFromInt(index)) / 4.0;
        rl.drawLineEx(
            .{ .x = bounds.x, .y = bounds.y + amount * bounds.height },
            .{ .x = bounds.x + bounds.width, .y = bounds.y + amount * bounds.height },
            1,
            color(48, 62, 77, 100),
        );
    }
    const columns: usize = if (logarithmic_x) 4 else 8;
    for (0..columns + 1) |index| {
        const amount = @as(f32, @floatFromInt(index)) /
            @as(f32, @floatFromInt(columns));
        rl.drawLineEx(
            .{ .x = bounds.x + amount * bounds.width, .y = bounds.y },
            .{ .x = bounds.x + amount * bounds.width, .y = bounds.y + bounds.height },
            1,
            color(48, 62, 77, 80),
        );
    }
}

fn inset(
    rectangle: rl.Rectangle,
    left: f32,
    top: f32,
    right: f32,
    bottom: f32,
) rl.Rectangle {
    return .{
        .x = rectangle.x + left,
        .y = rectangle.y + top,
        .width = @max(1.0, rectangle.width - left - right),
        .height = @max(1.0, rectangle.height - top - bottom),
    };
}

fn configureGuiStyle() void {
    gui.loadStyleDefault();
    gui.setStyle(.default, .{ .default = .text_size }, 13);
    gui.setStyle(.default, .{ .default = .background_color }, colorInt(palette.background));
    inline for (.{ gui.Control.button, gui.Control.slider }) |control| {
        gui.setStyle(control, .{ .control = .border_color_normal }, colorInt(palette.border));
        gui.setStyle(control, .{ .control = .base_color_normal }, colorInt(palette.panel_alt));
        gui.setStyle(control, .{ .control = .text_color_normal }, colorInt(palette.text));
        gui.setStyle(control, .{ .control = .border_color_focused }, colorInt(palette.cyan));
        gui.setStyle(control, .{ .control = .base_color_focused }, colorInt(color(32, 52, 65, 255)));
        gui.setStyle(control, .{ .control = .text_color_focused }, colorInt(palette.text));
        gui.setStyle(control, .{ .control = .border_color_pressed }, colorInt(palette.cyan));
        gui.setStyle(control, .{ .control = .base_color_pressed }, colorInt(color(34, 73, 80, 255)));
        gui.setStyle(control, .{ .control = .text_color_pressed }, colorInt(palette.text));
    }
    gui.setStyle(.slider, .{ .slider = .slider_width }, 12);
    gui.setStyle(.slider, .{ .slider = .slider_padding }, 1);
}

fn colorInt(value: rl.Color) i32 {
    const packed_color: u32 = (@as(u32, value.r) << 24) |
        (@as(u32, value.g) << 16) |
        (@as(u32, value.b) << 8) |
        @as(u32, value.a);
    return @bitCast(packed_color);
}

fn color(r: u8, g: u8, b: u8, a: u8) rl.Color {
    return .{ .r = r, .g = g, .b = b, .a = a };
}

fn square(value: f64) f64 {
    return value * value;
}
