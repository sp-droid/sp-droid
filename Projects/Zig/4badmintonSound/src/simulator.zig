const std = @import("std");
const model = @import("model.zig");
const dsp = @import("dsp.zig");

const maximum_line_segments = 33;

pub const Vec2 = struct {
    x: f64,
    y: f64,
};

pub const VisualSegment = struct {
    a_position: Vec2,
    b_position: Vec2,
    a_node: i32,
    b_node: i32,
};

pub const Diagnostics = struct {
    contact_duration_ms: f64 = 0.0,
    peak_force_n: f64 = 0.0,
    maximum_deflection_mm: f64 = 0.0,
    maximum_node_speed_mps: f64 = 0.0,
    dominant_frequency_hz: f64 = 0.0,
    render_time_ms: f64 = 0.0,
    apparent_restitution: f64 = 0.0,
    rms: f64 = 0.0,
    peak_before_dynamics: f64 = 0.0,
    peak_before_clamp: f64 = 0.0,
    maximum_gain_reduction_db: f64 = 0.0,
    limited_samples: usize = 0,
    clipped_samples: usize = 0,
    numerical_substeps: u32 = 1,
    node_count: usize = 0,
};

pub const SimulationResult = struct {
    allocator: std.mem.Allocator,
    // Contact/structure only, used by the desktop stream so its separate
    // continuous velocity-driven swoosh is not played twice.
    impact_only_audio: []f32,
    // Combined impact plus constant-velocity swoosh for plots and WAV export.
    audio: []f32,
    spectrum_db: []f32,
    output_sample_rate_hz: u32,
    node_positions: []Vec2,
    segments: []VisualSegment,
    visual_displacements: []f32,
    visual_frame_count: usize,
    visual_frame_rate_hz: u32,
    impact_audio_frame: usize,
    diagnostics: Diagnostics,
    hit: model.HitInput,
    hit_region: model.HitRegion,
    params: model.ModelParams,

    pub fn deinit(self: *SimulationResult) void {
        self.allocator.free(self.impact_only_audio);
        self.allocator.free(self.audio);
        self.allocator.free(self.spectrum_db);
        self.allocator.free(self.node_positions);
        self.allocator.free(self.segments);
        self.allocator.free(self.visual_displacements);
        self.* = undefined;
    }

    pub fn visualFrame(self: SimulationResult, index: usize) []const f32 {
        if (self.visual_frame_count == 0) return &.{};
        const frame = @min(index, self.visual_frame_count - 1);
        const start = frame * self.node_positions.len;
        return self.visual_displacements[start .. start + self.node_positions.len];
    }

    pub fn impactTimeSeconds(self: SimulationResult) f64 {
        return @as(f64, @floatFromInt(self.impact_audio_frame)) /
            @as(f64, @floatFromInt(self.output_sample_rate_hz));
    }
};

const Orientation = enum {
    main,
    cross,
};

const Anchor = struct {
    position: Vec2,
    shape: [4]f64,
};

const StringLine = struct {
    orientation: Orientation,
    node_start: usize,
    node_count: usize,
    anchor_start: usize,
    anchor_end: usize,
    rest_length_m: f64,
    nominal_tension_n: f64,
};

const Topology = struct {
    allocator: std.mem.Allocator,
    node_positions: []Vec2,
    node_masses: []f64,
    node_radiation_gain: []f64,
    node_audio_delay: []usize,
    line_nodes: []usize,
    lines: []StringLine,
    anchors: []Anchor,
    visual_segments: []VisualSegment,
    frame_modal_mass: [4]f64,
    frame_audio_gain: [4]f64,
    frame_audio_delay: usize,
    stability_omega_max: f64,

    fn deinit(self: *Topology) void {
        self.allocator.free(self.node_positions);
        self.allocator.free(self.node_masses);
        self.allocator.free(self.node_radiation_gain);
        self.allocator.free(self.node_audio_delay);
        self.allocator.free(self.line_nodes);
        self.allocator.free(self.lines);
        self.allocator.free(self.anchors);
        self.allocator.free(self.visual_segments);
        self.* = undefined;
    }
};

const DynamicState = struct {
    node_z: []f64,
    node_v: []f64,
    node_a: []f64,
    frame_q: [4]f64 = @splat(0.0),
    frame_v: [4]f64 = @splat(0.0),
    frame_a: [4]f64 = @splat(0.0),
    shuttle_z: f64 = 0.0,
    shuttle_v: f64,
    shuttle_a: f64 = 0.0,
};

const Workspace = struct {
    node_force: []f64,
    anchor_force: []f64,
    contact_weights: []f64,
};

const ContactState = struct {
    region: model.HitRegion,
    enabled: bool,
    started: bool = false,
    separated: bool = false,
    separation_time_s: f64 = 0.0,
    separation_relative_speed_mps: f64 = 0.0,
    force_n: f64 = 0.0,
};

pub fn simulate(
    allocator: std.mem.Allocator,
    profile: model.Profile,
) !SimulationResult {
    try model.validate(profile);
    const hit_region = model.hitRegion(profile).?;
    var topology = try buildTopology(allocator, profile);
    defer topology.deinit();

    const params = profile.model;
    const hit = profile.hit;
    const internal_rate: usize = params.internal_sample_rate_hz;
    const internal_frames: usize = @intFromFloat(@ceil(
        params.duration_s * @as(f64, @floatFromInt(internal_rate)),
    ));
    const output_frames: usize = @intFromFloat(@ceil(
        params.duration_s * @as(f64, @floatFromInt(params.output_sample_rate_hz)),
    ));
    const pre_roll_internal_frames: usize = @intFromFloat(@round(
        params.impact_pre_roll_ms * 0.001 *
            @as(f64, @floatFromInt(internal_rate)),
    ));
    const physics_internal_frames = internal_frames - pre_roll_internal_frames;

    var substeps: u32 = 1;
    const base_dt = 1.0 / @as(f64, @floatFromInt(internal_rate));
    while (topology.stability_omega_max * base_dt /
        @as(f64, @floatFromInt(substeps)) > 1.0 and substeps < 32)
    {
        substeps *= 2;
    }
    const dt = base_dt / @as(f64, @floatFromInt(substeps));

    const node_count = topology.node_positions.len;
    const node_z = try allocator.alloc(f64, node_count);
    defer allocator.free(node_z);
    const node_v = try allocator.alloc(f64, node_count);
    defer allocator.free(node_v);
    const node_a = try allocator.alloc(f64, node_count);
    defer allocator.free(node_a);
    @memset(node_z, 0.0);
    @memset(node_v, 0.0);
    @memset(node_a, 0.0);

    var state = DynamicState{
        .node_z = node_z,
        .node_v = node_v,
        .node_a = node_a,
        .shuttle_v = hit.relative_normal_speed_mps,
    };

    const node_force = try allocator.alloc(f64, node_count);
    defer allocator.free(node_force);
    const anchor_force = try allocator.alloc(f64, topology.anchors.len);
    defer allocator.free(anchor_force);
    const contact_weights = try allocator.alloc(f64, node_count);
    defer allocator.free(contact_weights);
    if (hit_region == .strings) {
        calculateContactWeights(&topology, profile, contact_weights);
    } else {
        @memset(contact_weights, 0.0);
    }
    var workspace = Workspace{
        .node_force = node_force,
        .anchor_force = anchor_force,
        .contact_weights = contact_weights,
    };

    const propagation_margin = @as(usize, @intFromFloat(@ceil(
        (params.microphone_distance_m + 0.05) /
            params.sound_speed_mps *
            @as(f64, @floatFromInt(params.internal_sample_rate_hz)),
    ))) + 128;
    const raw_pressure = try allocator.alloc(f64, internal_frames + propagation_margin);
    defer allocator.free(raw_pressure);
    @memset(raw_pressure, 0.0);

    const visual_step = params.internal_sample_rate_hz / params.visualization_rate_hz;
    const visual_internal_frames: usize = @min(
        physics_internal_frames,
        @as(usize, @intFromFloat(@ceil(
            params.visualization_duration_ms * 0.001 *
                @as(f64, @floatFromInt(params.internal_sample_rate_hz)),
        ))),
    );
    const visual_capacity = if (node_count == 0)
        0
    else
        (visual_internal_frames / visual_step + 1) * node_count;
    const visual_displacements = try allocator.alloc(f32, visual_capacity);
    errdefer allocator.free(visual_displacements);
    var visual_frames_written: usize = 0;

    var contact = ContactState{
        .region = hit_region,
        .enabled = hit.relative_normal_speed_mps > 0.0,
    };
    var contact_noise_highpass = dsp.Biquad.highPass(
        @floatFromInt(params.internal_sample_rate_hz),
        params.contact_noise_highpass_hz,
        0.707,
    );
    var contact_noise_lowpass = dsp.Biquad.lowPass(
        @floatFromInt(params.internal_sample_rate_hz),
        params.contact_noise_lowpass_hz,
        0.707,
    );
    var hard_impact_highpass = dsp.Biquad.highPass(
        @floatFromInt(params.internal_sample_rate_hz),
        params.hard_impact_highpass_hz,
        0.707,
    );
    var hard_impact_lowpass = dsp.Biquad.lowPass(
        @floatFromInt(params.internal_sample_rate_hz),
        params.hard_impact_lowpass_hz,
        0.707,
    );
    var frame_hit_highpass = dsp.Biquad.highPass(
        @floatFromInt(params.internal_sample_rate_hz),
        params.frame_hit_highpass_hz,
        0.707,
    );
    var frame_hit_lowpass = dsp.Biquad.lowPass(
        @floatFromInt(params.internal_sample_rate_hz),
        params.frame_hit_lowpass_hz,
        0.707,
    );
    var frame_hit_mode_filters: [model.frame_hit_mode_count]dsp.Biquad = undefined;
    var frame_hit_mode_gains: [model.frame_hit_mode_count]f64 = undefined;
    var upper_mode_filters: [model.upper_mode_count]dsp.Biquad = undefined;
    var upper_mode_acoustic_gains: [model.upper_mode_count]f64 = undefined;
    const mean_tension_lbf =
        0.5 * (hit.main_tension_lbf + hit.cross_tension_lbf);
    const upper_mode_frequency_scale =
        @sqrt(mean_tension_lbf / 30.0) *
        (model.bg66_diameter_mm / params.string_diameter_mm) *
        @sqrt(model.reference_string_density_kg_m3 / params.string_density_kg_m3);
    const hard_impact_amount = smoothStep01(
        (hit.relative_normal_speed_mps - params.hard_impact_threshold_mps) /
            (params.hard_impact_reference_speed_mps -
                params.hard_impact_threshold_mps),
    );
    const upper_mode_speed_decay_scale =
        1.0 - hard_impact_amount *
            (1.0 - params.upper_mode_fast_decay_scale);
    for (
        &upper_mode_filters,
        &upper_mode_acoustic_gains,
        params.upper_modes,
    ) |*filter, *acoustic_gain, mode| {
        const frequency_hz =
            mode.frequency_hz_at_30lb * upper_mode_frequency_scale;
        const decay_s = params.upper_mode_decay_ms * 0.001 *
            upper_mode_speed_decay_scale *
            mode.decay_scale *
            std.math.pow(f64, 3000.0 / @max(3000.0, frequency_hz), 0.25);
        const quality_factor = std.math.pi * frequency_hz * decay_s;
        filter.* = dsp.Biquad.bandPass(
            @floatFromInt(params.internal_sample_rate_hz),
            frequency_hz,
            quality_factor,
        );
        acoustic_gain.* = mode.relative_gain * std.math.pow(
            f64,
            frequency_hz / 3000.0,
            params.upper_mode_radiation_exponent,
        );
    }
    const impact_nx = hit.x_mm / (0.5 * params.head_width_mm);
    const impact_ny = hit.y_mm / (0.5 * params.head_height_mm);
    const frame_impact_nx = hit.x_mm / (0.5 * model.frameOuterWidthMm(params));
    const frame_impact_ny = hit.y_mm / (0.5 * model.frameOuterHeightMm(params));
    const frame_impact_angle = std.math.atan2(frame_impact_ny, frame_impact_nx);
    const frame_hit_microphone_radius = @sqrt(
        square(params.microphone_distance_m) +
            square(params.microphone_x_m) +
            square(params.microphone_y_m),
    );
    const frame_hit_propagation_scale =
        (params.air_density_kg_m3 / 1.204) / frame_hit_microphone_radius;
    for (
        &frame_hit_mode_filters,
        &frame_hit_mode_gains,
        params.frame_hit_modes,
    ) |*filter, *acoustic_gain, mode| {
        const decay_s = mode.decay_ms * 0.001;
        const quality_factor = std.math.pi * mode.frequency_hz * decay_s;
        filter.* = dsp.Biquad.bandPass(
            @floatFromInt(params.internal_sample_rate_hz),
            mode.frequency_hz,
            quality_factor,
        );
        const order_phase =
            @as(f64, @floatFromInt(mode.circumferential_order)) *
            frame_impact_angle + mode.phase_turns * 2.0 * std.math.pi;
        const location_coupling = if (mode.circumferential_order == 0)
            1.0
        else
            // The bank stores radiated magnitude, not the signed local modal
            // coordinate. Using the coupling magnitude keeps an on-axis,
            // geometrically symmetric left/right hit acoustically equivalent
            // while still changing the spectral mix around the circumference.
            @abs(@cos(order_phase));
        acoustic_gain.* = mode.relative_gain * location_coupling *
            std.math.pow(
                f64,
                mode.frequency_hz / 4500.0,
                params.frame_hit_radiation_exponent,
            );
    }
    const upper_mode_location_gain = @max(
        0.2,
        1.0 - 0.6 * (impact_nx * impact_nx + impact_ny * impact_ny),
    );
    // Force already contains the physical speed dependence. The exponent is
    // retained only as an optional, explicit empirical correction.
    const contact_texture_speed_scale = if (hit.relative_normal_speed_mps <= 0.0)
        0.0
    else
        std.math.pow(
            f64,
            hit.relative_normal_speed_mps / 5.0,
            params.contact_noise_speed_exponent,
        );
    const contact_texture_decay = if (params.contact_noise_decay_ms <= 0.0)
        0.0
    else
        @exp(-base_dt / (params.contact_noise_decay_ms * 0.001));
    const hard_impact_decay =
        @exp(-base_dt / (params.hard_impact_decay_ms * 0.001));
    const frame_hit_transient_decay =
        @exp(-base_dt / (params.frame_hit_transient_decay_ms * 0.001));
    var contact_texture_envelope: f64 = 0.0;
    var hard_impact_envelope: f64 = 0.0;
    var frame_hit_transient_envelope: f64 = 0.0;
    var previous_contact_force: f64 = 0.0;
    var diagnostics = Diagnostics{
        .numerical_substeps = substeps,
        .node_count = node_count,
    };

    _ = evaluateAccelerations(
        &topology,
        profile,
        &state,
        &workspace,
        &contact,
        0.0,
    );
    for (0..physics_internal_frames) |frame| {
        var last_force: f64 = 0.0;
        for (0..substeps) |substep| {
            // Velocity-Verlet: half kick, drift, refresh forces, half kick.
            // Damping and Hunt-Crossley velocity terms see the midpoint velocity.
            for (state.node_z, state.node_v, state.node_a) |*position, *velocity, acceleration| {
                velocity.* += 0.5 * acceleration * dt;
                position.* += velocity.* * dt;
            }
            for (0..4) |mode_index| {
                state.frame_v[mode_index] += 0.5 * state.frame_a[mode_index] * dt;
                state.frame_q[mode_index] += state.frame_v[mode_index] * dt;
            }
            state.shuttle_v += 0.5 * state.shuttle_a * dt;
            state.shuttle_z += state.shuttle_v * dt;

            const time_s = (@as(f64, @floatFromInt(frame)) +
                @as(f64, @floatFromInt(substep + 1)) /
                    @as(f64, @floatFromInt(substeps))) * base_dt;
            last_force = evaluateAccelerations(
                &topology,
                profile,
                &state,
                &workspace,
                &contact,
                time_s,
            );

            for (state.node_v, state.node_a) |*velocity, acceleration| {
                velocity.* += 0.5 * acceleration * dt;
            }
            for (0..4) |mode_index| {
                state.frame_v[mode_index] += 0.5 * state.frame_a[mode_index] * dt;
            }
            state.shuttle_v += 0.5 * state.shuttle_a * dt;
        }

        diagnostics.peak_force_n = @max(diagnostics.peak_force_n, last_force);
        for (state.node_z) |position| {
            diagnostics.maximum_deflection_mm = @max(
                diagnostics.maximum_deflection_mm,
                @abs(position) * 1000.0,
            );
        }
        if (hit_region == .frame) {
            const frame_contact_displacement = frameReferenceDisplacement(
                .{ .x = hit.x_mm * 0.001, .y = hit.y_mm * 0.001 },
                params,
                &state,
            );
            diagnostics.maximum_deflection_mm = @max(
                diagnostics.maximum_deflection_mm,
                @abs(frame_contact_displacement) * 1000.0,
            );
        }
        for (state.node_v) |velocity| {
            diagnostics.maximum_node_speed_mps = @max(
                diagnostics.maximum_node_speed_mps,
                @abs(velocity),
            );
        }

        const source_frame = pre_roll_internal_frames + frame;
        emitStructuralPressure(&topology, &state, raw_pressure, source_frame);
        const contact_roughness = deterministicContactNoise(frame);
        const contact_force_slew = (last_force - previous_contact_force) / base_dt;
        previous_contact_force = last_force;
        contact_texture_envelope = @max(
            last_force,
            contact_texture_envelope * contact_texture_decay,
        );
        const burst_probability = params.contact_noise_burst_probability;
        const burst_selector = 0.5 *
            (deterministicNoise(frame, 0xd1b5_4a32_d192_ed03) + 1.0);
        const burst_threshold = 1.0 - burst_probability;
        const micro_burst = if (burst_probability > 0.0 and
            burst_selector > burst_threshold)
            ((burst_selector - burst_threshold) / burst_probability) *
                deterministicNoise(frame, 0x94d0_49bb_1331_11eb) *
                params.contact_noise_burst_gain
        else
            0.0;
        // A dense rough residual plus sparse sub-frame contacts avoids the
        // stationary filtered-noise quality of a purely continuous exciter.
        const active_contact_fraction = if (contact_texture_envelope <= 1.0e-12)
            0.0
        else
            std.math.clamp(
                last_force / contact_texture_envelope,
                0.0,
                1.0,
            );
        const contact_activity = @sqrt(active_contact_fraction);
        const tail_grain =
            0.8 + 0.2 * deterministicInterpolatedNoise(
                frame,
                256,
                0x243f_6a88_85a3_08d3,
            );
        const dense_texture_amount =
            contact_activity +
            (1.0 - contact_activity) * 0.82 * tail_grain;
        const micro_contact_driver =
            0.92 * dense_texture_amount * contact_roughness +
            micro_burst;
        const micro_contact = micro_contact_driver *
            contact_texture_envelope *
            params.contact_noise_gain *
            contact_texture_speed_scale;
        const contact_pressure = if (hit_region == .strings)
            contact_noise_lowpass.process(
                contact_noise_highpass.process(micro_contact),
            )
        else
            0.0;
        const equivalent_slew_force =
            params.hard_impact_slew_time_ms * 0.001 * contact_force_slew;
        hard_impact_envelope = @max(
            @abs(equivalent_slew_force),
            hard_impact_envelope * hard_impact_decay,
        );
        // The signed dF/dt edge supplies the crack. A short decorrelated
        // residual represents unresolved cork/coating/feather micro-contacts
        // without turning the whole free decay into filtered noise.
        const hard_impact_driver =
            0.68 * equivalent_slew_force +
            0.32 * hard_impact_envelope *
                deterministicNoise(frame, 0x7f4a_7c15_9e37_79b9);
        const hard_impact_pressure = if (hit_region == .strings)
            hard_impact_amount *
                params.hard_impact_gain *
                hard_impact_lowpass.process(
                    hard_impact_highpass.process(hard_impact_driver),
                )
        else
            0.0;
        var upper_modal_pressure: f64 = 0.0;
        const modal_excitation = if (hit_region == .strings)
            last_force *
                (1.0 + params.upper_mode_contact_roughness * contact_roughness)
        else
            0.0;
        for (
            &upper_mode_filters,
            upper_mode_acoustic_gains,
        ) |*filter, acoustic_gain| {
            upper_modal_pressure += filter.process(modal_excitation) *
                acoustic_gain;
        }
        upper_modal_pressure *=
            params.upper_mode_gain * upper_mode_location_gain;

        // A direct hoop strike has two perceptually distinct pieces: the
        // very short force edge (the "tick") and a sparse, inharmonic bank of
        // local CFRP/paint/grommet modes. Carbon composite is not literally a
        // metal bell, but its stiff contact and high modal density create the
        // familiar metallic impression without pretending the frame is steel.
        const frame_equivalent_slew_force =
            params.frame_hit_slew_time_ms * 0.001 * contact_force_slew;
        frame_hit_transient_envelope = @max(
            @abs(frame_equivalent_slew_force),
            frame_hit_transient_envelope * frame_hit_transient_decay,
        );
        const frame_hit_driver =
            0.78 * frame_equivalent_slew_force +
            0.22 * frame_hit_transient_envelope *
                deterministicNoise(frame, 0x6a09_e667_f3bc_c909);
        const frame_hit_transient_pressure = if (hit_region == .frame)
            frame_hit_propagation_scale * params.frame_hit_transient_gain *
                frame_hit_lowpass.process(
                    frame_hit_highpass.process(frame_hit_driver),
                )
        else
            0.0;
        var frame_hit_modal_pressure: f64 = 0.0;
        const frame_hit_modal_excitation = if (hit_region == .frame)
            last_force
        else
            0.0;
        for (
            &frame_hit_mode_filters,
            frame_hit_mode_gains,
        ) |*filter, acoustic_gain| {
            frame_hit_modal_pressure +=
                filter.process(frame_hit_modal_excitation) * acoustic_gain;
        }
        frame_hit_modal_pressure *=
            frame_hit_propagation_scale * params.frame_hit_mode_gain;
        const contact_index = source_frame + topology.frame_audio_delay;
        if (contact_index < raw_pressure.len) {
            raw_pressure[contact_index] +=
                contact_pressure + hard_impact_pressure +
                upper_modal_pressure + frame_hit_transient_pressure +
                frame_hit_modal_pressure;
        }

        if (frame < visual_internal_frames and frame % visual_step == 0) {
            const destination_start = visual_frames_written * node_count;
            for (
                state.node_z,
                topology.node_positions,
                visual_displacements[destination_start .. destination_start + node_count],
            ) |position, node_position, *destination| {
                // The frame outline is stationary in the current view, so draw
                // bed deformation relative to the interpolated moving frame.
                // Storing absolute node z while drawing anchors at z=0 made a
                // nearly rigid 170 Hz frame motion look like undamped motion
                // of every outer string.
                destination.* = @floatCast(
                    position -
                        frameReferenceDisplacement(
                            node_position,
                            params,
                            &state,
                        ),
                );
            }
            visual_frames_written += 1;
        }
    }

    if (contact.started and contact.separated) {
        diagnostics.contact_duration_ms = contact.separation_time_s * 1000.0;
        diagnostics.apparent_restitution =
            contact.separation_relative_speed_mps /
            @max(1.0e-9, hit.relative_normal_speed_mps);
    }

    var impact_only_audio: []f32 = undefined;
    var audio: []f32 = undefined;
    const has_swoosh = hit.racket_speed_mps > params.swoosh_threshold_mps and
        params.swoosh_gain > 0.0;
    if (has_swoosh) {
        const impact_component = try dsp.decimateLowPass(
            allocator,
            raw_pressure,
            params.internal_sample_rate_hz,
            params.output_sample_rate_hz,
            output_frames,
        );
        errdefer allocator.free(impact_component);
        var impact_only_diagnostics = Diagnostics{};
        conditionOutput(impact_component, params, &impact_only_diagnostics);

        // Airflow is a separate continuous source. The offline render has one
        // scalar speed, so it represents a wind-tunnel-like constant velocity
        // across the whole buffer rather than an envelope tied to contact.
        emitRacketSwoosh(
            raw_pressure,
            profile,
            topology.frame_audio_delay,
            internal_frames,
        );
        impact_only_audio = impact_component;
        audio = try dsp.decimateLowPass(
            allocator,
            raw_pressure,
            params.internal_sample_rate_hz,
            params.output_sample_rate_hz,
            output_frames,
        );
        conditionOutput(audio, params, &diagnostics);
    } else {
        const combined = try dsp.decimateLowPass(
            allocator,
            raw_pressure,
            params.internal_sample_rate_hz,
            params.output_sample_rate_hz,
            output_frames,
        );
        errdefer allocator.free(combined);
        conditionOutput(combined, params, &diagnostics);
        audio = combined;
        impact_only_audio = try allocator.dupe(f32, combined);
    }
    errdefer allocator.free(impact_only_audio);
    errdefer allocator.free(audio);
    const decimation_ratio: usize =
        params.internal_sample_rate_hz / params.output_sample_rate_hz;
    const impact_audio_frame = @min(
        audio.len,
        (pre_roll_internal_frames + topology.frame_audio_delay +
            decimation_ratio / 2) / decimation_ratio,
    );
    const impact_audio = audio[impact_audio_frame..];
    // A frame mishit is an onset-dominated event. Analysing the entire render
    // with a Hann window places its brief local modes at the window's zero and
    // incorrectly reports the much later sympathetic string-bed ring. Keep the
    // frame diagnostic focused on the first 60 ms; normal string hits retain
    // the long window used for their stable bed note.
    const dominant_audio = if (hit_region == .frame)
        impact_audio[0..@min(
            impact_audio.len,
            @as(usize, @intCast(params.output_sample_rate_hz * 60 / 1000)),
        )]
    else
        impact_audio;
    diagnostics.dominant_frequency_hz = try dsp.dominantFrequency(
        allocator,
        dominant_audio,
        params.output_sample_rate_hz,
        if (hit_region == .strings) 500.0 else 1500.0,
        if (hit_region == .strings) 1800.0 else 18_000.0,
    );
    const fft_size = chooseFftSize(audio.len);
    const spectrum = try dsp.magnitudeSpectrumDb(allocator, audio, fft_size);
    errdefer allocator.free(spectrum);

    const result_positions = try allocator.dupe(Vec2, topology.node_positions);
    errdefer allocator.free(result_positions);
    const result_segments = try allocator.dupe(VisualSegment, topology.visual_segments);
    errdefer allocator.free(result_segments);

    return .{
        .allocator = allocator,
        .impact_only_audio = impact_only_audio,
        .audio = audio,
        .spectrum_db = spectrum,
        .output_sample_rate_hz = params.output_sample_rate_hz,
        .node_positions = result_positions,
        .segments = result_segments,
        // Keep the original allocation length so DebugAllocator can verify the
        // matching free. `visual_frame_count` marks the initialized prefix.
        .visual_displacements = visual_displacements,
        .visual_frame_count = visual_frames_written,
        .visual_frame_rate_hz = params.visualization_rate_hz,
        .impact_audio_frame = impact_audio_frame,
        .diagnostics = diagnostics,
        .hit = hit,
        .hit_region = hit_region,
        .params = params,
    };
}

fn buildTopology(allocator: std.mem.Allocator, profile: model.Profile) !Topology {
    const params = profile.model;
    const hit = profile.hit;
    const main_count: usize = params.main_count;
    const cross_count: usize = params.cross_count;
    const a = params.head_width_mm * 0.0005;
    const b = params.head_height_mm * 0.0005;

    const grid = try allocator.alloc(i32, main_count * cross_count);
    defer allocator.free(grid);
    @memset(grid, -1);

    var node_count: usize = 0;
    for (0..main_count) |main_index| {
        const x = distributedCoordinate(main_index, main_count, a);
        for (0..cross_count) |cross_index| {
            const y = distributedCoordinate(cross_index, cross_count, b);
            const ellipse = square(x / a) + square(y / b);
            if (ellipse < 1.0) {
                grid[main_index * cross_count + cross_index] = @intCast(node_count);
                node_count += 1;
            }
        }
    }
    if (node_count == 0) return error.EmptyStringBed;

    const node_positions = try allocator.alloc(Vec2, node_count);
    errdefer allocator.free(node_positions);
    for (0..main_count) |main_index| {
        const x = distributedCoordinate(main_index, main_count, a);
        for (0..cross_count) |cross_index| {
            const mapped = grid[main_index * cross_count + cross_index];
            if (mapped >= 0) {
                node_positions[@intCast(mapped)] = .{
                    .x = x,
                    .y = distributedCoordinate(cross_index, cross_count, b),
                };
            }
        }
    }

    const line_count = main_count + cross_count;
    const lines = try allocator.alloc(StringLine, line_count);
    errdefer allocator.free(lines);
    const anchors = try allocator.alloc(Anchor, line_count * 2);
    errdefer allocator.free(anchors);
    const line_nodes = try allocator.alloc(usize, node_count * 2);
    errdefer allocator.free(line_nodes);

    var node_cursor: usize = 0;
    var line_cursor: usize = 0;
    const main_tension = hit.main_tension_lbf * model.pounds_force_to_newtons;
    const cross_tension = hit.cross_tension_lbf * model.pounds_force_to_newtons;

    for (0..main_count) |main_index| {
        const x = distributedCoordinate(main_index, main_count, a);
        const boundary_y = b * @sqrt(@max(0.0, 1.0 - square(x / a)));
        const first_node = node_cursor;
        for (0..cross_count) |cross_index| {
            const mapped = grid[main_index * cross_count + cross_index];
            if (mapped >= 0) {
                line_nodes[node_cursor] = @intCast(mapped);
                node_cursor += 1;
            }
        }
        const anchor_start = line_cursor * 2;
        anchors[anchor_start] = makeAnchor(.{ .x = x, .y = -boundary_y }, a, b);
        anchors[anchor_start + 1] = makeAnchor(.{ .x = x, .y = boundary_y }, a, b);
        lines[line_cursor] = .{
            .orientation = .main,
            .node_start = first_node,
            .node_count = node_cursor - first_node,
            .anchor_start = anchor_start,
            .anchor_end = anchor_start + 1,
            .rest_length_m = 2.0 * boundary_y,
            .nominal_tension_n = main_tension,
        };
        line_cursor += 1;
    }

    for (0..cross_count) |cross_index| {
        const y = distributedCoordinate(cross_index, cross_count, b);
        const boundary_x = a * @sqrt(@max(0.0, 1.0 - square(y / b)));
        const first_node = node_cursor;
        for (0..main_count) |main_index| {
            const mapped = grid[main_index * cross_count + cross_index];
            if (mapped >= 0) {
                line_nodes[node_cursor] = @intCast(mapped);
                node_cursor += 1;
            }
        }
        const anchor_start = line_cursor * 2;
        anchors[anchor_start] = makeAnchor(.{ .x = -boundary_x, .y = y }, a, b);
        anchors[anchor_start + 1] = makeAnchor(.{ .x = boundary_x, .y = y }, a, b);
        lines[line_cursor] = .{
            .orientation = .cross,
            .node_start = first_node,
            .node_count = node_cursor - first_node,
            .anchor_start = anchor_start,
            .anchor_end = anchor_start + 1,
            .rest_length_m = 2.0 * boundary_x,
            .nominal_tension_n = cross_tension,
        };
        line_cursor += 1;
    }
    std.debug.assert(node_cursor == line_nodes.len);

    const node_masses = try allocator.alloc(f64, node_count);
    errdefer allocator.free(node_masses);
    const associated_length = try allocator.alloc(f64, node_count);
    defer allocator.free(associated_length);
    const stiffness_sum = try allocator.alloc(f64, node_count);
    defer allocator.free(stiffness_sum);
    @memset(node_masses, 0.0);
    @memset(associated_length, 0.0);
    @memset(stiffness_sum, 0.0);

    var segment_count: usize = 0;
    for (lines) |line| segment_count += line.node_count + 1;
    const visual_segments = try allocator.alloc(VisualSegment, segment_count);
    errdefer allocator.free(visual_segments);

    const diameter_m = params.string_diameter_mm * 0.001;
    const area = std.math.pi * square(0.5 * diameter_m);
    const linear_density = params.string_density_kg_m3 * area;
    const axial_rigidity = params.string_young_modulus_gpa * 1.0e9 * area;
    var visual_cursor: usize = 0;

    for (lines) |line| {
        const maximum_tension = line.nominal_tension_n +
            axial_rigidity * params.maximum_dynamic_strain;
        for (0..line.node_count + 1) |segment_index| {
            const left = linePointReference(
                line,
                segment_index,
                line_nodes,
                anchors,
                node_positions,
            );
            const right = linePointReference(
                line,
                segment_index + 1,
                line_nodes,
                anchors,
                node_positions,
            );
            const length = @max(1.0e-5, distance(left.position, right.position));
            if (left.node) |node| {
                node_masses[node] += 0.5 * linear_density * length;
                associated_length[node] += 0.5 * length;
                stiffness_sum[node] +=
                    params.transverse_stiffness_scale * maximum_tension / length;
            }
            if (right.node) |node| {
                node_masses[node] += 0.5 * linear_density * length;
                associated_length[node] += 0.5 * length;
                stiffness_sum[node] +=
                    params.transverse_stiffness_scale * maximum_tension / length;
            }
            visual_segments[visual_cursor] = .{
                .a_position = left.position,
                .b_position = right.position,
                .a_node = if (left.node) |node| @intCast(node) else -1,
                .b_node = if (right.node) |node| @intCast(node) else -1,
            };
            visual_cursor += 1;
        }
    }

    var omega_max: f64 = 0.0;
    for (node_masses, stiffness_sum) |mass, stiffness| {
        if (mass <= 0.0) return error.ZeroNodeMass;
        omega_max = @max(omega_max, @sqrt(stiffness / mass));
    }

    const node_radiation_gain = try allocator.alloc(f64, node_count);
    errdefer allocator.free(node_radiation_gain);
    const node_audio_delay = try allocator.alloc(usize, node_count);
    errdefer allocator.free(node_audio_delay);
    for (
        node_positions,
        associated_length,
        node_radiation_gain,
        node_audio_delay,
    ) |position, exposed_length, *gain, *delay| {
        const source_distance = @sqrt(
            square(params.microphone_distance_m) +
                square(position.x - params.microphone_x_m) +
                square(position.y - params.microphone_y_m),
        );
        gain.* = params.air_density_kg_m3 /
            (4.0 * std.math.pi * source_distance) *
            exposed_length *
            diameter_m *
            params.string_radiation_efficiency;
        delay.* = @intFromFloat(@round(
            source_distance / params.sound_speed_mps *
                @as(f64, @floatFromInt(params.internal_sample_rate_hz)),
        ));
    }

    var frame_modal_mass: [4]f64 = undefined;
    var frame_audio_gain: [4]f64 = undefined;
    const total_frame_mass = params.frame_mass_g * 0.001;
    for (0..4) |mode_index| {
        var mean_square: f64 = 0.0;
        for (anchors) |anchor| {
            const shape = anchor.shape[mode_index] *
                params.frame_modes[mode_index].force_coupling;
            mean_square += shape * shape;
        }
        mean_square /= @as(f64, @floatFromInt(anchors.len));
        frame_modal_mass[mode_index] = total_frame_mass * @max(0.05, mean_square);
        // The virtual microphone is on the centre normal. The ideal odd
        // left/right shape cancels in its far-field area integral. The
        // top/bottom mode retains an editable asymmetry term for the shaft,
        // handle and hand, while both remain mechanically coupled to strings.
        const microphone_radius = @sqrt(
            square(params.microphone_distance_m) +
                square(params.microphone_x_m) +
                square(params.microphone_y_m),
        );
        const on_axis_projection: f64 = switch (mode_index) {
            1 => params.microphone_x_m / microphone_radius,
            2 => params.frame_radiation_asymmetry +
                params.microphone_y_m / microphone_radius,
            else => 1.0,
        };
        frame_audio_gain[mode_index] = on_axis_projection *
            params.air_density_kg_m3 /
            (4.0 * std.math.pi * microphone_radius) *
            params.frame_modes[mode_index].radiation_area_m2 *
            params.frame_radiation_efficiency;
    }
    const microphone_radius = @sqrt(
        square(params.microphone_distance_m) +
            square(params.microphone_x_m) +
            square(params.microphone_y_m),
    );
    const frame_audio_delay: usize = @intFromFloat(@round(
        microphone_radius / params.sound_speed_mps *
            @as(f64, @floatFromInt(params.internal_sample_rate_hz)),
    ));

    return .{
        .allocator = allocator,
        .node_positions = node_positions,
        .node_masses = node_masses,
        .node_radiation_gain = node_radiation_gain,
        .node_audio_delay = node_audio_delay,
        .line_nodes = line_nodes,
        .lines = lines,
        .anchors = anchors,
        .visual_segments = visual_segments,
        .frame_modal_mass = frame_modal_mass,
        .frame_audio_gain = frame_audio_gain,
        .frame_audio_delay = frame_audio_delay,
        .stability_omega_max = omega_max,
    };
}

const PointReference = struct {
    position: Vec2,
    node: ?usize,
    anchor: ?usize,
};

fn linePointReference(
    line: StringLine,
    point_index: usize,
    line_nodes: []const usize,
    anchors: []const Anchor,
    node_positions: []const Vec2,
) PointReference {
    if (point_index == 0) {
        return .{
            .position = anchors[line.anchor_start].position,
            .node = null,
            .anchor = line.anchor_start,
        };
    }
    if (point_index == line.node_count + 1) {
        return .{
            .position = anchors[line.anchor_end].position,
            .node = null,
            .anchor = line.anchor_end,
        };
    }
    const node = line_nodes[line.node_start + point_index - 1];
    return .{
        .position = node_positions[node],
        .node = node,
        .anchor = null,
    };
}

fn makeAnchor(position: Vec2, a: f64, b: f64) Anchor {
    const nx = position.x / a;
    const ny = position.y / b;
    return .{
        .position = position,
        .shape = .{
            1.0,
            nx,
            ny,
            nx * nx - ny * ny,
        },
    };
}

fn distributedCoordinate(index: usize, count: usize, radius: f64) f64 {
    return ((@as(f64, @floatFromInt(index)) + 0.5) /
        @as(f64, @floatFromInt(count)) * 2.0 - 1.0) * radius;
}

fn calculateContactWeights(
    topology: *const Topology,
    profile: model.Profile,
    weights: []f64,
) void {
    const hit_x = profile.hit.x_mm * 0.001;
    const hit_y = profile.hit.y_mm * 0.001;
    const radius = 0.5 * profile.model.cork_diameter_mm * 0.001;
    var sum: f64 = 0.0;
    var nearest_index: usize = 0;
    var nearest_distance = std.math.inf(f64);
    for (topology.node_positions, weights, 0..) |position, *weight, index| {
        const dx = position.x - hit_x;
        const dy = position.y - hit_y;
        const radial_distance = @sqrt(dx * dx + dy * dy);
        if (radial_distance < nearest_distance) {
            nearest_distance = radial_distance;
            nearest_index = index;
        }
        if (radial_distance < radius) {
            const ratio = radial_distance / radius;
            weight.* = @sqrt(@max(0.0, 1.0 - ratio * ratio));
            sum += weight.*;
        } else {
            weight.* = 0.0;
        }
    }
    if (sum <= 1.0e-12) {
        weights[nearest_index] = 1.0;
        return;
    }
    for (weights) |*weight| weight.* /= sum;
}

fn evaluateAccelerations(
    topology: *const Topology,
    profile: model.Profile,
    state: *DynamicState,
    workspace: *Workspace,
    contact: *ContactState,
    time_s: f64,
) f64 {
    @memset(workspace.node_force, 0.0);
    @memset(workspace.anchor_force, 0.0);

    const params = profile.model;
    const diameter = params.string_diameter_mm * 0.001;
    const string_area = std.math.pi * square(0.5 * diameter);
    const string_linear_density =
        params.string_density_kg_m3 * string_area;
    const axial_rigidity = params.string_young_modulus_gpa * 1.0e9 * string_area;

    for (topology.lines) |line| {
        var segment_dz: [maximum_line_segments]f64 = undefined;
        var segment_dv: [maximum_line_segments]f64 = undefined;
        var segment_inverse_length: [maximum_line_segments]f64 = undefined;
        var extension: f64 = 0.0;
        const segment_count = line.node_count + 1;
        std.debug.assert(segment_count <= maximum_line_segments);

        for (0..segment_count) |segment_index| {
            const left = dynamicLinePoint(topology, line, segment_index, state, params);
            const right = dynamicLinePoint(topology, line, segment_index + 1, state, params);
            const rest_length = @max(1.0e-5, distance(left.position, right.position));
            const dz = right.z - left.z;
            const current_length = @sqrt(rest_length * rest_length + dz * dz);
            segment_dz[segment_index] = dz;
            segment_dv[segment_index] = right.velocity - left.velocity;
            segment_inverse_length[segment_index] = 1.0 / current_length;
            extension += current_length - rest_length;
        }

        const strain = std.math.clamp(
            extension / @max(1.0e-6, line.rest_length_m),
            0.0,
            params.maximum_dynamic_strain,
        );
        const tension = line.nominal_tension_n + axial_rigidity * strain;
        for (0..segment_count) |segment_index| {
            var force = params.transverse_stiffness_scale * tension *
                segment_dz[segment_index] *
                segment_inverse_length[segment_index];
            if (segment_index == 0 or segment_index + 1 == segment_count) {
                // A dashpot referenced to the string's transverse
                // characteristic impedance absorbs a small part of each wave
                // reaching a grommet. The clamp still enforces the mode-shape
                // boundary; it does not impose an unphysical radial fade.
                const transverse_impedance =
                    @sqrt(@max(0.0, tension * string_linear_density));
                force += params.grommet_loss_factor *
                    transverse_impedance *
                    segment_dv[segment_index];
            }
            addLinePointForce(
                topology,
                line,
                segment_index,
                force,
                workspace,
            );
            addLinePointForce(
                topology,
                line,
                segment_index + 1,
                -force,
                workspace,
            );
        }
    }

    var frame_generalized_force: [4]f64 = @splat(0.0);
    for (topology.anchors, workspace.anchor_force) |anchor, force| {
        for (0..4) |mode_index| {
            frame_generalized_force[mode_index] +=
                force *
                anchor.shape[mode_index] *
                params.frame_modes[mode_index].force_coupling;
        }
    }

    var contact_force: f64 = 0.0;
    if (contact.enabled and !contact.separated) {
        var contact_surface_z: f64 = 0.0;
        var contact_surface_v: f64 = 0.0;
        const impact_position = Vec2{
            .x = profile.hit.x_mm * 0.001,
            .y = profile.hit.y_mm * 0.001,
        };
        switch (contact.region) {
            .strings => for (
                workspace.contact_weights,
                state.node_z,
                state.node_v,
            ) |weight, position, velocity| {
                contact_surface_z += weight * position;
                contact_surface_v += weight * velocity;
            },
            .frame => {
                contact_surface_z = frameReferenceDisplacement(
                    impact_position,
                    params,
                    state,
                );
                contact_surface_v = frameReferenceVelocity(
                    impact_position,
                    params,
                    state,
                );
            },
        }
        const compression = state.shuttle_z - contact_surface_z;
        const relative_velocity = state.shuttle_v - contact_surface_v;
        if (compression > 0.0) {
            contact.started = true;
            const stiffness = switch (contact.region) {
                .strings => params.contact_stiffness_n_m_pow,
                .frame => params.frame_contact_stiffness_n_m_pow,
            };
            const exponent = switch (contact.region) {
                .strings => params.contact_exponent,
                .frame => params.frame_contact_exponent,
            };
            const restitution = std.math.clamp(switch (contact.region) {
                .strings => params.target_restitution,
                .frame => params.frame_contact_restitution,
            }, 0.01, 1.0);
            const damping_calibration = switch (contact.region) {
                .strings => params.contact_damping_calibration,
                .frame => params.frame_contact_damping_calibration,
            };
            const reference_speed = @max(1.0e-6, profile.hit.relative_normal_speed_mps);
            const hunt_crossley_damping =
                damping_calibration *
                1.5 * (1.0 - restitution) /
                (restitution * reference_speed);
            const damping_multiplier = std.math.clamp(
                1.0 + hunt_crossley_damping * relative_velocity,
                0.0,
                3.0,
            );
            contact_force = stiffness *
                std.math.pow(f64, compression, exponent) *
                damping_multiplier;
            switch (contact.region) {
                .strings => for (
                    workspace.contact_weights,
                    workspace.node_force,
                ) |weight, *force| {
                    force.* += weight * contact_force;
                },
                .frame => {
                    const shape = frameModeShapeAt(impact_position, params);
                    for (0..4) |mode_index| {
                        frame_generalized_force[mode_index] +=
                            contact_force * shape[mode_index] *
                            params.frame_modes[mode_index].force_coupling;
                    }
                },
            }
        } else if (contact.started and relative_velocity < 0.0) {
            contact.separated = true;
            contact.separation_time_s = time_s;
            contact.separation_relative_speed_mps = -relative_velocity;
        }
    }
    contact.force_n = contact_force;

    const small_signal_damping_rate = params.string_damping_rate_s *
        (1.0 + 0.5 * params.crossing_friction);
    for (
        state.node_a,
        state.node_v,
        workspace.node_force,
        topology.node_masses,
    ) |*acceleration, velocity, force, mass| {
        const nonlinear_excess = @max(
            0.0,
            @abs(velocity) - params.string_nonlinear_damping_onset_mps,
        );
        const nonlinear_amount = nonlinear_excess /
            (params.string_nonlinear_damping_transition_mps +
                nonlinear_excess);
        const damping_rate = small_signal_damping_rate +
            params.string_nonlinear_damping_rate_s * nonlinear_amount;
        acceleration.* = force / mass - damping_rate * velocity;
    }

    for (0..4) |mode_index| {
        const mode = params.frame_modes[mode_index];
        const omega = 2.0 * std.math.pi * mode.frequency_hz;
        const damping = omega / mode.quality_factor;
        state.frame_a[mode_index] =
            frame_generalized_force[mode_index] /
            topology.frame_modal_mass[mode_index] -
            damping * state.frame_v[mode_index] -
            omega * omega * state.frame_q[mode_index];
    }
    state.shuttle_a = -contact_force / (params.shuttle_mass_g * 0.001);
    return contact_force;
}

const DynamicPoint = struct {
    position: Vec2,
    z: f64,
    velocity: f64,
};

fn dynamicLinePoint(
    topology: *const Topology,
    line: StringLine,
    point_index: usize,
    state: *const DynamicState,
    params: model.ModelParams,
) DynamicPoint {
    const reference = linePointReference(
        line,
        point_index,
        topology.line_nodes,
        topology.anchors,
        topology.node_positions,
    );
    if (reference.node) |node| {
        return .{
            .position = reference.position,
            .z = state.node_z[node],
            .velocity = state.node_v[node],
        };
    }
    const anchor_index = reference.anchor.?;
    const anchor = topology.anchors[anchor_index];
    var z: f64 = 0.0;
    var velocity: f64 = 0.0;
    for (0..4) |mode_index| {
        const coupling =
            anchor.shape[mode_index] * params.frame_modes[mode_index].force_coupling;
        z += coupling * state.frame_q[mode_index];
        velocity += coupling * state.frame_v[mode_index];
    }
    return .{
        .position = reference.position,
        .z = z,
        .velocity = velocity,
    };
}

fn frameReferenceDisplacement(
    position: Vec2,
    params: model.ModelParams,
    state: *const DynamicState,
) f64 {
    const shape = frameModeShapeAt(position, params);
    var displacement: f64 = 0.0;
    for (0..4) |mode_index| {
        displacement += shape[mode_index] *
            params.frame_modes[mode_index].force_coupling *
            state.frame_q[mode_index];
    }
    return displacement;
}

fn frameReferenceVelocity(
    position: Vec2,
    params: model.ModelParams,
    state: *const DynamicState,
) f64 {
    const shape = frameModeShapeAt(position, params);
    var velocity: f64 = 0.0;
    for (0..4) |mode_index| {
        velocity += shape[mode_index] *
            params.frame_modes[mode_index].force_coupling *
            state.frame_v[mode_index];
    }
    return velocity;
}

fn frameModeShapeAt(position: Vec2, params: model.ModelParams) [4]f64 {
    const half_width = params.head_width_mm * 0.0005;
    const half_height = params.head_height_mm * 0.0005;
    const nx = position.x / half_width;
    const ny = position.y / half_height;
    return .{ 1.0, nx, ny, nx * nx - ny * ny };
}

fn addLinePointForce(
    topology: *const Topology,
    line: StringLine,
    point_index: usize,
    force: f64,
    workspace: *Workspace,
) void {
    if (point_index == 0) {
        workspace.anchor_force[line.anchor_start] += force;
    } else if (point_index == line.node_count + 1) {
        workspace.anchor_force[line.anchor_end] += force;
    } else {
        const node = topology.line_nodes[line.node_start + point_index - 1];
        workspace.node_force[node] += force;
    }
}

fn emitStructuralPressure(
    topology: *const Topology,
    state: *const DynamicState,
    pressure: []f64,
    frame: usize,
) void {
    for (
        state.node_a,
        topology.node_radiation_gain,
        topology.node_audio_delay,
    ) |acceleration, gain, delay| {
        const destination = frame + delay;
        if (destination < pressure.len) pressure[destination] += gain * acceleration;
    }
    const frame_destination = frame + topology.frame_audio_delay;
    if (frame_destination < pressure.len) {
        for (state.frame_a, topology.frame_audio_gain) |acceleration, gain| {
            pressure[frame_destination] += gain * acceleration;
        }
    }
}

fn emitRacketSwoosh(
    pressure: []f64,
    profile: model.Profile,
    propagation_delay: usize,
    source_frame_count: usize,
) void {
    const params = profile.model;
    const racket_speed = profile.hit.racket_speed_mps;
    if (racket_speed <= params.swoosh_threshold_mps or
        params.swoosh_gain <= 0.0 or
        propagation_delay >= pressure.len)
    {
        return;
    }

    const sample_rate: f64 = @floatFromInt(params.internal_sample_rate_hz);
    const speed_amount =
        (racket_speed - params.swoosh_threshold_mps) /
        (params.swoosh_reference_speed_mps - params.swoosh_threshold_mps);
    const velocity_scale = std.math.pow(
        f64,
        @max(0.0, speed_amount),
        params.swoosh_speed_exponent,
    );
    const frame_diameter_m = params.swoosh_frame_diameter_mm * 0.001;
    const string_diameter_m = params.string_diameter_mm * 0.001;
    const frame_frequency_hz = std.math.clamp(
        params.swoosh_strouhal_number * racket_speed / frame_diameter_m,
        60.0,
        sample_rate * 0.30,
    );
    const string_frequency_hz = std.math.clamp(
        params.swoosh_strouhal_number * racket_speed / string_diameter_m,
        600.0,
        sample_rate * 0.42,
    );
    // Low Q represents the many differently oriented frame and grid members:
    // their individual vortex tones overlap into the familiar broad "swoosh".
    var frame_band = dsp.Biquad.bandPass(sample_rate, frame_frequency_hz, 0.38);
    var string_band = dsp.Biquad.bandPass(sample_rate, string_frequency_hz, 0.48);

    const reference_face_area =
        std.math.pi * 0.188 * 0.253 * 0.25;
    const face_area =
        std.math.pi *
        params.head_width_mm * 0.001 *
        params.head_height_mm * 0.001 * 0.25;
    const propagation_scale =
        (params.air_density_kg_m3 / 1.204) *
        (face_area / reference_face_area) /
        params.microphone_distance_m;
    const calibrated_scale =
        params.swoosh_gain * velocity_scale * propagation_scale;
    const source_limit = @min(
        source_frame_count,
        pressure.len - propagation_delay,
    );

    for (0..source_limit) |source_frame| {
        const frame_noise = frame_band.process(
            deterministicNoise(source_frame, 0xa24b_aed4_963e_e407),
        );
        const string_noise = string_band.process(
            deterministicNoise(source_frame, 0x9fb2_1c65_1e98_df25),
        );
        const time_s = @as(f64, @floatFromInt(source_frame)) / sample_rate;
        const turbulent_flutter =
            1.0 + 0.10 * @sin(2.0 * std.math.pi * 31.0 * time_s) +
            0.06 * @sin(2.0 * std.math.pi * 73.0 * time_s + 0.7);
        const source_pressure = calibrated_scale *
            turbulent_flutter * (0.72 * frame_noise + 0.28 * string_noise);
        pressure[source_frame + propagation_delay] += source_pressure;
    }
}

fn conditionOutput(
    audio: []f32,
    params: model.ModelParams,
    diagnostics: *Diagnostics,
) void {
    const sample_rate: f64 = @floatFromInt(params.output_sample_rate_hz);
    const high_pass_r = @exp(-2.0 * std.math.pi * 30.0 / sample_rate);
    var previous_input: f64 = 0.0;
    var previous_output: f64 = 0.0;
    const fade_samples: usize = @min(audio.len, params.output_sample_rate_hz / 100);
    const fade_start = audio.len - fade_samples;

    for (audio, 0..) |*sample, index| {
        const input = @as(f64, sample.*);
        var output = input - previous_input + high_pass_r * previous_output;
        previous_input = input;
        previous_output = output;
        output *= params.master_gain;
        if (index >= fade_start and fade_samples > 1) {
            const phase = @as(f64, @floatFromInt(index - fade_start)) /
                @as(f64, @floatFromInt(fade_samples - 1));
            output *= 0.5 + 0.5 * @cos(std.math.pi * phase);
        }
        sample.* = @floatCast(output);
        diagnostics.peak_before_dynamics = @max(
            diagnostics.peak_before_dynamics,
            @abs(output),
        );
    }

    const threshold_db = params.compressor_threshold_dbfs;
    const ratio = params.compressor_ratio;
    const knee_db = params.compressor_knee_db;
    const lookahead_samples: usize = @min(
        audio.len,
        @as(usize, @intFromFloat(@round(
            params.compressor_lookahead_ms * 0.001 * sample_rate,
        ))),
    );
    const release_coefficient =
        @exp(-1.0 / (params.compressor_release_ms * 0.001 * sample_rate));
    const limiter_ceiling =
        std.math.pow(f64, 10.0, params.limiter_ceiling_dbfs / 20.0);
    var compressor_gain: f64 = 1.0;
    var sum_squares: f64 = 0.0;

    for (audio, 0..) |*sample, index| {
        var future_peak: f64 = 0.0;
        const future_end = @min(audio.len, index + lookahead_samples + 1);
        for (audio[index..future_end]) |future_sample| {
            future_peak = @max(future_peak, @abs(@as(f64, future_sample)));
        }
        const target_gain_db = compressorGainDb(
            future_peak,
            threshold_db,
            ratio,
            knee_db,
        );
        const target_gain = std.math.pow(f64, 10.0, target_gain_db / 20.0);
        if (target_gain < compressor_gain) {
            compressor_gain = target_gain;
        } else {
            compressor_gain =
                target_gain +
                release_coefficient * (compressor_gain - target_gain);
        }
        diagnostics.maximum_gain_reduction_db = @max(
            diagnostics.maximum_gain_reduction_db,
            -20.0 * std.math.log10(@max(1.0e-12, compressor_gain)),
        );

        var output = @as(f64, sample.*) * compressor_gain;
        if (@abs(output) > limiter_ceiling) {
            output = std.math.clamp(output, -limiter_ceiling, limiter_ceiling);
            diagnostics.limited_samples += 1;
        }
        diagnostics.peak_before_clamp = @max(
            diagnostics.peak_before_clamp,
            @abs(output),
        );
        if (@abs(output) > 1.0) diagnostics.clipped_samples += 1;
        output = std.math.clamp(output, -1.0, 1.0);
        sample.* = @floatCast(output);
        sum_squares += output * output;
    }
    diagnostics.rms = if (audio.len == 0)
        0.0
    else
        @sqrt(sum_squares / @as(f64, @floatFromInt(audio.len)));
}

fn compressorGainDb(
    peak: f64,
    threshold_db: f64,
    ratio: f64,
    knee_db: f64,
) f64 {
    if (peak <= 1.0e-20 or ratio <= 1.0) return 0.0;
    const input_db = 20.0 * std.math.log10(peak);
    const over_db = input_db - threshold_db;
    const slope = 1.0 / ratio - 1.0;
    if (knee_db <= 0.0) {
        return if (over_db > 0.0) slope * over_db else 0.0;
    }
    const half_knee = 0.5 * knee_db;
    if (over_db <= -half_knee) return 0.0;
    if (over_db >= half_knee) return slope * over_db;
    const knee_position = over_db + half_knee;
    return slope * knee_position * knee_position / (2.0 * knee_db);
}

fn chooseFftSize(sample_count: usize) usize {
    var result: usize = 256;
    while (result * 2 <= sample_count and result < 16_384) result *= 2;
    return result;
}

fn distance(a: Vec2, b: Vec2) f64 {
    return @sqrt(square(a.x - b.x) + square(a.y - b.y));
}

fn square(value: f64) f64 {
    return value * value;
}

fn smoothStep01(value: f64) f64 {
    const amount = std.math.clamp(value, 0.0, 1.0);
    return amount * amount * (3.0 - 2.0 * amount);
}

fn deterministicContactNoise(frame: usize) f64 {
    return deterministicNoise(frame, 0x9e37_79b9_7f4a_7c15);
}

fn deterministicInterpolatedNoise(
    frame: usize,
    period: usize,
    salt: u64,
) f64 {
    const safe_period = @max(1, period);
    const grain = frame / safe_period;
    const phase = @as(f64, @floatFromInt(frame % safe_period)) /
        @as(f64, @floatFromInt(safe_period));
    const smooth = phase * phase * (3.0 - 2.0 * phase);
    const first = deterministicNoise(grain, salt);
    const second = deterministicNoise(grain + 1, salt);
    return first + (second - first) * smooth;
}

fn deterministicNoise(frame: usize, salt: u64) f64 {
    var bits: u64 = @as(u64, @intCast(frame)) +% salt;
    bits = (bits ^ (bits >> 30)) *% 0xbf58_476d_1ce4_e5b9;
    bits = (bits ^ (bits >> 27)) *% 0x94d0_49bb_1331_11eb;
    bits ^= bits >> 31;
    const mantissa = bits >> 11;
    const unit = @as(f64, @floatFromInt(mantissa)) /
        @as(f64, @floatFromInt(@as(u64, 1) << 53));
    return unit * 2.0 - 1.0;
}

test "generated topology has shared, positive-mass nodes" {
    const allocator = std.testing.allocator;
    const profile = model.Profile{};
    var topology = try buildTopology(allocator, profile);
    defer topology.deinit();
    try std.testing.expect(topology.node_positions.len > 300);
    try std.testing.expectEqual(topology.node_positions.len * 2, topology.line_nodes.len);
    for (topology.node_masses) |mass| try std.testing.expect(mass > 0.0);
    try std.testing.expect(topology.stability_omega_max > 0.0);
}

test "zero collision speed produces digital silence" {
    const allocator = std.testing.allocator;
    var profile = model.Profile{};
    profile.hit.relative_normal_speed_mps = 0.0;
    profile.model.internal_sample_rate_hz = 48_000;
    profile.model.output_sample_rate_hz = 48_000;
    profile.model.visualization_rate_hz = 8_000;
    profile.model.duration_s = 0.05;
    profile.model.impact_pre_roll_ms = 10.0;
    var result = try simulate(allocator, profile);
    defer result.deinit();
    for (result.audio) |sample| try std.testing.expectEqual(@as(f32, 0.0), sample);
    try std.testing.expectEqual(@as(f64, 0.0), result.diagnostics.peak_force_n);
}

test "short impact render is deterministic and finite" {
    const allocator = std.testing.allocator;
    var profile = model.Profile{};
    profile.model.internal_sample_rate_hz = 96_000;
    profile.model.output_sample_rate_hz = 48_000;
    profile.model.visualization_rate_hz = 8_000;
    profile.model.duration_s = 0.06;
    profile.model.impact_pre_roll_ms = 10.0;
    var first = try simulate(allocator, profile);
    defer first.deinit();
    var second = try simulate(allocator, profile);
    defer second.deinit();
    try std.testing.expectEqualSlices(f32, first.audio, second.audio);
    try std.testing.expect(first.diagnostics.peak_force_n > 0.0);
    try std.testing.expect(first.diagnostics.maximum_deflection_mm > 0.0);
    for (first.audio) |sample| try std.testing.expect(std.math.isFinite(sample));
}

test "linearized topology is mirror symmetric and has a symmetric stiffness matrix" {
    const allocator = std.testing.allocator;
    const profile = model.Profile{};
    var topology = try buildTopology(allocator, profile);
    defer topology.deinit();

    for (topology.node_positions, topology.node_masses) |position, mass| {
        const mirror = findNodeAt(topology.node_positions, -position.x, position.y) orelse
            return error.MissingMirrorNode;
        try std.testing.expectApproxEqAbs(mass, topology.node_masses[mirror], 1.0e-12);
    }

    const count = topology.node_positions.len;
    const stiffness = try allocator.alloc(f64, count * count);
    defer allocator.free(stiffness);
    @memset(stiffness, 0.0);
    for (topology.lines) |line| {
        for (0..line.node_count + 1) |segment_index| {
            const left = linePointReference(
                line,
                segment_index,
                topology.line_nodes,
                topology.anchors,
                topology.node_positions,
            );
            const right = linePointReference(
                line,
                segment_index + 1,
                topology.line_nodes,
                topology.anchors,
                topology.node_positions,
            );
            const segment_length = distance(left.position, right.position);
            const spring = profile.model.transverse_stiffness_scale *
                line.nominal_tension_n / segment_length;
            if (left.node) |left_node| {
                stiffness[left_node * count + left_node] += spring;
            }
            if (right.node) |right_node| {
                stiffness[right_node * count + right_node] += spring;
            }
            if (left.node != null and right.node != null) {
                const left_node = left.node.?;
                const right_node = right.node.?;
                stiffness[left_node * count + right_node] -= spring;
                stiffness[right_node * count + left_node] -= spring;
            }
        }
    }
    for (0..count) |row| {
        for (0..count) |column| {
            try std.testing.expectEqual(
                stiffness[row * count + column],
                stiffness[column * count + row],
            );
        }
    }

    const base_dt = 1.0 /
        @as(f64, @floatFromInt(profile.model.internal_sample_rate_hz));
    var substeps: u32 = 1;
    while (topology.stability_omega_max * base_dt /
        @as(f64, @floatFromInt(substeps)) > 1.0 and substeps < 32)
    {
        substeps *= 2;
    }
    try std.testing.expect(std.math.isPowerOfTwo(substeps));
    try std.testing.expect(
        topology.stability_omega_max * base_dt /
            @as(f64, @floatFromInt(substeps)) <= 1.0,
    );
}

test "dominant string frequency follows square-root tension scaling" {
    const allocator = std.testing.allocator;
    var low_profile = shortTestProfile(0.20);
    low_profile.hit.main_tension_lbf = 18.0;
    low_profile.hit.cross_tension_lbf = 18.0;
    low_profile.hit.relative_normal_speed_mps = 5.0;
    low_profile.model.visualization_duration_ms = 1.0;
    // This test measures the resolved string-bed family, not the separately
    // calibrated unresolved local-contact resonators.
    low_profile.model.contact_noise_gain = 0.0;
    low_profile.model.upper_mode_gain = 0.0;
    low_profile.model.frame_radiation_efficiency = 0.0;
    for (&low_profile.model.frame_modes) |*mode| mode.force_coupling = 0.0;
    var high_profile = low_profile;
    high_profile.hit.main_tension_lbf = 32.0;
    high_profile.hit.cross_tension_lbf = 32.0;

    var low = try simulate(allocator, low_profile);
    defer low.deinit();
    var high = try simulate(allocator, high_profile);
    defer high.deinit();
    const low_peak = try dsp.dominantFrequency(
        allocator,
        low.audio,
        low.output_sample_rate_hz,
        500.0,
        1600.0,
    );
    const high_peak = try dsp.dominantFrequency(
        allocator,
        high.audio,
        high.output_sample_rate_hz,
        500.0,
        1600.0,
    );
    const observed = high_peak / low_peak;
    const expected = @sqrt(32.0 / 18.0);
    try std.testing.expectApproxEqRel(expected, observed, 0.05);
}

test "left and right impacts mirror motion and preserve the magnitude spectrum" {
    const allocator = std.testing.allocator;
    var left_profile = shortTestProfile(0.06);
    left_profile.hit.x_mm = -25.0;
    left_profile.hit.y_mm = 8.0;
    var right_profile = left_profile;
    right_profile.hit.x_mm = 25.0;

    var left = try simulate(allocator, left_profile);
    defer left.deinit();
    var right = try simulate(allocator, right_profile);
    defer right.deinit();
    try std.testing.expectEqual(left.node_positions.len, right.node_positions.len);
    try std.testing.expectEqual(left.visual_frame_count, right.visual_frame_count);

    const mirror_map = try allocator.alloc(usize, left.node_positions.len);
    defer allocator.free(mirror_map);
    for (left.node_positions, mirror_map) |position, *mapped| {
        mapped.* = findNodeAt(right.node_positions, -position.x, position.y) orelse
            return error.MissingMirrorNode;
    }
    for (0..left.visual_frame_count) |frame_index| {
        const left_frame = left.visualFrame(frame_index);
        const right_frame = right.visualFrame(frame_index);
        for (left_frame, mirror_map) |displacement, mirrored| {
            try std.testing.expectApproxEqAbs(
                displacement,
                right_frame[mirrored],
                2.0e-6,
            );
        }
    }
    for (left.spectrum_db, right.spectrum_db) |left_db, right_db| {
        try std.testing.expectApproxEqAbs(left_db, right_db, 0.08);
    }
}

test "central bed is more compliant than a near-frame impact" {
    const allocator = std.testing.allocator;
    var centre_profile = shortTestProfile(0.06);
    centre_profile.hit.relative_normal_speed_mps = 25.0;
    var edge_profile = centre_profile;
    edge_profile.hit.x_mm = centre_profile.model.head_width_mm * 0.38;

    var centre = try simulate(allocator, centre_profile);
    defer centre.deinit();
    var edge = try simulate(allocator, edge_profile);
    defer edge.deinit();
    try std.testing.expect(
        centre.diagnostics.maximum_deflection_mm >
            edge.diagnostics.maximum_deflection_mm,
    );
}

test "frame annulus selects a short stiff metallic contact regime" {
    const allocator = std.testing.allocator;
    var string_profile = shortTestProfile(0.08);
    string_profile.hit.relative_normal_speed_mps = 5.0;
    string_profile.hit.racket_speed_mps = 0.0;
    string_profile.hit.x_mm = 0.5 * string_profile.model.head_width_mm - 1.0;
    var frame_profile = string_profile;
    frame_profile.hit.x_mm = 0.5 * frame_profile.model.head_width_mm +
        0.5 * frame_profile.model.frame_radial_width_mm;

    var string_hit = try simulate(allocator, string_profile);
    defer string_hit.deinit();
    var frame_hit = try simulate(allocator, frame_profile);
    defer frame_hit.deinit();

    try std.testing.expectEqual(model.HitRegion.strings, string_hit.hit_region);
    try std.testing.expectEqual(model.HitRegion.frame, frame_hit.hit_region);
    try std.testing.expect(
        frame_hit.diagnostics.contact_duration_ms <
            string_hit.diagnostics.contact_duration_ms,
    );
    try std.testing.expect(
        frame_hit.diagnostics.peak_force_n >
            string_hit.diagnostics.peak_force_n,
    );
    try std.testing.expect(frame_hit.diagnostics.dominant_frequency_hz >= 2000.0);

    const attack_end = @min(
        frame_hit.audio.len,
        frame_hit.impact_audio_frame + frame_hit.output_sample_rate_hz / 50,
    );
    const metallic_energy = bandLimitedEnergy(
        frame_hit.audio,
        frame_hit.output_sample_rate_hz,
        6000.0,
        0.7,
        frame_hit.impact_audio_frame,
        attack_end,
    );
    const low_ring_energy = bandLimitedEnergy(
        frame_hit.audio,
        frame_hit.output_sample_rate_hz,
        900.0,
        1.0,
        frame_hit.impact_audio_frame,
        attack_end,
    );
    try std.testing.expect(metallic_energy > low_ring_energy);
}

test "mirrored frame hits preserve force and magnitude spectrum" {
    const allocator = std.testing.allocator;
    var left_profile = shortTestProfile(0.08);
    left_profile.hit.relative_normal_speed_mps = 5.0;
    left_profile.hit.racket_speed_mps = 0.0;
    left_profile.hit.x_mm = -(0.5 * left_profile.model.head_width_mm +
        0.5 * left_profile.model.frame_radial_width_mm);
    var right_profile = left_profile;
    right_profile.hit.x_mm = -left_profile.hit.x_mm;

    var left = try simulate(allocator, left_profile);
    defer left.deinit();
    var right = try simulate(allocator, right_profile);
    defer right.deinit();

    try std.testing.expectEqual(model.HitRegion.frame, left.hit_region);
    try std.testing.expectEqual(model.HitRegion.frame, right.hit_region);
    try std.testing.expectApproxEqRel(
        left.diagnostics.peak_force_n,
        right.diagnostics.peak_force_n,
        1.0e-10,
    );
    try std.testing.expectApproxEqRel(
        audioEnergy(left.audio),
        audioEnergy(right.audio),
        0.03,
    );
    try std.testing.expectApproxEqRel(
        left.diagnostics.dominant_frequency_hz,
        right.diagnostics.dominant_frequency_hz,
        0.01,
    );
    for (left.spectrum_db, right.spectrum_db) |left_db, right_db| {
        try std.testing.expectApproxEqAbs(left_db, right_db, 0.08);
    }
}

test "frame-relative visual motion decays toward the elliptical boundary" {
    const allocator = std.testing.allocator;
    var profile = shortTestProfile(0.06);
    profile.hit.relative_normal_speed_mps = 5.0;
    var result = try simulate(allocator, profile);
    defer result.deinit();

    const half_width = result.params.head_width_mm * 0.0005;
    const half_height = result.params.head_height_mm * 0.0005;
    var inner_energy: f64 = 0.0;
    var outer_energy: f64 = 0.0;
    var inner_count: usize = 0;
    var outer_count: usize = 0;
    const first_frame: usize = @intFromFloat(@round(
        0.012 * @as(f64, @floatFromInt(result.visual_frame_rate_hz)),
    ));
    for (first_frame..result.visual_frame_count) |frame_index| {
        const frame = result.visualFrame(frame_index);
        for (result.node_positions, frame) |position, displacement| {
            const radius_squared =
                square(position.x / half_width) +
                square(position.y / half_height);
            if (radius_squared <= 0.25) {
                inner_energy += square(displacement);
                inner_count += 1;
            } else if (radius_squared >= 0.64) {
                outer_energy += square(displacement);
                outer_count += 1;
            }
        }
    }
    const inner_rms = @sqrt(inner_energy / @as(f64, @floatFromInt(inner_count)));
    const outer_rms = @sqrt(outer_energy / @as(f64, @floatFromInt(outer_count)));
    try std.testing.expect(inner_rms > outer_rms * 1.5);
    var anchored_endpoint_count: usize = 0;
    for (result.segments) |segment| {
        if (segment.a_node < 0) {
            try std.testing.expectEqual(@as(i32, -1), segment.a_node);
            anchored_endpoint_count += 1;
        }
        if (segment.b_node < 0) {
            try std.testing.expectEqual(@as(i32, -1), segment.b_node);
            anchored_endpoint_count += 1;
        }
    }
    try std.testing.expectEqual(
        @as(usize, result.params.main_count + result.params.cross_count) * 2,
        anchored_endpoint_count,
    );
}

test "higher collision speed raises physical and mastered energy without hit normalization" {
    const allocator = std.testing.allocator;
    var slow_profile = shortTestProfile(0.08);
    slow_profile.hit.relative_normal_speed_mps = 15.0;
    var fast_profile = slow_profile;
    fast_profile.hit.relative_normal_speed_mps = 60.0;

    var slow = try simulate(allocator, slow_profile);
    defer slow.deinit();
    var fast = try simulate(allocator, fast_profile);
    defer fast.deinit();
    try std.testing.expect(audioEnergy(fast.audio) > audioEnergy(slow.audio));
    try std.testing.expect(
        fast.diagnostics.peak_before_dynamics >
            slow.diagnostics.peak_before_dynamics * 2.0,
    );
    try std.testing.expect(
        fast.diagnostics.maximum_gain_reduction_db >
            slow.diagnostics.maximum_gain_reduction_db,
    );
    try std.testing.expect(
        fast.diagnostics.contact_duration_ms <
            slow.diagnostics.contact_duration_ms,
    );
}

test "hard impacts trade the touch ping for a broadband attack" {
    const allocator = std.testing.allocator;
    var touch_profile = shortTestProfile(0.08);
    touch_profile.hit.main_tension_lbf = 30.0;
    touch_profile.hit.cross_tension_lbf = 30.0;
    touch_profile.hit.relative_normal_speed_mps = 5.0;
    touch_profile.hit.racket_speed_mps = 0.0;
    var hard_profile = touch_profile;
    hard_profile.hit.relative_normal_speed_mps = 30.0;

    var touch = try simulate(allocator, touch_profile);
    defer touch.deinit();
    var hard = try simulate(allocator, hard_profile);
    defer hard.deinit();
    const window: usize = touch.output_sample_rate_hz / 50;
    const touch_note = bandLimitedEnergy(
        touch.audio,
        touch.output_sample_rate_hz,
        1288.0,
        2.5,
        touch.impact_audio_frame,
        touch.impact_audio_frame + window,
    );
    const touch_crack = bandLimitedEnergy(
        touch.audio,
        touch.output_sample_rate_hz,
        5000.0,
        1.0,
        touch.impact_audio_frame,
        touch.impact_audio_frame + window,
    );
    const hard_note = bandLimitedEnergy(
        hard.audio,
        hard.output_sample_rate_hz,
        1288.0,
        2.5,
        hard.impact_audio_frame,
        hard.impact_audio_frame + window,
    );
    const hard_crack = bandLimitedEnergy(
        hard.audio,
        hard.output_sample_rate_hz,
        5000.0,
        1.0,
        hard.impact_audio_frame,
        hard.impact_audio_frame + window,
    );
    try std.testing.expect(
        hard_note / @max(1.0e-20, hard_crack) <
            touch_note / @max(1.0e-20, touch_crack) * 0.75,
    );
}

test "low-speed default attack has broadband upper-frequency texture" {
    const allocator = std.testing.allocator;
    var profile = shortTestProfile(0.08);
    profile.hit.main_tension_lbf = 30.0;
    profile.hit.cross_tension_lbf = 30.0;
    profile.hit.relative_normal_speed_mps = 5.0;
    var result = try simulate(allocator, profile);
    defer result.deinit();

    const attack_frames = @min(
        result.audio.len,
        result.output_sample_rate_hz / 50,
    );
    const attack_end = @min(
        result.audio.len,
        result.impact_audio_frame + attack_frames,
    );
    const flatness = try spectralFlatness(
        allocator,
        result.audio[result.impact_audio_frame..attack_end],
        result.output_sample_rate_hz,
        2000.0,
        12_000.0,
    );

    var structural_profile = profile;
    structural_profile.model.contact_noise_gain = 0.0;
    var structural = try simulate(allocator, structural_profile);
    defer structural.deinit();
    const structural_flatness = try spectralFlatness(
        allocator,
        structural.audio[structural.impact_audio_frame..@min(structural.audio.len, structural.impact_audio_frame + attack_frames)],
        structural.output_sample_rate_hz,
        2000.0,
        12_000.0,
    );
    try std.testing.expect(flatness > structural_flatness * 1.5);
    try std.testing.expect(flatness < 0.95);
}

test "upper radiation modes sustain the post-contact crispness band" {
    const allocator = std.testing.allocator;
    var modal_profile = shortTestProfile(0.08);
    modal_profile.hit.main_tension_lbf = 30.0;
    modal_profile.hit.cross_tension_lbf = 30.0;
    modal_profile.hit.relative_normal_speed_mps = 5.0;
    modal_profile.model.contact_noise_gain = 0.0;
    var modal = try simulate(allocator, modal_profile);
    defer modal.deinit();

    var resolved_profile = modal_profile;
    resolved_profile.model.upper_mode_gain = 0.0;
    var resolved = try simulate(allocator, resolved_profile);
    defer resolved.deinit();

    const first_frame: usize = modal.impact_audio_frame + @as(usize, @intCast(
        modal.output_sample_rate_hz * 8 / 1000,
    ));
    const last_frame: usize = modal.impact_audio_frame + @as(usize, @intCast(
        modal.output_sample_rate_hz * 32 / 1000,
    ));
    const modal_energy = bandLimitedEnergy(
        modal.audio,
        modal.output_sample_rate_hz,
        4500.0,
        0.75,
        first_frame,
        last_frame,
    );
    const resolved_energy = bandLimitedEnergy(
        resolved.audio,
        resolved.output_sample_rate_hz,
        4500.0,
        0.75,
        first_frame,
        last_frame,
    );
    try std.testing.expect(modal_energy > resolved_energy * 1.2);
}

test "racket swoosh is continuous velocity-driven and independent of contact" {
    const allocator = std.testing.allocator;
    var silent_profile = shortTestProfile(0.12);
    silent_profile.hit.relative_normal_speed_mps = 0.0;
    silent_profile.hit.racket_speed_mps =
        silent_profile.model.swoosh_threshold_mps;
    var silent = try simulate(allocator, silent_profile);
    defer silent.deinit();
    for (silent.audio) |sample| {
        try std.testing.expectEqual(@as(f32, 0.0), sample);
    }

    var moderate_profile = silent_profile;
    moderate_profile.hit.racket_speed_mps = 20.0;
    var fast_profile = silent_profile;
    fast_profile.hit.racket_speed_mps = 40.0;
    var moderate = try simulate(allocator, moderate_profile);
    defer moderate.deinit();
    var fast = try simulate(allocator, fast_profile);
    defer fast.deinit();
    try std.testing.expect(
        audioEnergy(fast.audio) > audioEnergy(moderate.audio) * 10.0,
    );
    for (fast.impact_only_audio) |sample| {
        try std.testing.expectEqual(@as(f32, 0.0), sample);
    }

    const ten_ms: usize = fast.output_sample_rate_hz / 100;
    const early_energy = audioEnergy(fast.audio[2 * ten_ms .. 3 * ten_ms]);
    const late_energy = audioEnergy(
        fast.audio[fast.audio.len - 3 * ten_ms .. fast.audio.len - 2 * ten_ms],
    );
    try std.testing.expect(early_energy > 0.0);
    try std.testing.expect(late_energy > early_energy * 0.25);
    try std.testing.expect(late_energy < early_energy * 4.0);
}

test "reference calibration hits contact, spectrum, gain, and decay windows" {
    const allocator = std.testing.allocator;
    var profile = model.Profile{};
    profile.hit.main_tension_lbf = 23.0;
    profile.hit.cross_tension_lbf = 23.0;
    profile.model.duration_s = 0.20;
    profile.model.visualization_duration_ms = 1.0;
    var result = try simulate(allocator, profile);
    defer result.deinit();

    try std.testing.expect(result.diagnostics.contact_duration_ms >= 1.8);
    try std.testing.expect(result.diagnostics.contact_duration_ms <= 2.7);
    try std.testing.expect(result.diagnostics.dominant_frequency_hz >= 972.0);
    try std.testing.expect(result.diagnostics.dominant_frequency_hz <= 1188.0);
    try std.testing.expect(result.diagnostics.peak_before_clamp >= 0.23);
    try std.testing.expect(result.diagnostics.peak_before_clamp <= 0.31);
    try std.testing.expectEqual(@as(usize, 0), result.diagnostics.clipped_samples);
    const expected_frames: usize = @intFromFloat(@ceil(
        profile.model.duration_s *
            @as(f64, @floatFromInt(profile.model.output_sample_rate_hz)),
    ));
    try std.testing.expectEqual(expected_frames, result.audio.len);

    const window: usize = result.output_sample_rate_hz / 50;
    const early_end = @min(
        result.audio.len,
        result.impact_audio_frame + window,
    );
    const early = audioEnergy(
        result.audio[result.impact_audio_frame..early_end],
    );
    const tail = audioEnergy(result.audio[result.audio.len - window ..]);
    // A 40 ms upper-mode amplitude decay remains safely inaudible long before
    // the three-second replay, without forcing the 200 ms test render to end
    // at numerical silence.
    try std.testing.expect(tail < early * 1.0e-2);
}

test "undamped contact-free string bed conserves mechanical energy" {
    const allocator = std.testing.allocator;
    var profile = model.Profile{};
    profile.model.string_damping_rate_s = 0.0;
    profile.model.string_nonlinear_damping_rate_s = 0.0;
    profile.model.crossing_friction = 0.0;
    profile.model.grommet_loss_factor = 0.0;
    for (&profile.model.frame_modes) |*mode| mode.force_coupling = 0.0;
    var topology = try buildTopology(allocator, profile);
    defer topology.deinit();
    const count = topology.node_positions.len;

    const node_z = try allocator.alloc(f64, count);
    defer allocator.free(node_z);
    const node_v = try allocator.alloc(f64, count);
    defer allocator.free(node_v);
    const node_a = try allocator.alloc(f64, count);
    defer allocator.free(node_a);
    const node_force = try allocator.alloc(f64, count);
    defer allocator.free(node_force);
    const anchor_force = try allocator.alloc(f64, topology.anchors.len);
    defer allocator.free(anchor_force);
    const contact_weights = try allocator.alloc(f64, count);
    defer allocator.free(contact_weights);
    @memset(node_z, 0.0);
    @memset(node_v, 0.0);
    @memset(node_a, 0.0);
    @memset(contact_weights, 0.0);
    const centre = findNearestNode(topology.node_positions, .{ .x = 0.0, .y = 0.0 });
    node_z[centre] = 0.0002;

    var state = DynamicState{
        .node_z = node_z,
        .node_v = node_v,
        .node_a = node_a,
        .shuttle_v = 0.0,
    };
    var workspace = Workspace{
        .node_force = node_force,
        .anchor_force = anchor_force,
        .contact_weights = contact_weights,
    };
    var contact = ContactState{ .region = .strings, .enabled = false };
    _ = evaluateAccelerations(
        &topology,
        profile,
        &state,
        &workspace,
        &contact,
        0.0,
    );
    const initial_energy = mechanicalEnergy(&topology, profile, &state);
    const dt = 0.05 / topology.stability_omega_max;
    for (0..5000) |step| {
        for (state.node_z, state.node_v, state.node_a) |*position, *velocity, acceleration| {
            velocity.* += 0.5 * acceleration * dt;
            position.* += velocity.* * dt;
        }
        _ = evaluateAccelerations(
            &topology,
            profile,
            &state,
            &workspace,
            &contact,
            @as(f64, @floatFromInt(step + 1)) * dt,
        );
        for (state.node_v, state.node_a) |*velocity, acceleration| {
            velocity.* += 0.5 * acceleration * dt;
        }
    }
    const final_energy = mechanicalEnergy(&topology, profile, &state);
    try std.testing.expectApproxEqRel(initial_energy, final_energy, 0.005);
}

fn shortTestProfile(duration_s: f64) model.Profile {
    var profile = model.Profile{};
    profile.model.internal_sample_rate_hz = 96_000;
    profile.model.output_sample_rate_hz = 48_000;
    profile.model.visualization_rate_hz = 8_000;
    profile.model.duration_s = duration_s;
    profile.model.impact_pre_roll_ms = @min(12.0, duration_s * 250.0);
    profile.model.visualization_duration_ms = @min(25.0, duration_s * 1000.0);
    return profile;
}

fn findNodeAt(positions: []const Vec2, x: f64, y: f64) ?usize {
    for (positions, 0..) |position, index| {
        if (@abs(position.x - x) < 1.0e-12 and
            @abs(position.y - y) < 1.0e-12) return index;
    }
    return null;
}

fn findNearestNode(positions: []const Vec2, target: Vec2) usize {
    var nearest: usize = 0;
    var nearest_distance = std.math.inf(f64);
    for (positions, 0..) |position, index| {
        const candidate = square(position.x - target.x) +
            square(position.y - target.y);
        if (candidate < nearest_distance) {
            nearest_distance = candidate;
            nearest = index;
        }
    }
    return nearest;
}

fn audioEnergy(samples: []const f32) f64 {
    var energy: f64 = 0.0;
    for (samples) |sample| energy += @as(f64, sample) * @as(f64, sample);
    return energy;
}

fn bandLimitedEnergy(
    samples: []const f32,
    sample_rate: u32,
    centre_hz: f64,
    quality_factor: f64,
    first_frame: usize,
    last_frame: usize,
) f64 {
    var filter = dsp.Biquad.bandPass(
        @floatFromInt(sample_rate),
        centre_hz,
        quality_factor,
    );
    var energy: f64 = 0.0;
    for (samples, 0..) |sample, frame| {
        const filtered = filter.process(sample);
        if (frame >= first_frame and frame < @min(last_frame, samples.len)) {
            energy += filtered * filtered;
        }
    }
    return energy;
}

fn spectralFlatness(
    allocator: std.mem.Allocator,
    samples: []const f32,
    sample_rate: u32,
    minimum_hz: f64,
    maximum_hz: f64,
) !f64 {
    const fft_size: usize = 1024;
    const spectrum = try dsp.magnitudeSpectrumDb(allocator, samples, fft_size);
    defer allocator.free(spectrum);
    const bin_hz = @as(f64, @floatFromInt(sample_rate)) /
        @as(f64, @floatFromInt(fft_size));
    const first: usize = @intFromFloat(@ceil(minimum_hz / bin_hz));
    const last: usize = @min(
        spectrum.len - 1,
        @as(usize, @intFromFloat(@floor(maximum_hz / bin_hz))),
    );
    if (first > last) return 0.0;

    var sum_power: f64 = 0.0;
    var sum_log_power: f64 = 0.0;
    for (spectrum[first .. last + 1]) |level_db| {
        const power = @max(
            1.0e-24,
            std.math.pow(f64, 10.0, @as(f64, level_db) / 10.0),
        );
        sum_power += power;
        sum_log_power += @log(power);
    }
    const count = @as(f64, @floatFromInt(last - first + 1));
    return @exp(sum_log_power / count) / (sum_power / count);
}

fn mechanicalEnergy(
    topology: *const Topology,
    profile: model.Profile,
    state: *const DynamicState,
) f64 {
    var energy: f64 = 0.0;
    for (topology.node_masses, state.node_v) |mass, velocity| {
        energy += 0.5 * mass * velocity * velocity;
    }
    const params = profile.model;
    const diameter = params.string_diameter_mm * 0.001;
    const area = std.math.pi * square(0.5 * diameter);
    const axial_rigidity = params.string_young_modulus_gpa * 1.0e9 * area;
    for (topology.lines) |line| {
        var extension: f64 = 0.0;
        for (0..line.node_count + 1) |segment_index| {
            const left = dynamicLinePoint(
                topology,
                line,
                segment_index,
                state,
                params,
            );
            const right = dynamicLinePoint(
                topology,
                line,
                segment_index + 1,
                state,
                params,
            );
            const rest_length = distance(left.position, right.position);
            const dz = right.z - left.z;
            extension += @sqrt(rest_length * rest_length + dz * dz) - rest_length;
        }
        energy += params.transverse_stiffness_scale *
            (line.nominal_tension_n * extension +
                0.5 * axial_rigidity * extension * extension / line.rest_length_m);
    }
    return energy;
}
