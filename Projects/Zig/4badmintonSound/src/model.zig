const std = @import("std");

pub const pounds_force_to_newtons = 4.448_221_615_260_5;
pub const bg66_diameter_mm = 0.66;
pub const reference_string_density_kg_m3 = 1100.0;
pub const bwf_max_racket_length_mm = 680.0;
pub const bwf_max_racket_width_mm = 230.0;
pub const bwf_max_stringed_length_mm = 280.0;
pub const bwf_max_stringed_width_mm = 220.0;

pub const HitInput = struct {
    main_tension_lbf: f64 = 27.0,
    cross_tension_lbf: f64 = 28.0,
    // Relative cork/string-bed closing speed controls contact mechanics.
    relative_normal_speed_mps: f64 = 5.0,
    // Racket-head speed through the air is independent: it continuously
    // controls the aerodynamic sound and may differ from closing speed.
    racket_speed_mps: f64 = 5.0,
    x_mm: f64 = 0.0,
    y_mm: f64 = 0.0,
};

pub const FrameMode = struct {
    frequency_hz: f64,
    quality_factor: f64,
    force_coupling: f64,
    radiation_area_m2: f64,
};

pub const upper_mode_count = 14;
pub const frame_hit_mode_count = 8;

pub const HitRegion = enum {
    strings,
    frame,
};

pub const UpperRadiationMode = struct {
    frequency_hz_at_30lb: f64,
    relative_gain: f64,
    decay_scale: f64 = 1.0,
};

pub const FrameHitRadiationMode = struct {
    frequency_hz: f64,
    relative_gain: f64,
    decay_ms: f64,
    circumferential_order: u32,
    phase_turns: f64 = 0.0,
};

pub const ModelParams = struct {
    // Elliptical string-bed boundary and the surrounding frame annulus. BWF
    // limits the stringed area to 280 x 220 mm and the whole racket width to
    // 230 mm; it does not mandate one universal oval head size.
    head_width_mm: f64 = 188.0,
    head_height_mm: f64 = 253.0,
    frame_radial_width_mm: f64 = 9.0,
    main_count: u32 = 22,
    cross_count: u32 = 21,

    // Original Yonex BG66 reference construction: 0.66 mm high-polymer nylon
    // multifilament with a braided high-polymer nylon outer. Yonex does not
    // publish product-specific density/modulus, so the measured/modelled
    // high-strength nylon values below remain the defensible defaults.
    string_diameter_mm: f64 = bg66_diameter_mm,
    string_density_kg_m3: f64 = reference_string_density_kg_m3,
    string_young_modulus_gpa: f64 = 7.2,
    // Small-amplitude loss preserves the familiar low-speed ping. Real
    // string-bed free decay is nonlinear, so an additional velocity-dependent
    // term removes energy much faster during clears and smashes.
    string_damping_rate_s: f64 = 30.0,
    string_nonlinear_damping_rate_s: f64 = 600.0,
    string_nonlinear_damping_onset_mps: f64 = 2.5,
    string_nonlinear_damping_transition_mps: f64 = 8.0,
    crossing_friction: f64 = 0.10,
    // Fraction of each string's transverse characteristic impedance used by a
    // terminal dashpot. Zero is a perfectly reflecting clamp; one approaches
    // an anechoic termination. Real grommets are much closer to zero.
    grommet_loss_factor: f64 = 0.04,
    // Effective transverse compliance left unresolved by point-like tied nodes
    // (crossing slip, grommets and local coating deformation).
    transverse_stiffness_scale: f64 = 0.7387,
    maximum_dynamic_strain: f64 = 0.060,

    // Shuttle and local cork/string contact.
    shuttle_mass_g: f64 = 5.0,
    cork_diameter_mm: f64 = 26.5,
    // Effective cork/local-bed stiffness. This is deliberately below a Hertz
    // estimate made from bulk cork modulus because the real layered cork,
    // coating, grommets and finite footprint add compliance. It is calibrated
    // to about 2 ms at 5 m/s and about 1.4 ms at smash-level speed.
    contact_stiffness_n_m_pow: f64 = 380_000.0,
    contact_exponent: f64 = 1.5,
    target_restitution: f64 = 0.55,
    contact_damping_calibration: f64 = 1.0,

    // Cork striking the carbon-composite hoop is much stiffer and shorter
    // than cork loading the string bed. These remain effective contact values
    // because the painted laminate, grommet strip and cork cap are layered.
    frame_contact_stiffness_n_m_pow: f64 = 28_000_000.0,
    frame_contact_exponent: f64 = 1.5,
    frame_contact_restitution: f64 = 0.35,
    frame_contact_damping_calibration: f64 = 1.0,

    // A reduced, hand-held carbon-frame model. Mode shapes are fixed;
    // these frequencies, Q values, couplings and radiation areas are editable.
    frame_mass_g: f64 = 35.0,
    // The shaft, handle and hand break ideal top/bottom hoop symmetry, so the
    // corresponding odd mode retains a small direct radiation term even for a
    // microphone on the centre normal.
    frame_radiation_asymmetry: f64 = 0.12,
    frame_modes: [4]FrameMode = .{
        .{ .frequency_hz = 170.0, .quality_factor = 12.0, .force_coupling = 1.0, .radiation_area_m2 = 0.0100 },
        .{ .frequency_hz = 380.0, .quality_factor = 15.0, .force_coupling = 0.85, .radiation_area_m2 = 0.0065 },
        .{ .frequency_hz = 700.0, .quality_factor = 20.0, .force_coupling = 0.70, .radiation_area_m2 = 0.0040 },
        .{ .frequency_hz = 1300.0, .quality_factor = 25.0, .force_coupling = 0.55, .radiation_area_m2 = 0.0025 },
    },
    // Local hoop/grommet modes used only for a geometrically classified frame
    // hit. CFRP is stiffer and more damped than a metal hoop, so these partials
    // are deliberately inharmonic and short even though they sound metallic.
    frame_hit_mode_gain: f64 = 3.0,
    frame_hit_modes: [frame_hit_mode_count]FrameHitRadiationMode = .{
        .{ .frequency_hz = 2350.0, .relative_gain = 0.24, .decay_ms = 15.0, .circumferential_order = 0 },
        .{ .frequency_hz = 3370.0, .relative_gain = 0.48, .decay_ms = 19.0, .circumferential_order = 2, .phase_turns = 0.08 },
        .{ .frequency_hz = 4680.0, .relative_gain = 1.00, .decay_ms = 22.0, .circumferential_order = 3, .phase_turns = 0.17 },
        .{ .frequency_hz = 6120.0, .relative_gain = 0.78, .decay_ms = 17.0, .circumferential_order = 4, .phase_turns = 0.04 },
        .{ .frequency_hz = 7930.0, .relative_gain = 0.60, .decay_ms = 13.0, .circumferential_order = 5, .phase_turns = 0.23 },
        .{ .frequency_hz = 10_150.0, .relative_gain = 0.42, .decay_ms = 10.0, .circumferential_order = 6, .phase_turns = 0.12 },
        .{ .frequency_hz = 12_850.0, .relative_gain = 0.25, .decay_ms = 7.0, .circumferential_order = 7, .phase_turns = 0.31 },
        .{ .frequency_hz = 16_100.0, .relative_gain = 0.12, .decay_ms = 4.5, .circumferential_order = 8, .phase_turns = 0.19 },
    },
    frame_hit_radiation_exponent: f64 = 1.15,
    frame_hit_transient_gain: f64 = 0.020,
    frame_hit_slew_time_ms: f64 = 0.08,
    frame_hit_transient_decay_ms: f64 = 1.8,
    frame_hit_highpass_hz: f64 = 3_000.0,
    frame_hit_lowpass_hz: f64 = 18_000.0,
    // Reduced upper string-bed/frame radiation modes. Frequencies and relative
    // levels are the stable peaks shared by the eight 30 lb real-shuttle
    // impacts; their frequencies scale from tension and linear density.
    upper_mode_gain: f64 = 8.0,
    upper_mode_decay_ms: f64 = 45.0,
    // High-energy impacts redistribute energy into short-lived local modes.
    // This is the remaining decay-time fraction at/above the hard-impact
    // reference speed; touch hits retain the full calibrated decay.
    upper_mode_fast_decay_scale: f64 = 0.35,
    upper_mode_contact_roughness: f64 = 0.04,
    // A vibrating mode radiates through acceleration, so upper modes need
    // more acoustic weight than a displacement-only filter would give them.
    upper_mode_radiation_exponent: f64 = 1.5,
    upper_modes: [upper_mode_count]UpperRadiationMode = .{
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
    },

    // Dry acoustic observation and calibrated unresolved-contact radiation.
    air_density_kg_m3: f64 = 1.204,
    sound_speed_mps: f64 = 343.0,
    microphone_distance_m: f64 = 1.0,
    microphone_x_m: f64 = 0.0,
    microphone_y_m: f64 = 0.0,
    string_radiation_efficiency: f64 = 0.85,
    frame_radiation_efficiency: f64 = 0.28,
    // Unresolved cork/coating micro-contacts: deterministic broadband texture
    // under the physical contact-force envelope.
    contact_noise_gain: f64 = 0.0220,
    // Contact force already contains most velocity dependence. This small
    // empirical exponent adds bite with speed without the former 0.70 curve,
    // which over-brightened smash-level impacts by more than five times.
    contact_noise_speed_exponent: f64 = 0.40,
    contact_noise_decay_ms: f64 = 18.0,
    contact_noise_burst_probability: f64 = 0.018,
    contact_noise_burst_gain: f64 = 2.2,
    contact_noise_highpass_hz: f64 = 900.0,
    contact_noise_lowpass_hz: f64 = 8_500.0,

    // A separate short, broadband hard-impact layer is driven by dF/dt. It is
    // absent for touch shots and progressively masks the narrow low-speed
    // string-bed ping during clears and smashes.
    hard_impact_threshold_mps: f64 = 6.5,
    hard_impact_reference_speed_mps: f64 = 20.0,
    hard_impact_gain: f64 = 0.280,
    hard_impact_slew_time_ms: f64 = 0.20,
    hard_impact_decay_ms: f64 = 4.0,
    hard_impact_highpass_hz: f64 = 2_200.0,
    hard_impact_lowpass_hz: f64 = 14_000.0,

    // Broadside racket aeroacoustics. A screen/cylinder dipole has acoustic
    // pressure proportional to roughly U^3 (power proportional to U^6), while
    // its characteristic frequency follows f = St U / d. The threshold is an
    // explicit audibility gate: at and below it the synthesized swoosh is
    // mathematically zero.
    swoosh_threshold_mps: f64 = 7.0,
    swoosh_reference_speed_mps: f64 = 40.0,
    swoosh_speed_exponent: f64 = 3.0,
    swoosh_gain: f64 = 1.20,
    swoosh_strouhal_number: f64 = 0.20,
    swoosh_frame_diameter_mm: f64 = 10.0,
    // Version-1 JSON compatibility keeps the old field name; this now controls
    // how quickly the continuous wind source follows a velocity change.
    swoosh_post_impact_decay_ms: f64 = 18.0,

    // Fixed playback calibration and dynamics. This is one global transfer
    // curve, never per-hit normalization: touch hits receive the same gain as
    // smashes, while the compressor prevents the physical dynamic range from
    // clipping consumer playback.
    master_gain: f64 = 0.15,
    compressor_threshold_dbfs: f64 = -8.0,
    compressor_ratio: f64 = 4.0,
    compressor_knee_db: f64 = 6.0,
    compressor_lookahead_ms: f64 = 1.0,
    compressor_release_ms: f64 = 40.0,
    limiter_ceiling_dbfs: f64 = -1.0,

    // Numerical and application settings.
    internal_sample_rate_hz: u32 = 192_000,
    output_sample_rate_hz: u32 = 48_000,
    duration_s: f64 = 0.50,
    impact_pre_roll_ms: f64 = 60.0,
    visualization_duration_ms: f64 = 25.0,
    visualization_rate_hz: u32 = 8_000,
    replay_interval_s: f64 = 3.0,
};

pub const Profile = struct {
    version: u32 = 1,
    hit: HitInput = .{},
    model: ModelParams = .{},
};

pub const ValidationError = error{
    UnsupportedProfileVersion,
    InvalidGeometry,
    InvalidStringCount,
    InvalidMaterial,
    InvalidContact,
    InvalidFrame,
    InvalidAcoustics,
    InvalidNumerics,
    InvalidHit,
    ImpactOutsideRacket,
};

pub fn validate(profile: Profile) ValidationError!void {
    const p = profile.model;
    const h = profile.hit;

    if (profile.version != 1) return error.UnsupportedProfileVersion;
    if (!positiveFinite(p.head_width_mm) or
        !positiveFinite(p.head_height_mm) or
        !positiveFinite(p.frame_radial_width_mm) or
        p.head_width_mm > bwf_max_stringed_width_mm or
        p.head_height_mm > bwf_max_stringed_length_mm or
        frameOuterWidthMm(p) > bwf_max_racket_width_mm)
    {
        return error.InvalidGeometry;
    }
    if (p.main_count < 4 or p.main_count > 32 or p.cross_count < 4 or p.cross_count > 32) {
        return error.InvalidStringCount;
    }
    if (!positiveFinite(p.string_diameter_mm) or
        !positiveFinite(p.string_density_kg_m3) or
        !positiveFinite(p.string_young_modulus_gpa) or
        !nonnegativeFinite(p.string_damping_rate_s) or
        !nonnegativeFinite(p.string_nonlinear_damping_rate_s) or
        !nonnegativeFinite(p.string_nonlinear_damping_onset_mps) or
        !positiveFinite(p.string_nonlinear_damping_transition_mps) or
        !nonnegativeFinite(p.crossing_friction) or
        !nonnegativeFinite(p.grommet_loss_factor) or
        p.grommet_loss_factor > 2.0 or
        !positiveFinite(p.transverse_stiffness_scale) or
        !positiveFinite(p.maximum_dynamic_strain))
    {
        return error.InvalidMaterial;
    }
    if (!positiveFinite(p.shuttle_mass_g) or
        !positiveFinite(p.cork_diameter_mm) or
        !positiveFinite(p.contact_stiffness_n_m_pow) or
        p.contact_exponent < 1.0 or p.contact_exponent > 3.0 or
        p.target_restitution <= 0.0 or p.target_restitution > 1.0 or
        !positiveFinite(p.contact_damping_calibration) or
        !positiveFinite(p.frame_contact_stiffness_n_m_pow) or
        p.frame_contact_exponent < 1.0 or p.frame_contact_exponent > 3.0 or
        p.frame_contact_restitution <= 0.0 or p.frame_contact_restitution > 1.0 or
        !positiveFinite(p.frame_contact_damping_calibration))
    {
        return error.InvalidContact;
    }
    if (!positiveFinite(p.frame_mass_g) or
        !nonnegativeFinite(p.frame_radiation_asymmetry) or
        p.frame_radiation_asymmetry > 1.0)
    {
        return error.InvalidFrame;
    }
    for (p.frame_modes) |mode| {
        if (!positiveFinite(mode.frequency_hz) or
            !positiveFinite(mode.quality_factor) or
            !nonnegativeFinite(mode.force_coupling) or
            !nonnegativeFinite(mode.radiation_area_m2))
        {
            return error.InvalidFrame;
        }
    }
    if (!nonnegativeFinite(p.frame_hit_mode_gain) or
        !nonnegativeFinite(p.frame_hit_radiation_exponent) or
        p.frame_hit_radiation_exponent > 3.0 or
        !nonnegativeFinite(p.frame_hit_transient_gain) or
        !positiveFinite(p.frame_hit_slew_time_ms) or
        !positiveFinite(p.frame_hit_transient_decay_ms) or
        !positiveFinite(p.frame_hit_highpass_hz) or
        !positiveFinite(p.frame_hit_lowpass_hz) or
        p.frame_hit_highpass_hz >= p.frame_hit_lowpass_hz)
    {
        return error.InvalidFrame;
    }
    for (p.frame_hit_modes) |mode| {
        if (!positiveFinite(mode.frequency_hz) or
            !nonnegativeFinite(mode.relative_gain) or
            !positiveFinite(mode.decay_ms) or
            mode.circumferential_order > 32 or
            !std.math.isFinite(mode.phase_turns))
        {
            return error.InvalidFrame;
        }
    }
    if (!nonnegativeFinite(p.upper_mode_gain) or
        !positiveFinite(p.upper_mode_decay_ms) or
        !positiveFinite(p.upper_mode_fast_decay_scale) or
        p.upper_mode_fast_decay_scale > 1.0 or
        !nonnegativeFinite(p.upper_mode_contact_roughness) or
        !nonnegativeFinite(p.upper_mode_radiation_exponent) or
        p.upper_mode_radiation_exponent > 3.0)
    {
        return error.InvalidFrame;
    }
    for (p.upper_modes) |mode| {
        if (!positiveFinite(mode.frequency_hz_at_30lb) or
            !nonnegativeFinite(mode.relative_gain) or
            !positiveFinite(mode.decay_scale) or
            mode.decay_scale > 3.0)
        {
            return error.InvalidFrame;
        }
    }
    if (!positiveFinite(p.air_density_kg_m3) or
        !positiveFinite(p.sound_speed_mps) or
        !positiveFinite(p.microphone_distance_m) or
        !std.math.isFinite(p.microphone_x_m) or
        !std.math.isFinite(p.microphone_y_m) or
        !nonnegativeFinite(p.string_radiation_efficiency) or
        !nonnegativeFinite(p.frame_radiation_efficiency) or
        !nonnegativeFinite(p.contact_noise_gain) or
        !nonnegativeFinite(p.contact_noise_speed_exponent) or
        !nonnegativeFinite(p.contact_noise_decay_ms) or
        !nonnegativeFinite(p.contact_noise_burst_probability) or
        p.contact_noise_burst_probability > 0.25 or
        !nonnegativeFinite(p.contact_noise_burst_gain) or
        !positiveFinite(p.contact_noise_highpass_hz) or
        !positiveFinite(p.contact_noise_lowpass_hz) or
        p.contact_noise_highpass_hz >= p.contact_noise_lowpass_hz or
        !nonnegativeFinite(p.hard_impact_threshold_mps) or
        !positiveFinite(p.hard_impact_reference_speed_mps) or
        p.hard_impact_reference_speed_mps <= p.hard_impact_threshold_mps or
        !nonnegativeFinite(p.hard_impact_gain) or
        !positiveFinite(p.hard_impact_slew_time_ms) or
        !positiveFinite(p.hard_impact_decay_ms) or
        !positiveFinite(p.hard_impact_highpass_hz) or
        !positiveFinite(p.hard_impact_lowpass_hz) or
        p.hard_impact_highpass_hz >= p.hard_impact_lowpass_hz or
        !nonnegativeFinite(p.swoosh_threshold_mps) or
        !positiveFinite(p.swoosh_reference_speed_mps) or
        p.swoosh_reference_speed_mps <= p.swoosh_threshold_mps or
        !positiveFinite(p.swoosh_speed_exponent) or
        !nonnegativeFinite(p.swoosh_gain) or
        !positiveFinite(p.swoosh_strouhal_number) or
        !positiveFinite(p.swoosh_frame_diameter_mm) or
        !positiveFinite(p.swoosh_post_impact_decay_ms) or
        !nonnegativeFinite(p.master_gain) or
        !std.math.isFinite(p.compressor_threshold_dbfs) or
        p.compressor_threshold_dbfs > 0.0 or
        p.compressor_threshold_dbfs < -80.0 or
        !positiveFinite(p.compressor_ratio) or
        p.compressor_ratio < 1.0 or
        !nonnegativeFinite(p.compressor_knee_db) or
        !nonnegativeFinite(p.compressor_lookahead_ms) or
        !positiveFinite(p.compressor_release_ms) or
        !std.math.isFinite(p.limiter_ceiling_dbfs) or
        p.limiter_ceiling_dbfs > 0.0 or
        p.limiter_ceiling_dbfs < -24.0)
    {
        return error.InvalidAcoustics;
    }
    if (p.internal_sample_rate_hz < 48_000 or
        p.output_sample_rate_hz < 8_000 or
        p.internal_sample_rate_hz % p.output_sample_rate_hz != 0 or
        p.duration_s < 0.05 or p.duration_s > 2.0 or
        !nonnegativeFinite(p.impact_pre_roll_ms) or
        p.impact_pre_roll_ms + 10.0 >= p.duration_s * 1000.0 or
        p.visualization_duration_ms <= 0.0 or
        p.visualization_rate_hz == 0 or
        p.internal_sample_rate_hz % p.visualization_rate_hz != 0 or
        p.replay_interval_s < 0.25 or p.replay_interval_s > 30.0)
    {
        return error.InvalidNumerics;
    }
    if (!positiveFinite(h.main_tension_lbf) or
        !positiveFinite(h.cross_tension_lbf) or
        !nonnegativeFinite(h.relative_normal_speed_mps) or
        !nonnegativeFinite(h.racket_speed_mps) or
        !std.math.isFinite(h.x_mm) or
        !std.math.isFinite(h.y_mm))
    {
        return error.InvalidHit;
    }

    if (hitRegion(profile) == null) return error.ImpactOutsideRacket;
}

pub fn frameOuterWidthMm(params: ModelParams) f64 {
    return params.head_width_mm + 2.0 * params.frame_radial_width_mm;
}

pub fn frameOuterHeightMm(params: ModelParams) f64 {
    return params.head_height_mm + 2.0 * params.frame_radial_width_mm;
}

pub fn hitRegion(profile: Profile) ?HitRegion {
    const p = profile.model;
    const h = profile.hit;
    const string_nx = h.x_mm / (0.5 * p.head_width_mm);
    const string_ny = h.y_mm / (0.5 * p.head_height_mm);
    if (string_nx * string_nx + string_ny * string_ny < 1.0) {
        return .strings;
    }
    const outer_nx = h.x_mm / (0.5 * frameOuterWidthMm(p));
    const outer_ny = h.y_mm / (0.5 * frameOuterHeightMm(p));
    if (outer_nx * outer_nx + outer_ny * outer_ny <= 1.0) {
        return .frame;
    }
    return null;
}

pub fn profileToJson(allocator: std.mem.Allocator, profile: Profile) ![]u8 {
    return std.json.Stringify.valueAlloc(allocator, profile, .{ .whitespace = .indent_2 });
}

pub fn profileFromJson(allocator: std.mem.Allocator, bytes: []const u8) !std.json.Parsed(Profile) {
    return std.json.parseFromSlice(Profile, allocator, bytes, .{
        .ignore_unknown_fields = true,
    });
}

fn positiveFinite(value: f64) bool {
    return std.math.isFinite(value) and value > 0.0;
}

fn nonnegativeFinite(value: f64) bool {
    return std.math.isFinite(value) and value >= 0.0;
}

test "default profile validates and round trips through JSON" {
    const allocator = std.testing.allocator;
    const original = Profile{};
    try validate(original);
    const json = try profileToJson(allocator, original);
    defer allocator.free(json);
    var parsed = try profileFromJson(allocator, json);
    defer parsed.deinit();
    try std.testing.expectEqual(original.hit.main_tension_lbf, parsed.value.hit.main_tension_lbf);
    try std.testing.expectEqual(original.model.frame_modes[3].frequency_hz, parsed.value.model.frame_modes[3].frequency_hz);
}

test "older version-one JSON receives defaults for new swing fields" {
    const allocator = std.testing.allocator;
    const legacy =
        \\{
        \\  "version": 1,
        \\  "hit": {
        \\    "relative_normal_speed_mps": 12.0
        \\  }
        \\}
    ;
    var parsed = try profileFromJson(allocator, legacy);
    defer parsed.deinit();
    try std.testing.expectEqual(@as(f64, 12.0), parsed.value.hit.relative_normal_speed_mps);
    try std.testing.expectEqual(@as(f64, 5.0), parsed.value.hit.racket_speed_mps);
    try std.testing.expectEqual(@as(f64, 60.0), parsed.value.model.impact_pre_roll_ms);
    try validate(parsed.value);
}

test "hit coordinates classify strings, frame annulus, and outside racket" {
    var profile = Profile{};
    try std.testing.expectEqual(HitRegion.strings, hitRegion(profile).?);
    profile.hit.x_mm = profile.model.head_width_mm * 0.5 + 2.0;
    try std.testing.expectEqual(HitRegion.frame, hitRegion(profile).?);
    try validate(profile);
    profile.hit.x_mm = frameOuterWidthMm(profile.model);
    try std.testing.expectEqual(@as(?HitRegion, null), hitRegion(profile));
    try std.testing.expectError(error.ImpactOutsideRacket, validate(profile));
}

test "BWF stringed-area and overall-width limits are enforced" {
    var profile = Profile{};
    profile.model.head_width_mm = bwf_max_stringed_width_mm + 0.1;
    try std.testing.expectError(error.InvalidGeometry, validate(profile));
    profile = .{};
    profile.model.head_height_mm = bwf_max_stringed_length_mm + 0.1;
    try std.testing.expectError(error.InvalidGeometry, validate(profile));
    profile = .{};
    profile.model.frame_radial_width_mm =
        0.5 * (bwf_max_racket_width_mm - profile.model.head_width_mm) + 0.1;
    try std.testing.expectError(error.InvalidGeometry, validate(profile));
}
