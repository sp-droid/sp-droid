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
    // Zero renders the canonical, repeatable strike. Any other value selects
    // a different realisation of the unresolved micro-contact and flight
    // turbulence plus a small closing-speed scatter, as no two real strikes
    // are identical.
    variation_seed: u32 = 0,
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
    transverse_stiffness_scale: f64 = 0.5437,
    maximum_dynamic_strain: f64 = 0.060,

    // Shuttle and local cork/string contact.
    shuttle_mass_g: f64 = 5.0,
    // While the cork is stopped and reversed, the feather skirt drives the
    // surrounding air like a disc: a compact dipole whose strength is the
    // contact force times the skirt's added-mass ratio (8/3 rho R^3 / m).
    // Its energy sits near 1 / contact time (roughly 300-700 Hz), the
    // "thock" the recorded strikes carry before any reflection arrives. The
    // gain above the rigid-disc estimate accounts for the feathers flexing
    // forward against the stopped cork (calibrated to the 31-11 lb series).
    shuttle_skirt_diameter_mm: f64 = 68.0,
    shuttle_dipole_gain: f64 = 6.0,
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
    frame_radiation_asymmetry: f64 = 0.35,
    frame_modes: [4]FrameMode = .{
        .{ .frequency_hz = 170.0, .quality_factor = 12.0, .force_coupling = 1.0, .radiation_area_m2 = 0.0050 },
        .{ .frequency_hz = 380.0, .quality_factor = 15.0, .force_coupling = 0.85, .radiation_area_m2 = 0.0065 },
        // Upper frame modes are lightly damped CFRP (0.5-0.8 % damping). At
        // Q 20-25 the 1.3 kHz mode drained the 27-32 lb bed note within
        // 25 ms, while recorded net shots ring on for 100+ ms.
        .{ .frequency_hz = 700.0, .quality_factor = 60.0, .force_coupling = 0.70, .radiation_area_m2 = 0.0040 },
        .{ .frequency_hz = 1300.0, .quality_factor = 100.0, .force_coupling = 0.55, .radiation_area_m2 = 0.0025 },
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
    upper_mode_gain: f64 = 15.0,
    // Unresolved modal residual. A real string grid, grommets and hoop have
    // hundreds of closely spaced, damped modes; the recorded strikes
    // (reference/sounds31to11lbs.mp3) are a continuous 1-15 kHz wash rather
    // than the few isolated lines the resolved modes produce. Octave bands
    // of noise follow the contact force and ring with a decay that shortens
    // with frequency (constant loss factor per cycle count above 1 kHz).
    modal_residual_gain: f64 = 0.55,
    modal_residual_decay_ms_at_1khz: f64 = 22.0,
    modal_residual_tilt_db_per_octave: f64 = -7.5,
    upper_mode_decay_ms: f64 = 45.0,
    // High-energy impacts redistribute energy into short-lived local modes.
    // This is the remaining decay-time fraction at/above the hard-impact
    // reference speed; touch hits retain the full calibrated decay.
    upper_mode_fast_decay_scale: f64 = 0.35,
    upper_mode_contact_roughness: f64 = 0.04,
    // A vibrating mode radiates through acceleration, so upper modes need
    // more acoustic weight than a displacement-only filter would give them.
    upper_mode_radiation_exponent: f64 = 1.5,
    // The first seven are the sustained string-bed tones of three
    // high-tension net shots (reference/net.mp3, fundamental 1359 Hz),
    // moved to the 30 lb BG66 chart frequency; the 3.8 kHz tone (2.84x the
    // fundamental) is the crisp "tink". The rest are weaker higher tones
    // from the earlier 30 lb real-shuttle calibration.
    upper_modes: [upper_mode_count]UpperRadiationMode = .{
        .{ .frequency_hz_at_30lb = 2120.0, .relative_gain = 0.2954, .decay_scale = 1.33 },
        .{ .frequency_hz_at_30lb = 2848.0, .relative_gain = 0.1344, .decay_scale = 1.33 },
        .{ .frequency_hz_at_30lb = 3008.0, .relative_gain = 0.6332, .decay_scale = 0.95 },
        .{ .frequency_hz_at_30lb = 3815.0, .relative_gain = 0.6970, .decay_scale = 1.00 },
        .{ .frequency_hz_at_30lb = 4176.0, .relative_gain = 0.4481, .decay_scale = 0.97 },
        .{ .frequency_hz_at_30lb = 4713.0, .relative_gain = 1.4902, .decay_scale = 0.75 },
        .{ .frequency_hz_at_30lb = 5532.0, .relative_gain = 0.1400, .decay_scale = 0.91 },
        .{ .frequency_hz_at_30lb = 3387.0, .relative_gain = 0.056, .decay_scale = 0.66 },
        .{ .frequency_hz_at_30lb = 4440.0, .relative_gain = 0.063, .decay_scale = 0.74 },
        .{ .frequency_hz_at_30lb = 5234.0, .relative_gain = 0.050, .decay_scale = 0.56 },
        .{ .frequency_hz_at_30lb = 6115.0, .relative_gain = 0.070, .decay_scale = 0.48 },
        .{ .frequency_hz_at_30lb = 6848.0, .relative_gain = 0.056, .decay_scale = 0.42 },
        .{ .frequency_hz_at_30lb = 7180.0, .relative_gain = 0.050, .decay_scale = 0.38 },
        .{ .frequency_hz_at_30lb = 9301.0, .relative_gain = 0.010, .decay_scale = 0.28 },
    },
    // Crispness of soft shots on tight strings: the upper tones ring louder
    // and longer when the contact is slow and the bed is tight. The weight
    // is 1 at <= 5 m/s and >= 30 lbf, fading to 0 by 15 m/s or at 22 lbf.
    upper_mode_crisp_gain: f64 = 23.600,
    upper_mode_crisp_decay: f64 = 1.0,
    upper_mode_crisp_level_compensation: f64 = 1.9,

    // Dry acoustic observation and calibrated unresolved-contact radiation.
    air_density_kg_m3: f64 = 1.204,
    sound_speed_mps: f64 = 343.0,
    microphone_distance_m: f64 = 1.0,
    microphone_x_m: f64 = 0.0,
    microphone_y_m: f64 = 0.0,
    string_radiation_efficiency: f64 = 0.85,
    frame_radiation_efficiency: f64 = 0.16,
    // Unresolved cork/coating micro-contacts: deterministic broadband texture
    // under the physical contact-force envelope.
    contact_noise_gain: f64 = 0.0300,
    // Contact force already contains most velocity dependence. This small
    // empirical exponent adds bite with speed without the former 0.70 curve,
    // which over-brightened smash-level impacts by more than five times.
    contact_noise_speed_exponent: f64 = 0.40,
    contact_noise_decay_ms: f64 = 18.0,
    contact_noise_burst_probability: f64 = 0.018,
    contact_noise_burst_gain: f64 = 2.2,
    contact_noise_highpass_hz: f64 = 400.0,
    contact_noise_lowpass_hz: f64 = 16_000.0,

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
    swoosh_gain: f64 = 2.54,
    swoosh_strouhal_number: f64 = 0.20,
    swoosh_frame_diameter_mm: f64 = 10.0,
    // Version-1 JSON compatibility keeps the old field name; this now controls
    // how quickly the continuous wind source follows a velocity change.
    swoosh_post_impact_decay_ms: f64 = 18.0,
    // Live audition swing: racket-head speed rises to the set value over the
    // forward swing, peaks at contact and falls through the follow-through,
    // as in a real stroke. A zero build-up keeps the old constant,
    // wind-tunnel swoosh that ignores impacts.
    swing_build_up_ms: f64 = 160.0,
    swing_follow_through_ms: f64 = 200.0,

    // The departing shuttle's feather skirt is a porous bluff body whose
    // turbulent wake radiates the "pssh" heard after hard strikes. Departure
    // speed is the racket-head speed plus the separation speed from the bed;
    // quadratic drag with the terminal speed below slows it, and the source
    // recedes from the microphone with the matching Doppler shift.
    // Wind-tunnel measurements (Physics of Fluids, 2026) find tonal vortex
    // shedding from the feather shafts and binding threads near 20 m/s that
    // becomes broadband by 40 m/s; the shaft diameter sets that tone. A
    // terminal speed of 6.9 m/s reproduces the measured halving of smash
    // speed every 3.35 m of flight (arXiv:2601.01412).
    shuttle_flight_gain: f64 = 6.29,
    shuttle_flight_threshold_mps: f64 = 12.0,
    shuttle_terminal_speed_mps: f64 = 6.9,
    shuttle_flight_vane_mm: f64 = 4.0,
    shuttle_rachis_mm: f64 = 1.5,


    // Fixed playback calibration and dynamics. This is one global transfer
    // curve, never per-hit normalization: touch hits receive the same gain as
    // smashes, while the compressor prevents the physical dynamic range from
    // clipping consumer playback.
    master_gain: f64 = 0.071,
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
        !positiveFinite(p.shuttle_skirt_diameter_mm) or
        !nonnegativeFinite(p.shuttle_dipole_gain) or
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
    if (!nonnegativeFinite(p.modal_residual_gain) or
        !positiveFinite(p.modal_residual_decay_ms_at_1khz) or
        !std.math.isFinite(p.modal_residual_tilt_db_per_octave) or
        !nonnegativeFinite(p.upper_mode_gain) or
        !nonnegativeFinite(p.upper_mode_crisp_gain) or
        !nonnegativeFinite(p.upper_mode_crisp_decay) or
        !nonnegativeFinite(p.upper_mode_crisp_level_compensation) or
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
        !nonnegativeFinite(p.swing_build_up_ms) or
        p.swing_build_up_ms > 1000.0 or
        !positiveFinite(p.swing_follow_through_ms) or
        p.swing_follow_through_ms > 1000.0 or
        !nonnegativeFinite(p.shuttle_flight_gain) or
        !nonnegativeFinite(p.shuttle_flight_threshold_mps) or
        !positiveFinite(p.shuttle_terminal_speed_mps) or
        !positiveFinite(p.shuttle_flight_vane_mm) or
        !positiveFinite(p.shuttle_rachis_mm) or
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

/// Fraction of the set racket-head speed at `time_s` relative to contact.
/// The forward swing accelerates hardest just before contact (sin^2 rise);
/// the follow-through decelerates smoothly (cos^2 fall).
pub fn swingEnvelope(params: ModelParams, time_s: f64) f64 {
    const build_up_s = params.swing_build_up_ms * 0.001;
    const follow_s = params.swing_follow_through_ms * 0.001;
    if (time_s <= -build_up_s or time_s >= follow_s) return 0.0;
    if (time_s < 0.0) {
        const rise = @sin(0.5 * std.math.pi * (time_s + build_up_s) / build_up_s);
        return rise * rise;
    }
    const fall = @cos(0.5 * std.math.pi * time_s / follow_s);
    return fall * fall;
}

/// Extra silence the live stream inserts before a strike so the swing's
/// build-up can be heard before contact.
pub fn swingLeadSeconds(params: ModelParams, impact_time_s: f64) f64 {
    return @max(0.0, params.swing_build_up_ms * 0.001 - impact_time_s);
}

pub const chart_min_tension_lbf = 22.0;
pub const chart_max_tension_lbf = 32.0;
pub const chart_min_diameter_mm = 0.61;
pub const chart_max_diameter_mm = 0.70;

/// String-bed frequency from the quadratic fit to the measured tension and
/// thickness chart (reference/string_frequency_fit.md). Inputs are held to
/// the measured 22-32 lbf, 0.61-0.70 mm range.
pub fn chartStringFrequencyHz(tension_lbf: f64, diameter_mm: f64) f64 {
    const t = std.math.clamp(tension_lbf, chart_min_tension_lbf, chart_max_tension_lbf);
    const d = std.math.clamp(diameter_mm, chart_min_diameter_mm, chart_max_diameter_mm);
    return -5671.32 + 169.51 * t + 15268.2 * d - 1.14452 * t * t -
        124.96 * t * d - 10660.6 * d * d;
}

/// Fundamental of the default simulated string network per sqrt(lbf) of
/// carried tension with BG66 (0.66 mm); it scales with 1/diameter.
const lab_fundamental_hz_per_root_lbf = 203.2;

/// Tension the simulated network carries so that its fundamental lands on
/// the chart frequency for this stringing tension and string diameter.
pub fn effectiveTensionLbf(params: ModelParams, nominal_lbf: f64) f64 {
    const target_hz = chartStringFrequencyHz(nominal_lbf, params.string_diameter_mm);
    const hz_per_root_lbf = lab_fundamental_hz_per_root_lbf *
        bg66_diameter_mm / params.string_diameter_mm;
    const root = target_hz / hz_per_root_lbf;
    return root * root;
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

test "swing envelope peaks at contact and is silent outside the stroke" {
    const params = ModelParams{};
    try std.testing.expectApproxEqAbs(@as(f64, 1.0), swingEnvelope(params, 0.0), 1.0e-12);
    try std.testing.expectEqual(@as(f64, 0.0), swingEnvelope(params, -0.2));
    try std.testing.expectEqual(@as(f64, 0.0), swingEnvelope(params, 0.25));
    try std.testing.expect(swingEnvelope(params, -0.04) > swingEnvelope(params, -0.12));
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
