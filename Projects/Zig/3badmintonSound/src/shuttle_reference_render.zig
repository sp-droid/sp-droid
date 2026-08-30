const std = @import("std");
const rl = @import("raylib");
const model = @import("model.zig");
const simulator = @import("simulator.zig");
const dsp = @import("dsp.zig");

const speeds_mps = [_]f64{ 2.0, 5.0, 10.0, 15.0 };
const output_paths = [_][:0]const u8{
    "reference/generated/shuttle_model_30lb_2mps.wav",
    "reference/generated/shuttle_model_30lb_5mps.wav",
    "reference/generated/shuttle_model_30lb_10mps.wav",
    "reference/generated/shuttle_model_30lb_15mps.wav",
};

pub fn main() !void {
    const allocator = std.heap.smp_allocator;
    for (speeds_mps, output_paths) |speed, output_path| {
        const profile = baseProfile(speed);
        try render(allocator, profile, output_path);
    }

    for (
        [_]f64{ 20.0, 40.0, 60.0 },
        [_][:0]const u8{
            "reference/generated/swoosh_only_20mps.wav",
            "reference/generated/swoosh_only_40mps.wav",
            "reference/generated/swoosh_only_60mps.wav",
        },
    ) |racket_speed, output_path| {
        var swoosh = baseProfile(0.0);
        swoosh.hit.racket_speed_mps = racket_speed;
        try render(allocator, swoosh, output_path);
    }
    var smash = baseProfile(60.0);
    smash.hit.racket_speed_mps = 50.0;
    try render(
        allocator,
        smash,
        "reference/generated/shuttle_model_smash_60mps_racket50.wav",
    );
    var clear = baseProfile(30.0);
    clear.hit.racket_speed_mps = 30.0;
    try render(
        allocator,
        clear,
        "reference/generated/shuttle_model_clear_30mps_racket30.wav",
    );

    for (
        [_]f64{ 5.0, 30.0 },
        [_][:0]const u8{
            "reference/generated/frame_hit_30lb_5mps.wav",
            "reference/generated/frame_hit_30lb_30mps.wav",
        },
    ) |speed, output_path| {
        var frame_hit = baseProfile(speed);
        frame_hit.hit.racket_speed_mps = 0.0;
        frame_hit.hit.x_mm = 0.5 * frame_hit.model.head_width_mm +
            0.5 * frame_hit.model.frame_radial_width_mm;
        try render(allocator, frame_hit, output_path);
    }
    var frame_components = baseProfile(5.0);
    frame_components.hit.racket_speed_mps = 0.0;
    frame_components.hit.x_mm = 0.5 * frame_components.model.head_width_mm +
        0.5 * frame_components.model.frame_radial_width_mm;
    var frame_modes_only = frame_components;
    frame_modes_only.model.string_radiation_efficiency = 0.0;
    frame_modes_only.model.frame_radiation_efficiency = 0.0;
    frame_modes_only.model.frame_hit_transient_gain = 0.0;
    try render(
        allocator,
        frame_modes_only,
        "reference/generated/frame_hit_probe_local_modes.wav",
    );
    var frame_transient_only = frame_components;
    frame_transient_only.model.string_radiation_efficiency = 0.0;
    frame_transient_only.model.frame_radiation_efficiency = 0.0;
    frame_transient_only.model.frame_hit_mode_gain = 0.0;
    try render(
        allocator,
        frame_transient_only,
        "reference/generated/frame_hit_probe_transient.wav",
    );
    var frame_structure_only = frame_components;
    frame_structure_only.model.frame_hit_mode_gain = 0.0;
    frame_structure_only.model.frame_hit_transient_gain = 0.0;
    try render(
        allocator,
        frame_structure_only,
        "reference/generated/frame_hit_probe_structure.wav",
    );

    var structure = baseProfile(5.0);
    structure.model.contact_noise_gain = 0.0;
    try render(
        allocator,
        structure,
        "reference/generated/shuttle_probe_structure.wav",
    );
    var resolved_structure = structure;
    resolved_structure.model.upper_mode_gain = 0.0;
    try render(
        allocator,
        resolved_structure,
        "reference/generated/shuttle_probe_resolved_structure.wav",
    );
    var upper_modes_only = structure;
    upper_modes_only.model.string_radiation_efficiency = 0.0;
    upper_modes_only.model.frame_radiation_efficiency = 0.0;
    try render(
        allocator,
        upper_modes_only,
        "reference/generated/shuttle_probe_upper_modes.wav",
    );
    var smooth_upper_modes = upper_modes_only;
    smooth_upper_modes.model.upper_mode_contact_roughness = 0.0;
    try render(
        allocator,
        smooth_upper_modes,
        "reference/generated/shuttle_probe_upper_modes_smooth.wav",
    );
    var strings_only = structure;
    strings_only.model.frame_radiation_efficiency = 0.0;
    strings_only.model.upper_mode_gain = 0.0;
    try render(
        allocator,
        strings_only,
        "reference/generated/shuttle_probe_strings.wav",
    );
    var frame_only = structure;
    frame_only.model.string_radiation_efficiency = 0.0;
    frame_only.model.upper_mode_gain = 0.0;
    try render(
        allocator,
        frame_only,
        "reference/generated/shuttle_probe_frame.wav",
    );
    var contact = baseProfile(5.0);
    contact.model.string_radiation_efficiency = 0.0;
    contact.model.frame_radiation_efficiency = 0.0;
    contact.model.upper_mode_gain = 0.0;
    try render(
        allocator,
        contact,
        "reference/generated/shuttle_probe_contact_texture.wav",
    );

    var smash_structure = baseProfile(60.0);
    smash_structure.hit.racket_speed_mps = 0.0;
    smash_structure.model.contact_noise_gain = 0.0;
    smash_structure.model.hard_impact_gain = 0.0;
    try render(
        allocator,
        smash_structure,
        "reference/generated/shuttle_probe_smash_structure.wav",
    );
    var smash_resolved = smash_structure;
    smash_resolved.model.upper_mode_gain = 0.0;
    try render(
        allocator,
        smash_resolved,
        "reference/generated/shuttle_probe_smash_resolved.wav",
    );
    var smash_upper = smash_structure;
    smash_upper.model.string_radiation_efficiency = 0.0;
    smash_upper.model.frame_radiation_efficiency = 0.0;
    try render(
        allocator,
        smash_upper,
        "reference/generated/shuttle_probe_smash_upper.wav",
    );
    var smash_contact = baseProfile(60.0);
    smash_contact.hit.racket_speed_mps = 0.0;
    smash_contact.model.string_radiation_efficiency = 0.0;
    smash_contact.model.frame_radiation_efficiency = 0.0;
    smash_contact.model.upper_mode_gain = 0.0;
    smash_contact.model.hard_impact_gain = 0.0;
    try render(
        allocator,
        smash_contact,
        "reference/generated/shuttle_probe_smash_contact.wav",
    );
    var smash_hard = baseProfile(60.0);
    smash_hard.hit.racket_speed_mps = 0.0;
    smash_hard.model.string_radiation_efficiency = 0.0;
    smash_hard.model.frame_radiation_efficiency = 0.0;
    smash_hard.model.upper_mode_gain = 0.0;
    smash_hard.model.contact_noise_gain = 0.0;
    try render(
        allocator,
        smash_hard,
        "reference/generated/shuttle_probe_smash_hard_transient.wav",
    );
}

fn baseProfile(speed_mps: f64) model.Profile {
    var profile = model.Profile{};
    profile.hit.main_tension_lbf = 30.0;
    profile.hit.cross_tension_lbf = 30.0;
    profile.hit.relative_normal_speed_mps = speed_mps;
    profile.hit.x_mm = 0.0;
    profile.hit.y_mm = 0.0;
    profile.model.duration_s = 0.32;
    profile.model.visualization_duration_ms = 1.0;
    return profile;
}

fn render(
    allocator: std.mem.Allocator,
    profile: model.Profile,
    output_path: [:0]const u8,
) !void {
    var result = try simulator.simulate(allocator, profile);
    defer result.deinit();
    const wav = try dsp.buildPcm16Wav(
        allocator,
        result.audio,
        result.output_sample_rate_hz,
    );
    defer allocator.free(wav);
    if (!rl.saveFileData(output_path, wav)) return error.CouldNotSaveReferenceWav;

    std.debug.print(
        "{s}: {s}, collision {d:.1}, racket {d:.1} m/s, peak {d:.1} Hz, " ++
            "contact {d:.3} ms, force {d:.1} N, node {d:.2} m/s, raw {d:.4}, " ++
            "peak {d:.4}, GR {d:.1} dB\n",
        .{
            output_path,
            @tagName(result.hit_region),
            profile.hit.relative_normal_speed_mps,
            profile.hit.racket_speed_mps,
            result.diagnostics.dominant_frequency_hz,
            result.diagnostics.contact_duration_ms,
            result.diagnostics.peak_force_n,
            result.diagnostics.maximum_node_speed_mps,
            result.diagnostics.peak_before_dynamics,
            result.diagnostics.peak_before_clamp,
            result.diagnostics.maximum_gain_reduction_db,
        },
    );
}
