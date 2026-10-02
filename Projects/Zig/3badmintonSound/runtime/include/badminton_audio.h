#ifndef BADMINTON_AUDIO_H
#define BADMINTON_AUDIO_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct BadmintonAudioEngine BadmintonAudioEngine;
typedef struct BadmintonSpatializer BadmintonSpatializer;

typedef struct BadmintonAudioConfig {
    uint32_t sample_rate_hz;
    float stringed_width_mm;
    float stringed_height_mm;
    float frame_radial_width_mm;
    float master_gain;
    float swoosh_gain;
    /* Appended in the 2026-10 sound update; start from
       badminton_audio_default_config() so these receive defaults. */
    /* Departing-shuttle aerodynamic noise; zero disables it. */
    float shuttle_flight_gain;
    /* String diameter in mm (0.61-0.70) for the chart-fitted bed pitch. */
    float string_diameter_mm;
} BadmintonAudioConfig;

typedef struct BadmintonAudioHit {
    float main_tension_lbf;
    float cross_tension_lbf;
    float relative_normal_speed_mps;
    float x_mm;
    float y_mm;
    uint32_t region_hint;
    /* Zero uses the engine config; positive values override per impact. */
    float stringed_width_mm;
    float stringed_height_mm;
    float frame_radial_width_mm;
} BadmintonAudioHit;

typedef enum BadmintonAudioHitRegionHint {
    BADMINTON_AUDIO_REGION_AUTOMATIC = 0,
    BADMINTON_AUDIO_REGION_STRINGS = 1,
    BADMINTON_AUDIO_REGION_FRAME = 2
} BadmintonAudioHitRegionHint;

typedef struct BadmintonAudioRelativePosition {
    float right_m;
    float up_m;
    float forward_m;
} BadmintonAudioRelativePosition;

typedef struct BadmintonAudioStats {
    uint64_t submitted_hits;
    uint64_t dropped_hits;
    uint64_t rendered_hits;
    uint64_t stolen_voices;
    uint64_t rendered_frames;
    uint32_t active_voices;
} BadmintonAudioStats;

typedef enum BadmintonAudioSubmitResult {
    BADMINTON_AUDIO_ACCEPTED = 0,
    BADMINTON_AUDIO_INVALID = 1,
    BADMINTON_AUDIO_OUTSIDE_RACKET = 2,
    BADMINTON_AUDIO_QUEUE_FULL = 3
} BadmintonAudioSubmitResult;

/* Allocate this amount once, outside every real-time callback. */
size_t badminton_audio_engine_size(void);
size_t badminton_audio_engine_alignment(void);
BadmintonAudioConfig badminton_audio_default_config(void);

/* memory must meet the reported size and alignment. No heap is used later. */
BadmintonAudioEngine *badminton_audio_init(
    void *memory,
    size_t memory_size,
    const BadmintonAudioConfig *config);

/* Single game/physics producer; these calls never allocate or wait. */
uint32_t badminton_audio_submit_hit(
    BadmintonAudioEngine *engine,
    const BadmintonAudioHit *hit);
void badminton_audio_set_racket_speed(
    BadmintonAudioEngine *engine,
    float speed_mps);
/* Updates the continuous airflow radiator without reallocating the engine. */
void badminton_audio_set_racket_geometry(
    BadmintonAudioEngine *engine,
    float stringed_width_mm,
    float stringed_height_mm);

/* Enqueues an ordered callback-side voice clear. Returns 1 when accepted. */
uint32_t badminton_audio_clear_transients(BadmintonAudioEngine *engine);

/* Single audio consumer. Overwrites frame_count mono float samples. */
void badminton_audio_render_mono(
    BadmintonAudioEngine *engine,
    float *output,
    size_t frame_count);

void badminton_audio_get_stats(
    const BadmintonAudioEngine *engine,
    BadmintonAudioStats *output);

/* Only call reset while producer and audio threads are stopped. */
void badminton_audio_reset(BadmintonAudioEngine *engine);

/* One fixed-memory spatializer per independently positioned mono source. */
size_t badminton_spatializer_size(void);
size_t badminton_spatializer_alignment(void);
BadmintonSpatializer *badminton_spatializer_init(
    void *memory,
    size_t memory_size,
    uint32_t sample_rate_hz);

/* Input is mono; output is frame_count interleaved stereo float frames. */
void badminton_spatializer_render(
    BadmintonSpatializer *spatializer,
    const float *mono,
    float *stereo_interleaved,
    size_t frame_count,
    BadmintonAudioRelativePosition relative_position);
void badminton_spatializer_reset(BadmintonSpatializer *spatializer);

#ifdef __cplusplus
}
#endif

#endif
