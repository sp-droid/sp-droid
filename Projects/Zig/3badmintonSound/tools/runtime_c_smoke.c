#include <math.h>
#include <malloc.h>
#include <stdio.h>
#include "badminton_audio.h"

int main(void) {
    const size_t bytes = badminton_audio_engine_size();
    const size_t alignment = badminton_audio_engine_alignment();
    void *storage = _aligned_malloc(bytes, alignment);
    if (!storage) return 1;

    BadmintonAudioConfig config = badminton_audio_default_config();
    BadmintonAudioEngine *engine = badminton_audio_init(storage, bytes, &config);
    if (!engine) return 2;

    BadmintonAudioHit hit = {27.0f, 28.0f, 5.0f, 0.0f, 0.0f};
    if (badminton_audio_submit_hit(engine, &hit) != BADMINTON_AUDIO_ACCEPTED) {
        return 3;
    }
    float block[128];
    double energy = 0.0;
    for (int callback = 0; callback < 100; ++callback) {
        badminton_audio_render_mono(engine, block, 128);
        for (int frame = 0; frame < 128; ++frame) {
            if (!isfinite(block[frame])) return 4;
            energy += block[frame] * block[frame];
        }
    }
    BadmintonAudioStats stats;
    badminton_audio_get_stats(engine, &stats);
    printf("bytes=%zu alignment=%zu energy=%.8f hits=%llu frames=%llu\n",
           bytes,
           alignment,
           energy,
           (unsigned long long)stats.rendered_hits,
           (unsigned long long)stats.rendered_frames);
    _aligned_free(storage);
    return energy > 0.0 && stats.rendered_hits == 1 ? 0 : 5;
}
