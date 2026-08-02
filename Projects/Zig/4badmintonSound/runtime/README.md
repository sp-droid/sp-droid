# Badminton Audio Runtime

This directory is the shippable sound library. It has no dependency on raylib,
the desktop UI, JSON, WAV files, FFTs, the full string-network simulator, or a
heap allocator. The installed product consists only of the static library and
`badminton_audio.h`.

Build the native host library with Zig 0.16:

```powershell
cd runtime
zig build -Doptimize=ReleaseFast
```

Build the Quest/Android ARM64 archive:

```powershell
cd runtime
zig build -Dtarget=aarch64-linux-android -Doptimize=ReleaseFast --prefix zig-out-quest
```

The Android archive is position-independent so it can be linked into a game
plugin shared object. The final Android application link must include the
platform math library (`-lm`).
See [BENCHMARKS.md](BENCHMARKS.md) for native measurements, the Android ARM64
compile check, and the limits of extrapolating them to Quest without a headset.

## Real-time contract

Create one engine for each independently spatialized racket. Allocate its
reported memory once during scene/plugin initialization. The game or physics
thread calls `badminton_audio_submit_hit` and updates racket-head speed with
`badminton_audio_set_racket_speed`. Both operations use atomics and a bounded
single-producer/single-consumer queue; they never lock or allocate.

Call `badminton_audio_render_mono` for every host audio callback, even while
idle. It directly overwrites the supplied 32-bit mono PCM block. It generates
exact zeroes while inactive and modal impact/swoosh samples while active. It
does not spawn a second audio thread because Unity, Unreal, AAudio, and Oboe
already invoke DSP on a dedicated high-priority audio callback. Adding another
producer thread and PCM ring would add buffering and latency without reducing
the callback's small fixed workload.

Swoosh does not require `badminton_audio_submit_hit`. Keep updating
`badminton_audio_set_racket_speed` from the latest racket velocity and it will
run indefinitely above the threshold—even with zero contact events—then follow
the velocity back to silence. A hit only starts the separate impact voice.

For Quest, follow the official
[Android low-latency game-audio checklist](https://developer.android.com/games/sdk/oboe/low-latency-audio):
use the device's native 48 kHz rate; request game usage and low-latency
performance; use the Oboe/AAudio data callback; and normally begin with a
two-burst buffer. Never allocate, perform file I/O, wait on a mutex, or sleep
in that callback.

Minimal C++ callback integration:

```cpp
// Initialization thread: aligned allocation is performed only once.
const size_t bytes = badminton_audio_engine_size();
const size_t alignment = badminton_audio_engine_alignment();
void *storage = aligned_alloc(alignment, (bytes + alignment - 1) & ~(alignment - 1));
BadmintonAudioConfig config = badminton_audio_default_config();
BadmintonAudioEngine *racket = badminton_audio_init(storage, bytes, &config);

// Game/physics thread.
BadmintonAudioHit hit{27.0f, 28.0f, 5.0f, 0.0f, 0.0f};
badminton_audio_submit_hit(racket, &hit);
badminton_audio_set_racket_speed(racket, current_racket_speed_mps);

// Existing host audio callback thread; pass this mono source to spatial audio.
badminton_audio_render_mono(racket, output_mono, frame_count);
```

The event queue has 32 entries and the synthesizer supports eight overlapping
voices. If more impacts arrive before the audio callback consumes them, the API
returns `BADMINTON_AUDIO_QUEUE_FULL`; if all voices are already active, the
quietest/oldest remaining voice is replaced. Inspect these cases through
`badminton_audio_get_stats` during development.

Only `zig-out/lib/badminton_audio.lib` (or Android
`zig-out-quest/lib/libbadminton_audio.a`) and
`zig-out/include/badminton_audio.h` are integration artifacts. Test programs,
the benchmark, the UI, and reference audio are outside this package and are
not linked into the archive.
