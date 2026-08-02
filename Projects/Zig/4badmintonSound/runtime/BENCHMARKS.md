# Runtime benchmarks

Measured on a 12th Gen Intel Core i5-12400F under Windows, using Zig 0.16
`ReleaseFast`, 48 kHz mono output, and 128-frame render calls. The table reports
the median of five sequential runs. CPU percentages are wall-clock render time
divided by simulated audio duration, so they express the load on one logical
core rather than the whole machine.

| Scenario | Simulated audio | Render time | One-core load | Real-time factor |
|---|---:|---:|---:|---:|
| Continuous idle stream | 300 s | 8.64 ms | 0.0029% | 34,720x |
| Typical gameplay, 2 hits/s | 120 s | 82.88 ms | 0.0691% | 1,448x |
| Sustained eight-voice stress, 20 hits/s | 30 s | 117.89 ms | 0.3930% | 254x |
| Continuous 60 m/s swoosh | 60 s | 30.06 ms | 0.0501% | 1,996x |

The deliberately hostile callback test submitted a new 40 m/s impact before
every callback while keeping all eight voices occupied. Median percentiles and
the worst maximum observed across the five runs (100,000 callbacks total) were:

- 128-frame callback deadline at 48 kHz: 2,666.67 us;
- median render: 12.50 us;
- p99 render: 22.70 us;
- maximum observed render: 129.90 us;
- internal event-to-first-sample delay: 0 frames;
- host scheduling bound with 128-frame callbacks: 2.667 ms.

The engine occupies 4,096 bytes with 128-byte alignment. Its event queue has
32 entries and it holds eight voices in that fixed storage. The Windows static
archive is approximately 62 KiB; the Android ARM64 archive is approximately
113 KiB. Symbol inspection finds only math and memory primitives (`sinf`,
`cosf`, `sincosf`, `expf`, `memcpy`, and `memset`) as Android dependencies—no
allocator, file, networking, audio-device, or thread API.

## Quest 3 interpretation

The library cross-compiles successfully as an Android AArch64 static archive
for Quest 3's
[Snapdragon XR2 Gen 2 platform](https://www.qualcomm.com/news/onq/2023/10/qualcomm-and-meta-are-expanding-your-reality-heres-how).
No Quest was connected to this development machine, so the numbers
above are not presented as an on-device thermal/performance measurement. As a
conservative sensitivity check, making the measured p99 ten times slower would
still use about 227 us, or 8.5% of a 128-frame callback deadline. The normal
two-hit-per-second path has much more headroom.

This is appropriate for a Quest 3 integration, but final sign-off should still
run the same callback benchmark on the headset inside the actual game while
tracking AAudio/Oboe underruns and the game's 72/90/120 Hz CPU frame timing.
Meta's current guidance gives those frame budgets as 13.9, 11.1, and 8.3 ms
respectively and requires at least 72 FPS for VRC compliance.
[Meta Quest optimization guidance](https://developers.meta.com/horizon/documentation/unreal/po-perf-opt-mobile/)

Run a fresh local benchmark from the repository root:

```powershell
zig build runtime-benchmark -Doptimize=ReleaseFast
```

The original authoring simulator remains intentionally separate. Its full
366-node nonlinear physics render takes roughly 2.2 seconds for 0.5 seconds of
audio on this machine and allocates result/visualization/FFT buffers; none of
that code is linked into `badminton_audio`.

The runtime preserves the calibrated output scale without normalizing hits:

- 27/28 lb, 5 m/s string touch: 0.270 peak and 1236 Hz dominant mode
  (authoring model: 0.294 and 1239 Hz);
- 30 lb, 5 m/s side-frame touch: 0.581 peak and 6117 Hz dominant mode
  (authoring frame probe: 0.579 and 6117 Hz);
- continuous 60 m/s swoosh: 0.301 peak (authoring probe: 0.275).
