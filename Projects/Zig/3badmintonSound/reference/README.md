# Reference analyses

## Palm-hit tension series

The local source is the user-supplied copy of
[this YouTube Short](https://www.youtube.com/shorts/1G0BkvzKUqA). The source
and generated audio are ignored by Git.

Run the repeatable comparison from the repository root:

```powershell
zig build reference-render -Doptimize=ReleaseFast
python tools/analyze_reference.py
```

The analyzer detects 20 transients, splits them at the three longest silences
into ordered 20/24/27/30 lb groups, and exports both each five-hit group and
hit 5 alone. PCM excerpts preserve the decoded source level; no normalization
is applied. `generated/REPORT.md`, `comparison.csv`, `analysis.json`, and
`comparison.svg` compare those clean hits against central, 60 m/s model
renders.

The recording uses a palm, an unknown phone microphone/processing chain, and
four physical rackets whose non-tension properties are not known. It is useful
for note progression, relative spectral balance, and decay—not absolute level
or direct identification of cork-contact parameters.

## Real-shuttle 30 lb calibration

Render the 30 lb low-speed sweep:

```powershell
zig build shuttle-reference-render -Doptimize=ReleaseFast
python tools/analyze_shuttle_reference.py
```

The analyzer detects all eight isolated contacts in the user-supplied
`audio_2.mp3`, exports unnormalised clips, and compares their median 20 ms
attack against 2/5/10/15 m/s synthetic strikes. It also measures 2-8 kHz
energy from 6-24 ms relative to the first 3 ms, which catches the previously
abrupt and muffled post-contact decay. It writes
`generated/SHUTTLE_REPORT.md`, `shuttle_analysis.json`,
`shuttle_comparison.csv`, and `shuttle_comparison.svg`. Long decay is reported
but is not used to tune the dry model because the recording includes the room,
microphone response, and phone processing.

Synthetic WAVs include the configured pre-impact interval; onset detection
searches past it before extracting attack metrics. The same render command
also writes isolated 20/40/60 m/s swoosh probes, a 30/30 m/s clear, a combined
60 m/s collision plus 50 m/s racket-head smash, and isolated resolved-bed,
upper-mode, contact-texture, and hard-transient probes. These are
level-preserving outputs, not normalized demonstrations.

The render also creates `frame_hit_30lb_5mps.wav` and
`frame_hit_30lb_30mps.wav` at the midpoint of the right-side frame annulus.
`frame_hit_probe_local_modes.wav`, `frame_hit_probe_transient.wav`, and
`frame_hit_probe_structure.wav` separate the inharmonic hoop bank, contact
tick, and mechanically coupled low frame/string response for level-preserving
frame-hit calibration.

## 31-11 lb real-shuttle tension series

`sounds31to11lbs.mp3` (local, ignored by Git) holds seven isolated shuttle
strikes on rackets strung from 31 lb down to 11 lb, recorded in stereo in a
reverberant room. Measured with the direct-sound window (2 ms before to 25 ms
after each peak, before the strong reflections):

- String-bed pitch falls from about 1100 Hz (31 lb) to 600 Hz (11 lb), faster
  than sqrt(tension). Both models now take their pitch from the tension and
  thickness chart fit instead (`string_frequency_fit.md`; 22-32 lbf,
  0.61-0.70 mm); this recording still calibrates timbre.
- Octave balance of the direct sound (relative to the 1 kHz peak): 250 Hz
  -11, 500 Hz -6, 2 kHz -4, 4 kHz -6, 8 kHz -9, 16 kHz -12 dB. Strong
  350-700 Hz energy is present in the first 6 ms, before any reflection; it
  is modelled as the shuttle-skirt dipole (`shuttle_dipole_gain`).
- The room's late decay (T20) is about 1.3 s. Room acoustics depend on the
  court, so the models stay dry: there is no hall or echo.

`zig build demo-render` writes `demo/tension_series_32to22lbs.wav`, stepping
through the chart's tension range.

### Objective similarity (log-mel / MR-STFT)

Each recorded strike is compared with renders at its pitch-derived tension,
fitting only the unknown closing speed. Metrics follow common practice in
audio-synthesis evaluation: mean |dB| distance of 96-band log-mel
spectrograms (first 60 ms, before room reflections dominate; and 500 ms),
plus multi-resolution STFT spectral convergence and log-magnitude L1.
Levels are normalised per strike and floored at -70 dB.

The comparison showed that the recorded strikes are a dense broadband wash
while both models produced a few isolated lines. An unresolved-modal
residual (octave-band noise following the contact force, decay ~1/sqrt(f))
was added to both and fitted by minimising the 60 ms log-mel distance:

| Model | log-mel 60 ms | log-mel 500 ms | MR-STFT SC | MR-STFT log-mag |
|---|---:|---:|---:|---:|
| Lab, before | 10.8 dB | 25.5 dB | 0.92 | 3.31 |
| Lab, after | 7.7 dB | 23.0 dB | 0.77 | 3.00 |
| Runtime, before | 14.0 dB | 27.9 dB | 0.94 | 3.16 |
| Runtime, after | 8.3 dB | 23.7 dB | 0.82 | 3.19 |

Most of the remaining 500 ms difference is the recording room's long tail
(about -20 dB at 200 ms), which the dry models intentionally omit.
Figures: `generated/mel_spectrograms_{before,after}.png` and
`generated/spectrum_and_decay_{before,after}.png`.

## High-tension net shots (Kento Momota)

`net.mp3` (local, ignored by Git) holds three soft net shots on a
high-tension racket over background music, crowd noise, shoe squeaks and the
shuttle landing. The strikes are at 417, 2715 and 4872 ms; the landings
(1425, 3748 ms) are short and low-frequency. The string-bed fundamental is
1359 Hz (about 30 lb BG66 on the chart fit). Pitch is not copied from it;
the crispness is.

Sustained tones after the strike (level 5-25 ms after contact, relative to
the strongest; amplitude decay time):

| Tone | x fundamental | Level | Decay |
|---:|---:|---:|---:|
| 1359 Hz | 1.00 | -19 dB | rings on (masked by music after ~150 ms) |
| 2145 Hz | 1.58 | -9 dB | ~120 ms |
| 2882 Hz | 2.12 | -22 dB | ~120 ms |
| 3044 Hz | 2.24 | -11 dB | ~85 ms |
| 3861 Hz | 2.84 | 0 dB | ~85 ms (the crisp "tink") |
| 4226 Hz | 3.11 | -16 dB | ~80 ms |
| 4769 Hz | 3.51 | -14 dB | ~60 ms |
| 5598 Hz | 4.12 | -13 dB | ~70 ms |

Both models now use these tones as their first seven upper string modes.
A crispness weight (1 at <= 5 m/s and >= 30 lbf, fading to 0 by 15 m/s or
22 lbf) raises their level and ring time, and the whole strike is scaled so
crisp shots get brighter rather than louder. Fitted at 30 lb / 5 m/s, every
tone level is within 0.2 dB and every decay within a few ms (lab) or 15 ms
(runtime) of the recording. The lab's upper frame modes were also given
realistic CFRP damping (Q 60 and 100): at Q 20-25 the 1.3 kHz frame mode
drained the 27-32 lb bed note within 25 ms.

`zig build demo-render` writes `demo/net_shot_30lb_4mps.wav`;
`demo/mel_net_vs_models.png` compares recorded and modelled net shots.
