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
