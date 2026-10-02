# String frequency vs tension and thickness: fitted curve

Source: `string_frequency_vs_tension_chart.webp`, the CK badminton chart
"Frequency (Hz) vs String Tension (lbs)" for four strings measured from 22 to
32 lbs (Babolat Life+ 70 0.70 mm, Yonex BG-80 0.68 mm, BG-66 Ultimax 0.65 mm,
Aerosonic 0.61 mm; 44 labelled points).

## Fit: full quadratic in tension and diameter

    f = -5671.32 + 169.51 T + 15268.2 d - 1.14452 T^2 - 124.96 T d - 10660.6 d^2

- `f`: string-bed frequency in Hz
- `T`: stringing tension in lbf, 22-32
- `d`: string diameter in mm, 0.61-0.70

Least squares over all 44 points: RMS error 9.4 Hz (0.7 %), maximum 21.5 Hz
(BG-80 at 23 lbs, 1.9 %). Plot: `string_frequency_fit.png` (curves against
the chart points, with the error per point underneath).

Both the lab model and the runtime set their string-bed fundamental from this
curve: the lab sets the tension its simulated string network carries so the
network's fundamental lands on the curve (within 0.7 % across the range), and
the runtime scales its modal bank from it. Tension and diameter sliders are
limited to the fitted range.

`demo/mel_touch_clear.png` (generated) shows a touch (5 m/s) and a clear (30 m/s) from both
models as heard: swing swoosh, strike, shuttle flight and hall.
