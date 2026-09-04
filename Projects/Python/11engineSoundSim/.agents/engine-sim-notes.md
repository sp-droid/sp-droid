# Engine Simulator — Ange The Great (Memory Notes)

Reference for building `engineSoundSim` together. These are my notes on the
original open-source project by Ange Yaghi (`ange-yaghi/engine-sim`), which is
the spiritual ancestor of what we're building.

## Sources
- Original open-source repo: `https://github.com/ange-yaghi/engine-sim` (C++, MIT, ~212 commits, master branch)
- Project "moved"/superseded by: `https://github.com/Engine-Simulator/engine-sim-community-edition` (v0.1.14a, binary distribution only, NOT open source)
- Author: Ange Yaghi (@ange-yaghi / AngeTheGreat on YouTube/Socials, Toronto)
- Community tooling ecosystem: Enginette (online engine builder), Engine Importer, Mod Loader (Lua bindings fork), Engine Sim Recorder (export to BeamNG), Engine Hoist (GUI engine switcher)

## What it is
A real-time internal combustion engine **sound** simulation. Goal is realistic
*audio* and *response characteristics*, NOT engineering-grade accuracy. Uses at
default a 10 kHz simulation rate, upsampled to 44.1 kHz audio on a separate
rendering thread.

---

## High-level architecture (data flow)

```
.mr engine script (Piranha DSL)
        │  (compiled at runtime into C++ object graph)
        ▼
Engine (owns Crankshafts, CylinderBanks, Pistons, ConnectingRods,
        Heads, Valvetrains, Intakes, ExhaustSystems, IgnitionModule,
        CombustionChambers, Fuel, Throttle)
        │  assemble()  (in engine_sim.cpp)
        ▼
PistonEngineSimulator (a Simulator)
        │  - builds atg_scs RigidBodySystem (2D constraint solver)
        │  - run loop: simulateStep_() per substep
        ▼
Chamber physics (CombustionChamber + GasSystem fluid model)
        │  - ignition, flame propagation, combustion
        │  - gas flow between intake/cylinder/exhaust
        ▼
writeToSynthesizer() → per-exhaust-system flow signals
        ▼
Synthesizer (audio chain: jitter → DC filter → derivative → air noise
            → mix → convolution(IR) → antialias → leveling → int16 PCM)
        ▼
Audio out (44.1 kHz) + App UI (ImGui-style custom UI, "Delta" engine)
```

---

## Build system (`CMakeLists.txt`)

- CMake, C++17, three main targets:
  - `engine-sim` — static lib, the pure simulation+audio core (no graphics/UI). This is the important part for us.
  - `engine-sim-script-interpreter` — static lib, the `.mr` Piranha scripting interpreter (compiles engine scripts at runtime).
  - `engine-sim-app` — the WIN32 executable (main.cpp + engine_sim_application.cpp + a load of UI/Object/render classes).
  - `engine-sim-test` — gtest test executable (FetchContent pulls googletest).
- Build options:
  - `DTV` — video capture output (compile def `ATG_ENGINE_SIM_VIDEO_CAPTURE`)
  - `PIRANHA_ENABLED` (default ON) — scripting input (`ATG_ENGINE_SIM_PIRANHA_ENABLED`)
  - `DISCORD_ENABLED` (default ON) — Discord rich presence
- External deps via submodules: `simple-2d-constraint-solver` (scs), `csv-io`, `delta-basic`, `piranha` (scripting language).

---

## Directory layout (original repo)
- `src/`, `include/` — simulation+audio core sources/headers
- `scripting/src`, `scripting/include` — Piranha interpreter
- `es/` — the Engine Simulator standard library written in `.mr` (types, constants, units, part library, impulse responses, settings)
- `assets/` — `.mr` engine scripts organized by maker/model, part-library, sound-library (WAV impulse responses), themes
- `test/` — gtest files (`function_test.cpp`, `gas_system_tests.cpp`, `synthesizer_tests.cpp`)
- `configuration/delta.conf` — UI/theme config
- `dependencies/` — submodules + Discord lib

---

## Core simulation classes (`include/`)

### Engine (`engine.h`)
Top-level container. Owns arrays of: `Crankshaft`, `CylinderBank`,
`CylinderHead` (per bank), `Piston`, `ConnectingRod`, `CombustionChamber`,
`ExhaustSystem`, `Intake`, plus single `IgnitionModule`, `Fuel`, `Throttle`.
- `Parameters`: cylinderBanks, cylinderCount, crankshaftCount, exhaustSystemCount,
  intakeCount, name, starter torque/speed, redline, dyno range, throttle, initial
  audio settings (sim frequency, HF gain, noise, jitter).
- Key getters for the audio chain: `getSimulationFrequency()`,
  `getInitialHighFrequencyGain()`, `getInitialNoise()`, `getInitialJitter()`.
- `createSimulator(vehicle, transmission)` — factory that returns the engine's
  Simulator (the PistonEngineSimulator).

### Simulator (`simulator.h`) — base
- Holds `Synthesizer`, `Dynamometer` (`m_dyno`), `StarterMotor`.
- `SystemType { NsvOptimized, Generic }` for the constraint solver.
- `setSimulationFrequency(int)` — the sim rate (default 10k, clamped 400 Hz–400 kHz).
- `readAudioOutput(samples, int16_t*)` — buffer of rendered PCM.
- Pure virtual `writeToSynthesizer()` and `simulateStep_()` implemented by subclass.

### PistonEngineSimulator (`piston_engine_simulator.h/.cpp`)
The concrete simulator. Builds the whole physical system:
- Creates rigid bodies/constraints in `atg_scs` (2D physics): FixedPositionConstraint
  for cranks, LineConstraint for cylinder walls, LinkConstraint for rod-piston-crank,
  RotationFrictionConstraint for crank friction, ClutchConstraint linking multiple cranks.
- `m_fluidSimulationSteps = 8` sub-steps of gas flow per sim step.
- `simulateStep_()`: updates ignition module, ignites chambers on spark events,
  `chamber->update(timestep)` for combustion, then a nested fluid loop that runs
  `exhaustSystem->process()`, `intake->process()`, `chamber->flow()`.
- `writeToSynthesizer()` (the audio input source):
  - Computes per-cylinder exhaust flow signal from the chamber's exhaust runner
    pressure + dynamic pressure (static pressure minus 1 atm, plus dynamic pressure
    terms), scaled by `attenuation_3` (derived from engine speed, cubically).
  - Passes each exhaust system's flow through a `DelayFilter` (accounts for the
    physical length of the exhaust: length / speed-of-sound).
  - Applies sound attenuation (per-cylinder `sound_attenuation`) and inverse-square
    distance falloff (`1 / exhaustLength²`).
  - Sums into `m_exhaustFlowStagingBuffer` (one entry per exhaust system) and calls
    `synthesizer().writeInput(buffer)`.
- `getAverageOutputSignal()` = mean pressure across exhaust systems (used for UI meter).

### CombustionChamber (`combustion_chamber.h/.cpp`)
The heart of the physics. Wraps three `GasSystem`s:
- `m_system` — cylinder gas
- `m_intakeRunnerAndManifold`
- `m_exhaustRunnerAndPrimary`
- Flame model: `FlameEvent` (lit_n, total_n, percentageLit, efficiency, flameSpeed,
  3D travel, global mix). Two-zone-ish combustion with turbulence lookup
  (`m_meanPistonSpeedToTurbulence` Function).
- Handles ignition, blowby, friction (`FrictionModelParams`), volume change from
  piston motion, peak temperature, burnt fuel tracking.
- `apply()` is the scs ForceGenerator that applies combustion pressure to the piston
  (via the piston body / cylinder wall).
- All exhaust flow out of a chamber in a timestep is tracked
  (`getLastTimestepExhaustFlow`) — that's what feeds audio in most engines.

### GasSystem (`gas_system.h`)
A physically-based ~1D gas dynamics model (NOT a lookup table). State:
- `n_mol` (moles), `E_k` (kinetic/thermal energy), `V` (volume), `momentum[2]`
  (bulk velocity components), `Mix` (fractions of fuel `p_fuel`, inert `p_inert`, O₂ `p_o2`).
- Degrees of freedom default 5 (diatomic; O₂/N₂). Compute kinetic energy per mol,
  heat capacity ratio (γ = 1 + 2/DoF), choked flow limits/rates.
- Central methods: `flow()` — mass/composition flow between two systems including
  **choked flow** handling; `react(n, mix)` — combustion reaction consuming fuel/O₂
  and releasing energy; `injectFuel`, `gainN`/`loseN` (transfer moles + energy),
  velocity dissipation.
- Units via `units.h` (`units::AirMolecularMass`, `constants::R`).

---

## Audio pipeline (`synthesizer.h/.cpp`)

### Synthesizer::renderAudio(inputSample) — exact per-channel signal chain
For each input channel (each exhaust system):
1. **Jitter** — `jitterFilter.fast_f(sample)` adds temporal jitter/randomness to
   simulate combustion irregularity (scale = `inputSampleNoise`).
2. **DC removal** — `f = f_in - inputDcFilter.fast_f(f_in)`.
3. **Derivative** — `f_p = derivative.f(f_in)` gives the rate-of-change of flow.
4. **Air noise** — filtered random noise, mixed: `r_mixed = airNoise*r + (1-airNoise)`.
5. **Mix** — `v_in = f_p * dF_F_mix + f * r_mixed * (1 - dF_F_mix)`. So the signal is
   a blend of the flow *derivative* (sharp transients) and the amplitude-modulated
   flow, with the derivative mix controlled by `dF_F_mix`.
6. **Convolution** — `v = convAmount * convolution.f(v_in) + (1-convAmount) * v_in`.
   The impulse response (from a WAV in the sound library) imparts the exhaust tone;
   `convolution` dial blends dry/passed-through vs convolved wet.
7. Sum all channels → `signal`.

Then globally (single sample path):
8. **Antialiasing** — `m_antialiasing.fast_f(signal)`.
9. **Leveler** — `m_levelingFilter.f(signal) * volume` (automatic gain to `levelerTarget`,
   clamped `min/maxGain`), times master `volume`.
10. **Clip** to `INT16_MIN..INT16_MAX`, return int16.

Input samples arrive from `writeInput(double*)` (one value per input channel per sim
step). Blocked transfer + a dedicated audio rendering thread (`startAudioRenderingThread`)
resample from the 10 kHz sim rate up to 44.1 kHz.

### AudioParameters (tunable, mapped to in-game keys)
- `volume`, `convolution` (X), `dF_F_mix` (high-frequency gain, C),
- `inputSampleNoise` (jitter, B), `inputSampleNoiseFrequencyCutoff`,
- `airNoise` (low-freq noise, V), `airNoiseFrequencyCutoff`,
- `levelerTarget/MaxGain/MinGain`.

---

## Scripting / Piranha (`.mr` files)

Engines are defined in the Piranha scripting language (`*.mr`). Key ideas:
- Scripts build the engine as a **node graph**: `engine`, `crankshaft`,
  `cylinder_bank`, `cylinder_head`, `piston`, `connecting_rod` (rods can be master/slave
  for radial engines), `intake`, `exhaust_system`, `ignition_module`, `ignition_wire`,
  `valvetrain` (standard/vtec), `function` (curve lookup), `fuel`, `units`, `constants`.
- Pattern: instantiate crank + rod_journals, add journals to crank, create banks and
  `.add_cylinder(piston:…, connecting_rod:…, rod_journal:…, intake:…, exhaust_system:…, ignition_wire:…)`,
  then `.add_cylinder_bank(b0)`, `engine.add_crankshaft(c0)`,
  `engine.add_ignition_module(...)`.
- Numbers use dimensional `units` (lb_ft, rpm, inch, g, deg, L, cfm, atm…) and helper
  conversions like `k_carb(cfm)` / `k_28inH2O(scfm)` for flow coefficients.
- `function` nodes define curves (e.g. ignition timing vs RPM) via `.add_sample(rpm, deg)`.
- Part library (`es/part-library`, `assets/part-library`) provides named, reusable parts
  (cam lobes, camshafts, heads, ignition modules, intakes) that scripts subclass/instantiate.
- Impulse responses: `es/sound-library/impulse_responses.mr` + `.wav` files
  (smooth/otherwise `smooth_*.wav`, `sharp_*`, `mild_exhaust*`). The WAV is fed to
  `Synthesizer::initializeImpulseResponse(int16_t*, samples, volume, index)` → per-channel
  convolution KIR.

### Example engine script (`assets/engines/chevrolet/engine_03_for_e1.mr`)
A 454 big-block V8: two 45° banks of 4 cylinders, 4 rod journals on one crank,
distributor ignition with an RPM-based timing curve, chevy BBC intake + peanut-port head,
two exhaust systems (one for each bank, differing audio volumes 1.0/0.1).

---

## Tests (`test/`)
gtest-based, target `engine-sim-test`:
- `synthesizer_tests.cpp` — builds a Synth at 32 Hz input / 32 Hz audio with 8 channels,
  asserts the render chain: after sim writes, output is quiet for the first 16 samples
  then equal to the derivative ramp (`(i-16)*10*8`, clipped), both single-threaded and
  threaded. Good model for how to test our audio chain.
- `gas_system_tests.cpp` — gas dynamics sanity.
- `function_test.cpp` — curve lookup.

---

## Controls / UX (for reference, Community Edition adds clutch/y/t/u)
Minimalistic keyboard-driven UI: A ignite, S starter, D dyno, H RPM hold,
G+scroll hold speed, F fullscreen, I dyno stats, Shift clutch, arrows gears,
Z/X/C/V/B/N scroll = volume / convolution / HF gain / LF noise / HF noise / sim frequency,
Enter reload script, Q/W/E/R throttle, Space+scroll fine throttle, 1-5 time warp, Tab screen.

---

## Things to keep in mind for OUR project (engineSoundSim)
1. The **audio signal** we care about originates in combustion stats (exhaust runner
   pressure / flow) — not a canned sample. The recipe is roughly:
   flow signal → per-exhaust-system mix → DelayFilter (exhaust length) → Jitter →
   DC-block → derivative mix → air-noise → convolution with an IR → antialias →
   leveler → PCM.
2. `dF_F_mix` (the derivative-of-flow term) is arguably what gives engine audio its
   characteristic "crack/pop"; convolution + IR gives the tonal character.
3. The sim is decoupled from audio: sim at ~10 kHz on a fixed timestep, audio
   rendered on a separate thread and upsampled to 44.1 kHz. We likely want the same
   decoupling.
4. Physics is a lightweight 2D constraint-based rigid-body solver (scs) + a real gas
   dynamics model — no heavyweight 3D engine needed for sound.
5. Original is C++/CMake, MIT-licensed, so we can freely study/adapt the simulation
   and audio ideas. (The newer "community edition" is NOT open source — avoid copying
   from it.)
