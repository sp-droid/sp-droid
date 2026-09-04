# Engine Simulator — Alternative Design (for engineSoundSim)

Reframing based on our discussion: the goal is to simulate **engine performance**
well (torque curve, rotational response, load behavior). A good sound is a
**byproduct** — it falls out of the same thermodynamic pressure pulses that
produce the torque. We should not spend complexity on audio-channel tuning as
the primary objective; we should spend it on the physics that define performance.

Original reference: `ange-yaghi/engine-sim` (MIT, C++). See `engine-sim-notes.md`.

---

## The original's architecture in one line
`Engine` object graph → `PistonEngineSimulator` builds an `atg_scs` 2D rigid-body
constraint solver → each cylinder's `CombustionChamber` (a ForceGenerator) applies
gas pressure along the bore axis to the piston → `LinkConstraint`s carry that force
to the crankshaft as torque → the exhaust pressure pulses are sampled and fed to a
`Synthesizer`.

## What the original gets right (keep)
1. **Real gas dynamics, not lookup tables.** `GasSystem` tracks moles `n`, internal
   energy `E_k`, volume `V`, momentum, and composition `Mix`. Derived
   `P = E_k / (0.5 * DoF * V)`, `T = E_k / (0.5 * DoF * n * R)`, γ = 1 + 2/DoF.
   Supports **choked flow** and a real combustion `react()` that consumes fuel/O₂
   and releases energy. This is the real "engine" — keep it.
2. **Intake/exhaust runner wave dynamics.** Flow between plenum → runner → cylinder
   → exhaust runner/collector with tuned lengths. Pressure waves here are *both* a
   torque-curve lever (runner tuning affects VE) *and* the acoustic signal. Serves
   both goals at once.
3. **Load coupling.** Dyno, starter motor, throttle linkage / governor, RPM hold.
   Engine *response under load* is the essence of performance simulation.

## What the original gets wrong for our goals (replace/drop)
1. **The rigid-body constraint solver is the wrong spend.**
   - The whole `atg_scs` machinery (LinkConstraint, FixedPositionConstraint,
     LineConstraint, RotationFrictionConstraint, ks/kd stiffnesses, iterative
     relaxation) exists to do generic multi-body dynamics. But the rotating assembly
     is effectively *kinematically determined*: the crank has one true degree of
     freedom (angle), and piston position is derived by slider-crank geometry.
   - It adds **numerical instability** (the original shipped a known "engine blows
     up / sim hangs" bug tied to constraint stiffness vs fluid timestep) for no
     performance-modeling benefit. For rotational response what matters is correct
     **inertias** (flywheel + reciprocating mass), which a scalar integrator captures
     cleanly.
2. **Under-invests in what actually sets the torque curve:**
   - Breathing / volumetric efficiency is approximated from lumped flow runs rather
     than crank-angle-resolved **valve flow** (timing, lift profiles, discharge
     coefficients). VE shaping dominates where the torque peak sits. This is the
     biggest lever on "performance" and the original leaves it coarse.
   - Friction/parasitic torque is parameterized (Stribeck + viscous + wall force),
     not derived from valvetrain/ring loads. Fine for a trend, not absolute accuracy.

## Recommended architecture for engineSoundSim (performance-first)
```
(mixer)  e.g. Wiebe heat-release, crank-angle-resolved valve flow
   │
cylinder gas state (single-zone GasSystem-style)  ← pressure drives everything
   │  intake flow in (choked orifice)        exhaust flow out (choked orifice)
intake plenum+runner  ◄──►  cylinder  ◄──►  exhaust runner+collector
                                 │
             gas pressure → slider-crank torque (closed form, per cylinder)
                                 │
             crank integrator (single DOF θ):  I·dω/dt = ΣT_gas + T_fric − T_load
                                 │
             flywheel / dyno / transmission / load  (RPM response)
                                 │
                exhaust runner pressure pulse ──► synthesizer (sound byproduct)
```

1. **Gas dynamics + combustion reaction** — adapt the `GasSystem` model and a Wiebe
   (or `react()`-style) heat release. This is the core and the source of the torque.
2. **Closed-form slider-crank kinematics + scalar crank integrator** instead of the
   constraint solver. Crank angle `θ` is the single state; integrate with a stable
   scheme (e.g. RK4 or small-substep Euler) against `I·dω/dt = ΣT_gas + T_fric − T_load`.
   Piston volume/torque from exact slider-crank formulas (no stiffness to blow up).
3. **Crank-angle-resolved valve flow** — the upgrade path for a believable torque
   curve: valve lift/timing curves with discharge coefficients controlling the
   choked intake/exhaust flows. This is where we add fidelity the original lacks.
4. **Load/response chain** — flywheel inertia, dyno load cell, throttle linkage,
   eventually transmission/vehicle, as the sim grows.
5. **Sound is downstream**: the exhaust-runner pressure pulse computed here is the
   same signal Ange feeds through jitter → derivative-mix → convolution(IR) → leveler.
   Build it last, only after the pressure trace is believable.

## Why this is "better" for our stated goal
- Correct torque curve & rotational response come from real thermodynamics + correct
  inertias, not from tuning constraint stiffnesses.
- Stable and deterministic (no solver tuning, no blow-up class of bugs).
- Simpler to debug: one scalar integrator + three gas states vs a multi-body solver.
- The sound genuinely *emerges* from the performance pressure pulses, matching the
  user's intent: "simulate engine performance decently well, and a good sound arises."

## Deliberately deferred (not needed for a performance-first core)
- Multi-crankshaft / master-slave rod (radial) articulation — only if we later need it.
- Full 1D gas wave (GT-Power class) resolution — lumped runner/plenum is enough for
  both torque trend and sound character at this scope.
