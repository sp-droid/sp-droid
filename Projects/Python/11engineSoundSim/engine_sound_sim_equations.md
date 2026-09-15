# Engine sound simulation: governing equations

Angles are measured from top dead centre (TDC), with \(\theta=0\) at TDC and \(\theta=\pi\) at bottom dead centre (BDC). The model is single-cylinder, quasi-one-dimensional, and uses SI units.

## 1. Constant RPM, no combustion

Assumptions: rigid crank-slider geometry, uniform cylinder pressure, no gas exchange or heat loss, and an adiabatic ideal-gas approximation. The crank speed is imposed as constant.

Let \(B\) be the bore, \(S\) the stroke, \(l\) the connecting-rod length, and \(h_c\) the clearance height:

\[
A = \pi\left(\frac{B}{2}\right)^2,\qquad r=\frac{S}{2},\qquad V_c=Ah_c.
\]

The piston displacement from TDC follows crank-slider geometry:

\[
x(\theta)=r(1-\cos\theta)+l\left[1-\sqrt{1-\left(\frac{r}{l}\sin\theta\right)^2}\right].
\]

Therefore, cylinder volume is

\[
V(\theta)=V_c+A\,x(\theta).
\]

Using \(pV^\gamma=\text{constant}\), with atmospheric pressure \(p_a\) at BDC,

\[
p(\theta)=p_a\left(\frac{V(\pi)}{V(\theta)}\right)^\gamma.
\]

Only pressure above atmosphere produces useful piston force:

\[
F(\theta)=A\,[p(\theta)-p_a].
\]

The simple torque approximation neglects connecting-rod angularity:

\[
\tau(\theta)=F(\theta)r\sin\theta.
\]

At constant engine speed,

\[
\omega=\frac{2\pi\,\mathrm{RPM}}{60},\qquad
\theta(t)=\omega t,\qquad
P(t)=\tau(\theta)\omega.
\]

## 2. Constant RPM with instantaneous combustion

This section retains all equations from Section 1. The new assumption is an instantaneous pressure rise \(\Delta p_c\) at TDC, followed by adiabatic expansion.

Define

\[
V_T=V(0),\qquad V_B=V(\pi),
\]

and the post-combustion TDC pressure

\[
p_T=p_a\left(\frac{V_B}{V_T}\right)^\gamma+\Delta p_c.
\]

For the cycle angle \(\theta_c=\theta\bmod 2\pi\), cylinder pressure is

\[
p(\theta_c)=
\begin{cases}
p_T\left(\dfrac{V_T}{V(\theta_c)}\right)^\gamma, & 0\leq\theta_c<\pi \quad\text{(expansion)},\\[1.1ex]
p_a\left(\dfrac{V_B}{V(\theta_c)}\right)^\gamma, & \pi\leq\theta_c<2\pi \quad\text{(compression)}.
\end{cases}
\]

Force, simple torque, and constant-speed power remain those from Section 1.

## 3. Flywheel inertia and variable RPM

This section retains the pressure model and simple torque from Section 2, but the crank speed is no longer imposed. The crankshaft is governed by rotational Newton’s law:

\[
J\alpha=\tau_{\text{net}},
\]

with constant load and friction torques,

\[
\tau_{\text{net}}=\tau_{\text{engine}}-\tau_{\text{load}}-\tau_{\text{friction}},
\qquad
\alpha=\frac{\tau_{\text{net}}}{J}.
\]

The state evolution is

\[
\frac{d\omega}{dt}=\alpha,\qquad
\frac{d\theta}{dt}=\omega,
\]

and the reported speed and mechanical power are

\[
\mathrm{RPM}=\frac{60\omega}{2\pi},qquad
P=\tau_{\text{engine}}\omega.
\]

## 4. Connecting-rod force to torque conversion

This section retains the pressure and inertia models from Sections 2–3, but replaces the simple torque approximation with the exact crank-slider lever arm.

The effective tangential lever arm is the derivative of piston displacement with respect to crank angle:

\[
r_{\text{eff}}(\theta)=\frac{dx}{d\theta}
=r\left[\sin\theta+
\frac{\sin(2\theta)}{2\sqrt{(l/r)^2-\sin^2\theta}}\right].
\]

Thus the engine torque is

\[
\tau_{\text{engine}}(\theta)=F(\theta)\,r_{\text{eff}}(\theta).
\]

The net-torque, flywheel-speed, crank-angle, and power equations remain those from Section 3.

## 5. Finite-duration combustion

This section retains the exact torque conversion and flywheel dynamics from Sections 1–4. The new assumption is that combustion releases its equivalent pressure rise over a finite crank-angle interval rather than instantaneously.

Let \(\Delta\theta_c\) be the combustion duration. A Wiebe burn fraction models the cumulative burned fraction:

\[
f_b(\theta)=
\begin{cases}
1-\exp\left[-a\left(\dfrac{\theta}{\Delta\theta_c}\right)^{m+1}\right],
&0\leq\theta\leq\Delta\theta_c,\\[1.1ex]
1,&\theta>\Delta\theta_c.
\end{cases}
\]

The pressure rise is \(f_b(\theta)\Delta p_c\), so during the expansion stroke:

\[
p(\theta)=
\left[p_{T,0}+f_b(\theta)\Delta p_c\right]
\left(\frac{V_T}{V(\theta)}\right)^\gamma,
\qquad 0\leq\theta<\pi,
\]

where the pre-ignition TDC pressure is

\[
p_{T,0}=p_a\left(\frac{V_B}{V_T}\right)^\gamma.
\]

During compression, the model remains

\[
p(\theta)=p_a\left(\frac{V_B}{V(\theta)}\right)^\gamma,
\qquad \pi\leq\theta<2\pi.
\]

This is a phenomenological heat-release model: the burn fraction controls an equivalent pressure increase, while the gas still expands or compresses adiabatically.
