import math

"""Step 0: the simplest possible engine model.

Just: as the crank turns, the piston moves, and the gas volume in the cylinder
changes. Nothing else yet — no combustion, no flows, no temperature.

The ONE relationship we need for this step:
    For a fixed amount of trapped gas,  pressure * volume = constant  (Boyle's law)
    i.e.  P * V = constant   ->   as V goes down, P goes up.
"""

# --- Geometry (fixed in this step) ---
BORE = 0.108            # m  (~4.25 inch)
STROKE = 0.0884         # m  (~3.48 inch)
ROD_LEN = 0.156         # m  (~6.13 inch)
V_DEAD = 1.01e-4        # m^3  (clearance volume so CR ~ 9)

CRANK_R = STROKE / 2.0             # m, crank throw
A_PISTON = math.pi * (BORE / 2.0) ** 2   # m^2, piston crown area

def volume(theta_rad):
    """Cylinder gas volume (m^3) at crank angle theta (0 = top of compression stroke)."""
    # piston height off TDC (true for a slider-crank mechanism)
    y = (
        CRANK_R * (1 - math.cos(theta_rad))
        + ROD_LEN * (1 - math.sqrt(1 - (CRANK_R / ROD_LEN * math.sin(theta_rad)) ** 2))
    )
    return V_DEAD + A_PISTON * y

if __name__ == "__main__":
    # Walk the crank one full turn and print volume at a few angles.
    for deg in (0, 90, 180, 270, 360):
        V = volume(math.radians(deg))
        print(f"crank {deg:3d} deg   volume = {V*1e6:7.1f} cm^3")
