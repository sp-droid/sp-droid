# AI exclusively coded project

## Prerequisites

- Zig 0.16.0.
- Internet access during the first build so Zig can download the pinned `raylib-zig` dependency.
- A graphical desktop with OpenGL support.
- On Linux, development libraries for X11, Xrandr, Xinerama, Xi, and Xcursor.

## Build

From the repository root, run:

```powershell
zig build -Doptimize=ReleaseFast
```

The executable is written to `zig-out/bin/badminton-simulator` (`.exe` on Windows).

## Run

```powershell
zig build run -Doptimize=ReleaseFast
```
