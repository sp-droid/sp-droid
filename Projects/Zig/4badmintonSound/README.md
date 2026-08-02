# AI exclusively coded project

First used and then deprecated by the VR project's own sound system

## Prerequisites

- Windows 10 or 11 on x86-64.
- [Zig 0.16.0](https://ziglang.org/download/) available as `zig` in `PATH`.
- An internet connection for the first build so Zig can download the pinned
  `raylib-zig` dependency.
- A working audio output device.

## Build and run

Open PowerShell in the project directory and run:

```powershell
zig build run -Doptimize=ReleaseFast
```

To build without launching the application:

```powershell
zig build -Doptimize=ReleaseFast
```

Then run:

```powershell
.\zig-out\bin\badminton-sound.exe
```
