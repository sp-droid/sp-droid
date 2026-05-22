// raylib-zig (c) Nikolas Wipper 2023

const std = @import("std");
const rl = @import("raylib");

pub fn main() anyerror!void {
    // Initialization
    //--------------------------------------------------------------------------------------
    const screenWidth = 1920.0;
    const screenHeight = 1080.0;
    const startX = 300.0;
    const startY = 200.0;

    rl.initWindow(screenWidth, screenHeight, "raylib-zig [core] example - basic window");
    defer rl.closeWindow(); // Close window and OpenGL context
    rl.setWindowPosition(startX, startY);

    const displayFPS = 144;
    rl.setTargetFPS(displayFPS); // Set our game to run at the display's refresh rate

    //--------------------------------------------------------------------------------------

    // Main game loop
    while (!rl.windowShouldClose()) { // Detect window close button or ESC key
        // Draw
        //----------------------------------------------------------------------------------
        rl.beginDrawing();
        defer rl.endDrawing();

        rl.drawFPS(20, 20);
        rl.clearBackground(.dark_green);
    }
}
