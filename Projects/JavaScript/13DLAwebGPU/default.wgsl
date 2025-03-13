struct VertexInput {
    @location(0) pos: vec2f,
    @builtin(instance_index) instance: u32
}

struct VertexOutput {
    @builtin(position) pos: vec4f,
    @location(0) cellColor: vec3f
}

@group(0) @binding(0) var<uniform> globalGrid: vec2u;
@group(0) @binding(1) var<storage> globalCellColor: array<u32>;

fn unpackRGBA(packedColor: u32) -> vec4f {
    let r: f32 = f32((packedColor >> 24) & 0xFF)/255.0;
    let g: f32 = f32((packedColor >> 16) & 0xFF)/255.0;
    let b: f32 = f32((packedColor >> 8) & 0xFF)/255.0;
    let a: f32 = f32(packedColor & 0xFF)/255.0;

    return vec4(r, g, b, a);
}

fn unpackRGB(packedColor: u32) -> vec3f {
    let r: f32 = f32((packedColor >> 24) & 0xFF)/255.0;
    let g: f32 = f32((packedColor >> 16) & 0xFF)/255.0;
    let b: f32 = f32((packedColor >> 8) & 0xFF)/255.0;

    return vec3(r, g, b);
}

@vertex
fn vertexMain (input: VertexInput) -> VertexOutput {
    let grid = vec2f(globalGrid);
    // 1D cell index
    let i = f32(input.instance);
    // 2D cell index
    let cell = vec2f(i % grid.x, floor(i / grid.x));
    
    // Cell position
    let gridPos = (input.pos + 1 + cell*2) / grid - 1;

    var output: VertexOutput;
    // Cell position
    output.pos = vec4f(gridPos, 0, 1);
    // Cell color    
    output.cellColor = unpackRGB(globalCellColor[input.instance]);

    return output;
}

@fragment
fn fragmentMain (input: VertexOutput) -> @location(0) vec4f {
    return vec4f(input.cellColor, 1);
}