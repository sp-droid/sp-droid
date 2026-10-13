struct VertexOutput {
    @builtin(position) pos: vec4f,
    @location(0) uv: vec2f
}

@group(0) @binding(0) var<uniform> gridGlobal: vec2u;
@group(0) @binding(1) var<storage> cellColorGlobal: array<u32>;

fn unpackRGB(packedColor: u32) -> vec3f {
    let r: f32 = f32((packedColor >> 24) & 0xFF)/255.0;
    let g: f32 = f32((packedColor >> 16) & 0xFF)/255.0;
    let b: f32 = f32((packedColor >> 8) & 0xFF)/255.0;

    return vec3(r, g, b);
}

// Single triangle covering the whole screen: (-1,-1), (3,-1), (-1,3)
@vertex
fn vertexMain (@builtin(vertex_index) vertexIndex: u32) -> VertexOutput {
    let clip = vec2f(f32((vertexIndex << 1) & 2), f32(vertexIndex & 2)) * 2 - 1;

    var output: VertexOutput;
    output.pos = vec4f(clip, 0, 1);
    output.uv = (clip + 1) * 0.5; // y up, so cell row 0 is at the bottom
    return output;
}

@fragment
fn fragmentMain (input: VertexOutput) -> @location(0) vec4f {
    let grid = gridGlobal;
    let cell = min(vec2u(input.uv * vec2f(grid)), grid - 1);

    return vec4f(unpackRGB(cellColorGlobal[cell.y * grid.x + cell.x]), 1);
}
