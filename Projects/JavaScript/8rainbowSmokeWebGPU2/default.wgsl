struct VertexInput {
    @location(0) pos: vec2f,
    @builtin(instance_index) instance: u32
}

struct VertexOutput {
    @builtin(position) pos: vec4f,
    @location(0) cellColor: vec3f
}

@group(0) @binding(0) var<uniform> gridGlobal: vec2u;
@group(0) @binding(1) var<storage> cellColorGlobal: array<f32>;

@vertex
fn vertexMain (input: VertexInput) -> VertexOutput {
    let grid = vec2f(gridGlobal);
    // 1D cell index
    let i = f32(input.instance);
    // 2D cell index
    let cell = vec2f(i % grid.x, floor(i / grid.x));
    
    // Cell position
    let gridPos = (input.pos + 1 + cell*2) / grid - 1;

    var output: VertexOutput;
    output.pos = vec4f(gridPos, 0, 1);

    // Cell color
    let i3 = input.instance*3;
    let color = vec3f(cellColorGlobal[i3], cellColorGlobal[i3+1], cellColorGlobal[i3+2]);
    output.cellColor = vec3f(color);
    return output;
}

@fragment
fn fragmentMain (input: VertexOutput) -> @location(0) vec4f {
    return vec4f(input.cellColor, 1);
}