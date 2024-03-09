struct VertexInput {
    @location(0) pos: vec2f,
    @builtin(instance_index) instance: u32
}

struct VertexOutput {
    @builtin(position) pos: vec4f,
    @location(0) cellColor: vec3f
}

@group(0) @binding(0) var<uniform> grid: vec2f;
@group(0) @binding(1) var<storage> cellColor: array<f32>;
@group(0) @binding(2) var<storage> cellState: array<u32>;

@vertex
fn vertexMain (input: VertexInput) -> VertexOutput {
    // 1D cell index
    let i = f32(input.instance);
    // 2D cell index
    let cell = vec2f(i % grid.x, floor(i / grid.x));
    
    // Cell position
    let gridPos = (input.pos + 1 + cell*2) / grid - 1;

    var output: VertexOutput;
    output.pos = vec4f(gridPos, 0, 1);

    // Cell color
    let i3 = input.instance*3u;
    //let color = vec3f(cellColor[i3], cellColor[i3+1u], cellColor[i3+2u]);
    let color = vec3f(cellColor[i3], cellColor[i3+1u], cellColor[i3+2u]);
    output.cellColor = vec3f(color);
    return output;
}

@fragment
fn fragmentMain (input: VertexOutput) -> @location(0) vec4f {
    return vec4f(input.cellColor, 1);
}