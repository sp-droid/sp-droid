struct VertexInput {
    @location(0) pos: vec2f,
    @builtin(instance_index) instance: u32
}

struct VertexOutput {
    @builtin(position) pos: vec4f,
    @location(0) cellColor: vec3f
}

@group(0) @binding(0) var<uniform> grid: vec2f;
@group(0) @binding(1) var<storage> cellState: array<u32>;

@vertex
fn vertexMain (input: VertexInput) -> VertexOutput {
    
    let i = f32(input.instance);
    let cell = vec2f(i % grid.x, floor(i / grid.x));
    let state = f32(cellState[input.instance]);

    let gridPos = (input.pos * state + 1 + cell*2) / grid - 1;

    var output: VertexOutput;
    output.pos = vec4f(gridPos, 0, 1);

    let c = cell / grid;
    output.cellColor = vec3f(c, 1-c.x);
    return output;
}

@fragment
fn fragmentMain (input: VertexOutput) -> @location(0) vec4f {
    return vec4f(input.cellColor, 1);
}