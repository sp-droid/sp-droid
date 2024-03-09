struct ComputeInput {
    @builtin(global_invocation_id) cell: vec3u
}

@group(0) @binding(0) var<uniform> grid: vec2f;
@group(0) @binding(1) var<storage, read_write> cellColor: array<f32>;
@group(0) @binding(2) var<storage, read_write> cellState: array<u32>;
@group(0) @binding(3) var<uniform> targetColor: vec3f;
@group(0) @binding(4) var<uniform> minIndex: u32;
@group(0) @binding(5) var<storage, read_write> distances: array<f32>;

fn cellIndex(x: u32, y: u32) -> u32 {
    return y * u32(grid.x) + x;
}

fn l2normSquared(v1: vec3f, v2: vec3f) -> f32 {
    return pow(v1.x-v2.x,2) + pow(v1.y-v2.y,2) + pow(v1.z-v2.z,2);
}

fn neighborDistance(x: u32, y: u32) -> vec2f {
    var nPainted = 0.0;
    var distance = 0.0;
    let index1Dneighbor = cellIndex(x, y);
    if (cellState[index1Dneighbor] == 2) {
        nPainted += 1;
        let i = index1Dneighbor*3;
        let color = vec3f(cellColor[i], cellColor[i+1], cellColor[i+2]);
        distance += l2normSquared(targetColor, color);
    }
    return vec2f(nPainted, distance);
}

// Calculate distance from active cells (in color) to target RGB
@compute @workgroup_size(8, 8)
fn distancesMain(input: ComputeInput) {
    let index1D = cellIndex(input.cell.x, input.cell.y);

    // Return if the cell is not active
    if (cellState[index1D] != 1) { 
        distances[index1D] = 10.0;
        return;
    }

    // Iterate over painted neighbors, adding them up (average method) or picking the smallest one (minimum method)
    var results = vec2f(0.0, 0.0); // First field is the number of hits, second is the distance
    if (input.cell.x - 1 >= 0) {
        if (input.cell.y - 1 >= 0) { results += neighborDistance(input.cell.x - 1, input.cell.y - 1); }
        results += neighborDistance(input.cell.x - 1, input.cell.y);
        if (input.cell.y + 1 < u32(grid.y)) { results += neighborDistance(input.cell.x - 1, input.cell.y + 1); }
    }
    if (input.cell.y + 1 < u32(grid.y)) { results += neighborDistance(input.cell.x, input.cell.y + 1); }
    if (input.cell.y - 1 >= 0) { results += neighborDistance(input.cell.x, input.cell.y - 1); }
    if (input.cell.x + 1 < u32(grid.x)) {
        if (input.cell.y - 1 >= 0) { results += neighborDistance(input.cell.x + 1, input.cell.y - 1); }
        results += neighborDistance(input.cell.x + 1, input.cell.y);
        if (input.cell.y + 1 < u32(grid.y)) { results += neighborDistance(input.cell.x + 1, input.cell.y + 1); }
    }
    distances[index1D] = results.y / results.x;
}

fn neighborActivation(x: u32, y: u32) {
    let index1Dneighbor = cellIndex(x, y);
    if (cellState[index1Dneighbor] == 0) {
        cellState[index1Dneighbor] = 1;
    }
}

// Place color on picked cell, activate it's neighbors
@compute @workgroup_size(1)
fn placeMain(input: ComputeInput) {
    let bestIndex1D = minIndex;

    let y = u32(floor(f32(bestIndex1D) / grid.x));
    let x = bestIndex1D - y * u32(grid.x);

    // Paint cell
    let bestIndex1D3 = bestIndex1D*3;
    cellColor[bestIndex1D3] = targetColor.r;
    cellColor[bestIndex1D3+1] = targetColor.g;
    cellColor[bestIndex1D3+2] = targetColor.b;

    // Mark cell as painted
    cellState[bestIndex1D] = 2;

    // Iterate over neighbors activating them
    if (x - 1 >= 0) {
        if (y - 1 >= 0) { neighborActivation(x - 1, y - 1); }
        neighborActivation(x - 1, y);
        if (y + 1 < u32(grid.y)) { neighborActivation(x - 1, y + 1); }
    }
    if (y + 1 < u32(grid.y)) { neighborActivation(x, y + 1); }
    if (y - 1 >= 0) { neighborActivation(x, y - 1); }
    if (x + 1 < u32(grid.x)) { 
        if (y - 1 >= 0) { neighborActivation(x + 1, y - 1); }
        neighborActivation(x + 1, y);
        if (y + 1 < u32(grid.y)) { neighborActivation(x + 1, y + 1); }
    }
}