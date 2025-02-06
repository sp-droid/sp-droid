struct ComputeInput {
    @builtin(global_invocation_id) cell: vec3u
}

// Global memory
@group(0) @binding(0) var<uniform> gridGlobal: vec2u;
@group(0) @binding(1) var<storage, read_write> cellColorGlobal: array<f32>;
@group(0) @binding(2) var<storage, read_write> cellStateGlobal: array<u32>;
@group(0) @binding(3) var<storage> colorPoolGlobal: array<f32>;
@group(0) @binding(4) var<storage, read_write> minIndexGlobal: u32;
@group(0) @binding(5) var<storage, read_write> distancesGlobal: array<f32>;
@group(0) @binding(6) var<storage, read_write> iterationGlobal: u32;
@group(0) @binding(7) var<storage, read_write> targetColorGlobal: vec3f;

fn cellIndex(x: u32, y: u32, gridX: u32) -> u32 {
    return y * gridX + x;
}

fn l2normSquared(v1: vec3f, v2: vec3f) -> f32 {
    return pow(v1.x-v2.x,2) + pow(v1.y-v2.y,2) + pow(v1.z-v2.z,2);
}

fn neighborDistance(x: u32, y: u32, targetColor: vec3f, gridX: u32) -> vec2f {
    var nPainted = 0.0;
    var distance = 0.0;
    let index1Dneighbor = cellIndex(x, y, gridX);
    if (cellStateGlobal[index1Dneighbor] == 2) {
        nPainted += 1;
        let i = index1Dneighbor*3;
        let color = vec3f(cellColorGlobal[i], cellColorGlobal[i+1], cellColorGlobal[i+2]);
        distance += l2normSquared(targetColor, color);
    }
    return vec2f(nPainted, distance);
}

fn neighborDistanceOnly(x: u32, y: u32, targetColor: vec3f, gridX: u32) -> f32 {
    var distance = 10.0;
    let index1Dneighbor = cellIndex(x, y, gridX);
    if (cellStateGlobal[index1Dneighbor] == 2) {
        let i = index1Dneighbor*3;
        let color = vec3f(cellColorGlobal[i], cellColorGlobal[i+1], cellColorGlobal[i+2]);
        distance = l2normSquared(targetColor, color);
    }
    return distance;
}

// Calculate distance from active cells (in color) to target RGB
@compute @workgroup_size(8, 8)
fn distancesAverageMethodMain(input: ComputeInput) {
    let grid = gridGlobal;

    let cellX = input.cell.x;
    let cellY = input.cell.y;
    let index1D = cellIndex(cellX, cellY, grid.x);

    let targetColor = targetColorGlobal;

    // Return if the cell is not active
    if (cellStateGlobal[index1D] != 1) { 
        distancesGlobal[index1D] = 10.0;
        return;
    }

    // Iterate over painted neighbors, adding them up (average method)
    var results = vec2f(0.0, 0.0); // First field is the number of hits, second is the distance
    if (cellX - 1 >= 0) {
        if (cellY - 1 >= 0) { results += neighborDistance(cellX - 1, cellY - 1, targetColor, grid.x); }
        results += neighborDistance(cellX - 1, cellY, targetColor, grid.x);
        if (cellY + 1 < grid.y) { results += neighborDistance(cellX - 1, cellY + 1, targetColor, grid.x); }
    }
    if (cellY + 1 < grid.y) { results += neighborDistance(cellX, cellY + 1, targetColor, grid.x); }
    if (cellY - 1 >= 0) { results += neighborDistance(cellX, cellY - 1, targetColor, grid.x); }
    if (cellX + 1 < grid.x) {
        if (cellY - 1 >= 0) { results += neighborDistance(cellX + 1, cellY - 1, targetColor, grid.x); }
        results += neighborDistance(cellX + 1, cellY, targetColor, grid.x);
        if (cellY + 1 < grid.y) { results += neighborDistance(cellX + 1, cellY + 1, targetColor, grid.x); }
    }
    distancesGlobal[index1D] = results.y / results.x;
}

@compute @workgroup_size(8, 8)
fn distancesMinimumMethodMain(input: ComputeInput) {
    let grid = gridGlobal;

    let cellX = input.cell.x;
    let cellY = input.cell.y;
    let index1D = cellIndex(cellX, cellY, grid.x);

    let targetColor = targetColorGlobal;

    // Return if the cell is not active
    if (cellStateGlobal[index1D] != 1) { 
        distancesGlobal[index1D] = 10.0;
        return;
    }

    // Iterate over painted neighbors, picking the smallest one (minimum method)
    var result = 10.0;
    if (cellX - 1 >= 0) {
        //if (cellY - 1 >= 0) { result = min(result, neighborDistanceOnly(cellX - 1, cellY - 1, targetColor, grid.x)); }
        result = min(result, neighborDistanceOnly(cellX - 1, cellY, targetColor, grid.x));
        //if (cellY + 1 < grid.y) { result = min(result, neighborDistanceOnly(cellX - 1, cellY + 1, targetColor, grid.x)); }
    }
    if (cellY + 1 < grid.y) { result = min(result, neighborDistanceOnly(cellX, cellY + 1, targetColor, grid.x)); }
    if (cellY - 1 >= 0) { result = min(result, neighborDistanceOnly(cellX, cellY - 1, targetColor, grid.x)); }
    if (cellX + 1 < grid.x) {
        //if (cellY - 1 >= 0) { result = min(result, neighborDistanceOnly(cellX + 1, cellY - 1, targetColor, grid.x)); }
        result = min(result, neighborDistanceOnly(cellX + 1, cellY, targetColor, grid.x));
        //if (cellY + 1 < grid.y) { result = min(result, neighborDistanceOnly(cellX + 1, cellY + 1, targetColor, grid.x)); }
    }
    distancesGlobal[index1D] = result;

}

fn neighborActivation(x: u32, y: u32, gridX: u32) {
    let index1Dneighbor = cellIndex(x, y, gridX);
    if (cellStateGlobal[index1Dneighbor] == 0) {
        cellStateGlobal[index1Dneighbor] = 1;
    }
}

// Place color on picked cell, activate it's neighbors
@compute @workgroup_size(1)
fn placeMain(input: ComputeInput) {
    let grid = gridGlobal;
    let bestIndex1D = minIndexGlobal;

    let y = bestIndex1D / grid.x;
    let x = bestIndex1D - y * grid.x;

    // Paint cell
    let bestIndex1D3 = bestIndex1D*3;
    cellColorGlobal[bestIndex1D3] = targetColorGlobal.r;
    cellColorGlobal[bestIndex1D3+1] = targetColorGlobal.g;
    cellColorGlobal[bestIndex1D3+2] = targetColorGlobal.b;

    // Mark cell as painted
    cellStateGlobal[bestIndex1D] = 2;

    // Iterate over neighbors activating them
    if (x - 1 >= 0) {
        if (y - 1 >= 0) { neighborActivation(x - 1, y - 1, grid.x); }
        neighborActivation(x - 1, y, grid.x);
        if (y + 1 < grid.y) { neighborActivation(x - 1, y + 1, grid.x); }
    }
    if (y + 1 < grid.y) { neighborActivation(x, y + 1, grid.x); }
    if (y - 1 >= 0) { neighborActivation(x, y - 1, grid.x); }
    if (x + 1 < grid.x) { 
        if (y - 1 >= 0) { neighborActivation(x + 1, y - 1, grid.x); }
        neighborActivation(x + 1, y, grid.x);
        if (y + 1 < grid.y) { neighborActivation(x + 1, y + 1, grid.x); }
    }
}

// Next iteration
@compute @workgroup_size(1)
fn iterationMain(input: ComputeInput) {
    let i = iterationGlobal+1;
    iterationGlobal = i;
    
    let i3 = i*3;
    targetColorGlobal.r = colorPoolGlobal[i3];
    targetColorGlobal.g = colorPoolGlobal[i3+1];
    targetColorGlobal.b = colorPoolGlobal[i3+2];
}

// Minimum distance
@compute @workgroup_size(1)
fn minimumSTMain(input: ComputeInput) {
    let nColors = gridGlobal.x*gridGlobal.y;

    var minIndex = 0u;
    var result = 9.0;
    for (var i = 0u; i < nColors; i++) {
        let value = distancesGlobal[i];
        if (value < result) {
            result = value;
            minIndex = i;
        }
    }

    minIndexGlobal = minIndex;
}