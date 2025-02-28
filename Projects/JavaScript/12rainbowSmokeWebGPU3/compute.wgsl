struct ComputeInput {
    @builtin(global_invocation_id) cell: vec3u
}

// Constants
const WORKGROUP_SIZE: u32 = $WORKGROUP_SIZE$;
const COARSE_FACTOR: u32 = $COARSE_FACTOR$;

// Global memory
@group(0) @binding(0) var<uniform> gridGlobal: vec2u;
@group(0) @binding(1) var<storage, read_write> cellColorGlobal: array<f32>;
@group(0) @binding(2) var<storage, read_write> cellStateGlobal: array<u32>;
@group(0) @binding(3) var<storage> colorPoolGlobal: array<f32>;
@group(0) @binding(4) var<storage> argminCounter: u32;
@group(0) @binding(5) var<storage, read_write> distancesGlobal: array<f32>;
@group(0) @binding(6) var<storage, read_write> iterationGlobal: u32;
@group(0) @binding(7) var<storage, read_write> targetColorGlobal: vec3f;
@group(0) @binding(8) var<storage, read_write> argminOutputGlobal: array<u32>;

// Shared memory
struct indexValueStruct {
    index: u32,
    value: f32,
};
var<workgroup> sdataWithIndex: array<indexValueStruct, WORKGROUP_SIZE>;

fn cellIndex(x: u32, y: u32, gridX: u32) -> u32 {
    return y * gridX + x;
}

fn l2normSquared(v1: vec3f, v2: vec3f) -> f32 {
    let a = v1.x-v2.x;
    let b = v1.y-v2.y;
    let c = v1.z-v2.z;
    return sqrt(a*a+b*b+c*c);
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
    distancesGlobal[index1D] = results.y / results.x;// +(f32(iterationGlobal+index1D) % 13)*0.0000001;
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
        if (cellY - 1 >= 0) { result = min(result, neighborDistanceOnly(cellX - 1, cellY - 1, targetColor, grid.x)); }
        result = min(result, neighborDistanceOnly(cellX - 1, cellY, targetColor, grid.x));
        if (cellY + 1 < grid.y) { result = min(result, neighborDistanceOnly(cellX - 1, cellY + 1, targetColor, grid.x)); }
    }
    if (cellY + 1 < grid.y) { result = min(result, neighborDistanceOnly(cellX, cellY + 1, targetColor, grid.x)); }
    if (cellY - 1 >= 0) { result = min(result, neighborDistanceOnly(cellX, cellY - 1, targetColor, grid.x)); }
    if (cellX + 1 < grid.x) {
        if (cellY - 1 >= 0) { result = min(result, neighborDistanceOnly(cellX + 1, cellY - 1, targetColor, grid.x)); }
        result = min(result, neighborDistanceOnly(cellX + 1, cellY, targetColor, grid.x));
        if (cellY + 1 < grid.y) { result = min(result, neighborDistanceOnly(cellX + 1, cellY + 1, targetColor, grid.x)); }
    }
    distancesGlobal[index1D] = result +(f32(iterationGlobal+index1D) % 13)*0.0000001;

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
    let bestIndex1D = argminOutputGlobal[0];

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

fn minSharedWithIndex(id1: u32, id2: u32) {
    let value = sdataWithIndex[id2].value;
    if (value < sdataWithIndex[id1].value) {
        sdataWithIndex[id1].index = sdataWithIndex[id2].index;
        sdataWithIndex[id1].value = value;
    }
}
@compute @workgroup_size(WORKGROUP_SIZE)
fn reductionArgminMain(
    @builtin(local_invocation_id) local_invocation_id: vec3u,
    @builtin(workgroup_id) workgroup_id: vec3u
) {
    let localID = local_invocation_id.x;
    let workgroupID = workgroup_id.x;
    var globalID = workgroupID * WORKGROUP_SIZE * COARSE_FACTOR + localID;
    let size = gridGlobal.x*gridGlobal.y;

    var index = 0u;
    var minValue = 10.0;
    if (globalID < size) {
        index = globalID;
        minValue = distancesGlobal[globalID];
    }
    for (var tile = 1u; tile < COARSE_FACTOR; tile++) {
        globalID += WORKGROUP_SIZE;
        if (globalID < size) {
            let value = distancesGlobal[globalID];
            if (value < minValue) {
                index = globalID;
                minValue = value;
            }
        }
    }
    sdataWithIndex[localID].index = index;
    sdataWithIndex[localID].value = minValue;
    workgroupBarrier();

    if (WORKGROUP_SIZE >= 2048) { if (localID < 1024) { minSharedWithIndex(localID, localID + 1024); } workgroupBarrier();}
    if (WORKGROUP_SIZE >= 1024) { if (localID < 512) { minSharedWithIndex(localID, localID + 512); } workgroupBarrier();}
    if (WORKGROUP_SIZE >= 512) { if (localID < 256) { minSharedWithIndex(localID, localID + 256); } workgroupBarrier();}
    if (WORKGROUP_SIZE >= 256) { if (localID < 128) { minSharedWithIndex(localID, localID + 128); } workgroupBarrier();}
    if (WORKGROUP_SIZE >= 128) { if (localID < 64) { minSharedWithIndex(localID, localID + 64); } workgroupBarrier();}
    if (localID < 32) { minSharedWithIndex(localID, localID + 32); } workgroupBarrier();
    if (localID < 16) { minSharedWithIndex(localID, localID + 16); } workgroupBarrier();
    if (localID < 8) { minSharedWithIndex(localID, localID + 8); } workgroupBarrier();
    if (localID < 4) { minSharedWithIndex(localID, localID + 4); } workgroupBarrier();
    if (localID < 2) { minSharedWithIndex(localID, localID + 2); } workgroupBarrier();

    if (localID == 0) {
        minSharedWithIndex(0, 1);
        argminOutputGlobal[workgroupID] = sdataWithIndex[0].index;
    }
}

@compute @workgroup_size(WORKGROUP_SIZE)
fn reductionArgminMain2step(
    @builtin(local_invocation_id) local_invocation_id: vec3u
) {
    let localID = local_invocation_id.x;
    var globalID = localID;
    let size = argminCounter;

    var index = 0u;
    var minValue = 10.0;
    if (globalID < size) {
        index = argminOutputGlobal[globalID];
        minValue = distancesGlobal[index];
    }
    globalID += WORKGROUP_SIZE;
    while (globalID < size) {
        let index2 = argminOutputGlobal[globalID];
        let value = distancesGlobal[index2];
        if (value < minValue) {
            index = index2;
            minValue = value;
        }
        globalID += WORKGROUP_SIZE;
    }
    sdataWithIndex[localID].index = index;
    sdataWithIndex[localID].value = minValue;
    workgroupBarrier();

    if (WORKGROUP_SIZE >= 2048) { if (localID < 1024) { minSharedWithIndex(localID, localID + 1024); } workgroupBarrier();}
    if (WORKGROUP_SIZE >= 1024) { if (localID < 512) { minSharedWithIndex(localID, localID + 512); } workgroupBarrier();}
    if (WORKGROUP_SIZE >= 512) { if (localID < 256) { minSharedWithIndex(localID, localID + 256); } workgroupBarrier();}
    if (WORKGROUP_SIZE >= 256) { if (localID < 128) { minSharedWithIndex(localID, localID + 128); } workgroupBarrier();}
    if (WORKGROUP_SIZE >= 128) { if (localID < 64) { minSharedWithIndex(localID, localID + 64); } workgroupBarrier();}
    if (localID < 32) { minSharedWithIndex(localID, localID + 32); } workgroupBarrier();
    if (localID < 16) { minSharedWithIndex(localID, localID + 16); } workgroupBarrier();
    if (localID < 8) { minSharedWithIndex(localID, localID + 8); } workgroupBarrier();
    if (localID < 4) { minSharedWithIndex(localID, localID + 4); } workgroupBarrier();
    if (localID < 2) { minSharedWithIndex(localID, localID + 2); } workgroupBarrier();

    if (localID == 0) {
        minSharedWithIndex(0, 1);
        argminOutputGlobal[0] = sdataWithIndex[0].index;
    }
}