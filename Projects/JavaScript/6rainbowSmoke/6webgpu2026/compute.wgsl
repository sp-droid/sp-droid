// Constants
const WORKGROUP_SIZE: u32 = $WORKGROUP_SIZE$;
const COARSE_FACTOR: u32 = $COARSE_FACTOR$;
override AVERAGE_METHOD: bool = true; // Set per pipeline: average or minimum Rainbow Smoke variant

const INACTIVE_DISTANCE: f32 = 10.0;
const PAINTED: u32 = 0xFFFFFFFF; // Marks a painted cell in its neighborStats.w

// Global memory
@group(0) @binding(0) var<uniform> gridGlobal: vec2u;
@group(0) @binding(1) var<storage, read_write> cellColorGlobal: array<u32>; // Unpainted cells are 0
@group(0) @binding(2) var<storage> colorPoolGlobal: array<u32>;
@group(0) @binding(3) var<uniform> argminCounter: u32;
@group(0) @binding(4) var<storage, read_write> iterationGlobal: u32;
@group(0) @binding(5) var<storage, read_write> targetColorGlobal: u32;
@group(0) @binding(6) var<storage, read_write> argminOutputGlobal: array<indexValueStruct>;
// Painted neighbors stats per cell, in 0-255 integer units: xyz = sum of RGB, w = (sum of |rgb|^2 << 4) | count.
// Doubles as the cell state: w == 0 inactive, count > 0 active, w == PAINTED painted
@group(0) @binding(7) var<storage, read_write> neighborStatsGlobal: array<vec4u>;

// Shared memory
struct indexValueStruct {
    index: u32,
    value: f32,
};
var<workgroup> sdataWithIndex: array<indexValueStruct, WORKGROUP_SIZE>;
var<private> tieSeed: u32;

fn cellIndex(x: u32, y: u32, gridX: u32) -> u32 {
    return y * gridX + x;
}

fn l2normSquared(v1: vec3f, v2: vec3f) -> f32 {
    let a = v1.x-v2.x;
    let b = v1.y-v2.y;
    let c = v1.z-v2.z;
    return a*a+b*b+c*c;
}

fn unpackRGB(packedColor: u32) -> vec3f {
    let r: f32 = f32((packedColor >> 24) & 0xFF)/255.0;
    let g: f32 = f32((packedColor >> 16) & 0xFF)/255.0;
    let b: f32 = f32((packedColor >> 8) & 0xFF)/255.0;

    return vec3(r, g, b);
}

fn unpackRGBu(packedColor: u32) -> vec3u {
    return vec3u((packedColor >> 24) & 0xFF, (packedColor >> 16) & 0xFF, (packedColor >> 8) & 0xFF);
}

// Exact distance ties are broken by a per-iteration hash of the cell index (one multiply), so no cell position is
// systematically favored and real distances are never perturbed
fn tieKey(index: u32) -> u32 {
    return (index ^ tieSeed) * 0x9E3779B1u;
}

fn isBetter(index: u32, value: f32, bestIndex: u32, bestValue: f32) -> bool {
    return value < bestValue || (value == bestValue && tieKey(index) < tieKey(bestIndex));
}

fn neighborDistanceOnly(x: u32, y: u32, targetColor: vec3f, gridX: u32) -> f32 {
    let color = cellColorGlobal[cellIndex(x, y, gridX)];
    if (color == 0) { return INACTIVE_DISTANCE; } // Not painted
    return l2normSquared(targetColor, unpackRGB(color));
}

// Distance from an active cell (in color) to the target color, INACTIVE_DISTANCE if the cell is not active
fn cellDistance(index1D: u32, targetColor: u32) -> f32 {
    let stats = neighborStatsGlobal[index1D];
    if (stats.w == 0 || stats.w == PAINTED) { return INACTIVE_DISTANCE; }

    if (AVERAGE_METHOD) {
        // Average of squared distances to the painted neighbors, from their running sums:
        // (1/n)·Σ|t-c|² = |t-μ|² + (1/n)·Σ|c|² - |μ|²
        let n = f32(stats.w & 0xF);
        let mean = vec3f(stats.xyz) / n;
        let variance = max(f32(stats.w >> 4) / n - dot(mean, mean), 0.0);
        return (l2normSquared(vec3f(unpackRGBu(targetColor)), mean) + variance) / 65025.0;
    }

    // Smallest distance to a painted neighbor (minimum method)
    let grid = gridGlobal;
    let cellY = index1D / grid.x;
    let cellX = index1D - cellY * grid.x;
    let targetRGB = unpackRGB(targetColor);
    var result = INACTIVE_DISTANCE;
    if (cellX > 0) {
        if (cellY > 0) { result = min(result, neighborDistanceOnly(cellX - 1, cellY - 1, targetRGB, grid.x)); }
        result = min(result, neighborDistanceOnly(cellX - 1, cellY, targetRGB, grid.x));
        if (cellY + 1 < grid.y) { result = min(result, neighborDistanceOnly(cellX - 1, cellY + 1, targetRGB, grid.x)); }
    }
    if (cellY + 1 < grid.y) { result = min(result, neighborDistanceOnly(cellX, cellY + 1, targetRGB, grid.x)); }
    if (cellY > 0) { result = min(result, neighborDistanceOnly(cellX, cellY - 1, targetRGB, grid.x)); }
    if (cellX + 1 < grid.x) {
        if (cellY > 0) { result = min(result, neighborDistanceOnly(cellX + 1, cellY - 1, targetRGB, grid.x)); }
        result = min(result, neighborDistanceOnly(cellX + 1, cellY, targetRGB, grid.x));
        if (cellY + 1 < grid.y) { result = min(result, neighborDistanceOnly(cellX + 1, cellY + 1, targetRGB, grid.x)); }
    }
    return result;
}

fn neighborActivation(x: u32, y: u32, gridX: u32, colorStats: vec4u) {
    let index1Dneighbor = cellIndex(x, y, gridX);
    if (neighborStatsGlobal[index1Dneighbor].w == PAINTED) { return; }
    neighborStatsGlobal[index1Dneighbor] += colorStats; // Also activates it, count > 0
}

// Place color on picked cell, activate it's neighbors
fn placeColor(bestIndex1D: u32) {
    let grid = gridGlobal;

    let y = bestIndex1D / grid.x;
    let x = bestIndex1D - y * grid.x;

    // Paint cell and mark it as painted
    cellColorGlobal[bestIndex1D] = targetColorGlobal;
    neighborStatsGlobal[bestIndex1D] = vec4u(0, 0, 0, PAINTED);

    // Contribution of this color to its neighbors stats
    let c = unpackRGBu(targetColorGlobal);
    let colorStats = vec4u(c, (dot(c, c) << 4) | 1);

    // Iterate over neighbors activating them
    if (x > 0) {
        if (y > 0) { neighborActivation(x - 1, y - 1, grid.x, colorStats); }
        neighborActivation(x - 1, y, grid.x, colorStats);
        if (y + 1 < grid.y) { neighborActivation(x - 1, y + 1, grid.x, colorStats); }
    }
    if (y + 1 < grid.y) { neighborActivation(x, y + 1, grid.x, colorStats); }
    if (y > 0) { neighborActivation(x, y - 1, grid.x, colorStats); }
    if (x + 1 < grid.x) {
        if (y > 0) { neighborActivation(x + 1, y - 1, grid.x, colorStats); }
        neighborActivation(x + 1, y, grid.x, colorStats);
        if (y + 1 < grid.y) { neighborActivation(x + 1, y + 1, grid.x, colorStats); }
    }
}

// Next iteration: load the color to be placed on the following step
fn advanceIteration() {
    let i = iterationGlobal + 1;
    iterationGlobal = i;

    if (i + 1 < arrayLength(&colorPoolGlobal)) {
        targetColorGlobal = colorPoolGlobal[i + 1];
    }
}

fn minSharedWithIndex(id1: u32, id2: u32) {
    let other = sdataWithIndex[id2];
    if (isBetter(other.index, other.value, sdataWithIndex[id1].index, sdataWithIndex[id1].value)) {
        sdataWithIndex[id1] = other;
    }
}

fn reduceShared(localID: u32) {
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
    if (localID == 0) { minSharedWithIndex(0, 1); }
}

// Distances computed on the fly and reduced to one (index, distance) argmin per workgroup
@compute @workgroup_size(WORKGROUP_SIZE)
fn distancesArgminMain(
    @builtin(local_invocation_id) local_invocation_id: vec3u,
    @builtin(workgroup_id) workgroup_id: vec3u
) {
    tieSeed = iterationGlobal * 0x85EBCA77u;
    let targetColor = targetColorGlobal;
    let localID = local_invocation_id.x;
    let workgroupID = workgroup_id.x;
    var globalID = workgroupID * WORKGROUP_SIZE * COARSE_FACTOR + localID;
    let size = gridGlobal.x*gridGlobal.y;

    var best = indexValueStruct(0u, INACTIVE_DISTANCE);
    for (var tile = 0u; tile < COARSE_FACTOR; tile++) {
        if (globalID < size) {
            let value = cellDistance(globalID, targetColor);
            if (isBetter(globalID, value, best.index, best.value)) {
                best = indexValueStruct(globalID, value);
            }
        }
        globalID += WORKGROUP_SIZE;
    }
    sdataWithIndex[localID] = best;
    workgroupBarrier();

    reduceShared(localID);
    if (localID == 0) {
        argminOutputGlobal[workgroupID] = sdataWithIndex[0];
    }
}

// Final argmin over the per-workgroup results, then place the color and advance in the same dispatch
@compute @workgroup_size(WORKGROUP_SIZE)
fn reduceAndPlaceMain(
    @builtin(local_invocation_id) local_invocation_id: vec3u
) {
    tieSeed = iterationGlobal * 0x85EBCA77u;
    let localID = local_invocation_id.x;
    var globalID = localID;
    let size = argminCounter;

    var best = indexValueStruct(0u, INACTIVE_DISTANCE);
    while (globalID < size) {
        let candidate = argminOutputGlobal[globalID];
        if (isBetter(candidate.index, candidate.value, best.index, best.value)) {
            best = candidate;
        }
        globalID += WORKGROUP_SIZE;
    }
    sdataWithIndex[localID] = best;
    workgroupBarrier();

    reduceShared(localID);
    if (localID == 0) {
        placeColor(sdataWithIndex[0].index);
        advanceIteration();
    }
}
