// Structs
struct structParentProgeny {
    parent: u32,
    progeny: u32
};

// Constants


// Global memory
@group(0) @binding(0) var<uniform> globalGrid: vec2u;
@group(0) @binding(1) var<storage, read_write> globalCellColor: array<u32>;
@group(0) @binding(2) var<storage> globalCellColorPool: array<u32>;
@group(0) @binding(3) var<storage, read_write> globalCellState: array<u32>;
@group(0) @binding(4) var<storage, read_write> globalFrozenParentProgeny: array<structParentProgeny>;
@group(0) @binding(5) var<storage, read_write> globalProgressMod: u32;
@group(0) @binding(6) var<storage, read_write> globalRandomNumbers: array<u32>;
@group(0) @binding(7) var<storage, read_write> atomicMaxProgeny: atomic<u32>;
@group(0) @binding(8) var<storage, read_write> globalMaxProgenyHistory: array<u32>;

// Shared memory


fn cellIndex(x: u32, y: u32, gridX: u32) -> u32 {
    return y * gridX + x;
}

fn unpackRGB(packedColor: u32) -> vec3f {
    let r: f32 = f32((packedColor >> 24) & 0xFF)/255.0;
    let g: f32 = f32((packedColor >> 16) & 0xFF)/255.0;
    let b: f32 = f32((packedColor >> 8) & 0xFF)/255.0;

    return vec3(r, g, b);
}

fn neighborFrozen(x: u32, y: u32, gridX: u32, result: vec2u) -> vec2u {
    let index1Dneighbor = cellIndex(x, y, gridX);
    let stateNeighbor = globalCellState[index1Dneighbor];
    if (stateNeighbor == 2u || stateNeighbor == 1u) {
        return vec2u(1u, index1Dneighbor);
    }
    return result;
}

fn randomU32(seed: u32) -> u32 { // PCG hash random int generator
    let state = seed * 747796405u + 2891336453u;
    let word = ((state >> ((state >> 28u) + 4u)) ^ state) * 277803737u;
    return (word >> 22u) ^ word;
}

fn randomF32(random: u32) -> f32 { // PCG hash random float generator
    const MAX_UINT32 = 4294967295f;
    return f32(random)/MAX_UINT32;
}

fn moveWalker(cellX: u32, cellY: u32, gridX: u32, state: u32, index1D: u32) {
    let neighborIndexID = cellIndex(cellX, cellY, gridX);

    // Order here below actually matters, because in collisions of nearby threads if the last operation is removing, both will disappear.
    globalCellState[index1D] = 0u;
    globalCellState[neighborIndexID] = state + 1u;
}

// Main grid DLA algorithm
@compute @workgroup_size(8, 8)
fn mainGridDLA(
    @builtin(global_invocation_id) threadID: vec3u
) {
    let grid = globalGrid;

    let cellX = threadID.x;
    let cellY = threadID.y;
    let index1D = cellIndex(cellX, cellY, grid.x);

    let state = globalCellState[index1D];

    if (state == 0u) { // Inactive cell, may spawn new walker
        let random = globalRandomNumbers[index1D];
        globalRandomNumbers[index1D] = randomU32(random);
        let spawnChance = randomF32(random);

        if (spawnChance < 0.001) {
            globalCellState[index1D] = 3u;
        }
    } else if (state > 2u) { // Active cell, may freeze or move
        // Check neighboring frozen cells
        var result = vec2u(0u, 0u); // [0] = result, [1] = parent
        if (cellX > 0) { result = neighborFrozen(cellX - 1, cellY, grid.x, result); }
        if (result.x == 0u && cellY + 1 < grid.y) { result = neighborFrozen(cellX, cellY + 1, grid.x, result); }
        if (result.x == 0u && cellY > 0) { result = neighborFrozen(cellX, cellY - 1, grid.x, result); }
        if (result.x == 0u && cellX + 1 < grid.x) { result = neighborFrozen(cellX + 1, cellY, grid.x, result); }
        if (result.x == 1u) {
            // Freeze if it has moved enough else deactivate
            if (state > globalProgressMod) {
                globalCellState[index1D] = 1u;
                let parentProgeny = structParentProgeny(
                    result.y,
                    0u
                );
                globalFrozenParentProgeny[index1D] = parentProgeny;
            } else {
                globalCellState[index1D] = 0u;
            }
            return;
        }

        // Random walk
        let random = globalRandomNumbers[index1D];
        globalRandomNumbers[index1D] = randomU32(random);
        let walkChance = randomF32(random);

        if (walkChance < 0.25) {
            if (cellX > 0) { moveWalker(cellX - 1, cellY, grid.x, state, index1D); } else { globalCellState[index1D] = state + 1; }
        } else if (walkChance < 0.5) {
            if (cellY + 1 < grid.y) { moveWalker(cellX, cellY + 1, grid.x, state, index1D); } else { globalCellState[index1D] = state + 1; }
        } else if (walkChance < 0.75) {
            if (cellY > 0) { moveWalker(cellX, cellY - 1, grid.x, state, index1D); } else { globalCellState[index1D] = state + 1; }
        } else if (walkChance < 1) {
            if (cellX + 1 < grid.x) { moveWalker(cellX + 1, cellY, grid.x, state, index1D); } else { globalCellState[index1D] = state + 1; }
        }
    }
}

// Finish freezing
@compute @workgroup_size(8, 8)
fn mainCompleteFreeze(
    @builtin(global_invocation_id) threadID: vec3u
) {
    let grid = globalGrid;

    let cellX = threadID.x;
    let cellY = threadID.y;
    let index1D = cellIndex(cellX, cellY, grid.x);

    let state = globalCellState[index1D];
    if (state == 1u) {
        // Finish freezing cell
        globalCellState[index1D] = 2u;

        // Backpropagate children
        var reachedRoot = false;
        var previousIndex = index1D;
        const MAX_UINT32 = 4294967295u;
        var i=0u;
        while (reachedRoot == false) {
            if (i > 1000000u) {
                break;
            }
            i += 1u;
            let parentIndex = globalFrozenParentProgeny[previousIndex].parent;
            if (parentIndex == MAX_UINT32) {
                atomicMax(&atomicMaxProgeny, globalFrozenParentProgeny[previousIndex].progeny);
                reachedRoot = true;
            } else {
                globalFrozenParentProgeny[parentIndex].progeny += 1u;
                previousIndex = parentIndex;
            }
        }
    }
}

// Finish freezing
@compute @workgroup_size(1)
fn mainDLAmaintenance() {
    let newMaxProgeny = atomicLoad(&atomicMaxProgeny);

    var consecutive = true;
    for (var i = 1u; i < 20; i = i + 1u) {
        let previous = globalMaxProgenyHistory[i-1];
        globalMaxProgenyHistory[i] = previous;
        if (previous != newMaxProgeny) {
            consecutive = false;
        }
    }
    globalMaxProgenyHistory[0] = newMaxProgeny;

    let progressMod = globalProgressMod;
    if (consecutive && newMaxProgeny>200u && progressMod > 0u) {
        globalProgressMod = u32(f32(progressMod) * 0.5f);
    }
}

// Finish freezing
@compute @workgroup_size(8, 8)
fn mainPaintFrozen(
    @builtin(global_invocation_id) threadID: vec3u
) {
    let grid = globalGrid;

    let cellX = threadID.x;
    let cellY = threadID.y;
    let index1D = cellIndex(cellX, cellY, grid.x);

    let state = globalCellState[index1D];
    if (state == 2u) {
        const threshold = 10f;
        let progenyMax = f32(atomicLoad(&atomicMaxProgeny)) - threshold;
        let progeny = max(1f, f32(globalFrozenParentProgeny[index1D].progeny)-threshold);
        
        let progenyIndex = u32(1000f * log(progeny)/log(progenyMax) );
        if (progenyIndex == 0) {
            globalCellColor[index1D] = 0x000000FFu;
        } else {
            globalCellColor[index1D] = globalCellColorPool[progenyIndex];
        }
    }
}