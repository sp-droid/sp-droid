// Structs
struct structParentProgeny {
    parent: u32,
    progeny: atomic<u32>
};

// Constants
const MAX_UINT32 = 4294967295u;
const N_COLORS = 1000u;
// 8-neighborhood (D8), clockwise starting from the left
const NEIGHBORS = array<vec2i, 8>(
    vec2i(-1,  0), vec2i(-1,  1), vec2i( 0,  1), vec2i( 1,  1),
    vec2i( 1,  0), vec2i( 1, -1), vec2i( 0, -1), vec2i(-1, -1)
);

// Global memory
@group(0) @binding(0) var<uniform> globalGrid: vec2u;
@group(0) @binding(1) var<storage, read_write> globalCellColor: array<u32>;
@group(0) @binding(2) var<storage> globalCellColorPool: array<u32>;
@group(0) @binding(3) var<storage, read_write> globalCellState: array<atomic<u32>>;
@group(0) @binding(4) var<storage, read_write> globalFrozenParentProgeny: array<structParentProgeny>;
@group(0) @binding(5) var<storage, read_write> globalProgressMod: u32;
@group(0) @binding(6) var<storage, read_write> globalRandomNumbers: array<u32>;
@group(0) @binding(7) var<storage, read_write> atomicMaxProgeny: atomic<u32>;
@group(0) @binding(8) var<storage, read_write> atomicFrozenCount: atomic<u32>;

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

// Returns (inside grid, 1D index) of the neighbor in direction dir
fn neighborIndex(cellX: u32, cellY: u32, grid: vec2u, dir: u32) -> vec2u {
    let neighbor = vec2i(i32(cellX), i32(cellY)) + NEIGHBORS[dir];
    if (neighbor.x < 0 || neighbor.y < 0 || neighbor.x >= i32(grid.x) || neighbor.y >= i32(grid.y)) {
        return vec2u(0u, 0u);
    }
    return vec2u(1u, cellIndex(u32(neighbor.x), u32(neighbor.y), grid.x));
}

// PCG random int generator. The stored per-cell state advances as an LCG (full 2^32 period) and only the output is hashed.
// Hashing the previous output instead can fall into short cycles where a cell never gets a low enough value to spawn.
fn nextRandom(index: u32) -> u32 {
    let state = globalRandomNumbers[index];
    globalRandomNumbers[index] = state * 747796405u + 2891336453u;
    let word = ((state >> ((state >> 28u) + 4u)) ^ state) * 277803737u;
    return (word >> 22u) ^ word;
}

fn randomF32(random: u32) -> f32 { // PCG hash random float generator
    const MAX_UINT32F = 4294967295f;
    return f32(random)/MAX_UINT32F;
}

// Main grid DLA algorithm
@compute @workgroup_size(8, 8)
fn mainGridDLA(
    @builtin(global_invocation_id) threadID: vec3u
) {
    let grid = globalGrid;

    let cellX = threadID.x;
    let cellY = threadID.y;
    if (cellX >= grid.x || cellY >= grid.y) { return; }
    let index1D = cellIndex(cellX, cellY, grid.x);

    let state = atomicLoad(&globalCellState[index1D]);

    if (state == 0u) { // Inactive cell, may spawn new walker
        let random = nextRandom(index1D);
        let spawnChance = randomF32(random);

        if (spawnChance < 0.001) {
            // A walker may have moved in during this same dispatch, only spawn if still empty
            atomicCompareExchangeWeak(&globalCellState[index1D], 0u, 3u);
        }
    } else if (state > 2u) { // Active cell, may freeze or move
        let random = nextRandom(index1D);

        // Check the 8 neighbors for frozen cells, starting from a random one so no direction is favored as parent
        let firstDir = random & 7u;
        for (var i = 0u; i < 8u; i++) {
            let neighbor = neighborIndex(cellX, cellY, grid, (firstDir + i) & 7u);
            if (neighbor.x == 0u) { continue; }
            let stateNeighbor = atomicLoad(&globalCellState[neighbor.y]);
            if (stateNeighbor == 1u || stateNeighbor == 2u) {
                // Freeze if it has moved enough else deactivate
                if (state > globalProgressMod) {
                    globalFrozenParentProgeny[index1D].parent = neighbor.y;
                    atomicStore(&globalFrozenParentProgeny[index1D].progeny, 0u);
                    atomicStore(&globalCellState[index1D], 1u);
                } else {
                    atomicStore(&globalCellState[index1D], 0u);
                }
                return;
            }
        }

        // Random walk to one of the 8 neighbors. Move only if the target is still empty, otherwise stay.
        // Occupied cells are never the target of a successful exchange, so a walker can't land on a frozen or freezing cell.
        let destination = neighborIndex(cellX, cellY, grid, random >> 29u);
        if (destination.x == 1u && atomicCompareExchangeWeak(&globalCellState[destination.y], 0u, state + 1u).exchanged) {
            atomicStore(&globalCellState[index1D], 0u);
        } else {
            atomicStore(&globalCellState[index1D], state + 1u);
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
    if (cellX >= grid.x || cellY >= grid.y) { return; }
    let index1D = cellIndex(cellX, cellY, grid.x);

    let state = atomicLoad(&globalCellState[index1D]);
    if (state == 1u) {
        // Finish freezing cell
        atomicStore(&globalCellState[index1D], 2u);
        atomicAdd(&atomicFrozenCount, 1u);

        // Backpropagate children. Many cells frozen in the same frame share ancestors, so increments must be atomic.
        var reachedRoot = false;
        var previousIndex = index1D;
        var i=0u;
        while (reachedRoot == false) {
            if (i > 1000000u) {
                break;
            }
            i += 1u;
            let parentIndex = globalFrozenParentProgeny[previousIndex].parent;
            if (parentIndex == MAX_UINT32) {
                atomicMax(&atomicMaxProgeny, atomicLoad(&globalFrozenParentProgeny[previousIndex].progeny));
                reachedRoot = true;
            } else {
                atomicAdd(&globalFrozenParentProgeny[parentIndex].progeny, 1u);
                previousIndex = parentIndex;
            }
        }
    }
}

// Paint frozen cells by progeny
@compute @workgroup_size(8, 8)
fn mainPaintFrozen(
    @builtin(global_invocation_id) threadID: vec3u
) {
    let grid = globalGrid;

    let cellX = threadID.x;
    let cellY = threadID.y;
    if (cellX >= grid.x || cellY >= grid.y) { return; }
    let index1D = cellIndex(cellX, cellY, grid.x);

    let state = atomicLoad(&globalCellState[index1D]);
    if (state == 2u) {
        const threshold = 10f;
        let progenyMax = max(2f, f32(atomicLoad(&atomicMaxProgeny)) - threshold);
        let progeny = max(1f, f32(atomicLoad(&globalFrozenParentProgeny[index1D].progeny))-threshold);

        let progenyIndex = min(N_COLORS - 1u, u32(f32(N_COLORS) * log(progeny)/log(progenyMax)));
        if (progenyIndex == 0) {
            globalCellColor[index1D] = 0x000000FFu;
        } else {
            globalCellColor[index1D] = globalCellColorPool[progenyIndex];
        }
    }
}
