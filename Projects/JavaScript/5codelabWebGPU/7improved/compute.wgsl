@group(0) @binding(0) var<uniform> grid: vec2f;

@group(0) @binding(1) var<storage> cellStateIn: array<u32>;
@group(0) @binding(2) var<storage, read_write> cellStateOut: array<u32>;

fn cellIndex(cell: vec2u) -> u32 {
    return cell.y * u32(grid.x) + cell.x;
}

fn cellActive(cell: vec2u) -> u32 {
    let wrappedCell = vec2u(
        cell.x % u32(grid.x),
        cell.y % u32(grid.y)
    );
    return cellStateIn[cellIndex(wrappedCell)];
}

@compute @workgroup_size(8, 8)
fn computeMain(@builtin(global_invocation_id) cell: vec3u) {
    let gridWidth = u32(grid.x);
    let gridHeight = u32(grid.y);

    // The dispatch is rounded up to whole workgroups, so the last workgroup
    // may contain invocations outside the logical grid.
    if (cell.x >= gridWidth || cell.y >= gridHeight) {
        return;
    }

    let activeNeighbors = 
        cellActive(vec2u(cell.x + 1, cell.y + 1)) +
        cellActive(vec2u(cell.x + 1, cell.y)) +
        cellActive(vec2u(cell.x + 1, cell.y + gridHeight - 1)) +
        cellActive(vec2u(cell.x, cell.y + 1)) +
        cellActive(vec2u(cell.x, cell.y + gridHeight - 1)) +
        cellActive(vec2u(cell.x + gridWidth - 1, cell.y + 1)) +
        cellActive(vec2u(cell.x + gridWidth - 1, cell.y)) +
        cellActive(vec2u(cell.x + gridWidth - 1, cell.y + gridHeight - 1));
    
    let i = cellIndex(cell.xy);
    switch activeNeighbors {
        case 2: {
            cellStateOut[i] = cellStateIn[i];
        }
        case 3: {
            cellStateOut[i] = 1;
        }
        default: {
            cellStateOut[i] = 0;
        }
    }
}
