let NcellsX;

let width;
let height;
let cellWidth;
let cellHeight;
let NcellsY;
let cells;
let activeCells;
let activeCellsList;
let colors;

let targetColorIndex;

function shuffleArray(array) {
    for (let i = array.length - 1; i > 0; i--) {
        const j = Math.floor(Math.random() * (i + 1));
        const temp = array[j];
        array[j] = array[i];
        array[i] = temp;
    }
}

function setup() {
    NcellsX = 100;

    width = window.innerWidth;
    height = window.innerHeight;
    cellWidth = width / NcellsX;
    NcellsY = Math.floor(height / cellWidth);
    cellHeight = height / NcellsY;
    
    // Possible colors, 0 is reserved for untouched cells
    const Ncells = NcellsX*NcellsY;
    const numIntervals = Math.ceil(Math.pow(Ncells, 1/3));
    const intervalSize = 255 / numIntervals;

    colors = new Array(numIntervals*3+1);
    
    // Iterate over each interval for each channel
    let i = 0;
    for (let r = 0; r < numIntervals; r++) {
        for (let g = 0; g < numIntervals; g++) {
            for (let b = 0; b < numIntervals; b++) {
                // Calculate RGB values for the current interval
                const red = Math.floor(r * intervalSize);
                const green = Math.floor(g * intervalSize);
                const blue = Math.floor(b * intervalSize);

                // Push the color to the colors array
                colors[i] = [red, green, blue];
                i += 1;
            }
        }
    }

    // Randomize
    shuffleArray(colors);
    colors[0] = [0,0,0];

    // Cell array
    cells = new Array(NcellsX);
    activeCells = new Array(NcellsX);
    activeCellsList = []
    for (let i=0; i < NcellsX; i++) {
        cells[i] = new Array(NcellsY);
        activeCells[i] = new Array(NcellsY);
        for (let j=0; j < NcellsY; j++) {
            cells[i][j] = 0;
            activeCells[i][j] = 0;
        }
    }
    // Seed on the center
    const x = Math.floor(NcellsX/2);
    const y = Math.floor(NcellsY/2);
    targetColorIndex = 1;
    cells[x][y] = targetColorIndex;
    // Add new possible active cells
    activeCells, activeCellsList = addNeighbors(x,y,cells,activeCells, activeCellsList);
    
    createCanvas(width, height);


    noStroke();
}

function draw() {
    if (activeCellsList.length !== 0) {
        background(200);
        // Pick the next color
        targetColorIndex += 1;
        targetRGB = colors[targetColorIndex];
        
        // Loop over active cells, calculate the diff to target and return index
        const chosenCellIndex = colorDiffs(targetRGB, activeCellsList, cells, colors);
        const chosenCell = activeCellsList[chosenCellIndex];
        const x = chosenCell[0];
        const y = chosenCell[1];
        
        // Assign to the newly painted cell
        cells[x][y] = targetColorIndex;

        // Deactivate cell
        activeCells[x][y] = 0;
        // Remove entry
        activeCellsList.splice(chosenCellIndex, 1);
        // Activate neighbors
        activeCells, activeCellsList = addNeighbors(x,y,cells,activeCells, activeCellsList);
        
        // Paint cells
        for (let i=0; i < NcellsX; i++) {
            for (let j=0; j < NcellsY; j++) {
                fill(colors[cells[i][j]]);
                rect(i*cellWidth,j*cellHeight,cellWidth,cellHeight);
            }
        }
    } else {
        noLoop();
    }
    
}

function colorDiffs(targetRGB, activeCellsList, cells, colors) {
    let distances = new Array(activeCellsList.length).fill(0);

    for (let i=0; i<distances.length; i++) {
        const activeCell = activeCellsList[i];
        x = activeCell[0];
        y = activeCell[1];
        distances[i] = neighborDiffs(targetRGB, x, y, cells, colors)
    }

    const chosenCellIndex = distances.indexOf(Math.min(...distances));
    return chosenCellIndex;
}

function neighborDiffs(targetRGB, x, y, cells, colors) {
    let diffs = new Array(8).fill(0);
    let nMatches = 0;
    if (x+1 < NcellsX) {                    // Cell is not out of bounds
        diffs, nMatches = neighborDiff(diffs, targetRGB, nMatches, x+1, y, cells, colors);
    }
    if (x-1 >= 0) {
        diffs, nMatches = neighborDiff(diffs, targetRGB, nMatches, x-1, y, cells, colors);
    }
    if (y+1 < NcellsY) {
        diffs, nMatches = neighborDiff(diffs, targetRGB, nMatches, x, y+1, cells, colors);
    }
    if (y-1 >= 0) {
        diffs, nMatches = neighborDiff(diffs, targetRGB, nMatches, x, y-1, cells, colors);
    }
    if ((x+1 < NcellsX) & (y+1 < NcellsY)) {
        diffs, nMatches = neighborDiff(diffs, targetRGB, nMatches, x+1, y+1, cells, colors);
    }
    if ((x-1 >= 0) & (y-1 >= 0)) {
        diffs, nMatches = neighborDiff(diffs, targetRGB, nMatches, x-1, y-1, cells, colors);
    }
    if ((y+1 < NcellsY) & (x-1 >= 0)) {
        diffs, nMatches = neighborDiff(diffs, targetRGB, nMatches, x-1, y+1, cells, colors);
    }
    if ((y-1 >= 0) & (x+1 < NcellsX) ) {
        diffs, nMatches = neighborDiff(diffs, targetRGB, nMatches, x+1, y-1, cells, colors);
    }

    // const diff = Math.min(...diffs.slice(0, nMatches));
    let diff = 0;
    for (let i=0; i<nMatches; i++) {
        diff += diffs[i];
    }
    diff /= nMatches;
    return diff;
}

function neighborDiff(diffs, targetRGB, nMatches, x, y, cells, colors) {
    const colorId = cells[x][y];
    if (colorId !== 0) {                // Cell is painted
        const color = colors[colorId];
        for (let i=0; i<3; i++) {
            diffs[nMatches] += (color[i]-targetRGB[i])**2;
        }
        nMatches += 1;
    }
    return diffs, nMatches;
}

function addNeighbors(x, y, cells, activeCells, activeCellsList) {
    if (x+1 < NcellsX) {                    // Cell is not out of bounds
        activeCells, activeCellsList = addNeighbor(x+1, y, cells, activeCells, activeCellsList);
    }
    if (x-1 >= 0) {
        activeCells, activeCellsList = addNeighbor(x-1, y, cells, activeCells, activeCellsList);
    }
    if (y+1 < NcellsY) {
        activeCells, activeCellsList = addNeighbor(x, y+1, cells, activeCells, activeCellsList);
    }
    if (y-1 >= 0) {
        activeCells, activeCellsList = addNeighbor(x, y-1, cells, activeCells, activeCellsList);
    }
    if ((x+1 < NcellsX) & (y+1 < NcellsY)) {
        activeCells, activeCellsList = addNeighbor(x+1, y+1, cells, activeCells, activeCellsList);
    }
    if ((x-1 >= 0) & (y-1 >= 0)) {
        activeCells, activeCellsList = addNeighbor(x-1, y-1, cells, activeCells, activeCellsList);
    }
    if ((y+1 < NcellsY) & (x-1 >= 0)) {
        activeCells, activeCellsList = addNeighbor(x-1, y+1, cells, activeCells, activeCellsList);
    }
    if ((y-1 >= 0) & (x+1 < NcellsX) ) {
        activeCells, activeCellsList = addNeighbor(x+1, y-1, cells, activeCells, activeCellsList);
    }
    return activeCells, activeCellsList;
}

function addNeighbor(x, y, cells, activeCells, activeCellsList) {
    if (cells[x][y] === 0) {                // If the cell is not already painted
        if (activeCells[x][y] === 0) {      // Cell was not activated previously
            activeCells[x][y] = 1;          // Activate cell
            activeCellsList.push([x,y]);    // Add cell to activated list
        }
    }
    return activeCells, activeCellsList;
}