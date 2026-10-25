// V1, no zoom, no cache eviction
const width = window.innerWidth;
const height = window.innerHeight;

let img0, img1;
let panX = 0;
let panY = 0;
let isDragging = false;
let lastMouseX = 0;
let lastMouseY = 0;

let zoomLevel = 0;
const maxZoomLevel = 3;

let loadedTiles = {};
const LOAD_TILE_DELAY = 100;
let isLoading = false;
let loadingTimeStart = 0;
let loadingTimeEnd = 0;

function setup() {
    // Create a canvas with the same dimensions as the window
    createCanvas(width, height);
    drawingContext.imageSmoothingEnabled = false;
}

function draw() {
    // Clear the canvas and set a dark background
    background(0);

    // Save current transformation state
    push();
    
    // Apply pan offset
    translate(panX, panY);

    // Draw the zoomed-in images
    const level = 2 ** zoomLevel;
    const tileSize = Math.max(height, width) / 2 / level;
    
    // Which tile is the crosshair pointing to
    let xTile = Math.floor((-panX+width/2) / tileSize);
    let yTile = Math.floor((-panY+height/2) / tileSize);

    // Draw tile borders
    for (let y = 0; y < level; y++) {
        for (let x = 0; x < level*2; x++) {
            const tile = getTile(zoomLevel, y, x);
            if (tile) {
                image(tile, x * tileSize, y * tileSize, tileSize, tileSize);
            }

            stroke(0, 255, 0); // Green borders
            strokeWeight(2);
            noFill();
            rect(x * tileSize, y * tileSize, tileSize, tileSize);
        }
    }

    // Draw a red rectangle of what would be the visible area if we were zoomed in
    stroke(255, 0, 0); // Red borders
    strokeWeight(2);
    noFill();
    rect(-panX+width/2 - width / 2 / level, -panY+height/2 - height / 2 / level, width / level, height / level);

    // Restore transformation state
    pop();

    // Draw center crosshair
    stroke(255, 0, 0); // Red
    strokeWeight(2);
    line(width / 2 - 10, height / 2, width / 2 + 10, height / 2); // Horizontal
    line(width / 2, height / 2 - 10, width / 2, height / 2 + 10); // Vertical

    // FPS counter
    fill(250);
    stroke(0);
    textSize(16);
    textAlign(LEFT, TOP);
    text(`FPS: ${frameRate().toFixed(1)}`, 10, 10);
    text(`Pan: (${panX.toFixed(0)}, ${panY.toFixed(0)})`, 10, 30);
    text(`xtile: ${xTile}, ytile: ${yTile}, zoom: ${zoomLevel}`, 10, 50);
    if (isLoading) {
        loadingTimeEnd = millis();
    }
    text(`Loading: ${isLoading ? 'Yes' : 'No'}, Loading time: ${((loadingTimeEnd - loadingTimeStart)).toFixed(0)}ms`, 10, 70);
}

function getTile(z, y, x) {
    const key = `${z}_${y}_${x}`;

    // Tile is in cache
    if (loadedTiles[key]) {
        return loadedTiles[key];
    }

    // Tile is not in cache and not being loaded
    if (!isLoading) {
        isLoading = true;
        setTimeout(() => {
            loadedTiles[key] = loadImage(`map/${z}/${y}/${x}.png`);
            isLoading = false;
        }, LOAD_TILE_DELAY);
    }

    return null;
}

function mouseWheel(event) {
    const newZoomLevel = zoomLevel + Math.sign(-event.delta);
    if (newZoomLevel >= 0 && newZoomLevel <= maxZoomLevel) {
        zoomLevel = newZoomLevel;
        loadingTimeStart = millis();
        console.log(`Zoom level: ${zoomLevel}`);
    }
}

function mousePressed() {
    isDragging = true;
    lastMouseX = mouseX;
    lastMouseY = mouseY;
}

function mouseDragged() {
    if (isDragging) {
        const deltaX = mouseX - lastMouseX;
        const deltaY = mouseY - lastMouseY;
        
        panX += deltaX;
        panY += deltaY;
        
        lastMouseX = mouseX;
        lastMouseY = mouseY;

        if (!isLoading) {
            loadingTimeStart = millis();
            loadingTimeEnd = millis();
        }
    }
}

function mouseReleased() {
    isDragging = false;
}
