// V6, only recomputed the cache requests when the camera moves instead of every frame
const width = window.innerWidth;
const height = window.innerHeight;

let panX = 0;
let panY = 0;
let isDragging = false;
let lastMouseX = 0;
let lastMouseY = 0;

let movedCamera = true;
let cacheRequests = [];

let zoomLevel = 0;
const MAX_ZOOM_LEVEL = 3;

let reusableTile;
const TILE_SIZE = 256;
const MAX_CACHED_TILES = 128;
const TILE_PIXEL_COUNT = TILE_SIZE * TILE_SIZE * 4; // RGBA
let tileCache = new Uint8Array(MAX_CACHED_TILES * TILE_PIXEL_COUNT);
let cachedKeys = new Map();
let cacheIndex = 0;
const LOAD_TILE_DELAY = 100;
let nTilesLoading = 0;
let loadingTimeStart = 0;
let loadingTimeEnd = 0;

function setup() {
    // Create a canvas with the same dimensions as the window
    createCanvas(width, height);
    drawingContext.imageSmoothingEnabled = false;

    reusableTile = createImage(TILE_SIZE, TILE_SIZE);
    reusableTile.loadPixels();
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

    if (movedCamera) {
        cacheRequests = [];
        rebuildCacheRequests(zoomLevel, panX, panY);
        movedCamera = false;
    }

    // Calculate which tiles would be visible if we were zoomed in near the crosshair
    const startX = Math.max(0, Math.floor((-panX+width/2 - width / 2 / level) / tileSize));
    const startY = Math.max(0, Math.floor((-panY+height/2 - height / 2 / level) / tileSize));
    const endX = Math.min(level * 2, Math.ceil((-panX+width/2 + width / 2 / level) / tileSize));
    const endY = Math.min(level, Math.ceil((-panY+height/2 + height / 2 / level) / tileSize));
    // Draw visible tiles
    for (let y = startY; y < endY; y++) {
        for (let x = startX; x < endX; x++) {
            const tileID = getTileID(zoomLevel, y, x);

            const pixels = getTile(tileID);
            if (pixels) {
                reusableTile.pixels.set(pixels);
                reusableTile.updatePixels();
                image(reusableTile, x * tileSize, y * tileSize, tileSize, tileSize);
            }
        }
    }

    // Draw tile borders
    for (let y = 0; y < level; y++) {
        for (let x = 0; x < level*2; x++) {
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

    // Restore transformation state. UI after this
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
    if (nTilesLoading > 0) {
        loadingTimeEnd = millis();
    }
    text(`Ntiles loading: ${nTilesLoading}, Loading time: ${((loadingTimeEnd - loadingTimeStart)).toFixed(0)}ms, Cache requests: ${cacheRequests.length}`, 10, 70);
}

function getTileID(z, y, x) {
    const tileID =
        ((z & 0xF) << 28) |
        ((y & 0x3FFF) << 14) |
        (x & 0x3FFF); // bit shifting for unique tile ID
    return tileID;
}

function trimCache() {
    if (cachedKeys.size < MAX_CACHED_TILES) {
        cacheIndex = cachedKeys.size;
        return;
    }

    const oldestKey = cachedKeys.keys().next().value; // Get the first key (oldest)
    cacheIndex = cachedKeys.get(oldestKey).slot;
    
    console.log(`Evicting tile ${oldestKey} from cache slot ${cacheIndex}`);
    cachedKeys.delete(oldestKey);
}

function cacheTile(tileID, z, y, x) {
    if (cachedKeys.has(tileID)) {
        return; // Tile is already in cache or being loaded
    }

    if (nTilesLoading >= MAX_CACHED_TILES) {
        return null; // Too many tiles loading, skip this one for now
    }

    trimCache();
    const reservedSlot = cacheIndex;
    cachedKeys.set(tileID, {
        slot: reservedSlot,
        loading: true
    });

    // Tile is not in cache and not being loaded
    nTilesLoading += 1;
    setTimeout(() => {
        loadImage(`map/${z}/${y}/${x}.png`, (img) => {
            img.loadPixels();

            const arrayIndex = reservedSlot * TILE_PIXEL_COUNT;
            if (cachedKeys.has(tileID)) { // Tile wasn't evicted while loading
                cachedKeys.get(tileID).loading = false;

                tileCache.set(img.pixels, arrayIndex);  

                nTilesLoading -= 1;
            }
        });
    }, LOAD_TILE_DELAY);
}

function getTile(tileID) {
    // Tile is in cache;
    if (cachedKeys.has(tileID)) {
        const entry = cachedKeys.get(tileID);
        if (entry.loading) return null; // Tile is still loading

        // Update last used for LRU eviction
        cachedKeys.delete(tileID);
        cachedKeys.set(tileID, {
            slot: entry.slot,
            loading: false
        });

        const arrayIndex = entry.slot * TILE_SIZE * TILE_SIZE * 4;
        return tileCache.subarray(arrayIndex, arrayIndex + TILE_PIXEL_COUNT);
    }

    return null;
}

function rebuildCacheRequests(zoomLevel, panX, panY) {
    const level = 2 ** zoomLevel;
    const tileSize = Math.max(height, width) / 2 / level;
    
    // Calculate which tiles would be visible if we were zoomed in near the crosshair (with an offset to pre-load neighboring tiles)
    const offset = 1;
    const startXNN = Math.max(0, Math.floor((-panX+width/2 - width / 2 / level) / tileSize) - offset);
    const startYNN = Math.max(0, Math.floor((-panY+height/2 - height / 2 / level) / tileSize) - offset);
    const endXNN = Math.min(level * 2, Math.ceil((-panX+width/2 + width / 2 / level) / tileSize) + offset);
    const endYNN = Math.min(level, Math.ceil((-panY+height/2 + height / 2 / level) / tileSize) + offset);

    // Build cache requests
    
    for (let y = startYNN; y < endYNN; y++) {
        for (let x = startXNN; x < endXNN; x++) {
            const distanceToCenter = dist(x*tileSize + tileSize/2, y*tileSize + tileSize/2, -panX+width/2, -panY+height/2);

            cacheRequests.push({z: zoomLevel, y, x, distanceToCenter});
        }
    }
    cacheRequests.sort((a, b) => a.distanceToCenter - b.distanceToCenter); // Sort by distance to center (crosshair)

    // Add zoom out and zoom in tiles at the end of the queue
    if (zoomLevel > 0) {
        const parentZoom = zoomLevel - 1;
        const level = 2 ** parentZoom;
        const tileSize = Math.max(height, width) / 2 / level;

        const startX = Math.max(0, Math.floor((-panX+width/2 - width / 2 / level) / tileSize));
        const startY = Math.max(0, Math.floor((-panY+height/2 - height / 2 / level) / tileSize));
        const endX = Math.min(level * 2, Math.ceil((-panX+width/2 + width / 2 / level) / tileSize));
        const endY = Math.min(level, Math.ceil((-panY+height/2 + height / 2 / level) / tileSize));

        for (let y = startY; y < endY; y++) {
            for (let x = startX; x < endX; x++) {
                cacheRequests.push({z: parentZoom, y, x, distanceToCenter:0});
            }
        }
    }
    if (zoomLevel < MAX_ZOOM_LEVEL) {
        const childZoom = zoomLevel + 1;
        const level = 2 ** childZoom;
        const tileSize = Math.max(height, width) / 2 / level;

        const startX = Math.max(0, Math.floor((-panX+width/2 - width / 2 / level) / tileSize));
        const startY = Math.max(0, Math.floor((-panY+height/2 - height / 2 / level) / tileSize));
        const endX = Math.min(level * 2, Math.ceil((-panX+width/2 + width / 2 / level) / tileSize));
        const endY = Math.min(level, Math.ceil((-panY+height/2 + height / 2 / level) / tileSize));

        for (let y = startY; y < endY; y++) {
            for (let x = startX; x < endX; x++) {
                cacheRequests.push({z: childZoom, y, x, distanceToCenter:0});
            }
        }
    }

    for (const req of cacheRequests) {
        const tileID = getTileID(req.z, req.y, req.x);

        cacheTile(tileID, req.z, req.y, req.x);
    }
}

function mouseWheel(event) {
    const newZoomLevel = zoomLevel + Math.sign(-event.delta);
    if (newZoomLevel >= 0 && newZoomLevel <= MAX_ZOOM_LEVEL) {
        zoomLevel = newZoomLevel;
        movedCamera = true;

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

        movedCamera = true;
        if (nTilesLoading > 0) {
            loadingTimeStart = millis();
            loadingTimeEnd = millis();
        }
    }
}

function mouseReleased() {
    isDragging = false;
}
