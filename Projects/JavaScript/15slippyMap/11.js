// V11, noise warp in the shader: each pixel's sample position is displaced by 3 octaves read from a tileable noise texture (world space, faded out when smaller than a few screen pixels), zoom goes past the last level
const width = window.innerWidth;
const height = window.innerHeight;

let panX = 0;
let panY = 0;
let isDragging = false;
let lastMouseX = 0;
let lastMouseY = 0;

let movedCamera = true;
let cacheRequests = [];

const TILE_SIZE = 256;
const TILE_BORDER = 1; // Extra pixels on each side of a tile file, copied from the neighbouring tiles
const TILE_FILE_SIZE = TILE_SIZE + 2 * TILE_BORDER;
const MAX_TILE_ZOOM = 3; // Deepest folder in map_withborder/
// At zoomLevel 0 a tile of level z covers max(width, height) / 2 / 2^z screen pixels.
// Adding this offset to zoomLevel gives the level whose tiles are shown at exactly 1x
const TILE_ZOOM_OFFSET = Math.log2(Math.max(width, height) / 2 / TILE_SIZE);

const MAX_ZOOM_LEVEL = MAX_TILE_ZOOM + 4; // Past the last level tiles are only magnified, the noise adds the detail

// Noise warp octaves. Cell: size of one noise cell in base (level 3) pixels. Amplitude: displacement in base pixels.
// If the amplitudes add up to more than NOISE_MAX_DISPLACEMENT, the sum is squashed smoothly with
// NOISE_MAX_DISPLACEMENT * tanh(sum / NOISE_MAX_DISPLACEMENT) so it never passes the limit
const NOISE_OCTAVES = [
    {cell: 10, amplitude: 0.25},
    {cell: 1, amplitude: 0.15},
    {cell: 0.1, amplitude: 0.09},
];
// Further than 0.5 base pixels the majority filter would read past the 1 px tile border
const NOISE_MAX_DISPLACEMENT = 0.49;
const NOISE_SEED = 12345;
const NOISE_TEXTURE_SIZE = 256;
const NOISE_CELLS = 32; // Noise cells across the texture, it repeats after that
// Octaves fade in between these sizes of a noise cell in screen (device) pixels, smaller ones would only add speckle
const NOISE_FADE_ENABLED = false; // Disabled for now, every octave always at full strength
const NOISE_FADE_START = 2;
const NOISE_FADE_END = 4;
let noiseImage;
// Zooming out goes below 0 (map smaller than the window) until level 0 is shown at 1x, so every level is reachable
const MIN_ZOOM_LEVEL = -TILE_ZOOM_OFFSET;
let zoomLevel = 0; // Map fills the window
const MAX_CACHED_TILES = 128;
let slotImages = []; // One image per cache slot. p5 keeps a GPU texture per image, re-uploaded only when the slot gets a new tile
let cachedKeys = new Map();
// cacheIndex replaced by trimCache returning slot directly
const LOAD_TILE_DELAY = 100;
let nTilesLoading = 0;
let loadingTimeStart = 0;
let loadingTimeEnd = 0;

let zoomed = true;

let majorityShader;
let hud;

const VERTEX_SHADER = `
precision highp float;
attribute vec3 aPosition;
attribute vec2 aTexCoord;
uniform mat4 uModelViewMatrix;
uniform mat4 uProjectionMatrix;
varying vec2 vTexCoord;

void main() {
    vTexCoord = aTexCoord;
    gl_Position = uProjectionMatrix * uModelViewMatrix * vec4(aPosition, 1.0);
}`;

// Majority (discrete bilinear) interpolation: weigh the 4 nearest texels like bilinear, but instead of
// averaging colours pick the colour with the most weight. Ties go to the first one: up-left, up-right, bottom-left, bottom-right
const FRAGMENT_SHADER = `
precision highp float;
varying vec2 vTexCoord;
uniform sampler2D tile;
uniform sampler2D noiseMap;
uniform float tilePxToBase;  // Base pixels per tile pixel
uniform float basePixelSize; // Screen (device) pixels per base pixel
uniform vec2 octaveOrigin0;  // Tile origin in noise texture units per octave, wrapped on the CPU so floats stay small
uniform vec2 octaveOrigin1;
uniform vec2 octaveOrigin2;
const float NOISE_CELLS = ${NOISE_CELLS.toFixed(1)};
const float MAX_DISPLACEMENT = ${NOISE_MAX_DISPLACEMENT.toFixed(4)};
const float TILE_SIZE = ${TILE_SIZE.toFixed(1)};
const float TILE_FILE_SIZE = ${TILE_FILE_SIZE.toFixed(1)};
const float TILE_BORDER = ${TILE_BORDER.toFixed(1)};

vec4 texel(vec2 index) {
    return texture2D(tile, (index + 0.5) / TILE_FILE_SIZE);
}

float same(vec4 a, vec4 b) {
    return all(lessThan(abs(a - b), vec4(0.5 / 255.0))) ? 1.0 : 0.0;
}

// Displacement in base pixels from one octave of the noise texture (R = x, G = y)
vec2 octave(vec2 localBase, vec2 origin, float cell, float amplitude, vec2 offset) {
    float fade = ${NOISE_FADE_ENABLED ? `smoothstep(${NOISE_FADE_START.toFixed(1)}, ${NOISE_FADE_END.toFixed(1)}, cell * basePixelSize)` : '1.0'};
    if (fade == 0.0) return vec2(0.0);
    vec2 n = texture2D(noiseMap, origin + offset + localBase / (cell * NOISE_CELLS)).rg;
    return (n * 2.0 - 1.0) * amplitude * fade;
}

void main() {
    // Warp: displace the position this pixel samples, in world space so it lines up across tiles and zoom levels
    vec2 local = vTexCoord * TILE_SIZE;
    vec2 localBase = local * tilePxToBase;
    vec2 displacement =
        octave(localBase, octaveOrigin0, ${NOISE_OCTAVES[0].cell.toFixed(4)}, ${NOISE_OCTAVES[0].amplitude.toFixed(4)}, vec2(0.0, 0.0)) +
        octave(localBase, octaveOrigin1, ${NOISE_OCTAVES[1].cell.toFixed(4)}, ${NOISE_OCTAVES[1].amplitude.toFixed(4)}, vec2(0.37, 0.61)) +
        octave(localBase, octaveOrigin2, ${NOISE_OCTAVES[2].cell.toFixed(4)}, ${NOISE_OCTAVES[2].amplitude.toFixed(4)}, vec2(0.73, 0.19));
${NOISE_OCTAVES.reduce((sum, o) => sum + o.amplitude, 0) > NOISE_MAX_DISPLACEMENT ? `
    // Soft limit (tanh, which GLSL ES 1.0 doesn't have)
    vec2 e = exp(2.0 * clamp(displacement / MAX_DISPLACEMENT, -10.0, 10.0));
    displacement = MAX_DISPLACEMENT * (e - 1.0) / (e + 1.0);` : '    // Amplitudes add up to less than the limit, no soft limit needed'}

    // Position in tile file pixels, with pixel centres at integers. Displacement is under 0.5 base pixels
    // (even less in tile pixels at lower levels), so this reads at most 1 pixel into the border
    vec2 p = local - 0.5 + TILE_BORDER + displacement / tilePxToBase;
    vec2 corner = floor(p);
    vec2 f = p - corner;

    vec4 c0 = texel(corner);
    vec4 c1 = texel(corner + vec2(1.0, 0.0));
    vec4 c2 = texel(corner + vec2(0.0, 1.0));
    vec4 c3 = texel(corner + vec2(1.0, 1.0));
    float w0 = (1.0 - f.x) * (1.0 - f.y);
    float w1 = f.x * (1.0 - f.y);
    float w2 = (1.0 - f.x) * f.y;
    float w3 = f.x * f.y;

    // Score of a candidate = weights of all corners with its colour
    float s0 = w0 + same(c1, c0) * w1 + same(c2, c0) * w2 + same(c3, c0) * w3;
    float s1 = w1 + same(c0, c1) * w0 + same(c2, c1) * w2 + same(c3, c1) * w3;
    float s2 = w2 + same(c0, c2) * w0 + same(c1, c2) * w1 + same(c3, c2) * w3;
    float s3 = w3 + same(c0, c3) * w0 + same(c1, c3) * w1 + same(c2, c3) * w2;

    vec4 best = c0;
    float bestScore = s0;
    if (s1 > bestScore) { best = c1; bestScore = s1; }
    if (s2 > bestScore) { best = c2; bestScore = s2; }
    if (s3 > bestScore) { best = c3; }
    gl_FragColor = best;
}`;

function setup() {
    // Create a canvas with the same dimensions as the window
    createCanvas(width, height, WEBGL);
    majorityShader = createShader(VERTEX_SHADER, FRAGMENT_SHADER);

    for (let i = 0; i < MAX_CACHED_TILES; i++) {
        const img = createImage(TILE_FILE_SIZE, TILE_FILE_SIZE);
        // The shader picks exact texels itself, so no GPU blending
        p5.instance._renderer.getTexture(img).setInterpolation(NEAREST, NEAREST);
        slotImages.push(img);
    }

    // Noise values are smooth numbers, so here the GPU's bilinear filter is exactly right
    noiseImage = makeNoiseImage(NOISE_SEED);
    const noiseTexture = p5.instance._renderer.getTexture(noiseImage);
    noiseTexture.setInterpolation(LINEAR, LINEAR);
    noiseTexture.setWrapMode(REPEAT, REPEAT);

    // WEBGL text needs a loaded font, so the HUD is an HTML overlay
    hud = createDiv('');
    hud.position(10, 10);
    hud.style('color', '#fafafa');
    hud.style('font', '16px sans-serif');
    hud.style('text-shadow', '0 0 3px #000, 0 0 3px #000');
    hud.style('pointer-events', 'none');
    hud.style('line-height', '20px');

    let button = createButton(`Zoom: ${zoomed ? 'ON' : 'OFF'}`);
    button.position(20, height - 40);
    button.mousePressed(() => {
        const level = 2 ** zoomLevel;
        if (zoomed) {
            // Switching FROM zoomed TO non-zoomed: scale pan down to world coords
            panX /= level;
            panY /= level;
        } else {
            // Switching FROM non-zoomed TO zoomed: scale pan up to zoomed coords
            panX *= level;
            panY *= level;
        }
        zoomed = !zoomed;
        button.html(`Zoom: ${zoomed ? 'ON' : 'OFF'}`);
        movedCamera = true;
    });
}

function draw() {
    // Clear the canvas and set a dark background
    background(0);
    // Everything is flat and drawn in order, the depth test would only make later shapes fight earlier ones
    drawingContext.disable(drawingContext.DEPTH_TEST);
    // WEBGL puts the origin at the centre, move it to the top left like the 2D canvas
    translate(-width / 2, -height / 2);

    // Save current transformation state
    push();
    
    // Apply pan offset
    translate(panX, panY);

    // Draw the zoomed-in images
    // Use the most detailed level that is still magnified (1x-2x), so nearest neighbour never skips pixels
    const tileZoom = constrain(Math.floor(zoomLevel + TILE_ZOOM_OFFSET), 0, MAX_TILE_ZOOM);

    const tileLevel = 2 ** tileZoom;
    const level = 2 ** zoomLevel;
    const tileSize = Math.max(height, width) / 2 / tileLevel;

    let worldPanX = panX;
    let worldPanY = panY;
    if (zoomed) {
        worldPanX /= level;
        worldPanY /= level;
    }
    
    // Which tile is the crosshair pointing to
    let xTile = Math.floor((-worldPanX +width/2) / tileSize);
    let yTile = Math.floor((-worldPanY +height/2) / tileSize);

    if (movedCamera) {
        cacheRequests = [];
        rebuildCacheRequests(tileZoom, level, worldPanX, worldPanY);
        movedCamera = false;
    }

    // Calculate which tiles would be visible if we were zoomed in near the crosshair
    const startX = Math.max(0, Math.floor((-worldPanX +width/2 - width / 2 / level) / tileSize));
    const startY = Math.max(0, Math.floor((-worldPanY +height/2 - height / 2 / level) / tileSize));
    const endX = Math.min(tileLevel * 2, Math.ceil((-worldPanX +width/2 + width / 2 / level) / tileSize));
    const endY = Math.min(tileLevel, Math.ceil((-worldPanY +height/2 + height / 2 / level) / tileSize));

    if (zoomed) {
        const trueTileSize = tileSize * level;
        // Draw visible tiles
        startTileDrawing();
        for (let y = startY; y < endY; y++) {
            for (let x = startX; x < endX; x++) {
                const tileID = getTileID(tileZoom, y, x);

                const tileImage = getTile(tileID);
                if (tileImage) {
                    drawTile(tileImage, tileZoom, x, y, x * trueTileSize -width/2*(level-1), y * trueTileSize -height/2*(level-1), trueTileSize);
                }
            }
        }
        resetShader();

        // Draw tile borders
        for (let y = 0; y < tileLevel; y++) {
            for (let x = 0; x < tileLevel*2; x++) {
                stroke(0, 255, 0); // Green borders
                strokeWeight(2);
                noFill();
                rect(x * trueTileSize -width/2*(level-1), y * trueTileSize -height/2*(level-1), trueTileSize, trueTileSize);
            }
        }

        // Draw a red rectangle of what would be the visible area if we were zoomed in
        stroke(255, 0, 0); // Red borders
        strokeWeight(2);
        noFill();
        rect(-panX+width/2 - width / 2, -panY+height/2 - height / 2, width, height);
    } else {
        const drawnTiles = new Set();
        // Draw visible tiles
        startTileDrawing();
        for (let y = startY; y < endY; y++) {
            for (let x = startX; x < endX; x++) {
                const tileID = getTileID(tileZoom, y, x);
                const tileImage = getTile(tileID);
                if (tileImage) {
                    drawTile(tileImage, tileZoom, x, y, x * tileSize, y * tileSize, tileSize);
                    drawnTiles.add(tileID);  // ← track it
                }
            }
        }
        resetShader();

        // Draw tile borders
        for (let y = 0; y < tileLevel; y++) {
            for (let x = 0; x < tileLevel*2; x++) {
                stroke(0, 255, 0); // Green borders
                strokeWeight(2);
                noFill();
                rect(x * tileSize, y * tileSize, tileSize, tileSize);
            }
        }

        // Debug: paint cached tiles by status
        noStroke();
        for (let y = 0; y < tileLevel; y++) {
            for (let x = 0; x < tileLevel * 2; x++) {
                const tileID = getTileID(tileZoom, y, x);
                const entry = cachedKeys.get(tileID);
                if (entry && entry.loading) {
                    fill(180, 0, 180, 150); // Purple = loading
                    rect(x * tileSize, y * tileSize, tileSize, tileSize);
                } else if (entry && !drawnTiles.has(tileID)) {
                    fill(0, 255, 0, 150);   // Green = cached but not drawn
                    rect(x * tileSize, y * tileSize, tileSize, tileSize);
                }
            }
        }

        // Draw a red rectangle of what would be the visible area if we were zoomed in
        stroke(255, 0, 0); // Red borders
        strokeWeight(2);
        noFill();
        rect(-panX+width/2 - width / 2 / level, -panY+height/2 - height / 2 / level, width / level, height / level);

        // Paint tiles that are loading or part of the cache but not drawn yet

    }
    

    // Restore transformation state. UI after this
    pop();

    // Draw center crosshair
    stroke(255, 0, 0); // Red
    strokeWeight(2);
    line(width / 2 - 10, height / 2, width / 2 + 10, height / 2); // Horizontal
    line(width / 2, height / 2 - 10, width / 2, height / 2 + 10); // Vertical

    // FPS counter
    if (nTilesLoading > 0) {
        loadingTimeEnd = millis();
    }
    hud.html([
        `FPS: ${frameRate().toFixed(1)}`,
        `Pan: (${panX.toFixed(0)}, ${panY.toFixed(0)})`,
        `xtile: ${xTile}, ytile: ${yTile}, zoom: ${zoomLevel.toFixed(2)}, tile zoom: ${tileZoom}, tile scale: ${(tileSize * level / TILE_SIZE).toFixed(2)}x`,
        `Noise octaves (cell ${NOISE_OCTAVES.map(o => o.cell).join(' / ')} px): ${noiseFades((zoomed ? tileSize * level : tileSize) / TILE_SIZE / 2 ** (MAX_TILE_ZOOM - tileZoom) * pixelDensity()).map(f => `${Math.round(f * 100)}%`).join(' / ')}`,
        `Ntiles loading: ${nTilesLoading}, Loading time: ${((loadingTimeEnd - loadingTimeStart)).toFixed(0)}ms, Cache requests: ${cacheRequests.length}`,
    ].join('<br>'));
}

function startTileDrawing() {
    shader(majorityShader);
    noStroke();
    fill(255); // rect() only runs the shader when filled
}

function drawTile(tileImage, z, tileX, tileY, x, y, size) {
    const tilePxToBase = 2 ** (MAX_TILE_ZOOM - z);
    majorityShader.setUniform('tile', tileImage);
    majorityShader.setUniform('noiseMap', noiseImage);
    majorityShader.setUniform('tilePxToBase', tilePxToBase);
    majorityShader.setUniform('basePixelSize', size / TILE_SIZE / tilePxToBase * pixelDensity());
    // Tile origin in noise texture units, wrapped here in double precision (the texture repeats anyway)
    NOISE_OCTAVES.forEach((o, i) => {
        const period = o.cell * NOISE_CELLS;
        const ox = tileX * TILE_SIZE * tilePxToBase / period;
        const oy = tileY * TILE_SIZE * tilePxToBase / period;
        majorityShader.setUniform(`octaveOrigin${i}`, [ox - Math.floor(ox), oy - Math.floor(oy)]);
    });
    rect(x, y, size, size);
}

function noiseFades(basePixelSize) {
    // Same fade as the shader, for the HUD
    return NOISE_OCTAVES.map(o => {
        if (!NOISE_FADE_ENABLED) return 1;
        const t = constrain((o.cell * basePixelSize - NOISE_FADE_START) / (NOISE_FADE_END - NOISE_FADE_START), 0, 1);
        return t * t * (3 - 2 * t);
    });
}

function makeNoiseImage(seed) {
    // Tileable value noise: random values on a NOISE_CELLS x NOISE_CELLS lattice that wraps around,
    // smoothly interpolated. R = x displacement, G = y displacement
    let state = seed >>> 0;
    const random = () => { // mulberry32, seeded so game logic can rebuild the exact same texture
        state = (state + 0x6D2B79F5) >>> 0;
        let t = state;
        t = Math.imul(t ^ (t >>> 15), t | 1);
        t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
        return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
    };
    const latticeX = Float32Array.from({length: NOISE_CELLS * NOISE_CELLS}, random);
    const latticeY = Float32Array.from({length: NOISE_CELLS * NOISE_CELLS}, random);
    const smooth = (t) => t * t * t * (t * (t * 6 - 15) + 10); // Quintic, no creases at the lattice

    const img = createImage(NOISE_TEXTURE_SIZE, NOISE_TEXTURE_SIZE);
    img.loadPixels();
    const texelsPerCell = NOISE_TEXTURE_SIZE / NOISE_CELLS;
    for (let j = 0; j < NOISE_TEXTURE_SIZE; j++) {
        for (let i = 0; i < NOISE_TEXTURE_SIZE; i++) {
            const u = (i + 0.5) / texelsPerCell;
            const v = (j + 0.5) / texelsPerCell;
            const x0 = Math.floor(u) % NOISE_CELLS, x1 = (x0 + 1) % NOISE_CELLS;
            const y0 = Math.floor(v) % NOISE_CELLS, y1 = (y0 + 1) % NOISE_CELLS;
            const fx = smooth(u - Math.floor(u));
            const fy = smooth(v - Math.floor(v));
            const sample = (lattice) => {
                const top = lattice[y0 * NOISE_CELLS + x0] * (1 - fx) + lattice[y0 * NOISE_CELLS + x1] * fx;
                const bottom = lattice[y1 * NOISE_CELLS + x0] * (1 - fx) + lattice[y1 * NOISE_CELLS + x1] * fx;
                return top * (1 - fy) + bottom * fy;
            };
            const index = (j * NOISE_TEXTURE_SIZE + i) * 4;
            img.pixels[index] = Math.round(sample(latticeX) * 255);
            img.pixels[index + 1] = Math.round(sample(latticeY) * 255);
            img.pixels[index + 2] = 0;
            img.pixels[index + 3] = 255;
        }
    }
    img.updatePixels();
    return img;
}

function getTileID(z, y, x) {
    const tileID =
        ((z & 0xF) << 28) |
        ((y & 0x3FFF) << 14) |
        (x & 0x3FFF); // bit shifting for unique tile ID
    return tileID;
}

function trimCache() {
    // If cache is full, evict the oldest entry and reuse its slot
    if (cachedKeys.size >= MAX_CACHED_TILES) {
        const oldestKey = cachedKeys.keys().next().value; // Get the first key (oldest)
        const evictedSlot = cachedKeys.get(oldestKey).slot;
        console.log(`Evicting tile ${oldestKey} from cache slot ${evictedSlot}`);
        cachedKeys.delete(oldestKey);
        return evictedSlot;
    }

    // Cache not yet full — find the first unused slot (O(n) but MAX_CACHED_TILES is small)
    const usedSlots = new Set();
    for (const [, entry] of cachedKeys) {
        usedSlots.add(entry.slot);
    }
    for (let i = 0; i < MAX_CACHED_TILES; i++) {
        if (!usedSlots.has(i)) return i;
    }

    // Fallback (shouldn't reach here)
    const oldestKey = cachedKeys.keys().next().value;
    const evictedSlot = cachedKeys.get(oldestKey).slot;
    cachedKeys.delete(oldestKey);
    return evictedSlot;
}

function cacheTile(tileID, z, y, x) {
    if (cachedKeys.has(tileID)) {
        return; // Tile is already in cache or being loaded
    }

    if (nTilesLoading >= MAX_CACHED_TILES) {
        return null; // Too many tiles loading, skip this one for now
    }

    const reservedSlot = trimCache();
    cachedKeys.set(tileID, {
        slot: reservedSlot,
        loading: true
    });

    nTilesLoading += 1;
    setTimeout(() => {
        loadImage(`map_withborder/${z}/${y}/${x}.png`,
            (img) => { // Success callback
                // Tile wasn't evicted while loading. Also check the slot: if it was evicted and requested again,
                // it has a new slot and this one may already belong to another tile
                if (cachedKeys.get(tileID)?.slot === reservedSlot) {
                    const slotImage = slotImages[reservedSlot];
                    slotImage.drawingContext.drawImage(img.canvas, 0, 0);
                    slotImage.setModified(true); // Uploaded to its texture the next time it is drawn
                    cachedKeys.get(tileID).loading = false;
                }
                nTilesLoading -= 1;
            },
            () => { // Failure callback — image missing or load error
                cachedKeys.delete(tileID);
                nTilesLoading -= 1;
            }
        );
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

        return slotImages[entry.slot];
    }

    return null;
}

function rebuildCacheRequests(tileZoom, trueLevel, panX, panY) {
    // Add zoom out tiles at the start of the queue
    if (tileZoom > 0) {
        const parentZoom = tileZoom - 1;
        const level = 2 ** parentZoom;
        const tileSize = Math.max(height, width) / 2 / level;

        const startX = Math.max(0, Math.floor((-panX+width/2 - width / 2 / level) / tileSize));
        const startY = Math.max(0, Math.floor((-panY+height/2 - height / 2 / level) / tileSize));
        const endX = Math.min(level * 2, Math.ceil((-panX+width/2 + width / 2 / level) / tileSize));
        const endY = Math.min(level, Math.ceil((-panY+height/2 + height / 2 / level) / tileSize));

        for (let y = startY; y < endY; y++) {
            for (let x = startX; x < endX; x++) {
                cacheRequests.unshift({z: parentZoom, y, x, distanceToCenter:0});
            }
        }
    }
    
    const tileLevel = 2 ** tileZoom;
    const tileSize = Math.max(height, width) / 2 / tileLevel;

    // Calculate which tiles would be visible if we were zoomed in near the crosshair (with an offset to pre-load neighboring tiles)
    const offset = 1;
    const startXNN = Math.max(0, Math.floor((-panX+width/2 - width / 2 / trueLevel) / tileSize) - offset);
    const startYNN = Math.max(0, Math.floor((-panY+height/2 - height / 2 / trueLevel) / tileSize) - offset);
    const endXNN = Math.min(tileLevel * 2, Math.ceil((-panX+width/2 + width / 2 / trueLevel) / tileSize) + offset);
    const endYNN = Math.min(tileLevel, Math.ceil((-panY+height/2 + height / 2 / trueLevel) / tileSize) + offset);

    // Build cache requests
    
    for (let y = startYNN; y < endYNN; y++) {
        for (let x = startXNN; x < endXNN; x++) {
            const distanceToCenter = dist(x*tileSize + tileSize/2, y*tileSize + tileSize/2, -panX+width/2, -panY+height/2);

            cacheRequests.push({z: tileZoom, y, x, distanceToCenter});
        }
    }
    cacheRequests.sort((a, b) => a.distanceToCenter - b.distanceToCenter); // Sort by distance to center (crosshair)

    // Add zoom in tiles at the end of the queue
    if (tileZoom < MAX_TILE_ZOOM && zoomLevel + TILE_ZOOM_OFFSET - tileZoom > 0.49) {
        const childZoom = tileZoom + 1;
        const level = 2 ** childZoom;
        const tileSize = Math.max(height, width) / 2 / level;

        const startX = Math.max(0, Math.floor((-panX+width/2 - width / 2 / trueLevel) / tileSize));
        const startY = Math.max(0, Math.floor((-panY+height/2 - height / 2 / trueLevel) / tileSize));
        const endX = Math.min(level * 2, Math.ceil((-panX+width/2 + width / 2 / trueLevel) / tileSize));
        const endY = Math.min(level, Math.ceil((-panY+height/2 + height / 2 / trueLevel) / tileSize));

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
    const sensitivity = 0.1;
    const newZoomLevel = constrain(zoomLevel - Math.sign(event.delta) * sensitivity, MIN_ZOOM_LEVEL, MAX_ZOOM_LEVEL);
        
    const levelOld = 2 ** zoomLevel;

    movedCamera = true;
    zoomLevel = newZoomLevel;

    // Fix pan
    const levelNew = 2 ** zoomLevel;
    if (zoomed) {
        panX = panX / levelOld * levelNew;
        panY = panY / levelOld * levelNew;
    }

    loadingTimeStart = millis();
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
        if (nTilesLoading === 0) {
            // Starting a fresh batch of loads — reset the timer
            loadingTimeStart = millis();
        }
        loadingTimeEnd = millis();
    }
}

function mouseReleased() {
    isDragging = false;
}

// Touchscreen support
let touchStartDist = 0;     // distance between fingers when pinch starts
let touchStartZoom = 0;     // zoomLevel when pinch started
let touchStartPanX = 0;     // panX when pinch started
let touchStartPanY = 0;     // panY when pinch started
let pinchMidX = 0;          // midpoint X between fingers
let pinchMidY = 0;          // midpoint Y between fingers

function touchStarted() {
    if (touches.length === 1) {
        isDragging = true;
        lastMouseX = touches[0].x;
        lastMouseY = touches[0].y;
    } else if (touches.length === 2) {
        const dx = touches[0].x - touches[1].x;
        const dy = touches[0].y - touches[1].y;
        touchStartDist = Math.sqrt(dx * dx + dy * dy);
        touchStartZoom = zoomLevel;
        touchStartPanX = panX;
        touchStartPanY = panY;
        pinchMidX = (touches[0].x + touches[1].x) / 2;
        pinchMidY = (touches[0].y + touches[1].y) / 2;
    }
    return false; // prevent default scroll/zoom
}

function touchMoved() {
    if (touches.length === 1 && isDragging) {
        panX += touches[0].x - lastMouseX;
        panY += touches[0].y - lastMouseY;
        lastMouseX = touches[0].x;
        lastMouseY = touches[0].y;
        movedCamera = true;
    } else if (touches.length === 2) {
        const dx = touches[0].x - touches[1].x;
        const dy = touches[0].y - touches[1].y;
        const dist = Math.sqrt(dx * dx + dy * dy);

        const ratio = dist / touchStartDist;
        const levelOld = 2 ** zoomLevel;
        zoomLevel = constrain(
            touchStartZoom + Math.log2(ratio),
            MIN_ZOOM_LEVEL, MAX_ZOOM_LEVEL
        );

        // Fix pan so zoom is centered (same as mouse wheel)
        const levelNew = 2 ** zoomLevel;
        if (zoomed) {
            panX = panX / levelOld * levelNew;
            panY = panY / levelOld * levelNew;
        }

        movedCamera = true;
    }
    return false;
}

function touchEnded() {
    isDragging = false;
    return false;
}