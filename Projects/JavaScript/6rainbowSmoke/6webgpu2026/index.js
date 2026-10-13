// Constants
const COARSE_FACTOR = 8; // Cells per thread in the fused distances + argmin pass. 8 measured best: fewer cells leaves more workgroups to reduce, more starves the GPU of workgroups
let WORKGROUP_SIZE = 1024;

// UI
const canvas = document.querySelector("canvas");
const dpr = window.devicePixelRatio || 1;
const cssWidth = canvas.parentElement.clientWidth || window.innerWidth || 512;
const cssHeight = canvas.parentElement.clientHeight || window.innerHeight || 512;

let horizontalResolutionPrompt = parseInt(prompt("Horizontal resolution:", "512"));
if (!Number.isFinite(horizontalResolutionPrompt) || horizontalResolutionPrompt < 3) { horizontalResolutionPrompt = 512; }
const algorithmPrompt = prompt("Type 1 to use the AVERAGE or anything else to use the MINIMUM Rainbow Smoke algorithm variant:", "1");
const colorOrderPrompt = prompt("Type 1 to use RANDOM or anything else for HUE ORDERED colors:", "1");

let hueOffset; let hueDirection;
if (colorOrderPrompt != 1) {
    let huePrompt = parseFloat(prompt("Type any number from -1 to 1 to control hue order", "0.0"));
    if (!Number.isFinite(huePrompt)) { huePrompt = 0; }
    huePrompt = Math.max(-1, Math.min(1, huePrompt));
    hueOffset = Math.abs(huePrompt);
    hueDirection = huePrompt > -0.01 ? 1 : -1;
}

let algorithm;
if (algorithmPrompt == 1) {
    algorithm = "Average";
} else {
    algorithm = "Minimum";
}

let GRID_SIZEx = horizontalResolutionPrompt;
let squareSizeX = cssWidth / GRID_SIZEx;
let GRID_SIZEy = Math.max(3, Math.floor(cssHeight / squareSizeX));

// Size the canvas to the grid so cells stay square, at device pixel resolution
canvas.style.width = `${cssWidth}px`;
canvas.style.height = `${GRID_SIZEy * squareSizeX}px`;
canvas.width = Math.round(cssWidth * dpr);
canvas.height = Math.round(GRID_SIZEy * squareSizeX * dpr);

const nColors = GRID_SIZEx * GRID_SIZEy;

//#region WebGPU initialization
// Checking WebGPU browser support
if (!navigator.gpu) {
    throw new Error("WebGPU not supported on this browser.");
} else { console.log("WebGPU supported.") }

// Requesting GPU adapter
const adapter = await navigator.gpu.requestAdapter();
if (!adapter) { throw new Error("No appropriate GPUAdapter found.") }

// Handling adapter limits
if (adapter.limits.maxComputeWorkgroupSizeX < WORKGROUP_SIZE ||
    adapter.limits.maxComputeInvocationsPerWorkgroup < WORKGROUP_SIZE) {
    WORKGROUP_SIZE = 256;
}
const dispatches = Math.ceil(nColors/WORKGROUP_SIZE/COARSE_FACTOR);
const largestBufferSize = nColors*16; // Neighbor stats, one vec4u per cell
if (adapter.limits.maxStorageBufferBindingSize < largestBufferSize) {
    throw new Error("Storage buffer size not supported.");
}
if (adapter.limits.maxBufferSize < largestBufferSize) {
    throw new Error("Storage buffer size not supported.");
}

// Requesting GPU device
const device = await adapter.requestDevice({
    requiredLimits: {
        maxComputeWorkgroupSizeX: WORKGROUP_SIZE,
        maxComputeInvocationsPerWorkgroup: WORKGROUP_SIZE,
        maxStorageBufferBindingSize: largestBufferSize,
        maxBufferSize: largestBufferSize
    }
});

// Requesting GPU canvas context
const context = canvas.getContext('webgpu');
const canvasFormat = navigator.gpu.getPreferredCanvasFormat();
context.configure({
    device: device,
    format: canvasFormat
});

// Shaders
const cellShaderModule = device.createShaderModule({
    label: "Cell shader",
    code: await loadTextFile("default.wgsl")
});

let fileComputeShader = await loadTextFile("compute.wgsl");
fileComputeShader = fileComputeShader
    .replace("$WORKGROUP_SIZE$", WORKGROUP_SIZE)
    .replace("$COARSE_FACTOR$", COARSE_FACTOR);
const simulationShaderModule = device.createShaderModule({
    label: "Rainbow Smoke shader",
    code: fileComputeShader
});

//#endregion


//#region Data initialization
// Grid uniform |Painting sizes
let gridUniform = new Uint32Array([GRID_SIZEx, GRID_SIZEy]);

// Color pool parameter |colors to be painted
let colorPoolStorage = new Uint32Array(nColors);

// Initially i assumed that, on a large painting, the sampling pattern wouldn't matter, but I was very wrong. Doing it randomly vs evenly sampling colors and then
// randomizing is the proper way
// Build the full RGB cube, shuffle it (Fisher-Yates) and keep the first nColors, so the dropped colors are spread evenly
let nColors1D = Math.ceil(nColors**(1/3));
console.log("Missing colors: ", nColors1D**3-nColors, " out of ",nColors1D**3/1000,"K")
const cube = new Uint32Array(nColors1D**3);
for (let r=0, i=0; r<nColors1D; r++) {
    for (let g=0; g<nColors1D; g++) {
        for (let b=0; b<nColors1D; b++, i++) {
            const [r2, g2, b2] = [Math.round(255*r/(nColors1D-1)), Math.round(255*g/(nColors1D-1)), Math.round(255*b/(nColors1D-1))];
            cube[i] = ((r2 << 24) | (g2 << 16) | (b2 << 8) | 255) >>> 0;
        }
    }
}
for (let i = cube.length - 1; i > 0; i--) {
    const j = Math.floor(Math.random() * (i + 1));
    [cube[i], cube[j]] = [cube[j], cube[i]];
}
colorPoolStorage.set(cube.subarray(0, nColors));

if (colorOrderPrompt != 1) { // Ordered by hue
    const shiftedHue = (c) => (rgbToHue(c >>> 24, (c >>> 16) & 0xFF, (c >>> 8) & 0xFF) - hueOffset + 1) % 1;
    const colors = Array.from(colorPoolStorage, (c) => ({ c, hue: shiftedHue(c) }));
    colors.sort((a, b) => hueDirection*(a.hue - b.hue));
    colors.forEach((entry, i) => { colorPoolStorage[i] = entry.c; });
}

// Cell color storage |painted colors, 0 = not painted
let cellColorStorage = new Uint32Array(nColors);

// Seed
let x = Math.floor(GRID_SIZEx/2); // Middle option
let y = Math.floor(GRID_SIZEy/2); // Middle option
let seedCoord = from2Dto1Dindex(x, y, GRID_SIZEx);
cellColorStorage[seedCoord] = colorPoolStorage[0];

// Neighbor stats |painted neighbors running sums: rgb sums, (sum of |rgb|^2 << 4) | count
// Also the cell state: w = 0 inactive, count > 0 active, w = PAINTED painted
const PAINTED = 0xFFFFFFFF; // Must match compute.wgsl
let neighborStatsStorage = new Uint32Array(nColors*4);
neighborStatsStorage[seedCoord*4 + 3] = PAINTED;
const seedRGB = [colorPoolStorage[0] >>> 24, (colorPoolStorage[0] >>> 16) & 0xFF, (colorPoolStorage[0] >>> 8) & 0xFF];
const seedSumSq = seedRGB[0]**2 + seedRGB[1]**2 + seedRGB[2]**2;
for (const [dx, dy] of [[1,0], [-1,0], [0,1], [0,-1], [1,1], [1,-1], [-1,1], [-1,-1]]) {
    const neighbor = from2Dto1Dindex(x+dx, y+dy, GRID_SIZEx);
    neighborStatsStorage.set([...seedRGB, (seedSumSq << 4) | 1], neighbor*4);
}

function from2Dto1Dindex(x, y, gridX) {
    return y * gridX + x;
}

// Iteration
let iteration = 0;

//#endregion

//#region Buffers initialization & binding
// Defining buffers
let commonBuffers = [
    device.createBuffer({
        label: "X, Y grid size dimensions",
        size: gridUniform.byteLength,
        usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST
    }),
    device.createBuffer({
        label: "Cell color RGB",
        size: cellColorStorage.byteLength,
        usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_DST
    })
]
let computeBuffers = [
    device.createBuffer({
        label: "Color pool",
        size: colorPoolStorage.byteLength,
        usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_DST
    }),
    device.createBuffer({
        label: "Argmin dispatches count",
        size: 4,
        usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST
    }),
    device.createBuffer({
        label: "Iteration",
        size: 4,
        usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_DST
    }),
    device.createBuffer({
        label: "Target color",
        size: 4,
        usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_DST
    }),
    device.createBuffer({
        label: "Argmin result", // (index, distance) per workgroup
        size: dispatches*8,
        usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_DST
    }),
    device.createBuffer({
        label: "Neighbor stats",
        size: neighborStatsStorage.byteLength,
        usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_DST
    })
];

// Writing buffers
device.queue.writeBuffer(commonBuffers[0], 0, gridUniform);
device.queue.writeBuffer(commonBuffers[1], 0, cellColorStorage);

device.queue.writeBuffer(computeBuffers[0], 0, colorPoolStorage);
device.queue.writeBuffer(computeBuffers[1], 0, new Uint32Array([dispatches]));
device.queue.writeBuffer(computeBuffers[3], 0, colorPoolStorage, 1, 1); // First target color (pool[0] is the seed)
device.queue.writeBuffer(computeBuffers[5], 0, neighborStatsStorage);

// Bind layouts
const computeBufferEntry = (binding, type) => ({ binding, visibility: GPUShaderStage.COMPUTE, buffer: { type } });
let bindGroupLayouts = [
    device.createBindGroupLayout({
        label: "Render layout",
        entries: [{
            binding: 0,
            visibility: GPUShaderStage.FRAGMENT,
            buffer: {} // Grid dimensions
        }, {
            binding: 1,
            visibility: GPUShaderStage.FRAGMENT,
            buffer: { type: "read-only-storage" } // Cell color
        }]
    }),
    device.createBindGroupLayout({
        label: "Compute layout",
        entries: [
            computeBufferEntry(0, "uniform"),           // Grid dimensions
            computeBufferEntry(1, "storage"),           // Cell color
            computeBufferEntry(2, "read-only-storage"), // Color pool
            computeBufferEntry(3, "uniform"),           // Argmin dispatches count
            computeBufferEntry(4, "storage"),           // Iteration
            computeBufferEntry(5, "storage"),           // Target color
            computeBufferEntry(6, "storage"),           // Argmin result
            computeBufferEntry(7, "storage")            // Neighbor stats
        ]
    })
];

// Create a bind group
let bindGroups = [
    device.createBindGroup({
        label: "Render bind group",
        layout: bindGroupLayouts[0],
        entries: [{
            binding: 0,
            resource: { buffer: commonBuffers[0] }
        },
        {
            binding: 1,
            resource: { buffer: commonBuffers[1] }
        }]
    }),
    device.createBindGroup({
        label: "Compute bind group",
        layout: bindGroupLayouts[1],
        entries: [...commonBuffers, ...computeBuffers].map((buffer, binding) => ({ binding, resource: { buffer } }))
    })
]
//#endregion

//#region Pipelines
let pipelineLayouts = [
    device.createPipelineLayout({
        label: "Render pipeline layout",
        bindGroupLayouts: [ bindGroupLayouts[0] ]
    }),
    device.createPipelineLayout({
        label: "Compute pipeline layout",
        bindGroupLayouts: [ bindGroupLayouts[1] ]
    })
]

// Render pipelines
let renderCellsPipeline = device.createRenderPipeline({
    label: "Cell pipeline",
    layout: pipelineLayouts[0],
    vertex: {
        module: cellShaderModule,
        entryPoint: "vertexMain"
    },
    fragment: {
        module: cellShaderModule,
        entryPoint: "fragmentMain",
        targets: [{
            format: canvasFormat
        }]
    }
});

// Compute pipelines
let computePipelines = [
    device.createComputePipeline({
        label: "Distances and argmin parallel reduction",
        layout: pipelineLayouts[1],
        compute: {
            module: simulationShaderModule,
            entryPoint: "distancesArgminMain",
            constants: { AVERAGE_METHOD: algorithm === "Average" ? 1 : 0 }
        }
    }),
    device.createComputePipeline({
        label: "Argmin 2nd step, place pixel and advance iteration",
        layout: pipelineLayouts[1],
        compute: {
            module: simulationShaderModule,
            entryPoint: "reduceAndPlaceMain"
        }
    })
]
//#endregion

console.log("Dispatches: ", dispatches);
console.log("Number of colors: ", nColors);

// Compute and render run decoupled: compute is submitted in chunks as fast as the GPU completes them, and each display
// frame just renders whatever state the cells are in. Chunk size adapts so a chunk takes ~COMPUTE_CHUNK_MS of GPU time,
// which bounds how long a render waits behind compute in the queue
const COMPUTE_CHUNK_MS = 12;
const MAX_CHUNKS_IN_FLIGHT = 2; // Keeps the GPU fed while JS submits the next chunk
let iterationsPerChunk = 64;

async function computeLoop() {
    const inFlight = [];
    let lastCompletion = performance.now();
    while (iteration < nColors) {
        const chunkIterations = iterationsPerChunk;
        const encoder = device.createCommandEncoder();
        updateCompute(chunkIterations, encoder);
        device.queue.submit([encoder.finish()]);
        inFlight.push(device.queue.onSubmittedWorkDone());

        if (inFlight.length < MAX_CHUNKS_IN_FLIGHT) { continue; }
        await inFlight.shift();

        // With the queue saturated, the time between chunk completions is the GPU time of a chunk
        const now = performance.now();
        const chunkMs = Math.max(now - lastCompletion, 0.1);
        lastCompletion = now;
        const ideal = chunkIterations * COMPUTE_CHUNK_MS / chunkMs;
        iterationsPerChunk = Math.round(Math.min(Math.max(0.5*iterationsPerChunk + 0.5*ideal, 16), 1e5));
    }
    await Promise.all(inFlight);
    console.log(`Done: ${((performance.now() - startTime)/1000).toFixed(2)} s`);
}

function renderLoop() {
    const encoder = device.createCommandEncoder();
    updateRender(encoder);
    device.queue.submit([encoder.finish()]);

    // Queue order guarantees the frame submitted after the last compute chunk shows the finished painting
    if (iteration < nColors) { requestAnimationFrame(renderLoop); }
}

const startTime = performance.now();
computeLoop();
requestAnimationFrame(renderLoop);

// WebGPU functions
function updateCompute(nTimes, encoder) {
    const computePass = encoder.beginComputePass();
    computePass.setBindGroup(0, bindGroups[1]);
    for (let i=0; i<nTimes; i++) {
        iteration++;
        if (iteration === nColors) { break; }

        /////////////////////////////////////////////////////////////////////////////////////////////
        // COMPUTE distances and their per-workgroup minimum
        computePass.setPipeline(computePipelines[0]);
        computePass.dispatchWorkgroups(dispatches);

        /////////////////////////////////////////////////////////////////////////////////////////////
        // COMPUTE minimum distance, then place pixel, activate neighbors and advance iteration
        computePass.setPipeline(computePipelines[1]);
        computePass.dispatchWorkgroups(1);
    }
    computePass.end();
}

function updateRender(encoder) {
    /////////////////////////////////////////////////////////////////////////////////////////////
    // RENDER cells
    const renderPass = encoder.beginRenderPass({
        colorAttachments: [{
            view: context.getCurrentTexture().createView(),
            loadOp: "clear",
            clearValue: [0,0,0.4,1],
            storeOp: "store"
        }]
    });

    renderPass.setPipeline(renderCellsPipeline);
    renderPass.setBindGroup(0, bindGroups[0]);
    renderPass.draw(3); // Full screen triangle
    renderPass.end();
}

// Functions
async function loadTextFile(filePath) {
    try {
        const response = await fetch(filePath, { cache: "no-cache" }); // Revalidate so shaders never go stale against index.js
        if (!response.ok) { throw new Error('Network response was not ok') }
        const text = await response.text();
        return text;
    } catch (error) {
        console.error('There was a problem fetching the file:', error);
    }
}

function rgbToHue(r, g, b) {
    r /= 255;
    g /= 255;
    b /= 255;

    let max = Math.max(r, g, b), min = Math.min(r, g, b);
    let h;
    
    if (max === min) {
        h = 0; // achromatic
    } else {
        let d = max - min;
        switch(max) {
            case r: h = (g - b) / d + (g < b ? 6 : 0); break;
            case g: h = (b - r) / d + 2; break;
            case b: h = (r - g) / d + 4; break;
        }
        h /= 6;
    }

    return h; // hue is in range [0, 1]
}