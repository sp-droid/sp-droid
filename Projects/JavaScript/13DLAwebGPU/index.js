// Constants
const WORKGROUP_SIZE2D = 8;

// UI
const canvas = document.querySelector("canvas");
canvas.width = canvas.parentElement.clientWidth;
canvas.height = canvas.parentElement.clientHeight;

const horizontalResolutionPrompt = parseInt(prompt("Horizontal resolution:", "56"));
const nSeedsPrompt = parseInt(prompt("Number of seeds:", "10"));

let GRID_SIZEx = horizontalResolutionPrompt;
let squareSizeX = canvas.width / GRID_SIZEx;
let GRID_SIZEy = Math.floor(canvas.height / squareSizeX);

const nCells = GRID_SIZEx * GRID_SIZEy;

//#region WebGPU initialization
// Constants

// Checking WebGPU browser support
if (!navigator.gpu) {
    throw new Error("WebGPU not supported on this browser.");
} else { console.log("WebGPU supported.") }

// Requesting GPU adapter
const adapter = await navigator.gpu.requestAdapter();
if (!adapter) { throw new Error("No appropriate GPUAdapter found.") }

// Handling adapter limits
if (adapter.limits.maxStorageBufferBindingSize < nCells*4) {
    throw new Error("Storage buffer size not supported.");
}
if (adapter.limits.maxBufferSize < nCells*4) {
    throw new Error("Storage buffer size not supported.");
}

// Requesting GPU device
const device = await adapter.requestDevice({
    requiredLimits: {
        maxStorageBufferBindingSize: nCells*4,
        maxBufferSize: nCells*4
    }
});

// Requesting GPU canvas context
const context = canvas.getContext('webgpu');
const canvasFormat = navigator.gpu.getPreferredCanvasFormat();
context.configure({
    device: device,
    format: canvasFormat
});

// Defining a square as two triangles
const pos = 1.00;
const vertices = new Float32Array([
    -pos, -pos,
     pos, -pos,
     pos,  pos,

    -pos, -pos,
     pos,  pos,
    -pos,  pos
]);
// Allocating GPU memory for the vertices using a vertex buffer
const vertexBuffer = device.createBuffer({
    label: "Cell vertices", // Buffer label
    size: vertices.byteLength, // Buffer byte size
    // (32bit = 4 bytes) 6 2D-vertices = 12 float32 = 12 * 4 bytes = 48 bytes
    usage: GPUBufferUsage.VERTEX | GPUBufferUsage.COPY_DST // two flags, one to declare
    // it's going to be used for vertex data and the other to be able to copy data into it
});
// Writing GPU memory
device.queue.writeBuffer(vertexBuffer, /*bufferOffset=*/0, vertices);
// Defining the structure of the GPU memory object.
const vertexBufferLayout = {
    arrayStride: 8, // Vertex byte size. float32 * 2 = 8 bytes
    attributes: [{
        format: "float32x2", // Vertex byte structure
        offset: 0, // After how many bytes does the attribute starts
        shaderLocation: 0 // Unique int between 0 and 15
    }]
};

// Shaders
const cellShaderModule = device.createShaderModule({
    label: "Cell shader",
    code: await loadTextFile("default.wgsl")
});
const computeShaderModule = device.createShaderModule({
    label: "Compute shader",
    code: await loadTextFile("compute.wgsl")
});

//#endregion


//#region Data initialization
// Grid uniform |Painting sizes
const MAX_UINT32 = 4294967295;
let dataGridSize = new Uint32Array([GRID_SIZEx, GRID_SIZEy]);

// Color pool parameter |colors to be painted
const nColors = 1000;
let dataCellColorPool = new Uint32Array(nColors);

for (let i=0; i<nColors; i++) {
    const [r, g, b] = evaluate_cmap(i/nColors, 'binary', true); //gist_rainbow //binary
    dataCellColorPool[i] = (r << 24) | (g << 16) | (b << 8) | 255;
}

let dataCellState = new Uint32Array(nCells);
let dataCellColor = new Uint32Array(nCells);
let dataCellFrozenParentProgeny = new Uint32Array(nCells*2);

const nSeeds = nSeedsPrompt;
for (let i=0; i<nSeeds; i++) {
    let index1D;
    if (i == 0) {
        const x = Math.floor(GRID_SIZEx/2);
        const y = Math.floor(GRID_SIZEy/2);
        index1D = y * GRID_SIZEx + x;
    } else {
        index1D = Math.floor(Math.random() * nCells);
    }
    dataCellState[index1D] = 2;
    dataCellColor[index1D] = dataCellColorPool[i];
    dataCellFrozenParentProgeny[index1D*2] = MAX_UINT32;
}

let randomInts = new Uint32Array(nCells);
for (let i=0; i<nCells; i++) {
    randomInts[i] = Math.round(Math.random() * MAX_UINT32);
}

//#endregion

//#region Buffers initialization & binding
// Defining buffers
const bufferUniformGrid = device.createBuffer({
    label: "X, Y grid size dimensions",
    size: 4*2,
    usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST
});
const bufferStorageCellColor = device.createBuffer({
    label: "Cell packed color RGB",
    size: nCells*4,
    usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_DST | GPUBufferUsage.COPY_SRC
});
const bufferStorageCellColorPool = device.createBuffer({
    label: "Possible RGBs, packed",
    size: nColors*4,
    usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_DST
});
const bufferStorageCellState = device.createBuffer({
    label: "Cell state",
    size: nCells*4,
    usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_DST
});
const bufferStorageFrozenParentProgeny = device.createBuffer({
    label: "Frozen parent and progeny",
    size: nCells*4*2,
    usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_DST
});
const bufferStorageProgressMod = device.createBuffer({
    label: "Progress modifier",
    size: 4,
    usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_DST | GPUBufferUsage.COPY_SRC
});
const bufferStorageRandom = device.createBuffer({
    label: "Random numbers",
    size: nCells*4,
    usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_DST
});
const bufferatomicMaxProgeny = device.createBuffer({
    label: "Atomic counter",
    size: 4,
    usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_DST | GPUBufferUsage.COPY_SRC
});
const bufferStorageMaxProgenyHistory = device.createBuffer({
    label: "Max progeny history",
    size: 20*4,
    usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_DST
});

// Writing buffers
device.queue.writeBuffer(bufferUniformGrid, 0, dataGridSize);
device.queue.writeBuffer(bufferStorageCellColor, 0, dataCellColor);
device.queue.writeBuffer(bufferStorageCellColorPool, 0, dataCellColorPool);
device.queue.writeBuffer(bufferStorageCellState, 0, dataCellState);
device.queue.writeBuffer(bufferStorageFrozenParentProgeny, 0, dataCellFrozenParentProgeny);
device.queue.writeBuffer(bufferStorageRandom, 0, randomInts);
device.queue.writeBuffer(bufferatomicMaxProgeny, 0, new Uint32Array([nSeeds]));
let progressModifier = 60;
device.queue.writeBuffer(bufferStorageProgressMod, 0, new Uint32Array([progressModifier]));

// Bind layouts
const bindGroupLayoutRender = device.createBindGroupLayout({
    label: "Render layout",
    entries: [{
        binding: 0,
        visibility: GPUShaderStage.VERTEX,
        buffer: {} // Grid dimensions
    }, {
        binding: 1,
        visibility: GPUShaderStage.VERTEX,
        buffer: { type: "read-only-storage" } // Cell color
    }]
})

const bindGroupLayoutCompute = device.createBindGroupLayout({
    label: "Compute layout",
    entries: [{
        binding: 0,
        visibility: GPUShaderStage.COMPUTE,
        buffer: {} // Grid dimensions
    }, {
        binding: 1,
        visibility: GPUShaderStage.COMPUTE,
        buffer: { type: "storage" } // Cell color
    }, {
        binding: 2,
        visibility: GPUShaderStage.COMPUTE,
        buffer: { type: "read-only-storage" } // Cell color pool
    }, {
        binding: 3,
        visibility: GPUShaderStage.COMPUTE,
        buffer: { type: "storage" } // Cell state
    }, {
        binding: 4,
        visibility: GPUShaderStage.COMPUTE,
        buffer: { type: "storage" } // Frozen parent and progeny
    }, {
        binding: 5,
        visibility: GPUShaderStage.COMPUTE,
        buffer: { type: "storage" } // Progress modifier
    }, {
        binding: 6,
        visibility: GPUShaderStage.COMPUTE,
        buffer: { type: "storage" } // Random numbers
    }, {
        binding: 7,
        visibility: GPUShaderStage.COMPUTE,
        buffer: { type: "storage" } // Atomic max progeny
    }, {
        binding: 8,
        visibility: GPUShaderStage.COMPUTE,
        buffer: { type: "storage" } // Max progeny history
    }]
})

// Create a bind group
const bindGroupsRender = [
    device.createBindGroup({
        label: "Render bind group",
        layout: bindGroupLayoutRender,
        entries: [{
            binding: 0,
            resource: { buffer: bufferUniformGrid }
        },
        {
            binding: 1,
            resource: { buffer: bufferStorageCellColor }
        }]
    })
]

const bindGroupsCompute = [
    device.createBindGroup({
        label: "Compute bind group",
        layout: bindGroupLayoutCompute,
        entries: [{
            binding: 0,
            resource: { buffer: bufferUniformGrid }
        },
        {
            binding: 1,
            resource: { buffer: bufferStorageCellColor }
        },
        {
            binding: 2,
            resource: { buffer: bufferStorageCellColorPool }
        },
        {
            binding: 3,
            resource: { buffer: bufferStorageCellState }
        },
        {
            binding: 4,
            resource: { buffer: bufferStorageFrozenParentProgeny }
        },
        {
            binding: 5,
            resource: { buffer: bufferStorageProgressMod }
        },
        {
            binding: 6,
            resource: { buffer: bufferStorageRandom }
        },
        {
            binding: 7,
            resource: { buffer: bufferatomicMaxProgeny }
        },
        {
            binding: 8,
            resource: { buffer: bufferStorageMaxProgenyHistory }
        }]
    })
]

//#endregion

//#region Pipelines
const pipelineLayoutRender = device.createPipelineLayout({
    label: "Render pipeline layout",
    bindGroupLayouts: [ bindGroupLayoutRender ]
})

const pipelineLayoutCompute = device.createPipelineLayout({
    label: "Compute pipeline layout",
    bindGroupLayouts: [ bindGroupLayoutCompute ]
})

// Render pipelines
const pipelineRender = device.createRenderPipeline({
    label: "Cell pipeline",
    layout: pipelineLayoutRender,
    vertex: {
        module: cellShaderModule,
        entryPoint: "vertexMain",
        buffers: [vertexBufferLayout]
    },
    fragment: {
        module: cellShaderModule,
        entryPoint: "fragmentMain",
        targets: [{
            format: canvasFormat
        }]
    }
});

const pipelineComputeGridDLA = device.createComputePipeline({
    label: "Compute pipeline mainGridDLA",
    layout: pipelineLayoutCompute,
    compute: {
        module: computeShaderModule,
        entryPoint: "mainGridDLA"
    }
});
const pipelineComputeCompleteFreeze = device.createComputePipeline({
    label: "Compute pipeline mainCompleteFreeze",
    layout: pipelineLayoutCompute,
    compute: {
        module: computeShaderModule,
        entryPoint: "mainCompleteFreeze"
    }
});
const pipelineComputeDLAmaintenance = device.createComputePipeline({
    label: "Compute pipeline mainDLAmaintenance",
    layout: pipelineLayoutCompute,
    compute: {
        module: computeShaderModule,
        entryPoint: "mainDLAmaintenance"
    }
});
const pipelineComputePaintFrozen = device.createComputePipeline({
    label: "Compute pipeline mainPaintFrozen",
    layout: pipelineLayoutCompute,
    compute: {
        module: computeShaderModule,
        entryPoint: "mainPaintFrozen"
    }
});

//#endregion

console.log("Number of cells: ", nCells);
const workgroupCountX = Math.ceil(GRID_SIZEx / WORKGROUP_SIZE2D);
const workgroupCountY = Math.ceil(GRID_SIZEy / WORKGROUP_SIZE2D);
const WORKGROUP_SIZE = 256;
const COARSE_FACTOR = 32;
const workgroupCount1D = Math.ceil(nCells/WORKGROUP_SIZE/COARSE_FACTOR);

let lastTime = 0.0; const UPDATE_INTERVAL = 1000/144;

const stagingBufferProgressMod = device.createBuffer({
    label: "Staging buffer for progress modifier",
    size: 4,
    usage: GPUBufferUsage.MAP_READ | GPUBufferUsage.COPY_DST
});
let remainingFrames = 200;
async function gameLoop() {
    const timestamp = performance.now();
    const elapsed = timestamp - lastTime;

    // Only proceed if the time passed is greater than the target interval (e.g., 33.33ms for 30 FPS)
    if (elapsed < UPDATE_INTERVAL) {
        requestAnimationFrame(gameLoop);
        return;
    } else {
        lastTime = timestamp;
    }
    
    const encoder = device.createCommandEncoder();
    updateCompute(500, encoder);
    updateRender(encoder);

    encoder.copyBufferToBuffer(bufferStorageProgressMod, 0, stagingBufferProgressMod, 0, 4);
    device.queue.submit([encoder.finish()]);

    await stagingBufferProgressMod.mapAsync(GPUMapMode.READ);
    const arrayBuffer = stagingBufferProgressMod.getMappedRange();
    progressModifier = new Uint32Array(arrayBuffer)[0];
    console.log("Progress Modifier Value:", progressModifier);
    stagingBufferProgressMod.unmap();

    if (progressModifier == 0) { remainingFrames -= 1; }
    if (remainingFrames != 0) { requestAnimationFrame(gameLoop); }
}
requestAnimationFrame(gameLoop);


async function updateCompute(nTimes, encoder) {
    const computePass = encoder.beginComputePass();
    for (let i=0; i<nTimes; i++) {
        computePass.setBindGroup(0, bindGroupsCompute[0]);

        computePass.setPipeline(pipelineComputeGridDLA);
        computePass.dispatchWorkgroups(workgroupCountX, workgroupCountY);
    }
    computePass.end();
}

async function updateRender(encoder) {
    const computePass = encoder.beginComputePass();
    computePass.setBindGroup(0, bindGroupsCompute[0]);
    computePass.setPipeline(pipelineComputeCompleteFreeze);
    computePass.dispatchWorkgroups(workgroupCountX, workgroupCountY);

    computePass.setPipeline(pipelineComputeDLAmaintenance);
    computePass.dispatchWorkgroups(1);

    computePass.setPipeline(pipelineComputePaintFrozen);
    computePass.dispatchWorkgroups(workgroupCountX, workgroupCountY);
    computePass.end();

    const renderPass = encoder.beginRenderPass({
        colorAttachments: [{
            view: context.getCurrentTexture().createView(),
            loadOp: "clear",
            clearValue: [0,0,0.4,1],
            storeOp: "store"
        }]
    });

    renderPass.setPipeline(pipelineRender);
    renderPass.setBindGroup(0, bindGroupsRender[0]);
    renderPass.setVertexBuffer(0, vertexBuffer);
    renderPass.draw(vertices.length / 2, GRID_SIZEx*GRID_SIZEy); // 6 vertices
    renderPass.end();
}

// Functions
async function loadTextFile(filePath) {
    try {
        const response = await fetch(filePath);
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