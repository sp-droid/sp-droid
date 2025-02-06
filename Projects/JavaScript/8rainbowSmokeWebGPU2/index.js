// UI
const canvas = document.querySelector("canvas");
canvas.width = canvas.parentElement.clientWidth;
canvas.height = canvas.parentElement.clientHeight;

const horizontalResolutionPrompt = parseInt(prompt("Horizontal resolution:", "56"));
const algorithmPrompt = prompt("Type 1 to use the AVERAGE or anything else to use the MINIMUM Rainbow Smoke algorithm variant:", "1");
const colorOrderPrompt = prompt("Type 1 to use RANDOM or anything else for HUE ORDERED colors:", "1");

let hueOffset; let hueDirection;
if (colorOrderPrompt != 1) {
    const huePrompt = parseFloat(prompt("Type any number from -1 to 1 to control hue order", "0.0"));
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
let FPS_VALUE = 144;
let UPDATE_INTERVAL = 1000/FPS_VALUE;

//#region WebGPU initialization
// Constants
const WORKGROUP_SIZE = 8;
let workgroupCountX;
let workgroupCountY;

// Checking WebGPU browser support
if (!navigator.gpu) {
    throw new Error("WebGPU not supported on this browser.");
} else { console.log("WebGPU supported.") }

// Requesting GPU adapter
const adapter = await navigator.gpu.requestAdapter();
if (!adapter) { throw new Error("No appropriate GPUAdapter found.") }
// Requesting GPU device
const device = await adapter.requestDevice();

// Requesting GPU canvas context
const context = canvas.getContext('webgpu');
const canvasFormat = navigator.gpu.getPreferredCanvasFormat();
context.configure({
    device: device,
    format: canvasFormat
});

// Defining the scale in Y direction
let squareSizeX = canvas.width / GRID_SIZEx;
let GRID_SIZEy = Math.floor(canvas.height / squareSizeX);

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
const simulationShaderModule = device.createShaderModule({
    label: "Rainbow Smoke shader",
    code: await loadTextFile("compute.wgsl")
});

//#endregion


//#region Data initialization
// Grid uniform |Painting sizes
let gridUniform = new Uint32Array([GRID_SIZEx, GRID_SIZEy]);

// Color pool parameter |colors to be painted
const nColors = GRID_SIZEx * GRID_SIZEy;
let colorPoolStorage = new Float32Array(nColors * 3);

if (colorOrderPrompt == 1) {
    for (let i = 0; i < colorPoolStorage.length; ++i) {
        colorPoolStorage[i] = Math.random();
    }
} else { // Ordered by hue
    console.log(hueOffset, hueDirection)
    let colors = [];
    for (let i = 0; i < nColors; ++i) {
        const r = Math.round(256*Math.random());
        const g = Math.round(256*Math.random());
        const b = Math.round(256*Math.random());

        let hue = rgbToHue(r,g,b);
        if (hue < 0.5) { hue  = hue + 1 }

        colors.push({hue: hue, r: r/255, g: g/255, b: b/255})
    }
    colors.sort((a, b) => hueDirection*((a.hue - hueOffset + 1) % 1 - (b.hue - hueOffset + 1) % 1));

    for (let i = 0; i < nColors; ++i) {
        colorPoolStorage[i*3] = colors[i].r;
        colorPoolStorage[i*3+1] = colors[i].g;
        colorPoolStorage[i*3+2] = colors[i].b;
    }
}


// Cell state storage |states: 0=inactive, 1=active, 2=painted
let cellStateStorage = new Uint32Array(nColors);
for (let i = 0; i < cellStateStorage.length; ++i) {
    cellStateStorage[i] = 0;
}

// Cell color storage |painted colors
let cellColorStorage = new Float32Array(nColors * 3);
for (let i = 0; i < cellColorStorage.length; ++i) {
    cellColorStorage[i] = 0.0;
}

// Seed
let x = Math.floor(GRID_SIZEx/2); // Middle option
let y = Math.floor(GRID_SIZEy/2); // Middle option
let seedCoord = from2Dto1Dindex(x, y, GRID_SIZEx);
cellStateStorage[seedCoord] = 2;
cellColorStorage[seedCoord*3] = colorPoolStorage[0];
cellColorStorage[seedCoord*3+1] = colorPoolStorage[1];
cellColorStorage[seedCoord*3+2] = colorPoolStorage[2];

cellStateStorage[from2Dto1Dindex(x+1, y, GRID_SIZEx)] = 1;
cellStateStorage[from2Dto1Dindex(x-1, y, GRID_SIZEx)] = 1;
cellStateStorage[from2Dto1Dindex(x, y+1, GRID_SIZEx)] = 1;
cellStateStorage[from2Dto1Dindex(x, y-1, GRID_SIZEx)] = 1;
cellStateStorage[from2Dto1Dindex(x+1, y+1, GRID_SIZEx)] = 1;
cellStateStorage[from2Dto1Dindex(x+1, y-1, GRID_SIZEx)] = 1;
cellStateStorage[from2Dto1Dindex(x-1, y+1, GRID_SIZEx)] = 1;
cellStateStorage[from2Dto1Dindex(x-1, y-1, GRID_SIZEx)] = 1;

function from2Dto1Dindex(x, y, gridX) {
    return y * gridX + x;
}

// Iteration
let iterationStorage = new Float32Array(1);
let iteration = 0;

// Distances
let minIndexStorage = new Uint32Array(1);
let distancesStorage = new Float32Array(GRID_SIZEx * GRID_SIZEy);

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
        label: "Cell State", // 0 = free and isolated, 1 = free but near color, 2 = colored
        size: cellStateStorage.byteLength,
        usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_DST
    }),
    device.createBuffer({
        label: "Color pool",
        size: colorPoolStorage.byteLength,
        usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_DST
    }),
    device.createBuffer({
        label: "Minimum index of distance",
        size: minIndexStorage.byteLength,
        usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_DST
    }),
    device.createBuffer({
        label: "Distances",
        size: distancesStorage.byteLength,
        usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_DST | GPUBufferUsage.COPY_SRC
    }),
    device.createBuffer({
        label: "Iteration",
        size: iterationStorage.byteLength,
        usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_DST
    }),
    device.createBuffer({
        label: "Target color",
        size: new Float32Array(3).byteLength,
        usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_DST
    })
];

// Staging buffers
let stagingBuffers = [
    device.createBuffer({
        label: "Staging buffer for reading distances",
        size: distancesStorage.byteLength,
        usage: GPUBufferUsage.MAP_READ | GPUBufferUsage.COPY_DST
    }),
    device.createBuffer({
        label: "Staging buffer for writing the index of the minimum value",
        size: minIndexStorage.byteLength,
        usage: GPUBufferUsage.MAP_WRITE | GPUBufferUsage.COPY_SRC
    })
]

// Writing buffers
device.queue.writeBuffer(commonBuffers[0], 0, gridUniform);
device.queue.writeBuffer(commonBuffers[1], 0, cellColorStorage);

device.queue.writeBuffer(computeBuffers[0], 0, cellStateStorage);
device.queue.writeBuffer(computeBuffers[1], 0, colorPoolStorage);

// Bind layouts
let bindGroupLayouts = [
    device.createBindGroupLayout({
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
    }),
    device.createBindGroupLayout({
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
            buffer: { type: "storage" } // Cell state
        }, {
            binding: 3,
            visibility: GPUShaderStage.COMPUTE,
            buffer: { type: "read-only-storage" } // Color pool
        }, {
            binding: 4,
            visibility: GPUShaderStage.COMPUTE,
            buffer: { type: "storage" } // Minimum index
        }, {
            binding: 5,
            visibility: GPUShaderStage.COMPUTE,
            buffer: { type: "storage" } // Distances
        }, {
            binding: 6,
            visibility: GPUShaderStage.COMPUTE,
            buffer: { type: "storage" } // Iteration
        }, {
            binding: 7,
            visibility: GPUShaderStage.COMPUTE,
            buffer: { type: "storage" } // Target color
        }]
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
        entries: [{
            binding: 0,
            resource: { buffer: commonBuffers[0] }
        },
        {
            binding: 1,
            resource: { buffer: commonBuffers[1] }
        },
        {
            binding: 2,
            resource: { buffer: computeBuffers[0] }
        },
        {
            binding: 3,
            resource: { buffer: computeBuffers[1] }
        },
        {
            binding: 4,
            resource: { buffer: computeBuffers[2] }
        },
        {
            binding: 5,
            resource: { buffer: computeBuffers[3] }
        },
        {
            binding: 6,
            resource: { buffer: computeBuffers[4] }
        },
        {
            binding: 7,
            resource: { buffer: computeBuffers[5] }
        }]
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

// Compute pipelines
let computePipelines = [
    device.createComputePipeline({
        label: "Compute distances pipeline",
        layout: pipelineLayouts[1],
        compute: {
            module: simulationShaderModule,
            entryPoint: `distances${algorithm}MethodMain`
        }
    }),
    device.createComputePipeline({
        label: "Compute place and activate neighbors pipeline",
        layout: pipelineLayouts[1],
        compute: {
            module: simulationShaderModule,
            entryPoint: "placeMain"
        }
    }),
    device.createComputePipeline({
        label: "Advance iteration",
        layout: pipelineLayouts[1],
        compute: {
            module: simulationShaderModule,
            entryPoint: "iterationMain"
        }
    }),
    device.createComputePipeline({
        label: "Minimum single-threaded",
        layout: pipelineLayouts[1],
        compute: {
            module: simulationShaderModule,
            entryPoint: "minimumSTMain"
        }
    })
]
//#endregion

function gameLoop() {
    // Iteration
    iteration++;
    if (iteration === nColors) { return; }

    updateGrid();
    requestAnimationFrame(gameLoop);
}
requestAnimationFrame(gameLoop);

// WebGPU functions
async function updateGrid() {
    // console.time('RENDER pass')
    /////////////////////////////////////////////////////////////////////////////////////////////
    let encoder = device.createCommandEncoder();
    let computePass;

    computePass = encoder.beginComputePass();
    computePass.setPipeline(computePipelines[2]);
    computePass.setBindGroup(0, bindGroups[1]);
    computePass.dispatchWorkgroups(1,1,1);
    computePass.end();
    
    /////////////////////////////////////////////////////////////////////////////////////////////
    // COMPUTE distances
    computePass = encoder.beginComputePass();
    computePass.setPipeline(computePipelines[0]);
    computePass.setBindGroup(0, bindGroups[1]);
    workgroupCountX = Math.ceil(GRID_SIZEx / WORKGROUP_SIZE);
    workgroupCountY = Math.ceil(GRID_SIZEy / WORKGROUP_SIZE);
    computePass.dispatchWorkgroups(workgroupCountX, workgroupCountY);
    computePass.end();
    
    /////////////////////////////////////////////////////////////////////////////////////////////
    // COMPUTE minimum distance
    computePass = encoder.beginComputePass();
    computePass.setPipeline(computePipelines[3]);
    computePass.setBindGroup(0, bindGroups[1]);
    computePass.dispatchWorkgroups(1,1,1);
    computePass.end();

    /////////////////////////////////////////////////////////////////////////////////////////////
    // COMPUTE Place pixel, activate neighbors
    computePass = encoder.beginComputePass();
    computePass.setPipeline(computePipelines[1]);
    computePass.setBindGroup(0, bindGroups[1]);
    computePass.dispatchWorkgroups(1);
    computePass.end();
    
    device.queue.submit([encoder.finish()]);
    /////////////////////////////////////////////////////////////////////////////////////////////
    // RENDER cells
    encoder = device.createCommandEncoder();
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
    renderPass.setVertexBuffer(0, vertexBuffer);
    renderPass.draw(vertices.length / 2, GRID_SIZEx*GRID_SIZEy); // 6 vertices
    renderPass.end();

    device.queue.submit([encoder.finish()]);
    // console.timeEnd('RENDER pass')
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