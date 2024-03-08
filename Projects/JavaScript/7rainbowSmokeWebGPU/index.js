// UI
const canvas = document.querySelector("canvas");
canvas.width = canvas.parentElement.clientWidth;
canvas.height = canvas.parentElement.clientHeight;

let GRID_SIZEx = 22;
let FPS_VALUE = 0.5;
let UPDATE_INTERVAL = 1000/FPS_VALUE;

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
    label: "Game of Life simulation shader",
    code: await loadTextFile("compute.wgsl")
});

// Possibly variable part

// Grid uniform |2
let gridUniform = new Float32Array([GRID_SIZEx, GRID_SIZEy]);

// Color pool parameter |X*Y*3
let colorPoolArray = new Float32Array(GRID_SIZEx * GRID_SIZEy * 3);
for (let i = 0; i < colorPoolArray.length; ++i) {
    colorPoolArray[i] = Math.random();
}

// Cell state storage |X*Y
let cellStateStorage = new Uint32Array(GRID_SIZEx * GRID_SIZEy);
for (let i = 0; i < cellStateStorage.length; ++i) {
    cellStateStorage[i] = 0;
}

// Cell color storage |X*Y*3
let cellColorStorage = new Float32Array(GRID_SIZEx * GRID_SIZEy * 3);
for (let i = 0; i < cellColorStorage.length; ++i) {
    cellColorStorage[i] = 0.0;
}

// Seed
let x = Math.floor(GRID_SIZEx/2); // Middle option
let y = Math.floor(GRID_SIZEy/2); // Middle option
let seedCoord = from2Dto1Dindex(x, y, GRID_SIZEx);
cellStateStorage[seedCoord] = 2;
cellColorStorage[seedCoord*3] = colorPoolArray[0];
cellColorStorage[seedCoord*3+1] = colorPoolArray[1];
cellColorStorage[seedCoord*3+2] = colorPoolArray[2];

cellStateStorage[from2Dto1Dindex(x+1, y, GRID_SIZEx)] = 1;
cellStateStorage[from2Dto1Dindex(x-1, y, GRID_SIZEx)] = 1;
cellStateStorage[from2Dto1Dindex(x, y+1, GRID_SIZEx)] = 1;
cellStateStorage[from2Dto1Dindex(x, y-1, GRID_SIZEx)] = 1;

function from2Dto1Dindex(x, y, gridX) {
    return y * gridX + x;
}

// Color pool next candidate
let targetColor = 0;
let targetColorUniform = new Float32Array(3);

// Distances
let minIndexStorage = new Uint32Array(1);
let minimumValueIndex = from2Dto1Dindex(x+1, y, GRID_SIZEx); // Set it as initial value to one of the first neighbors
minIndexStorage[0] = minimumValueIndex;
let distancesStorage = new Float32Array(GRID_SIZEx * GRID_SIZEy);

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
        label: "Target color RGB",
        size: targetColorUniform.byteLength,
        usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST
    }),
    device.createBuffer({
        label: "Minimum index of distance",
        size: minIndexStorage.byteLength,
        usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST
    }),
    device.createBuffer({
        label: "Distances",
        size: distancesStorage.byteLength,
        usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_DST | GPUBufferUsage.COPY_SRC
    })
];

// Staging buffers
let stagingBuffers = [
    device.createBuffer({
        label: "Staging buffer for writing target color RGB",
        size: targetColorUniform.byteLength,
        usage: GPUBufferUsage.MAP_WRITE | GPUBufferUsage.COPY_SRC
    }),
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
device.queue.writeBuffer(computeBuffers[2], 0, minIndexStorage);

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
        }, {
            binding: 2,
            visibility: GPUShaderStage.VERTEX,
            buffer: { type: "read-only-storage" } // Cell state
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
            buffer: {} // Target color
        }, {
            binding: 4,
            visibility: GPUShaderStage.COMPUTE,
            buffer: {} // Minimum index
        }, {
            binding: 5,
            visibility: GPUShaderStage.COMPUTE,
            buffer: { type: "storage" } // Distances
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
        },
        {
            binding: 2,
            resource: { buffer: computeBuffers[0] }
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
        }]
    })
]

// Pipeline layout
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
            entryPoint: "distancesMain"
        }
    }),
    device.createComputePipeline({
        label: "Compute place and activate neighbors pipeline",
        layout: pipelineLayouts[1],
        compute: {
            module: simulationShaderModule,
            entryPoint: "placeMain"
        }
    })
]

let gameLoop = setInterval(updateGrid, UPDATE_INTERVAL);

// WebGPU functions
async function updateGrid() {
    targetColor++;
    if (targetColor < GRID_SIZEx * GRID_SIZEy) {
        // Encoder that sends instructions to the GPU
        let encoder = device.createCommandEncoder();

        // Copy new target color into uniform
        await stagingBuffers[0].mapAsync(GPUMapMode.WRITE);
        
        let data = new Float32Array(stagingBuffers[0].getMappedRange());
        data.set([colorPoolArray[targetColor*3], colorPoolArray[targetColor*3+1], colorPoolArray[targetColor*3+2]]);
        if ((targetColor === 1) | (targetColor === 2) | (targetColor === 3)) {console.log(data)}
        stagingBuffers[0].unmap();

        encoder.copyBufferToBuffer(
            stagingBuffers[0], 0,
            computeBuffers[1], 0,
            computeBuffers[1].size
        );

        device.queue.submit([encoder.finish()]);
        await device.queue.onSubmittedWorkDone();
        encoder = device.createCommandEncoder();

        // Compute pass
        const computePass = encoder.beginComputePass();
        // Compute distances
        computePass.setPipeline(computePipelines[0]);
        computePass.setBindGroup(0, bindGroups[1]);
        workgroupCountX = Math.ceil(GRID_SIZEx / WORKGROUP_SIZE);
        workgroupCountY = Math.ceil(GRID_SIZEy / WORKGROUP_SIZE);
        computePass.dispatchWorkgroups(workgroupCountX, workgroupCountY);
        computePass.end();

        device.queue.submit([encoder.finish()]);
        await device.queue.onSubmittedWorkDone();
        encoder = device.createCommandEncoder();

        // Copy distances from GPU
        await stagingBuffers[1].mapAsync(GPUMapMode.READ);
        
        encoder.copyBufferToBuffer(
            computeBuffers[3], 0,
            stagingBuffers[1], 0,
            stagingBuffers[1].size
        );
        
        let data2 = new Float32Array(stagingBuffers[1].getMappedRange());
        let result = 10.0;
        minimumValueIndex = 0;
        if ((targetColor === 1) | (targetColor === 2) | (targetColor === 3)) {console.log(data2)}
        data2.forEach((x, i) => {
            if (x < result) {
                result = x;
                minimumValueIndex = i;
            }
        })
        // console.log(minimumValueIndex)
        stagingBuffers[1].unmap();

        device.queue.submit([encoder.finish()]);
        await device.queue.onSubmittedWorkDone();

        encoder = device.createCommandEncoder();
        // Copy chosen cell index into uniform
        await stagingBuffers[2].mapAsync(GPUMapMode.WRITE);
        
        let data3 = new Uint32Array(stagingBuffers[2].getMappedRange());
        data3.set([minimumValueIndex]);
        stagingBuffers[2].unmap();

        encoder.copyBufferToBuffer(
            stagingBuffers[2], 0,
            computeBuffers[2], 0,
            computeBuffers[2].size
        );

        // Place pixel, activate neighbors
        const computePass2 = encoder.beginComputePass();
        computePass2.setPipeline(computePipelines[1]);
        computePass2.setBindGroup(0, bindGroups[1]);
        computePass2.dispatchWorkgroups(1);
        computePass2.end();
        
        // Render pass
        const pass = encoder.beginRenderPass({
            colorAttachments: [{
                view: context.getCurrentTexture().createView(),
                loadOp: "clear",
                clearValue: [0,0,0.4,1],
                storeOp: "store"
            }]
        });

        pass.setPipeline(renderCellsPipeline);
        pass.setBindGroup(0, bindGroups[0]);
        pass.setVertexBuffer(0, vertexBuffer);
        pass.draw(vertices.length / 2, GRID_SIZEx*GRID_SIZEy); // 6 vertices

        pass.end();

        // Create a command buffer and submit it to the queue of the GPU device
        device.queue.submit([encoder.finish()]);
        await device.queue.onSubmittedWorkDone();
    }
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