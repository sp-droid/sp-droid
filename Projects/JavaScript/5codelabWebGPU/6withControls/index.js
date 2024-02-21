// UI
const canvas = document.querySelector("canvas");
canvas.width = canvas.parentElement.clientWidth;
canvas.height = canvas.parentElement.clientHeight;

const textGridSize = document.getElementById("textGridSize");
const sliderGridSize = document.getElementById("sliderGridSize");
const textFPS = document.getElementById("textFPS");
const sliderFPS = document.getElementById("sliderFPS");
const resetButton = document.getElementById("resetButton");

let GRID_SIZEx = sliderGridSize.value;
let UPDATE_INTERVAL = 1000/sliderFPS.value;

// Constants
const WORKGROUP_SIZE = 8;

sliderGridSize.oninput = function() {
    const yValue = Math.floor(canvas.height / canvas.width * this.value);
    textGridSize.textContent = `Grid size: ${this.value}x${yValue} (${this.value*yValue} cells)`;
}
sliderGridSize.oninput();
    
sliderFPS.oninput = function() {
    textFPS.textContent = `Target FPS: ${this.value}`;
}
resetButton.onclick = function() {
    clearInterval(gameLoop);

    GRID_SIZEx = sliderGridSize.value;
    squareSizeX = canvas.width / GRID_SIZEx
    GRID_SIZEy = Math.floor(canvas.height / squareSizeX);
    
    UPDATE_INTERVAL = 1000/sliderFPS.value;
    frameNumber = 0;
    startGame();
}

// Variables
let frameNumber = 0;

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
const pos = 0.90;
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

let uniformArray;
let uniformBuffer;

let cellStateArray;
let cellStateStorage;

let bindGroupLayout;
let bindGroups;

let pipelineLayout;
let cellPipeline;
let simulationPipeline;

let gameLoop;
startGame();

// WebGPU functions
function updateGrid() {
    // Encoder that sends instructions to the GPU
    const encoder = device.createCommandEncoder();

    // Compute pass
    const computePass = encoder.beginComputePass();

    computePass.setPipeline(simulationPipeline);
    computePass.setBindGroup(0, bindGroups[frameNumber % 2]);

    const workgroupCountX = Math.ceil(GRID_SIZEx / WORKGROUP_SIZE);
    const workgroupCountY = Math.ceil(GRID_SIZEy / WORKGROUP_SIZE);
    computePass.dispatchWorkgroups(workgroupCountX, workgroupCountY);

    computePass.end();

    frameNumber++;

    // Render pass
    const pass = encoder.beginRenderPass({
        colorAttachments: [{
            view: context.getCurrentTexture().createView(),
            loadOp: "clear",
            clearValue: [0,0,0.4,1],
            storeOp: "store"
        }]
    });

    pass.setPipeline(cellPipeline);
    pass.setBindGroup(0, bindGroups[frameNumber % 2]);
    pass.setVertexBuffer(0, vertexBuffer);
    pass.draw(vertices.length / 2, GRID_SIZEx*GRID_SIZEy); // 6 vertices

    pass.end();

    // Create a command buffer and submit it to the queue of the GPU device
    device.queue.submit([encoder.finish()]);
}

function startGame() {
    // Allocating GPU memory for the gridsize using an uniform buffer
    uniformArray = new Float32Array([GRID_SIZEx, GRID_SIZEy]);
    uniformBuffer = device.createBuffer({
        label: "Grid uniforms",
        size: uniformArray.byteLength,
        usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST
    });
    device.queue.writeBuffer(uniformBuffer, 0, uniformArray);

    // Allocating GPU memory for the cell states using a storage buffer
    cellStateArray = new Uint32Array(GRID_SIZEx * GRID_SIZEy);
    cellStateStorage = [
        device.createBuffer({
            label: "Cell State A",
            size: cellStateArray.byteLength,
            usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_DST
        }),
        device.createBuffer({
            label: "Cell State B",
            size: cellStateArray.byteLength,
            usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_DST
        })
    ];

    // Set each cell to a random state, then copy the JavaScript array into the storage buffer.
    for (let i = 0; i < cellStateArray.length; ++i) {
        cellStateArray[i] = Math.random() > 0.6 ? 1 : 0;
    }
    device.queue.writeBuffer(cellStateStorage[0], 0, cellStateArray);

    // Bind layout
    bindGroupLayout = device.createBindGroupLayout({
        label: "Cell bind group layout",
        entries: [{
            binding: 0,
            visibility: GPUShaderStage.VERTEX | GPUShaderStage.COMPUTE,
            buffer: {} // Grid uniform buffer
        }, {
            binding: 1,
            visibility: GPUShaderStage.VERTEX | GPUShaderStage.COMPUTE,
            buffer: { type: "read-only-storage" } // Cell state input buffer
        }, {
            binding: 2,
            visibility: GPUShaderStage.COMPUTE,
            buffer: { type: "storage" } // Cell state output buffer
        }]
    });

    // Create a bind group
    bindGroups = [
        device.createBindGroup({
            label: "Cell renderer bind group A",
            layout: bindGroupLayout,
            entries: [{
                binding: 0,
                resource: { buffer: uniformBuffer }
            },
            {
                binding: 1,
                resource: { buffer: cellStateStorage[0] }
            },
            {
                binding: 2,
                resource: { buffer: cellStateStorage[1] }
            }]
        }),
        device.createBindGroup({
            label: "Cell renderer bind group B",
            layout: bindGroupLayout,
            entries: [{
                binding: 0,
                resource: { buffer: uniformBuffer }
            },
            {
                binding: 1,
                resource: { buffer: cellStateStorage[1] }
            },
            {
                binding: 2,
                resource: { buffer: cellStateStorage[0] }
            }]
        })
    ]

    // Pipeline layout
    pipelineLayout = device.createPipelineLayout({
        label: "Cell pipeline layout",
        bindGroupLayouts: [ bindGroupLayout ]
    });

    // Create render pipeline
    cellPipeline = device.createRenderPipeline({
        label: "Cell pipeline",
        layout: pipelineLayout,
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

    // Simulation pipeline
    simulationPipeline = device.createComputePipeline({
        label: "Simulation pipeline",
        layout: pipelineLayout,
        compute: {
            module: simulationShaderModule,
            entryPoint: "computeMain"
        }
    });

    gameLoop = setInterval(updateGrid, UPDATE_INTERVAL);
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