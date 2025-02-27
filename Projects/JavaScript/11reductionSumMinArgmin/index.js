// Constants
const COARSE_FACTOR = 32;
let WORKGROUP_SIZE = 256;

const SIZE1D = 8192;
const n = 1000;

// UI
const canvas = document.querySelector("canvas");
canvas.width = canvas.parentElement.clientWidth;
canvas.height = canvas.parentElement.clientHeight;

//#region WebGPU initialization
// Checking WebGPU browser support
if (!navigator.gpu) {
    throw new Error("WebGPU not supported on this browser.");
} else { console.log("WebGPU supported.") }

// Requesting GPU adapter
const adapter = await navigator.gpu.requestAdapter();
if (!adapter) { throw new Error("No appropriate GPUAdapter found.") }

// Handling adapter limits
if (adapter.limits.maxComputeWorkgroupSizeX < WORKGROUP_SIZE) {
    WORKGROUP_SIZE = 256;
}
if (adapter.limits.maxStorageBufferBindingSize < SIZE1D*SIZE1D*4) {
    throw new Error("Storage buffer size not supported.");
}

// Requesting GPU device
const device = await adapter.requestDevice({
    requiredLimits: {
        maxComputeWorkgroupSizeX: WORKGROUP_SIZE,
        maxComputeInvocationsPerWorkgroup: WORKGROUP_SIZE,
        maxStorageBufferBindingSize: SIZE1D*SIZE1D*4
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
let fileComputeShader = await loadTextFile("compute.wgsl");
fileComputeShader = fileComputeShader
    .replace("$WORKGROUP_SIZE$", WORKGROUP_SIZE)
    .replace("$COARSE_FACTOR$", COARSE_FACTOR);
const computeShaderModule = device.createShaderModule({
    label: "Parallel reduction shader",
    code: fileComputeShader
});

//#endregion

//#region Data initialization

// Color pool parameter |colors to be painted
const nNumbers = SIZE1D * SIZE1D;
const numberArray = new Uint32Array(nNumbers);
const numberArrayBytes = nNumbers*4;
console.log("Number of elements: ", nNumbers/1000000,"M, in ",numberArrayBytes/1000000, "MB");

for (let i = 0; i < nNumbers; i++) {
    numberArray[i] = Math.round(Math.random() * 100)+1;
}

//#endregion

//#region Buffers initialization & binding
// Defining buffers
const bufferComputeUniformSize = device.createBuffer({
    label: "Size uniform",
    size: 4,
    usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST
})
const bufferComputeNumbers = device.createBuffer({
    label: "Number buffer",
    size: numberArrayBytes,
    usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_DST
})
const bufferComputeResultAtomicValue = device.createBuffer({
    label: "Result buffer atomic",
    size: 4,
    usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_DST | GPUBufferUsage.COPY_SRC
})

// Writing buffers
device.queue.writeBuffer(bufferComputeUniformSize, 0, new Uint32Array([nNumbers]));
device.queue.writeBuffer(bufferComputeNumbers, 0, numberArray);

// Staging buffer
const stagingBufferResult = device.createBuffer({
    size: 4,
    usage: GPUBufferUsage.COPY_DST | GPUBufferUsage.MAP_READ,
});

// Bind layouts
const bindGroupLayout = device.createBindGroupLayout({
    label: "Compute layout",
    entries: [
        { binding: 0, visibility: GPUShaderStage.COMPUTE, buffer: { } }, // Size uniform
        { binding: 1, visibility: GPUShaderStage.COMPUTE, buffer: { type: "read-only-storage" } }, // Number buffer
        { binding: 2, visibility: GPUShaderStage.COMPUTE, buffer: { type: "storage" } }, // Result buffer atomic sum
    ]
})

// Create a bind group
let bindGroup = device.createBindGroup({
    label: "Compute bind group",
    layout: bindGroupLayout,
    entries: [
        { binding: 0, resource: { buffer: bufferComputeUniformSize } },
        { binding: 1, resource: { buffer: bufferComputeNumbers } },
        { binding: 2, resource: { buffer: bufferComputeResultAtomicValue } },
    ]
})
//#endregion

//#region Pipelines
const pipelineLayout = device.createPipelineLayout({
    label: "Compute pipeline layout",
    bindGroupLayouts: [ bindGroupLayout ]
})

// Compute pipelines
const computePipelineReductionSum = device.createComputePipeline({
    label: "Reduction sum",
    layout: pipelineLayout,
    compute: {
        module: computeShaderModule,
        entryPoint: "reductionSum"
    }})
const computePipelineReductionMin = device.createComputePipeline({
    label: "Reduction min",
    layout: pipelineLayout,
    compute: {
        module: computeShaderModule,
        entryPoint: "reductionMin"
    }})
const computePipelineReductionArgmin = device.createComputePipeline({
    label: "Reduction argmin",
    layout: pipelineLayout,
    compute: {
        module: computeShaderModule,
        entryPoint: "reductionArgmin"
    }})
//#endregion

const dispatches = Math.ceil(nNumbers/WORKGROUP_SIZE/COARSE_FACTOR);
console.log("Dispatches: ", dispatches);

// CPU
let startTime = performance.now();
const sum = numberArray.reduce((accumulator, currentValue) => accumulator + currentValue, 0);
let totalTime = performance.now() - startTime;
console.log(`Result CPU sum:\n${totalTime.toFixed(2)} ms [${(numberArrayBytes/1000000/totalTime).toFixed(2)} GB/s]`);

startTime = performance.now();
const min = numberArray.reduce((min, num) => Math.min(min, num), Infinity);
totalTime = performance.now() - startTime;
console.log(`Result CPU min:\n${totalTime.toFixed(2)} ms [${(numberArrayBytes/1000000/totalTime).toFixed(2)} GB/s]`);

startTime = performance.now();
const argmin = numberArray.reduce((minIndex, num, index, array) => 
    num < array[minIndex] ? index : minIndex, 0);
totalTime = performance.now() - startTime;
console.log(`Result CPU argmin:\n${totalTime.toFixed(2)} ms [${(numberArrayBytes/1000000/totalTime).toFixed(2)} GB/s]`);

// GPU

let encoder;
let computePass;

/////////////////////////////////////////////////////////////////////////////////////////////
// COMPUTE advance iteration
let targetTime;
let targetValue = sum;
await multipleChecks("reductionSum", 1);
await multipleChecks("reductionSum", n);
targetValue = min;
await multipleChecks("reductionMin", n);
await multipleChecks("reductionArgmin", n);


function reductionSum() {
    computePass = encoder.beginComputePass();
    computePass.setPipeline(computePipelineReductionSum);
    computePass.setBindGroup(0, bindGroup);
    computePass.dispatchWorkgroups(dispatches,1,1);
    computePass.end();
}

function reductionMin() {
    computePass = encoder.beginComputePass();
    computePass.setPipeline(computePipelineReductionMin);
    computePass.setBindGroup(0, bindGroup);
    computePass.dispatchWorkgroups(dispatches,1,1);
    computePass.end();
}

function reductionArgmin() {
    computePass = encoder.beginComputePass();
    computePass.setPipeline(computePipelineReductionArgmin);
    computePass.setBindGroup(0, bindGroup);
    computePass.dispatchWorkgroups(dispatches,1,1);
    computePass.end();
}

async function multipleChecks(algorithm, nChecks) {
    let totalTime = 0;
    for (let i = 0; i < nChecks; i++) {
        const startTime = performance.now();
        await computeAndCheck(algorithm);
        const endTime = performance.now();
        totalTime += (endTime - startTime);
    }
    totalTime /= nChecks;
    if (algorithm === "reductionSum") { targetTime = totalTime; }
    console.log(`Result GPU ${algorithm}: (x${(targetTime/totalTime).toFixed(2)})\n${totalTime.toFixed(2)} ms [${(numberArrayBytes/1000000/totalTime).toFixed(2)} GB/s]`);
}

async function computeAndCheck(algorithm) {
    encoder = device.createCommandEncoder();

    if (algorithm === "reductionSum") {
        device.queue.writeBuffer(bufferComputeResultAtomicValue, 0, new Uint32Array([0]));
        reductionSum();
    } else if (algorithm === "reductionMin") {
        device.queue.writeBuffer(bufferComputeResultAtomicValue, 0, new Uint32Array([4294967295]));
        reductionMin();
    } else if (algorithm === "reductionArgmin") {
        device.queue.writeBuffer(bufferComputeResultAtomicValue, 0, new Uint32Array([0]));
        reductionArgmin();
    }

    encoder.copyBufferToBuffer(bufferComputeResultAtomicValue, 0, stagingBufferResult, 0, 4);
    device.queue.submit([encoder.finish()]);

    await stagingBufferResult.mapAsync(GPUMapMode.READ);
    let result = new Uint32Array(stagingBufferResult.getMappedRange())[0];
    
    if (algorithm === "reductionArgmin") {
        result = numberArray[result];
    }
    if (result != targetValue) { console.error(`${algorithm} result (${result}) not matching true: ${targetValue}`) }
    stagingBufferResult.unmap();
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