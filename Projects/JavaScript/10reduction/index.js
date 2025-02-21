// UI
const canvas = document.querySelector("canvas");
canvas.width = canvas.parentElement.clientWidth;
canvas.height = canvas.parentElement.clientHeight;

const SIZE1D = 3072;

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

// Shaders
const computeShaderModule = device.createShaderModule({
    label: "Parallel reduction shader",
    code: await loadTextFile("compute.wgsl")
});

//#endregion

//#region Data initialization

// Color pool parameter |colors to be painted
const nNumbers = SIZE1D * SIZE1D;
const numberArray = new Uint32Array(nNumbers);
const numberArrayBytes = nNumbers*4;
const auxZeros = new Uint32Array(Math.ceil(nNumbers/256));
console.log("Number of elements: ", nNumbers/1000000,"M, in ",numberArrayBytes/1000000, "MB");

let sum = 0;
for (let i = 0; i < nNumbers; i++) {
    numberArray[i] = Math.round(Math.random() * 10);

    sum += numberArray[i];
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
const bufferComputeResultA = device.createBuffer({
    label: "Result buffer A",
    size: Math.ceil(nNumbers/256)*4,
    usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_DST | GPUBufferUsage.COPY_SRC
})
const bufferComputeResultB = device.createBuffer({
    label: "Result buffer B",
    size: Math.ceil(nNumbers/256)*4,
    usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_DST | GPUBufferUsage.COPY_SRC
})
const bufferComputeResultAtomic = device.createBuffer({
    label: "Result buffer C",
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
        { binding: 2, visibility: GPUShaderStage.COMPUTE, buffer: { type: "storage" } }, // Result buffer
        { binding: 3, visibility: GPUShaderStage.COMPUTE, buffer: { type: "storage" } } // Result buffer atomic
    ]
})

// Create a bind group
let bindGroup;
//#endregion

//#region Pipelines
const pipelineLayout = device.createPipelineLayout({
    label: "Compute pipeline layout",
    bindGroupLayouts: [ bindGroupLayout ]
})

// Compute pipelines
const computePipelines = [
    device.createComputePipeline({
        label: "Single threaded sum",
        layout: pipelineLayout,
        compute: {
            module: computeShaderModule,
            entryPoint: "sumST"
        }
    }),
    device.createComputePipeline({
        label: "Reduce 0",
        layout: pipelineLayout,
        compute: {
            module: computeShaderModule,
            entryPoint: "sumReduce0"
        }
    }),
    device.createComputePipeline({
        label: "Reduce 1",
        layout: pipelineLayout,
        compute: {
            module: computeShaderModule,
            entryPoint: "sumReduce1"
        }
    }),
    device.createComputePipeline({
        label: "Reduce 2",
        layout: pipelineLayout,
        compute: {
            module: computeShaderModule,
            entryPoint: "sumReduce2"
        }
    }),
    device.createComputePipeline({
        label: "Reduce 3",
        layout: pipelineLayout,
        compute: {
            module: computeShaderModule,
            entryPoint: "sumReduce3"
        }
    }),
    device.createComputePipeline({
        label: "Reduce 4",
        layout: pipelineLayout,
        compute: {
            module: computeShaderModule,
            entryPoint: "sumReduce4"
        }
    }),
    device.createComputePipeline({
        label: "Reduce 5",
        layout: pipelineLayout,
        compute: {
            module: computeShaderModule,
            entryPoint: "sumReduce5"
        }
    }),
    device.createComputePipeline({
        label: "Reduce 6",
        layout: pipelineLayout,
        compute: {
            module: computeShaderModule,
            entryPoint: "sumReduce6"
        }
    }),
    device.createComputePipeline({
        label: "Reduce 7",
        layout: pipelineLayout,
        compute: {
            module: computeShaderModule,
            entryPoint: "sumReduce7"
        }
    })
]
//#endregion

// CPU
const startTime = performance.now();
let result = 0;
for (let i = 0; i < nNumbers; i++) {
    result += numberArray[i];
}
const endTime = performance.now();
const totalTime = (endTime - startTime);
console.log(`Result CPU ST:\n${totalTime.toFixed(2)} ms [${(numberArrayBytes/1000000/totalTime).toFixed(2)} GB/s]`);
if (result != sum) { console.error("CPU result (",sum,") not matching: ", result) }

// GPU

let encoder;
let computePass;

/////////////////////////////////////////////////////////////////////////////////////////////
// COMPUTE advance iteration
let auxBuffer;
let finalResultBuffer;
let targetTime;
const n = 400;
await multipleChecks("reduction", 0, n);
await multipleChecks("reduction", 1, n);
await multipleChecks("reduction", 2, n);
await multipleChecks("reduction", 3, n);
await multipleChecks("reduction", 4, n);
await multipleChecks("reduction", 5, n);
await multipleChecks("reduction", 6, n);
await multipleChecks("reduction", 7, n);
await multipleChecks("naive ST", "", 1);


function sumReduce(pipelineIndex) {
    const COARSE_FACTOR = 32;

    let workgroupNumberModifier = 1;
    if (pipelineIndex >= 6) { workgroupNumberModifier = COARSE_FACTOR; 
    } else if (pipelineIndex >= 3) { workgroupNumberModifier = 2; }

    computePass = encoder.beginComputePass();
    computePass.setPipeline(computePipelines[pipelineIndex+1]);

    let dispatches = Math.ceil(nNumbers/256/workgroupNumberModifier);

    if (pipelineIndex == 7) {
        device.queue.writeBuffer(bufferComputeResultAtomic, 0, new Uint32Array([0]));
        computePass.setBindGroup(0, bindGroup);
        computePass.dispatchWorkgroups(dispatches,1,1);
        computePass.end();
        finalResultBuffer = bufferComputeResultAtomic
        return;
    }

    let i = 0;
    while (dispatches > 1) {
        computePass.setBindGroup(0, bindGroup);
        computePass.dispatchWorkgroups(dispatches,1,1);

        if (i % 2 == 0) {
            auxBuffer = bufferComputeResultA;
            finalResultBuffer = bufferComputeResultB;
        } else {
            auxBuffer = bufferComputeResultB;
            finalResultBuffer = bufferComputeResultA;
        }
        bindGroup = device.createBindGroup({
            label: "Compute bind group",
            layout: bindGroupLayout,
            entries: [
                { binding: 0, resource: { buffer: bufferComputeUniformSize } },
                { binding: 1, resource: { buffer: auxBuffer } },
                { binding: 2, resource: { buffer: finalResultBuffer } },
                { binding: 3, resource: { buffer: bufferComputeResultAtomic } }
            ]
        })
        dispatches = Math.ceil(dispatches/256/workgroupNumberModifier);
        i++;
    }
    computePass.setBindGroup(0, bindGroup);
    computePass.dispatchWorkgroups(1,1,1);
    computePass.end();
}

function sumST() {
    computePass = encoder.beginComputePass();
    computePass.setPipeline(computePipelines[0]);
    computePass.setBindGroup(0, bindGroup);
    computePass.dispatchWorkgroups(1,1,1);
    computePass.end();
}

async function multipleChecks(algorithm, reduceVariant, nChecks) {
    device.queue.writeBuffer(bufferComputeResultA, 0, auxZeros);
    device.queue.writeBuffer(bufferComputeResultB, 0, auxZeros);
    let totalTime = 0;
    for (let i = 0; i < nChecks; i++) {
        const startTime = performance.now();
        await computeAndCheck(algorithm, reduceVariant);
        const endTime = performance.now();
        totalTime += (endTime - startTime);
    }
    totalTime /= nChecks;
    if (reduceVariant === 0) { targetTime = totalTime; }
    console.log(`Result GPU ${algorithm}${reduceVariant}: (x${(targetTime/totalTime).toFixed(2)})\n${totalTime.toFixed(2)} ms [${(numberArrayBytes/1000000/totalTime).toFixed(2)} GB/s]`);
}

async function computeAndCheck(algorithm, reduceVariant) {
    encoder = device.createCommandEncoder();

    finalResultBuffer = bufferComputeResultA;
    bindGroup = device.createBindGroup({
        label: "Compute bind group",
        layout: bindGroupLayout,
        entries: [
            { binding: 0, resource: { buffer: bufferComputeUniformSize } },
            { binding: 1, resource: { buffer: bufferComputeNumbers } },
            { binding: 2, resource: { buffer: finalResultBuffer } },
            { binding: 3, resource: { buffer: bufferComputeResultAtomic } }
        ]
    })
    if (algorithm === "naive ST") {
        sumST();
    } else if (algorithm === "reduction") {
        sumReduce(reduceVariant);
    }

    encoder.copyBufferToBuffer(finalResultBuffer, 0, stagingBufferResult, 0, 4);
    device.queue.submit([encoder.finish()]);

    await stagingBufferResult.mapAsync(GPUMapMode.READ);
    result = new Uint32Array(stagingBufferResult.getMappedRange())[0];
    if (result != sum) { console.error(`${algorithm} result (${result}) not matching true: ${sum}`) }
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