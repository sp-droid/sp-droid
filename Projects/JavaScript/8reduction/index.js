// Constants
const COARSE_FACTOR = 32;
let WORKGROUP_SIZE = 1024;

const SIZE1D = 4096;
const n = 50;

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
    console.log("WORKGROUP_SIZE reduced to 256");
}
if (adapter.limits.maxStorageBufferBindingSize < SIZE1D*SIZE1D*4) {
    throw new Error("Storage buffer size not supported.");
}
console.log("Device has timestamp query feature: ", adapter.features.has('timestamp-query'))
console.log("Device has subgroups feature: ", adapter.features.has('subgroups'))

// Requesting GPU device
const device = await adapter.requestDevice({
    requiredLimits: {
        maxComputeWorkgroupSizeX: WORKGROUP_SIZE,
        maxComputeInvocationsPerWorkgroup: WORKGROUP_SIZE,
        maxStorageBufferBindingSize: SIZE1D*SIZE1D*4
    },
    requiredFeatures: ['timestamp-query', 'subgroups']
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
const auxZeros = new Uint32Array(Math.ceil(nNumbers/WORKGROUP_SIZE));
console.log("Number of elements: ", nNumbers/1000000,"M, in ",numberArrayBytes/1000000, "MB");

for (let i = 0; i < nNumbers; i++) {
    numberArray[i] = Math.round(Math.random() * 10);
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
    size: Math.ceil(nNumbers/WORKGROUP_SIZE)*4,
    usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_DST | GPUBufferUsage.COPY_SRC
})
const bufferComputeResultB = device.createBuffer({
    label: "Result buffer B",
    size: Math.ceil(nNumbers/WORKGROUP_SIZE)*4,
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
    }),
    device.createComputePipeline({
        label: "Reduce 8",
        layout: pipelineLayout,
        compute: {
            module: computeShaderModule,
            entryPoint: "sumReduce8"
        }
    }),
    device.createComputePipeline({
        label: "Reduce 9",
        layout: pipelineLayout,
        compute: {
            module: computeShaderModule,
            entryPoint: "sumReduce9"
        }
    }),
    device.createComputePipeline({
        label: "Reduce 10",
        layout: pipelineLayout,
        compute: {
            module: computeShaderModule,
            entryPoint: "sumReduce10"
        }
    })
]
//#endregion

// CPU
let startTime = performance.now();
const sum = numberArray.reduce((accumulator, currentValue) => accumulator + currentValue, 0);
let totalTime = performance.now() - startTime;
console.log(`Result CPU sum:\n${totalTime.toFixed(2)} ms [${(numberArrayBytes/1000000/totalTime).toFixed(2)} GB/s]`);

// GPU
class TimingHelper {
    #canTimestamp;
    #device;
    #querySet;
    #resolveBuffer;
    #resultBuffer;
    #resultBuffers = [];
    // state can be 'free', 'need resolve', 'wait for result'
    #state = 'free';

    constructor(device) {
    this.#device = device;
    this.#canTimestamp = device.features.has('timestamp-query');
    if (this.#canTimestamp) {
        this.#querySet = device.createQuerySet({
        type: 'timestamp',
        count: 2,
        });
        this.#resolveBuffer = device.createBuffer({
        size: this.#querySet.count * 8,
        usage: GPUBufferUsage.QUERY_RESOLVE | GPUBufferUsage.COPY_SRC,
        });
    }
    }

    #beginTimestampPass(encoder, fnName, descriptor) {
    if (this.#canTimestamp) {
        this.#state = 'need resolve';

        const pass = encoder[fnName]({
        ...descriptor,
        ...{
            timestampWrites: {
            querySet: this.#querySet,
            beginningOfPassWriteIndex: 0,
            endOfPassWriteIndex: 1,
            },
        },
        });

        const resolve = () => this.#resolveTiming(encoder);
        pass.end = (function(origFn) {
        return function() {
            origFn.call(this);
            resolve();
        };
        })(pass.end);

        return pass;
    } else {
        return encoder[fnName](descriptor);
    }
    }

    beginRenderPass(encoder, descriptor = {}) {
    return this.#beginTimestampPass(encoder, 'beginRenderPass', descriptor);
    }

    beginComputePass(encoder, descriptor = {}) {
    return this.#beginTimestampPass(encoder, 'beginComputePass', descriptor);
    }

    #resolveTiming(encoder) {
    if (!this.#canTimestamp) {
        return;
    }
    this.#state = 'wait for result';

    this.#resultBuffer = this.#resultBuffers.pop() || this.#device.createBuffer({
        size: this.#resolveBuffer.size,
        usage: GPUBufferUsage.COPY_DST | GPUBufferUsage.MAP_READ,
    });

    encoder.resolveQuerySet(this.#querySet, 0, this.#querySet.count, this.#resolveBuffer, 0);
    encoder.copyBufferToBuffer(this.#resolveBuffer, 0, this.#resultBuffer, 0, this.#resultBuffer.size);
    }

    async getResult() { // Returns result in microseconds
    if (!this.#canTimestamp) {
        return 0;
    }
    this.#state = 'free';

    const resultBuffer = this.#resultBuffer;
    await resultBuffer.mapAsync(GPUMapMode.READ);
    const times = new BigInt64Array(resultBuffer.getMappedRange());
    const duration = Number((times[1] - times[0])/1000n);
    resultBuffer.unmap();
    this.#resultBuffers.push(resultBuffer);
    return duration;
    }
}
const timingHelper = new TimingHelper(device);

let encoder;
let computePass;

/////////////////////////////////////////////////////////////////////////////////////////////
// COMPUTE advance iteration
let auxBuffer;
let finalResultBuffer;
let targetTime;
await multipleChecks("naive ST", "", 1);
await multipleChecks("reduction", 0, 1); // warm up

console.log("--------------------------------------------------");
await multipleChecks("reduction", 0, n);
await multipleChecks("reduction", 8, n);
await multipleChecks("reduction", 10, n);
await multipleChecks("reduction", 9, n);
await multipleChecks("reduction", 7, n);
await multipleChecks("reduction", 6, n);
await multipleChecks("reduction", 5, n);
await multipleChecks("reduction", 4, n);
await multipleChecks("reduction", 3, n);
await multipleChecks("reduction", 2, n);
await multipleChecks("reduction", 1, n);

function sumReduce(pipelineIndex, computePass) {
    const COARSE_FACTOR = 32;

    let workgroupNumberModifier = 1;
    if (pipelineIndex >= 6) { workgroupNumberModifier = COARSE_FACTOR; 
    } else if (pipelineIndex >= 3) { workgroupNumberModifier = 2; }

    computePass.setPipeline(computePipelines[pipelineIndex+1]);

    let dispatches = Math.ceil(nNumbers/WORKGROUP_SIZE/workgroupNumberModifier);

    if (pipelineIndex == 7 || pipelineIndex == 9 || pipelineIndex == 10) {
        device.queue.writeBuffer(bufferComputeResultAtomic, 0, new Uint32Array([0]));
        computePass.setBindGroup(0, bindGroup);
        computePass.dispatchWorkgroups(dispatches,1,1);
        finalResultBuffer = bufferComputeResultAtomic
        return;
    }
    if (pipelineIndex == 8) {
        device.queue.writeBuffer(bufferComputeResultAtomic, 0, new Uint32Array([dispatches]));
        computePass.setBindGroup(0, bindGroup);
        computePass.dispatchWorkgroups(dispatches,1,1);
        finalResultBuffer = bufferComputeResultA;
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
        dispatches = Math.ceil(dispatches/WORKGROUP_SIZE/workgroupNumberModifier);
        i++;
    }
    computePass.setBindGroup(0, bindGroup);
    computePass.dispatchWorkgroups(1,1,1);
}

function sumST(computePass) {
    computePass.setPipeline(computePipelines[0]);
    computePass.setBindGroup(0, bindGroup);
    computePass.dispatchWorkgroups(1,1,1);
}

async function multipleChecks(algorithm, reduceVariant, nChecks) {
    device.queue.writeBuffer(bufferComputeResultA, 0, auxZeros);
    device.queue.writeBuffer(bufferComputeResultB, 0, auxZeros);

    totalTime = 0;
    for (let i = 0; i < nChecks; i++) {
        totalTime += await computeAndCheck(algorithm, reduceVariant);
    }
    totalTime /= nChecks*1000;

    if (reduceVariant === 0 && nChecks === 1) { return;}
    if (reduceVariant === 0) { targetTime = totalTime; }
    console.log(`Result GPU ${algorithm}${reduceVariant}:\t(x${(targetTime/totalTime).toFixed(2)}) \t${totalTime.toFixed(2)} ms [${(numberArrayBytes/1000000/totalTime).toFixed(2)} GB/s]`);
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
    computePass = timingHelper.beginComputePass(encoder);
    if (algorithm === "naive ST") {
        sumST(computePass);
    } else if (algorithm === "reduction") {
        sumReduce(reduceVariant, computePass);
    }
    computePass.end();

    encoder.copyBufferToBuffer(finalResultBuffer, 0, stagingBufferResult, 0, 4); // Check if the sum is correct
    device.queue.submit([encoder.finish()]);

    await stagingBufferResult.mapAsync(GPUMapMode.READ);
    const result = new Uint32Array(stagingBufferResult.getMappedRange())[0];
    if (result != sum) { console.error(`${algorithm}-${reduceVariant} result (${result}) not matching true: ${sum}`) }
    stagingBufferResult.unmap();

    return timingHelper.getResult();
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