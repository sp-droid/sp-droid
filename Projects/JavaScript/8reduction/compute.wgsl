enable subgroups;

struct ComputeInput {
    @builtin(global_invocation_id) cell: vec3u
}

// Constants
const WORKGROUP_SIZE: u32 = $WORKGROUP_SIZE$;
const COARSE_FACTOR: u32 = $COARSE_FACTOR$;

// Global memory
@group(0) @binding(0) var<uniform> size: u32;
@group(0) @binding(1) var<storage, read> inputGlobal: array<u32>;
@group(0) @binding(2) var<storage, read_write> outputGlobal: array<u32>;
@group(0) @binding(3) var<storage, read_write> outputGlobalAtomic: atomic<u32>;

// Shared memory
var<workgroup> sdata: array<u32, WORKGROUP_SIZE>;
var<workgroup> scounter: u32;

// Sum of array ST
@compute @workgroup_size(1)
fn sumST() {
    let nNumbers = size;

    var sum = 0u;
    for (var i = 0u; i < nNumbers; i++) {
        sum += inputGlobal[i];
    }

    outputGlobal[0] = sum;
}

@compute @workgroup_size(WORKGROUP_SIZE) // Parallel reduction with interleaved addressing
fn sumReduce0(
    @builtin(global_invocation_id) global_invocation_id: vec3u,
    @builtin(local_invocation_id) local_invocation_id: vec3u,
    @builtin(workgroup_id) workgroup_id: vec3u
) {
    let globalID = global_invocation_id.x;
    let localID = local_invocation_id.x;
    let workgroupID = workgroup_id.x;

    sdata[localID] = inputGlobal[globalID];
    workgroupBarrier();

    for (var s = 1u; s < WORKGROUP_SIZE; s *= 2) {
        if (localID % (2 * s) == 0) {
            sdata[localID] += sdata[localID + s];
        }
        workgroupBarrier();
    }
    if (localID == 0) {
        outputGlobal[workgroupID] = sdata[0];
    }
}

@compute @workgroup_size(WORKGROUP_SIZE)  // Parallel reduction with strided index and non-divergent branch
fn sumReduce1(
    @builtin(global_invocation_id) global_invocation_id: vec3u,
    @builtin(local_invocation_id) local_invocation_id: vec3u,
    @builtin(workgroup_id) workgroup_id: vec3u
) {
    let globalID = global_invocation_id.x;
    let localID = local_invocation_id.x;
    let workgroupID = workgroup_id.x;

    sdata[localID] = inputGlobal[globalID];
    workgroupBarrier();

    for (var s = 1u; s < WORKGROUP_SIZE; s *= 2) {
        let index = 2*s*localID;
        if (index < WORKGROUP_SIZE) {
            sdata[index] += sdata[index + s];
        }
        workgroupBarrier();
    }
    if (localID == 0) {
        outputGlobal[workgroupID] = sdata[0];
    }
}

@compute @workgroup_size(WORKGROUP_SIZE) // Parallel reduction with sequential addressing
fn sumReduce2(
    @builtin(global_invocation_id) global_invocation_id: vec3u,
    @builtin(local_invocation_id) local_invocation_id: vec3u,
    @builtin(workgroup_id) workgroup_id: vec3u
) {
    let globalID = global_invocation_id.x;
    let localID = local_invocation_id.x;
    let workgroupID = workgroup_id.x;

    sdata[localID] = inputGlobal[globalID];
    workgroupBarrier();

    for (var s = WORKGROUP_SIZE/2; s > 0; s >>= 1) {
        if (localID < s) {
            sdata[localID] += sdata[localID + s];
        }
        workgroupBarrier();
    }
    if (localID == 0) {
        outputGlobal[workgroupID] = sdata[0];
    }
}

@compute @workgroup_size(WORKGROUP_SIZE) // Parallel reduction with first add during load
fn sumReduce3(
    @builtin(local_invocation_id) local_invocation_id: vec3u,
    @builtin(workgroup_id) workgroup_id: vec3u
) {
    let localID = local_invocation_id.x;
    let workgroupID = workgroup_id.x;
    let globalID = workgroupID * WORKGROUP_SIZE*2 + localID;

    sdata[localID] = inputGlobal[globalID] + inputGlobal[globalID + WORKGROUP_SIZE];
    workgroupBarrier();

    for (var s = WORKGROUP_SIZE/2; s > 0; s >>= 1) {
        if (localID < s) {
            sdata[localID] += sdata[localID + s];
        }
        workgroupBarrier();
    }
    if (localID == 0) {
        outputGlobal[workgroupID] = sdata[0];
    }
}

@compute @workgroup_size(WORKGROUP_SIZE) // Multiple pass reduction with unrolled loop
fn sumReduce4(
    @builtin(local_invocation_id) local_invocation_id: vec3u,
    @builtin(workgroup_id) workgroup_id: vec3u
) {
    let localID = local_invocation_id.x;
    let workgroupID = workgroup_id.x;
    let globalID = workgroupID * WORKGROUP_SIZE*2 + localID;

    sdata[localID] = inputGlobal[globalID] + inputGlobal[globalID + WORKGROUP_SIZE];
    workgroupBarrier();

    if (WORKGROUP_SIZE >= 2048) { if (localID < 1024) { sdata[localID] += sdata[localID + 1024]; } workgroupBarrier();}
    if (WORKGROUP_SIZE >= 1024) { if (localID < 512) { sdata[localID] += sdata[localID + 512]; } workgroupBarrier();}
    if (WORKGROUP_SIZE >= 512) { if (localID < 256) { sdata[localID] += sdata[localID + 256]; } workgroupBarrier();}
    if (WORKGROUP_SIZE >= 256) { if (localID < 128) { sdata[localID] += sdata[localID + 128]; } workgroupBarrier();}
    if (WORKGROUP_SIZE >= 128) { if (localID < 64) { sdata[localID] += sdata[localID + 64]; } workgroupBarrier();}
    if (localID < 32) { sdata[localID] += sdata[localID + 32]; } workgroupBarrier();
    if (localID < 16) { sdata[localID] += sdata[localID + 16]; } workgroupBarrier();
    if (localID < 8) { sdata[localID] += sdata[localID + 8]; } workgroupBarrier();
    if (localID < 4) { sdata[localID] += sdata[localID + 4]; } workgroupBarrier();
    if (localID < 2) { sdata[localID] += sdata[localID + 2]; } workgroupBarrier();
    if (localID < 1) { sdata[localID] += sdata[localID + 1]; } workgroupBarrier();

    if (localID == 0) {
        outputGlobal[workgroupID] = sdata[0];
    }
}

//const B
fn warpReduce(localID: u32, blockSize: u32) {
    sdata[localID] += sdata[localID + 32];
    sdata[localID] += sdata[localID + 16];
    sdata[localID] += sdata[localID + 8];
    sdata[localID] += sdata[localID + 4];
    sdata[localID] += sdata[localID + 2];
    sdata[localID] += sdata[localID + 1];
}

@compute @workgroup_size(WORKGROUP_SIZE) // Multiple pass reduction with inner warp SIMD 
fn sumReduce5(
    @builtin(local_invocation_id) local_invocation_id: vec3u,
    @builtin(workgroup_id) workgroup_id: vec3u
) {
    let localID = local_invocation_id.x;
    let workgroupID = workgroup_id.x;
    let globalID = workgroupID * WORKGROUP_SIZE*2 + localID;

    sdata[localID] = inputGlobal[globalID] + inputGlobal[globalID + WORKGROUP_SIZE];
    workgroupBarrier();

    if (WORKGROUP_SIZE >= 2048) { if (localID < 1024) { sdata[localID] += sdata[localID + 1024]; } workgroupBarrier();}
    if (WORKGROUP_SIZE >= 1024) { if (localID < 512) { sdata[localID] += sdata[localID + 512]; } workgroupBarrier();}
    if (WORKGROUP_SIZE >= 512) { if (localID < 256) { sdata[localID] += sdata[localID + 256]; } workgroupBarrier();}
    if (WORKGROUP_SIZE >= 256) { if (localID < 128) { sdata[localID] += sdata[localID + 128]; } workgroupBarrier();}
    if (WORKGROUP_SIZE >= 128) { if (localID < 64) { sdata[localID] += sdata[localID + 64]; } workgroupBarrier();}
    if (localID < 32) { warpReduce(localID, WORKGROUP_SIZE); }

    workgroupBarrier();
    if (localID == 0) {
        outputGlobal[workgroupID] = sdata[0];
    }
}

@compute @workgroup_size(WORKGROUP_SIZE) // Multiple pass reduction with arbitrary coarse factor
fn sumReduce6(
    @builtin(local_invocation_id) local_invocation_id: vec3u,
    @builtin(workgroup_id) workgroup_id: vec3u
) {
    let localID = local_invocation_id.x;
    let workgroupID = workgroup_id.x;
    var globalID = workgroupID * WORKGROUP_SIZE * COARSE_FACTOR + localID;

    var sum = 0u;
    for (var tile = 0u; tile < COARSE_FACTOR; tile++) {
        if (globalID < size) {
            sum +=  inputGlobal[globalID];
        }
        
        globalID += WORKGROUP_SIZE;
    }
    sdata[localID] = sum;
    workgroupBarrier();

    if (WORKGROUP_SIZE >= 2048) { if (localID < 1024) { sdata[localID] += sdata[localID + 1024]; } workgroupBarrier();}
    if (WORKGROUP_SIZE >= 1024) { if (localID < 512) { sdata[localID] += sdata[localID + 512]; } workgroupBarrier();}
    if (WORKGROUP_SIZE >= 512) { if (localID < 256) { sdata[localID] += sdata[localID + 256]; } workgroupBarrier();}
    if (WORKGROUP_SIZE >= 256) { if (localID < 128) { sdata[localID] += sdata[localID + 128]; } workgroupBarrier();}
    if (WORKGROUP_SIZE >= 128) { if (localID < 64) { sdata[localID] += sdata[localID + 64]; } workgroupBarrier();}
    if (localID < 32) { sdata[localID] += sdata[localID + 32]; } workgroupBarrier();
    if (localID < 16) { sdata[localID] += sdata[localID + 16]; } workgroupBarrier();
    if (localID < 8) { sdata[localID] += sdata[localID + 8]; } workgroupBarrier();
    if (localID < 4) { sdata[localID] += sdata[localID + 4]; } workgroupBarrier();
    if (localID < 2) { sdata[localID] += sdata[localID + 2]; } workgroupBarrier();
    if (localID < 1) { sdata[localID] += sdata[localID + 1]; };

    if (localID == 0) {
        outputGlobal[workgroupID] = sdata[0];
    }
}

@compute @workgroup_size(WORKGROUP_SIZE) // Single pass reduction with atomics
fn sumReduce7(
    @builtin(local_invocation_id) local_invocation_id: vec3u,
    @builtin(workgroup_id) workgroup_id: vec3u
) {
    let localID = local_invocation_id.x;
    let workgroupID = workgroup_id.x;
    var globalID = workgroupID * WORKGROUP_SIZE * COARSE_FACTOR + localID;

    var sum = 0u;
    for (var tile = 0u; tile < COARSE_FACTOR; tile++) {
        if (globalID < size) {
            sum +=  inputGlobal[globalID];
        }
        
        globalID += WORKGROUP_SIZE;
    }
    sdata[localID] = sum;
    workgroupBarrier();

    if (WORKGROUP_SIZE >= 2048) { if (localID < 1024) { sdata[localID] += sdata[localID + 1024]; } workgroupBarrier();}
    if (WORKGROUP_SIZE >= 1024) { if (localID < 512) { sdata[localID] += sdata[localID + 512]; } workgroupBarrier();}
    if (WORKGROUP_SIZE >= 512) { if (localID < 256) { sdata[localID] += sdata[localID + 256]; } workgroupBarrier();}
    if (WORKGROUP_SIZE >= 256) { if (localID < 128) { sdata[localID] += sdata[localID + 128]; } workgroupBarrier();}
    if (WORKGROUP_SIZE >= 128) { if (localID < 64) { sdata[localID] += sdata[localID + 64]; } workgroupBarrier();}
    if (localID < 32) { sdata[localID] += sdata[localID + 32]; } workgroupBarrier();
    if (localID < 16) { sdata[localID] += sdata[localID + 16]; } workgroupBarrier();
    if (localID < 8) { sdata[localID] += sdata[localID + 8]; } workgroupBarrier();
    if (localID < 4) { sdata[localID] += sdata[localID + 4]; } workgroupBarrier();
    if (localID < 2) { sdata[localID] += sdata[localID + 2]; } workgroupBarrier();
    if (localID < 1) { sdata[localID] += sdata[localID + 1]; }

    if (localID == 0) {
        atomicAdd(&outputGlobalAtomic, sdata[0]);
    }
}

@compute @workgroup_size(WORKGROUP_SIZE) // Single pass double step reduction
fn sumReduce8(
    @builtin(local_invocation_id) local_invocation_id: vec3u,
    @builtin(workgroup_id) workgroup_id: vec3u,
    @builtin(num_workgroups) num_workgroups: vec3u
) {
    let localID = local_invocation_id.x;
    let workgroupID = workgroup_id.x;
    var globalID = workgroupID * WORKGROUP_SIZE * COARSE_FACTOR + localID;

    var sum = 0u;
    for (var tile = 0u; tile < COARSE_FACTOR; tile++) {
        if (globalID < size) {
            sum +=  inputGlobal[globalID];
        }
        
        globalID += WORKGROUP_SIZE;
    }
    sdata[localID] = sum;
    workgroupBarrier();

    if (WORKGROUP_SIZE >= 2048) { if (localID < 1024) { sdata[localID] += sdata[localID + 1024]; } workgroupBarrier();}
    if (WORKGROUP_SIZE >= 1024) { if (localID < 512) { sdata[localID] += sdata[localID + 512]; } workgroupBarrier();}
    if (WORKGROUP_SIZE >= 512) { if (localID < 256) { sdata[localID] += sdata[localID + 256]; } workgroupBarrier();}
    if (WORKGROUP_SIZE >= 256) { if (localID < 128) { sdata[localID] += sdata[localID + 128]; } workgroupBarrier();}
    if (WORKGROUP_SIZE >= 128) { if (localID < 64) { sdata[localID] += sdata[localID + 64]; } workgroupBarrier();}
    if (localID < 32) { sdata[localID] += sdata[localID + 32]; } workgroupBarrier();
    if (localID < 16) { sdata[localID] += sdata[localID + 16]; } workgroupBarrier();
    if (localID < 8) { sdata[localID] += sdata[localID + 8]; } workgroupBarrier();
    if (localID < 4) { sdata[localID] += sdata[localID + 4]; } workgroupBarrier();
    if (localID < 2) { sdata[localID] += sdata[localID + 2]; } workgroupBarrier();

    if (localID == 0) {
        outputGlobal[workgroupID] = sdata[0] + sdata[1];
        atomicSub(&outputGlobalAtomic, 1);

        let remainingWorkgroups = atomicLoad(&outputGlobalAtomic);
        scounter = remainingWorkgroups;
    }
    workgroupBarrier();
    
    let counter = scounter;
    if (counter == 0) {
        let numWorkgroups = num_workgroups.x;
        globalID = localID;

        sum = 0u;
        while (globalID < numWorkgroups) {
            sum += outputGlobal[globalID];
            globalID += WORKGROUP_SIZE;
        }
        sdata[localID] = sum;
    }
    workgroupBarrier();
    
    if (WORKGROUP_SIZE >= 2048) { if (localID < 1024) { sdata[localID] += sdata[localID + 1024]; } workgroupBarrier();}
    if (WORKGROUP_SIZE >= 1024) { if (localID < 512) { sdata[localID] += sdata[localID + 512]; } workgroupBarrier();}
    if (WORKGROUP_SIZE >= 512) { if (localID < 256) { sdata[localID] += sdata[localID + 256]; } workgroupBarrier();}
    if (WORKGROUP_SIZE >= 256) { if (localID < 128) { sdata[localID] += sdata[localID + 128]; } workgroupBarrier();}
    if (WORKGROUP_SIZE >= 128) { if (localID < 64) { sdata[localID] += sdata[localID + 64]; } workgroupBarrier();}
    if (localID < 32) { sdata[localID] += sdata[localID + 32]; } workgroupBarrier();
    if (localID < 16) { sdata[localID] += sdata[localID + 16]; } workgroupBarrier();
    if (localID < 8) { sdata[localID] += sdata[localID + 8]; } workgroupBarrier();
    if (localID < 4) { sdata[localID] += sdata[localID + 4]; } workgroupBarrier();
    if (localID < 2) { sdata[localID] += sdata[localID + 2]; } workgroupBarrier();

    if (localID == 0 && counter == 0) {
        outputGlobal[0] = sdata[0] + sdata[1];
    }
    
}

@compute @workgroup_size(WORKGROUP_SIZE) // Single pass reduction with atomics, only subgroups used for the final sum
fn sumReduce9(
    @builtin(local_invocation_id) local_invocation_id: vec3u,
    @builtin(workgroup_id) workgroup_id: vec3u
) {
    let localID = local_invocation_id.x;
    let workgroupID = workgroup_id.x;
    var globalID = workgroupID * WORKGROUP_SIZE * COARSE_FACTOR + localID;

    var sum = 0u;
    for (var tile = 0u; tile < COARSE_FACTOR; tile++) {
        if (globalID < size) {
            sum +=  inputGlobal[globalID];
        }
        
        globalID += WORKGROUP_SIZE;
    }

    let subgroupSum = subgroupAdd(sum);
    if (subgroupElect()) {
        atomicAdd(&outputGlobalAtomic, subgroupSum);
    }
}

@compute @workgroup_size(WORKGROUP_SIZE) // Single pass reduction with atomics, hybrid approach (shared memory and subgroups)
fn sumReduce10(
    @builtin(local_invocation_id) local_invocation_id: vec3u,
    @builtin(workgroup_id) workgroup_id: vec3u,
    @builtin(subgroup_size) subgroup_size: u32
) {
    let localID = local_invocation_id.x;
    let workgroupID = workgroup_id.x;
    var globalID = workgroupID * WORKGROUP_SIZE * COARSE_FACTOR + localID;

    var sum = 0u;
    for (var tile = 0u; tile < COARSE_FACTOR; tile++) {
        if (globalID < size) {
            sum +=  inputGlobal[globalID];
        }
        
        globalID += WORKGROUP_SIZE;
    }
    sdata[localID] = sum;
    workgroupBarrier();

    // Shared memory reduction
    for (var s = WORKGROUP_SIZE/2; s >= subgroup_size; s >>= 1) {
        if (localID < s) {
            sdata[localID] += sdata[localID + s];
        }
        workgroupBarrier();
    }

    // Subgroup SIMD operation
    let subgroupSum = subgroupAdd(sdata[localID]);

    if (localID == 0) {
        atomicAdd(&outputGlobalAtomic, subgroupSum);
    }
}