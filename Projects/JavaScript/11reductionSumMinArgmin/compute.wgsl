struct ComputeInput {
    @builtin(global_invocation_id) cell: vec3u
}

// Constants
const WORKGROUP_SIZE: u32 = $WORKGROUP_SIZE$;
const COARSE_FACTOR: u32 = $COARSE_FACTOR$;

// Global memory
@group(0) @binding(0) var<uniform> size: u32;
@group(0) @binding(1) var<storage, read> inputGlobal: array<u32>;
@group(0) @binding(2) var<storage, read_write> outputGlobalAtomicValue: atomic<u32>;

// Shared memory
var<workgroup> sdata: array<u32, WORKGROUP_SIZE>;
var<workgroup> sdataWithIndex: array<array<u32, WORKGROUP_SIZE>, 2>;

@compute @workgroup_size(WORKGROUP_SIZE)
fn reductionSum(
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
        atomicAdd(&outputGlobalAtomicValue, sdata[0]);
    }
}

const MAX_UINT32: u32 = 4294967295u;
fn minShared(id1: u32, id2: u32) {
    let value = sdata[id2];
    if (value < sdata[id1]) {
        sdata[id1] = value;
    }
}
@compute @workgroup_size(WORKGROUP_SIZE)
fn reductionMin(
    @builtin(local_invocation_id) local_invocation_id: vec3u,
    @builtin(workgroup_id) workgroup_id: vec3u
) {
    let localID = local_invocation_id.x;
    let workgroupID = workgroup_id.x;
    var globalID = workgroupID * WORKGROUP_SIZE * COARSE_FACTOR + localID;

    var min = MAX_UINT32;
    for (var tile = 0u; tile < COARSE_FACTOR; tile++) {
        if (globalID < size) {
            let value = inputGlobal[globalID];
            if (value < min) {
                min = value;
            }
        }
        globalID += WORKGROUP_SIZE;
    }
    sdata[localID] = min;
    workgroupBarrier();

    if (WORKGROUP_SIZE >= 2048) { if (localID < 1024) { minShared(localID, localID + 1024); } workgroupBarrier();}
    if (WORKGROUP_SIZE >= 1024) { if (localID < 512) { minShared(localID, localID + 512); } workgroupBarrier();}
    if (WORKGROUP_SIZE >= 512) { if (localID < 256) { minShared(localID, localID + 256); } workgroupBarrier();}
    if (WORKGROUP_SIZE >= 256) { if (localID < 128) { minShared(localID, localID + 128); } workgroupBarrier();}
    if (WORKGROUP_SIZE >= 128) { if (localID < 64) { minShared(localID, localID + 64); } workgroupBarrier();}
    if (localID < 32) { minShared(localID, localID + 32); } workgroupBarrier();
    if (localID < 16) { minShared(localID, localID + 16); } workgroupBarrier();
    if (localID < 8) { minShared(localID, localID + 8); } workgroupBarrier();
    if (localID < 4) { minShared(localID, localID + 4); } workgroupBarrier();
    if (localID < 2) { minShared(localID, localID + 2); } workgroupBarrier();
    if (localID < 1) { minShared(localID, localID + 1); }

    if (localID == 0) {
        atomicMin(&outputGlobalAtomicValue, sdata[0]);
    }
}

fn minSharedWithIndex(id1: u32, id2: u32) {
    let value = sdataWithIndex[1][id2];
    if (value < sdataWithIndex[1][id2]) {
        sdataWithIndex[0][id1] = sdataWithIndex[0][id2];
        sdataWithIndex[1][id1] = value;
    }
}
@compute @workgroup_size(WORKGROUP_SIZE)
fn reductionArgmin(
    @builtin(local_invocation_id) local_invocation_id: vec3u,
    @builtin(workgroup_id) workgroup_id: vec3u
) {
    let localID = local_invocation_id.x;
    let workgroupID = workgroup_id.x;
    var globalID = workgroupID * WORKGROUP_SIZE * COARSE_FACTOR + localID;

    var index = 0u;
    var min = 4294967295u;
    if (globalID < size) {
        index = globalID;
        min = inputGlobal[globalID];
    }
    for (var tile = 1u; tile < COARSE_FACTOR; tile++) {
        if (globalID < size) {
            let value = inputGlobal[globalID];
            if (value < min) {
                index = globalID;
                min = value;
            }
        }
        globalID += WORKGROUP_SIZE;
    }
    sdataWithIndex[0][localID] = index;
    sdataWithIndex[1][localID] = min;
    workgroupBarrier();

    if (WORKGROUP_SIZE >= 2048) { if (localID < 1024) { minSharedWithIndex(localID, localID + 1024); } workgroupBarrier();}
    if (WORKGROUP_SIZE >= 1024) { if (localID < 512) { minSharedWithIndex(localID, localID + 512); } workgroupBarrier();}
    if (WORKGROUP_SIZE >= 512) { if (localID < 256) { minSharedWithIndex(localID, localID + 256); } workgroupBarrier();}
    if (WORKGROUP_SIZE >= 256) { if (localID < 128) { minSharedWithIndex(localID, localID + 128); } workgroupBarrier();}
    if (WORKGROUP_SIZE >= 128) { if (localID < 64) { minSharedWithIndex(localID, localID + 64); } workgroupBarrier();}
    if (localID < 32) { minSharedWithIndex(localID, localID + 32); } workgroupBarrier();
    if (localID < 16) { minSharedWithIndex(localID, localID + 16); } workgroupBarrier();
    if (localID < 8) { minSharedWithIndex(localID, localID + 8); } workgroupBarrier();
    if (localID < 4) { minSharedWithIndex(localID, localID + 4); } workgroupBarrier();
    if (localID < 2) { minSharedWithIndex(localID, localID + 2); } workgroupBarrier();
    if (localID < 1) { minSharedWithIndex(localID, localID + 1); }

    if (localID == 0) {
        globalID = atomicLoad(&outputGlobalAtomicValue);
        min = inputGlobal[globalID];

        if (sdataWithIndex[1][0] < min) {
            atomicStore(&outputGlobalAtomicValue, sdataWithIndex[0][0]);
        }
    }
}