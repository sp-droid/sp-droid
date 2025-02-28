// Constants
const WORKGROUP_SIZE: u32 = $WORKGROUP_SIZE$;
const COARSE_FACTOR: u32 = $COARSE_FACTOR$;

// Global memory
@group(0) @binding(0) var<uniform> size: u32;
@group(0) @binding(1) var<storage, read> inputGlobal: array<u32>;
@group(0) @binding(2) var<storage, read_write> globalAtomicCounter: atomic<u32>;
@group(0) @binding(3) var<storage, read_write> outputGlobal: array<u32>;

// Shared memory
var<workgroup> sdata: array<u32, WORKGROUP_SIZE>;
var<workgroup> sdataWithIndex: array<array<u32, WORKGROUP_SIZE>, 2>;
var<workgroup> scounter: u32;

@compute @workgroup_size(WORKGROUP_SIZE)
fn reductionSum(
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

        atomicSub(&globalAtomicCounter, 1);
        let remainingWorkgroups = atomicLoad(&globalAtomicCounter);
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
    @builtin(workgroup_id) workgroup_id: vec3u,
    @builtin(num_workgroups) num_workgroups: vec3u
) {
    let localID = local_invocation_id.x;
    let workgroupID = workgroup_id.x;
    var globalID = workgroupID * WORKGROUP_SIZE * COARSE_FACTOR + localID;

    var minValue = MAX_UINT32;
    for (var tile = 0u; tile < COARSE_FACTOR; tile++) {
        if (globalID < size) {
            let value = inputGlobal[globalID];
            if (value < minValue) {
                minValue = value;
            }
        }
        globalID += WORKGROUP_SIZE;
    }
    sdata[localID] = minValue;
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
    
    if (localID == 0) {
        outputGlobal[workgroupID] = min(sdata[0],sdata[1]);

        atomicSub(&globalAtomicCounter, 1);
        let remainingWorkgroups = atomicLoad(&globalAtomicCounter);
        scounter = remainingWorkgroups;
    }
    workgroupBarrier();
    
    let counter = scounter;
    if (counter == 0) {
        let numWorkgroups = num_workgroups.x;
        globalID = localID;

        minValue = MAX_UINT32;
        while (globalID < numWorkgroups) {
            let value = outputGlobal[globalID];
            if (value < minValue) {
                minValue = value;
            }
            globalID += WORKGROUP_SIZE;
        }
        sdata[localID] = minValue;
    }
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

    if (localID == 0 && counter == 0) {
        outputGlobal[0] = min(sdata[0],sdata[1]);
    }
}

fn minSharedWithIndex(id1: u32, id2: u32) {
    let value = sdataWithIndex[1][id2];
    if (value < sdataWithIndex[1][id1]) {
        sdataWithIndex[0][id1] = sdataWithIndex[0][id2];
        sdataWithIndex[1][id1] = value;
    }
}
@compute @workgroup_size(WORKGROUP_SIZE)
fn reductionArgmin(
    @builtin(local_invocation_id) local_invocation_id: vec3u,
    @builtin(workgroup_id) workgroup_id: vec3u,
    @builtin(num_workgroups) num_workgroups: vec3u
) {
    let localID = local_invocation_id.x;
    let workgroupID = workgroup_id.x;
    var globalID = workgroupID * WORKGROUP_SIZE * COARSE_FACTOR + localID;

    var index = 0u;
    var minValue = 4294967295u;
    if (globalID < size) {
        index = globalID;
        minValue = inputGlobal[globalID];
    }
    for (var tile = 1u; tile < COARSE_FACTOR; tile++) {
        globalID += WORKGROUP_SIZE;
        if (globalID < size) {
            let value = inputGlobal[globalID];
            if (value < minValue) {
                index = globalID;
                minValue = value;
            }
        }
    }
    sdataWithIndex[0][localID] = index;
    sdataWithIndex[1][localID] = minValue;
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

    if (localID == 0) {
        minSharedWithIndex(0, 1);
        outputGlobal[workgroupID] = sdataWithIndex[0][0];

        atomicSub(&globalAtomicCounter, 1);
        let remainingWorkgroups = atomicLoad(&globalAtomicCounter);
        scounter = remainingWorkgroups;
    }
    workgroupBarrier();
    
    let counter = scounter;
    if (counter == 0) {
        let numWorkgroups = num_workgroups.x;
        globalID = localID;

        index = 0u;
        minValue = 4294967295u;
        if (globalID < numWorkgroups) {
            index = outputGlobal[globalID];
            minValue = inputGlobal[index];
        }
        globalID += WORKGROUP_SIZE;
        while (globalID < numWorkgroups) {
            let index2 = outputGlobal[globalID];
            let value = inputGlobal[globalID];
            if (value < minValue) {
                index = index2;
                minValue = value;
            }
            globalID += WORKGROUP_SIZE;
        }
        sdataWithIndex[0][localID] = index;
        sdataWithIndex[1][localID] = minValue;
    }
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

    if (localID == 0 && counter == 0) {
        minSharedWithIndex(0, 1);
        outputGlobal[0] = sdataWithIndex[0][0];
    }
}