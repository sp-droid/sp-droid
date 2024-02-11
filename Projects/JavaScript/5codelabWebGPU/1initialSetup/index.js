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
const canvas = document.querySelector("canvas");
const context = canvas.getContext('webgpu');
const canvasFormat = navigator.gpu.getPreferredCanvasFormat();
context.configure({
    device: device,
    format: canvasFormat
});

// Encoder that sends instructions to the GPU
const encoder = device.createCommandEncoder();

// Render pass
const pass = encoder.beginRenderPass({
    colorAttachments: [{
        view: context.getCurrentTexture().createView(),
        loadOp: "clear",
        clearValue: [0,0,0.4,1],
        storeOp: "store"
    }]
});
pass.end();

// Create a command buffer and submit it to the queue of the GPU device
device.queue.submit([encoder.finish()]);

