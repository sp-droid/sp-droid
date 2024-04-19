#include <GLFW/glfw3.h>
#include <webgpu/webgpu.h>
#include <glfw3webgpu.h>

#include "webgpu-utils.h"

#include <iostream>
#include <cassert>
#include <vector>

int main(int, char**) {
    WGPUInstanceDescriptor desc = {};
    desc.nextInChain = nullptr;
    WGPUInstance instance = wgpuCreateInstance(&desc);
    if (!instance) {
        std::cerr << "Could not initialize WebGPU!" << std::endl;
        return 1;
    }

    glfwInit();
    if (!glfwInit()) {
        std::cerr << "Could not initialize GLFW!" << std::endl;
        return 1;
    }
    
    // Tell GLFW not to care about the API because it does not know WebGPU and we won't use what it could by default setup for other APIs
    glfwWindowHint(GLFW_CLIENT_API, GLFW_NO_API);
    glfwWindowHint(GLFW_RESIZABLE, GLFW_FALSE); // for now
    // Create the window
    GLFWwindow* window = glfwCreateWindow(1280, 720, "Learn WebGPU", NULL, NULL);
    if (!window) {
        std::cerr << "Could not open window!" << std::endl;
        return 1;
    }
    int width, height;
    glfwGetWindowSize(window, &width, &height);

    std::cout << "Requesting adapter..." << std::endl;
    
    // Get drawing surface
    WGPUSurface surface = glfwGetWGPUSurface(instance, window);
    WGPURequestAdapterOptions adapterOpts = {};
    adapterOpts.nextInChain = nullptr;
    adapterOpts.compatibleSurface = surface;

    WGPUAdapter adapter = requestAdapter(instance, &adapterOpts);
    std::cout << "Got adapter: " << adapter << std::endl;

	std::cout << "Requesting device..." << std::endl;
	WGPUDeviceDescriptor deviceDesc = {};
    deviceDesc.nextInChain = nullptr;
    deviceDesc.label = "My Device"; // anything works here, that's your call
    deviceDesc.requiredFeaturesCount = 0; // we do not require any specific feature
    deviceDesc.requiredLimits = nullptr; // we do not require any specific limit
    deviceDesc.defaultQueue.nextInChain = nullptr;
    deviceDesc.defaultQueue.label = "The default queue";
	// [...] Build device descriptor
	WGPUDevice device = requestDevice(adapter, &deviceDesc);
    
    std::cout << "Got device: " << device << std::endl;

    auto onDeviceError = [](WGPUErrorType type, char const* message, void* /* pUserData */) {
        std::cout << "Uncaptured device error: type " << type;
        if (message) std::cout << " (" << message << ")";
        std::cout << std::endl;
        };
    wgpuDeviceSetUncapturedErrorCallback(device, onDeviceError, nullptr /* pUserData */);

    // inspectDevice(device);

    WGPUQueue queue = wgpuDeviceGetQueue(device);

    WGPUSwapChainDescriptor swapChainDesc = {};
    swapChainDesc.nextInChain = nullptr;
    swapChainDesc.width = width;
    swapChainDesc.height = height;
    #ifdef WEBGPU_BACKEND_WGPU
        swapChainDesc.format = wgpuSurfaceGetPreferredFormat(surface, adapter);
    #else
        swapChainDesc.format = WGPUTextureFormat_BGRA8Unorm;
    #endif
    swapChainDesc.usage = WGPUTextureUsage_RenderAttachment;
    swapChainDesc.presentMode = WGPUPresentMode_Fifo;

    WGPUSwapChain swapChain = wgpuDeviceCreateSwapChain(device, surface, &swapChainDesc);
    std::cout << "Swapchain: " << swapChain << std::endl;

    // Main loop
    int frameNumber = 0;
    while (!glfwWindowShouldClose(window)) {
        // Check whether the user clicked on the close button (and any other
        // mouse/key event, which we don't use so far)
        glfwPollEvents();

        // Get the texture where to draw the next frame
        WGPUTextureView nextTexture = wgpuSwapChainGetCurrentTextureView(swapChain);
        // Getting the texture may fail, in particular if the window has been resized
        // and thus the target surface changed.
        if (!nextTexture) {
            std::cerr << "Cannot acquire next swap chain texture" << std::endl;
            break;
        }

        frameNumber++;
        std::cout << "nextTexture: " << nextTexture << " - " << frameNumber << std::endl;
        
        WGPUCommandEncoderDescriptor commandEncoderDesc = {};
        commandEncoderDesc.nextInChain = nullptr;
        commandEncoderDesc.label = "Command Encoder";
        WGPUCommandEncoder encoder = wgpuDeviceCreateCommandEncoder(device, &commandEncoderDesc);

        // Describe a render pass, which targets the texture view
        WGPURenderPassDescriptor renderPassDesc = {};

        WGPURenderPassColorAttachment renderPassColorAttachment = {};
        // The attachment is tighed to the view returned by the swap chain, so that
        // the render pass draws directly on screen.
        renderPassColorAttachment.view = nextTexture;
        // Not relevant here because we do not use multi-sampling
        renderPassColorAttachment.resolveTarget = nullptr;
        renderPassColorAttachment.loadOp = WGPULoadOp_Clear;
        renderPassColorAttachment.storeOp = WGPUStoreOp_Store;
        renderPassColorAttachment.clearValue = WGPUColor{ 0.5, 0.1, 0.2, 1.0 };
        renderPassDesc.colorAttachmentCount = 1;
        renderPassDesc.colorAttachments = &renderPassColorAttachment;

        // No depth buffer for now
        renderPassDesc.depthStencilAttachment = nullptr;

        // We do not use timers for now neither
        renderPassDesc.timestampWriteCount = 0;
        renderPassDesc.timestampWrites = nullptr;

        renderPassDesc.nextInChain = nullptr;

        // Create a render pass. We end it immediately because we use its built-in
        // mechanism for clearing the screen when it begins (see descriptor).
        WGPURenderPassEncoder renderPass = wgpuCommandEncoderBeginRenderPass(encoder, &renderPassDesc);
        wgpuRenderPassEncoderEnd(renderPass);
        wgpuRenderPassEncoderRelease(renderPass);

        wgpuTextureViewRelease(nextTexture);

        WGPUCommandBufferDescriptor cmdBufferDescriptor = {};
        cmdBufferDescriptor.nextInChain = nullptr;
        cmdBufferDescriptor.label = "Command buffer";
        WGPUCommandBuffer command = wgpuCommandEncoderFinish(encoder, &cmdBufferDescriptor);
        wgpuCommandEncoderRelease(encoder);
        wgpuQueueSubmit(queue, 1, &command);
        wgpuCommandBufferRelease(command);

        // We can tell the swap chain to present the next texture.
        wgpuSwapChainPresent(swapChain);
    }
    
    // Clean up
    wgpuSwapChainRelease(swapChain);
    wgpuQueueRelease(queue);
    wgpuDeviceRelease(device);
    wgpuAdapterRelease(adapter);
    wgpuSurfaceRelease(surface);
    wgpuInstanceRelease(instance);

    // At the end of the program, destroy the window
    glfwDestroyWindow(window);
    glfwTerminate();

    return 0;
}

