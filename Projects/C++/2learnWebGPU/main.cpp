#include <GLFW/glfw3.h>
#include <webgpu/webgpu.h>
#include <glfw3webgpu.h>

#include "webgpu-utils.h"

#include <iostream>
#include <cassert>
#include <vector>

int main(int, char**) {
    std::cout << "Starting program" << std::endl;

    glfwInit();
    if (!glfwInit()) {
        std::cerr << "Could not initialize GLFW!" << std::endl;
        return 1;
    }
    
    // Tell GLFW not to care about the API because it does not know WebGPU and we won't use what it could by default setup for other APIs
    glfwWindowHint(GLFW_CLIENT_API, GLFW_NO_API);
    // Create the window
    GLFWwindow* window = glfwCreateWindow(1280, 720, "Learn WebGPU", NULL, NULL);

    // Create a descriptor
    WGPUInstanceDescriptor desc = {};
    desc.nextInChain = nullptr;
    // Create the instance using this descriptor
    WGPUInstance instance = wgpuCreateInstance(&desc);
    if (!instance) {
        std::cerr << "Could not initialize WebGPU!" << std::endl;
        return 1;
    }
    // Display the object (WGPUInstance is a simple pointer)
    std::cout << "WGPU instance: " << instance << std::endl;

    std::cout << "Requesting adapter..." << std::endl;

    // Get drawing surface
    WGPUSurface surface = glfwGetWGPUSurface(instance, window);

    WGPURequestAdapterOptions adapterOpts = {};
    adapterOpts.nextInChain = nullptr;
    adapterOpts.compatibleSurface = surface;

    WGPUAdapter adapter = requestAdapter(instance, &adapterOpts);

    std::cout << "Got adapter: " << adapter << std::endl;

    std::vector<WGPUFeatureName> features;
    // First get the number of features
    size_t featureCount = wgpuAdapterEnumerateFeatures(adapter, nullptr);
    // Allocate memory for them
    features.resize(featureCount);
    // Retrieve the features
    wgpuAdapterEnumerateFeatures(adapter, features.data());

    std::cout << "Adapter features:" << std::endl;
    for (auto feature : features) {
        std::cout << " - " << feature << std::endl;
    }
	
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

    auto onDeviceError = [](WGPUErrorType type, char const* message, void* /* pUserData */) {
        std::cout << "Uncaptured device error: type " << type;
        if (message) std::cout << " (" << message << ")";
        std::cout << std::endl;
        };
    wgpuDeviceSetUncapturedErrorCallback(device, onDeviceError, nullptr /* pUserData */);

	std::cout << "Got device: " << device << std::endl;

    // Main loop
    while (!glfwWindowShouldClose(window)) {
        // Check whether the user clicked on the close button (and any other
        // mouse/key event, which we don't use so far)
        glfwPollEvents();
    }
    
    // Clean up
    wgpuDeviceRelease(device);
    wgpuAdapterRelease(adapter);
    wgpuSurfaceRelease(surface);
    wgpuInstanceRelease(instance);

    // At the end of the program, destroy the window
    glfwDestroyWindow(window);
    glfwTerminate();

    return 0;
}

