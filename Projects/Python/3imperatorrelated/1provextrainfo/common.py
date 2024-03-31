# Function collection file

from PIL import Image

import numpy as np
from numba import cuda

# Crop image according to latitude, assuming it originally covers the whole globe
def latitudeCropImage(image: np.ndarray, MIN_LAT: float, MAX_LAT: float):
    # Get the width and height of the image
    height, width = image.shape

    # Calculate the y-coordinates for the clipping range
    min_y = int((90 - MAX_LAT) / 180 * height)
    max_y = int((90 - MIN_LAT) / 180 * height)

    # Create a new image with the clipped region
    return image[min_y:max_y+1,:]

# Save image to test.png file, normalizing for uint8 grayscale values
def smallNormImage(image: np.ndarray):
    minValue, maxValue = np.min(image), np.max(image)
    normImage = (image - minValue) / (maxValue - minValue) * 255
    return Image.fromarray(np.uint8(normImage)).resize((400,200))

# Save image to test.png file, normalizing for uint8 grayscale values
def saveTestImage(image: np.ndarray):
    minValue, maxValue = np.min(image), np.max(image)
    normImage = (image - minValue) / (maxValue - minValue) * 255
    Image.fromarray(np.uint8(normImage)).save('test.png')
    return

# In 2D, given a mask of target values, a mask of valid values and a data array, fix all targets to the nearest valid values
def kernelSearchAndFixToNearest(yLimit: int, xLimit: int):
    @cuda.jit
    def func(data, maskTarget, maskValid):
        y, x = cuda.grid(2)
        
        # Out of bonds
        if y >= yLimit or x >= xLimit: return

        # Non targets
        if maskTarget[y,x] == False: return

        radius = 0
        while True:
            radius += 1
            rx = x
            ry = max(y - radius,0)
            if maskValid[ry,rx] == True: break

            rx = max(x - radius,0)
            ry = y
            if maskValid[ry,rx] == True: break

            rx = x
            ry = min(y + radius,yLimit-1)
            if maskValid[ry,rx] == True: break

            rx = min(x + radius,xLimit-1)
            ry = y
            if maskValid[ry,rx] == True: break

            rx = max(x - radius,0)
            ry = max(y - radius,0)
            if maskValid[ry,rx] == True: break

            rx = max(x - radius,0)
            ry = min(y + radius,yLimit-1)
            if maskValid[ry,rx] == True: break

            rx = min(x + radius,xLimit-1)
            ry = max(y - radius,0)
            if maskValid[ry,rx] == True: break

            rx = min(x + radius,xLimit-1)
            ry = min(y + radius,yLimit-1)
            if maskValid[ry,rx] == True: break

        data[y,x] = data[ry,rx]

    return func
def GPUsearchAndFixToNearest(
        data: np.ndarray,
        maskTarget: np.ndarray,
        maskValid: np.ndarray,
        debug: bool=True
    ) -> np.ndarray:
    if maskTarget.shape != data.shape: raise ValueError(f"Mask dims: {maskTarget.shape}; Data dims: {data.shape}")
    if maskValid.shape != data.shape: raise ValueError(f"Mask dims: {maskValid.shape}; Data dims: {data.shape}")
    
    if debug: print(f"Target values in mask: {np.sum(maskTarget)}")
    
    yLimit, xLimit = data.shape
    gpuMaskTarget = cuda.to_device(np.bool8(maskTarget))
    gpuMaskValid = cuda.to_device(np.bool8(maskValid))
    gpuData = cuda.to_device(np.float32(data))

    threadsperblock = (16,16)
    blockspergrid = tuple(int(np.ceil(data.shape[i]/threadsperblock[i])) for i in range(2))

    kernelSearchAndFixToNearest(yLimit, xLimit)[blockspergrid, threadsperblock](gpuData, gpuMaskTarget, gpuMaskValid)
    
    modifiedData = gpuData.copy_to_host()

    return modifiedData

# In 2D-RGB, given a keyed (0, 1, 2...) 2D array and a list with colors corresponding to the keys in the same order, it paints an RGB 2D array
def kernelPaintColorKeys(yLimit: int, xLimit: int):
    @cuda.jit
    def func(keyedMap, colorKeys):
        y, x = cuda.grid(2)
        
        # Out of bonds
        if y >= yLimit or x >= xLimit: return

        key = keyedMap[y,x,0]
        for i in range(3): keyedMap[y,x,i] = colorKeys[key][i]

    return func

def GPUpaintColorKeys(keyedMap: np.ndarray, colorKeys: list):
    yLimit, xLimit = keyedMap.shape

    gpuColorKeys = cuda.to_device(colorKeys)
    coloredMap = np.zeros(shape=(yLimit,xLimit,3), dtype=np.uint8)
    coloredMap[:,:,0] = keyedMap
    gpuColoredMap = cuda.to_device(coloredMap)

    threadsperblock = (16,16)
    blockspergrid = tuple(int(np.ceil(keyedMap.shape[i]/threadsperblock[i])) for i in range(2))

    kernelPaintColorKeys(yLimit, xLimit)[blockspergrid, threadsperblock](gpuColoredMap, gpuColorKeys)
    coloredMap = gpuColoredMap.copy_to_host()

    return coloredMap