import os
import numpy as np
import imageio.v2 as imageio
from PIL import Image
from tqdm import tqdm

Image.MAX_IMAGE_PIXELS = None

## OUTPUT OPTIONS
name = 'europe'
delay = 500 #in ms
loop = 4
leftTopRightBottom = (12600,4800,16300,7400)
horizontalSize = 1920
##

aspectRatio = (leftTopRightBottom[2]-leftTopRightBottom[0])/(leftTopRightBottom[3]-leftTopRightBottom[1])
sizes = (horizontalSize, int(horizontalSize/aspectRatio))

filenames = [file for file in os.listdir() if file.endswith('png')]

images = []
for filename in tqdm(filenames):
    image = imageio.imread(filename)

    image = Image.fromarray(image).crop(leftTopRightBottom).resize(sizes, Image.Resampling.LANCZOS)
    images.append(np.array(image, dtype=np.uint8))
for _ in range(5):
    images.append(np.array(image, dtype=np.uint8))

imageio.mimsave('europe.gif', images, format='GIF', loop=loop, duration=delay)