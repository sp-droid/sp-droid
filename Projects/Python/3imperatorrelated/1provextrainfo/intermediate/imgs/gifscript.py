import os
import pillow_avif
from PIL import Image
from tqdm import tqdm
from joblib import Parallel, delayed

## OUTPUT OPTIONS
name = 'europe'
delay = 100 #in ms
endDelay = 2000 #in ms
leftTopRightBottom = (12600,4800,16300,7400)
horizontalSize = 1920
## OUTPUT OPTIONS

aspectRatio = (leftTopRightBottom[2]-leftTopRightBottom[0])/(leftTopRightBottom[3]-leftTopRightBottom[1])
sizes = (horizontalSize, int(horizontalSize/aspectRatio))

filenames = [file for file in os.listdir() if file.endswith('png')]

# images = []
# for filename in tqdm(filenames):
#     image = Image.open(filename).crop(leftTopRightBottom).resize(sizes, Image.Resampling.LANCZOS)
#     images.append(image)
def loadImage(filename):
    Image.MAX_IMAGE_PIXELS = None
    return Image.open(filename).crop(leftTopRightBottom).resize(sizes, Image.Resampling.LANCZOS)
images = Parallel(n_jobs=-1)(delayed(loadImage)(filename) for filename in tqdm(filenames))

Image.MAX_IMAGE_PIXELS = None
image = Image.open(filenames[-1]).crop(leftTopRightBottom).resize(sizes, Image.Resampling.LANCZOS)
for _ in range(int(endDelay/delay)):
    images.append(image)

images[0].save(f'{name}.avif', quality=70, save_all=True, optimize=False, append_images=images[1:], duration=delay)