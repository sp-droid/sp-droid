import os
import pillow_avif
from PIL import Image
from tqdm import tqdm
from joblib import Parallel, delayed

## OUTPUT OPTIONS
name = 'europe'
delay = 20 #in ms
endDelay = 1000 #in ms
leftTopRightBottom = (7352,854,10287,2504)
horizontalSize = 1920
## OUTPUT OPTIONS

aspectRatio = (leftTopRightBottom[2]-leftTopRightBottom[0])/(leftTopRightBottom[3]-leftTopRightBottom[1])
sizes = (horizontalSize, int(horizontalSize/aspectRatio))

#filenames = [file for i, file in enumerate(os.listdir('steps')) if file.endswith('png') and i%4==0 ]
filenames = [file for file in os.listdir('steps') if file.endswith('png') ]

def loadImage(filename):
    Image.MAX_IMAGE_PIXELS = None
    return Image.open('steps/'+filename).crop(leftTopRightBottom).resize(sizes, Image.Resampling.LANCZOS)
images = Parallel(n_jobs=-1)(delayed(loadImage)(filename) for filename in tqdm(filenames))

Image.MAX_IMAGE_PIXELS = None
image = Image.open('steps/'+filenames[-1]).crop(leftTopRightBottom).resize(sizes, Image.Resampling.LANCZOS)
image.save('thumbnail.jpg')
for _ in range(int(endDelay/delay)):
    images.append(image)

images[0].save(f'{name}.avif', quality=80, save_all=True, optimize=False, append_images=images[1:], duration=delay)