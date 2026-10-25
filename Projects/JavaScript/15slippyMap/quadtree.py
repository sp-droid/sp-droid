import os
from PIL import Image
import numpy as np
import math

def majority_downscale(img):
    """
    Halves the image with majority (discrete bilinear) interpolation, so no new colours are created.
    When halving, the output pixel centre lies exactly between 4 source pixels, so all bilinear weights
    are 0.25 and the majority vote becomes the most common colour of each 2x2 block.
    Ties (2-2 or all different) go to the first candidate: up-left, up-right, bottom-left, bottom-right.
    """
    pixels = np.asarray(img)
    if pixels.ndim == 2:
        pixels = pixels[:, :, None]

    # Pack each colour into one integer so whole colours are compared, not single channels
    keys = np.zeros(pixels.shape[:2], dtype=np.uint32)
    for c in range(pixels.shape[2]):
        keys |= pixels[:, :, c].astype(np.uint32) << (8 * c)

    candidates = np.stack([keys[0::2, 0::2], keys[0::2, 1::2], keys[1::2, 0::2], keys[1::2, 1::2]])
    candidatePixels = np.stack([pixels[0::2, 0::2], pixels[0::2, 1::2], pixels[1::2, 0::2], pixels[1::2, 1::2]])

    # Score of each candidate = number of pixels in the block with the same colour
    scores = (candidates[:, None] == candidates[None, :]).sum(axis=1)
    winner = scores.argmax(axis=0) # argmax returns the first index on ties

    rows, cols = np.indices(winner.shape)
    result = candidatePixels[winner, rows, cols]
    if np.asarray(img).ndim == 2:
        result = result[:, :, 0]

    downscaled = Image.fromarray(result, img.mode)
    if img.mode == "P":
        downscaled.putpalette(img.getpalette())
    return downscaled

# Output directory -> extra pixels on each side of every tile, copied from the neighbouring tiles
# (edge pixels repeated at the map edges). With 1 a tile is 258x258 and the viewer's majority filter
# never needs another tile
OUTPUTS = {
    "map": 0,             # 256x256, V1-V9
    "map_withborder": 1,  # 258x258, V10+
}

def create_quadtree_tiles(image_path):
    """
    Creates a quadtree tile structure for a slippy map, once per entry in OUTPUTS.
    Takes an image and generates tiles from the highest zoom level down to zoom level 0 (single tile).

    Args:
        image_path: Path to the source image (e.g., "map.png")
    """

    # Open the image
    try:
        img = Image.open(image_path)
        print(f"Loaded image: {image_path}")
        print(f"Image size: {img.size[0]} x {img.size[1]} pixels")
    except Exception as e:
        print(f"Error loading image: {e}")
        return
    
    width, height = img.size
    tile_size = 256
    
    # Calculate the maximum zoom level based on image dimensions
    # For a slippy map, image width should be 2^n * 256 and height should be 2^m * 256
    max_tiles_x = width // tile_size
    max_tiles_y = height // tile_size
    
    # Verify image is divisible by 256
    if width % tile_size != 0 or height % tile_size != 0:
        print(f"Warning: Image dimensions ({width}x{height}) are not divisible by 256")
        print("Cropping to nearest multiple of 256...")
        width = (width // tile_size) * tile_size
        height = (height // tile_size) * tile_size
        img = img.crop((0, 0, width, height))
    
    # Calculate zoom levels
    # Highest zoom level is where the image is divided into tile_size x tile_size tiles
    max_zoom = max(
        int(math.log2(max_tiles_x)) if max_tiles_x > 0 else 0,
        int(math.log2(max_tiles_y)) if max_tiles_y > 0 else 0
    )
    
    print(f"Tiles at max zoom: {max_tiles_x} x {max_tiles_y}")
    print(f"Generating tiles from zoom level {max_zoom} down to zoom level 0...")
    
    current_img = img.copy()
    
    # Generate tiles from highest zoom down to lowest
    for zoom in range(max_zoom, -1, -1):
        current_width, current_height = current_img.size
        tiles_x = current_width // tile_size
        tiles_y = current_height // tile_size
        
        print(f"\nZoom level {zoom}: {tiles_x}x{tiles_y} tiles ({current_width}x{current_height} pixels)")
        
        for output_dir, border in OUTPUTS.items():
            # Create zoom directory
            zoom_dir = os.path.join(output_dir, str(zoom-1))
            os.makedirs(zoom_dir, exist_ok=True)

            # Pad the whole level once, so tiles at the map edges also get a border
            pixels = np.asarray(current_img)
            padding = ((border, border), (border, border)) + ((0, 0),) * (pixels.ndim - 2)
            padded_img = Image.fromarray(np.pad(pixels, padding, mode="edge"), current_img.mode)
            if current_img.mode == "P":
                padded_img.putpalette(current_img.getpalette())

            # Extract and save individual tiles
            for y in range(tiles_y):
                x_dir = os.path.join(zoom_dir, str(y))
                os.makedirs(x_dir, exist_ok=True)

                for x in range(tiles_x):
                    # Extract tile plus its border (padded coordinates, the tile itself starts at left + border)
                    left = x * tile_size
                    top = y * tile_size
                    right = left + tile_size + 2 * border
                    bottom = top + tile_size + 2 * border

                    tile = padded_img.crop((left, top, right, bottom))

                    # Save tile as map/z/y/x.png (standard slippy map format)
                    tile_path = os.path.join(x_dir, f"{x}.png")
                    tile.save(tile_path, "PNG")
        
        # Stop if we've reached zoom 0 (when one dimension is 256)
        if tiles_x == 1 or tiles_y == 1:
            print("Reached zoom level 0. Done!")
            break
        
        # Downscale image for next zoom level
        current_img = majority_downscale(current_img)
    
    print(f"\nTiles saved to {', '.join(f'{d}/' for d in OUTPUTS)}")

if __name__ == "__main__":
    # Run the tile generator
    create_quadtree_tiles("map.png")
