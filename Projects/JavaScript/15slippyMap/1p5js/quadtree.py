import os
from PIL import Image
import math

def create_quadtree_tiles(image_path, output_dir="map"):
    """
    Creates a quadtree tile structure for a slippy map.
    Takes an image and generates tiles from the highest zoom level down to zoom level 0 (single tile).
    
    Args:
        image_path: Path to the source image (e.g., "map.png")
        output_dir: Directory to save tiles (default: "map")
    """
    
    # Open the image
    try:
        img = Image.open(image_path)
        print(f"Loaded image: {image_path}")
        print(f"Image size: {img.size[0]} x {img.size[1]} pixels")
    except Exception as e:
        print(f"Error loading image: {e}")
        return
    
    # Create output directory if it doesn't exist
    os.makedirs(output_dir, exist_ok=True)
    
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
        
        # Create zoom directory
        zoom_dir = os.path.join(output_dir, str(zoom-1))
        os.makedirs(zoom_dir, exist_ok=True)
        
        # Extract and save individual tiles
        for y in range(tiles_y):
            x_dir = os.path.join(zoom_dir, str(y))
            os.makedirs(x_dir, exist_ok=True)
            
            for x in range(tiles_x):
                # Extract tile
                left = x * tile_size
                top = y * tile_size
                right = left + tile_size
                bottom = top + tile_size
                
                tile = current_img.crop((left, top, right, bottom))
                
                # Save tile as map/z/y/x.png (standard slippy map format)
                tile_path = os.path.join(x_dir, f"{x}.png")
                tile.save(tile_path, "PNG")
        
        # Stop if we've reached zoom 0 (when one dimension is 256)
        if tiles_x == 1 or tiles_y == 1:
            print("Reached zoom level 0. Done!")
            break
        
        # Downscale image for next zoom level
        next_width = max(tile_size, current_width // 2)
        next_height = max(tile_size, current_height // 2)
        current_img = current_img.resize((next_width, next_height), Image.Resampling.NEAREST)
    
    print(f"\nTiles saved to '{output_dir}/' directory")

if __name__ == "__main__":
    # Run the tile generator
    create_quadtree_tiles("map.png", output_dir="map")
