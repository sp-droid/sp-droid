from PIL import Image
import numpy as np

# Load PNG image
image_path = 'provinces.png'  # Replace with the actual path to your image
img = Image.open(image_path).convert('RGB')

# Convert image to NumPy array
img_array = np.array(img)
print(img_array.shape)
# Save the array as a text file
output_csv_path = 'provinces.csv'  # Replace with the desired output path

with open('arr3D.js', 'w') as file:
    file.write('function arr3D() {\n\tconst arr = [\n')
    for y in range(img_array.shape[0]):
        file.write('\t\t[\n')
        for x in range(img_array.shape[1]):
            file.write(f'\t\t\t[{img_array[y,x,0]},{img_array[y,x,1]},{img_array[y,x,2]}],\n')
        file.write('\t\t],\n')
    file.write('\t]\n\treturn arr\n}')