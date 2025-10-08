import os
from PIL import Image

def convert_png_to_tga(directory):
    # Loop through all the files in the directory
    for filename in os.listdir(directory):
        # Check if the file is a .png file
        if filename.endswith('.png'):
            # Construct the full file path
            print("Found one.\n")
            file_path = os.path.join(directory, filename)
            # Open the image file
            with Image.open(file_path) as img:
                # Replace .png with .tga in the filename
                new_filename = filename.replace('.png', '.tga')
                # Save the image in .tga format
                img.save(os.path.join(directory, new_filename))
                print(f'Converted {filename} to {new_filename}')

if __name__ == "__main__":
    convert_png_to_tga('.')
