#!/usr/bin/env -S python -u

import os
import random
import numpy as np
from PIL import Image, ImageOps

def create_image_grid(images, grid_shape=(4, 4), output_path='output_grid.png'):
    # Check if we have the correct number of images
    if len(images) != grid_shape[0] * grid_shape[1]:
        raise ValueError(f"Expected {grid_shape[0] * grid_shape[1]} images, but got {len(images)}")

    # Resize images to the size of the first image (assuming all images are the same size)
    #width, height = images[0].size
    #images = [img.resize((width, height)) for img in images]

    with Image.open(images[0]) as img:
        width, height = img.size
    print('image size:', str(width) + 'x' + str(height))

    for file in images:
        with Image.open(file) as img:
            resized_img = img.resize((width, height))
            resized_img.save(file)

    # Create a new blank image for the grid
    #grid_image = Image.new('RGB', (width * grid_shape[1], height * grid_shape[0]))
    full_width  = (width  + pad) * grid_shape[1] + pad
    full_height = (height + pad) * grid_shape[0] + pad
    print('full card size:', str(full_width) + 'x' + str(full_height))
    grid_image = Image.new('RGB', (full_width , full_height), color='white')

    # Paste images into the grid
    for i in range(grid_shape[0]):
        for j in range(grid_shape[1]):
            index = i * grid_shape[1] + j
            img = Image.open(images[index])

            # resize to make room for border
            resized_img = img.resize((width - 2*border_size, height - 2*border_size))
            img = ImageOps.expand(resized_img, border=border_size, fill='black')

            grid_image.paste(img, (j * (width + pad) + pad, i * (height + pad) + pad))

    # Save the grid image
    grid_image.save(output_path)
    print(f'Grid image saved to {output_path}')


# Defaults
pad = 100 # pixels
border_size = 10 # pixels
num_cards = 3
input_path='input/' # place all base images in here
os.makedirs('output/', exist_ok=True)

# maybe you want to make alternate grid shapes
grid_shape=(4, 4)
n_images = grid_shape[0] * grid_shape[1]

if not os.path.exists('input/'):
    raise SystemExit('ERROR: base card images must exist in input/ directory.')

image_paths = []
for filename in os.listdir(input_path):
    image_paths.append(input_path + filename)

for icard in range(num_cards):
    random.shuffle(image_paths)
    create_image_grid(image_paths[0:n_images], grid_shape=grid_shape, output_path=f'output/card-{icard}.png')
