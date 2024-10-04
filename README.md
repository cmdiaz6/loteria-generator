# loteria-generator
generate loteria cards from custom user-supplied images

pip install Pillow
pip install numpy

Place your base images in a directory named 'input/'. Naming doesn't matter, just make sure they're images. Normal Loteria uses 54 images, so probably aim for somewhere around that
Then open up loteria-generator.py and customize those inputs manually. There are no input arguments. I'm not gonna hold your hand!
modify num_cards to change how many cards get generated. it's set at 3 now for testing. also feel free to change the gaps between each card and the border size. That's in there somewhere too

Outputs will appear as if by miraculous sorcery in an 'output/' directory
