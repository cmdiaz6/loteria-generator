from PIL import Image
from fpdf import FPDF

# Define the page size in inches
page_width, page_height = 18, 24  # in inches
dpi = 300  # PDF resolution in dots per inch
page_width_px = int(page_width * dpi)  # Convert width to pixels
page_height_px = int(page_height * dpi)  # Convert height to pixels

# Open the PNG file
image = Image.open("card-0.png")

# Create a blank white canvas with the page size
canvas = Image.new("RGB", (page_width_px, page_height_px), "white")

# Calculate the scaling factor to fit the image inside the page
image_aspect = image.width / image.height
page_aspect = page_width_px / page_height_px

if image_aspect > page_aspect:
    # Image is wider relative to the page
    new_width = page_width_px
    new_height = int(new_width / image_aspect)
else:
    # Image is taller relative to the page
    new_height = page_height_px
    new_width = int(new_height * image_aspect)

# Resize the image while maintaining aspect ratio
resized_image = image.resize((new_width, new_height), Image.LANCZOS)

# Calculate the position to center the image on the canvas
x_offset = (page_width_px - new_width) // 2
y_offset = (page_height_px - new_height) // 2

# Paste the resized image onto the white canvas
canvas.paste(resized_image, (x_offset, y_offset))

# Save the canvas as a temporary image
canvas.save("temp_image.png", dpi=(dpi, dpi))

# Create a PDF with the specified page size
pdf = FPDF(unit="in", format=(page_width, page_height))
pdf.add_page()
pdf.image("temp_image.png", x=0, y=0, w=page_width, h=page_height)

# Output the PDF
pdf.output("output.pdf")
