# Load necessary libraries
library(spatstat)
library(diagram)
library(magick)

# Create PDF device with specified dimensions and settings
png("./img/sampling.png",
    family = "Helvetica",
    width = 4.5 * 300,      # 4.5 inches * 300 dpi = 1350 pixels
    height = 4.5 * 300,     # 4.5 inches * 300 dpi = 1350 pixels
    res = 300,              # Resolution
    bg = "transparent")     # Background color; adjust as needed

# Set graphical parameters:
# - Remove all margins
# - Set axis styles to internal to avoid extra padding
par(mar = c(0, 0, 0, 0),    # Margins: bottom, left, top, right
    xaxs = "i",              # Internal x-axis style
    yaxs = "i",              # Internal y-axis style
    asp = 1)                 # Aspect ratio 1:1

# Initialize a new plot
plot.new()

# Define plot window limits tightly around the data and labels
# Adjust ylim to include all y-coordinates of text labels and arrows
plot.window(xlim = c(-8.6, 5.6),
            ylim = c(-5, 5))  # Adjusted to fit all labels

# Draw the first ellipse
W1 <- ellipse(a = 2.5, b = 2, centre = c(-6, 0), phi = 0, npoly = 1024)
plot(W1, add = TRUE)

# Draw the second ellipse
W2 <- ellipse(a = 2.5, b = 2, centre = c(3, 0), phi = 0, npoly = 1024)
plot(W2, add = TRUE)

# Add text labels
# It's more efficient to store label coordinates and texts in a data frame
labels <- data.frame(
  x = c(-6.5, -7, -5, -4, -6, -6.5, -8.2, -5, -6.2, -7, -8, -5.8,
        -7, -7.5, -5, -6.8, -5.5, -4.7, -6.2, -7.5, -4.3, -4.5,
        -4.3, -5.3, -6.3, -6.1, 2, 3, 4, 2, 3.7),
  y = c(-1, 1.1, 1.2, 0, -1.5, 0.5, 0.5, 0.6, 1.2, 1.5, -0.5,
        0.8, -1.1, 1, -1, -0.3, -0.2, -0.8, 1.8, 0.2, 0.5,
        -0.2, -0.7, 1.5, -0.1, -0.6, 1.1, -1.5, 0.6, -1.1, 1.5),
  label = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j",
            "k", "l", "m", "n", "o", "p", "q", "r", "s", "t",
            "u", "v", "w", "x", "y", "z", "b", "e", "h", "m", "x"),
  stringsAsFactors = FALSE
)

# Add all text labels
with(labels, text(x, y, label, cex = 0.6))

# Add additional text labels within the plot limits
text(-6, 3, "Population", cex = 0.75)  # y=3 is outside initial ylim; adjusted to y=3.2
text(3, 3, "Sample", cex = 0.75)        # y=3 is within ylim
text(-1.6, 1.3, "Sampling", cex = 0.75)
text(-1.4, -1.3, "Inference", cex = 0.75)

# Add arrows
arrows(-3.5, 1, 0.5, 1, angle = 20, length = 0.1)
arrows(0.5, -1, -3.5, -1, angle = 20, length = 0.1)

# Close the PDF device
dev.off()



# Load the original image
img <- image_read("./img/sampling.png")

# Get image dimensions
info <- image_info(img)
width <- info$width
height <- info$height

# Calculate the dimensions for the central 60%
new_width <- width * 1
new_height <- height * 0.65

# Calculate the coordinates to center the crop
x_offset <- (width - new_width) / 2
y_offset <- (height - new_height) / 2

# Perform the crop
cropped_img <- image_crop(img, geometry = geometry_area(width = new_width, height = new_height, x = x_offset, y = y_offset))

# Save the cropped image
image_write(cropped_img, path = "./img/sampling-cropped.png", format = "png")

