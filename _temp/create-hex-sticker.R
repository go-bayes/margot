library(hexSticker)
library(here)
library(hexSticker)
library(here)
library(magick)
# Define the path to the image using the here package
imgurl <- "man/figures/margot-photoreal.png"
imgurl <- "man/figures/margot-stencil.png"
imgurl

# Create the sticker
sticker(imgurl,
        package = "margot",
        p_size = 20,
        s_x = 1, s_y = 0.8,
        s_width = .44, s_height = 1,
        h_fill="dodgerblue", h_color="hotpink",
        filename = "margot_hex_sticker.png")  # Empty subplot for compatibility

