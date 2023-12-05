# Load necessary libraries
library(magick)
library(MetBrewer)
library(colorspace)
library(ggplot2)
library(glue)
library(stringr)

# Read input image
img <- image_read("FinalMap.png")

# Define color palette
colors <- met.brewer("Greek")

# Display color swatch
swatchplot(colors)

# Adjust text color for better visibility
text_color <- darken(colors[2], .25)
swatchplot(text_color)

# Create annotation text with line wrapping
annotation <- glue("Exploring Population Density in Rabat-Salé-Kénitra region. ",
                   "Representation for 400m H3 Hexagons.") |> 
  str_wrap(60)

# Modify the image
img |> 
  image_crop(gravity = "center", geometry = "6000x3500+0-150") |> 
  
  image_annotate("Rabat-Salé-Kénitra Population Density",
                 gravity = "northwest", location = "+200+100",
                 color = text_color, size = 120, weight = 700,
                 font = "El Messiri") |> 
  image_annotate(annotation, gravity = "northwest", location = "+280+300",
                 color = text_color, size = 50, font = "El Messiri") |> 
  image_annotate(glue("Graphic by Mouad en-nasiry (@mouadenna) | ",
                      "Data: Kontur Population (Release 2023-11-01)"),
                 gravity = "south", location = "+0+100",
                 font = "El Messiri", color = alpha(text_color, .3),
                 size = 50) |> 
  image_write("final.png")

