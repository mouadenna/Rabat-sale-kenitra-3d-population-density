library(sf)
library(tidyverse)
library(dplyr)
library(geomaroc)
library(stars)
library(rayshader)
library(MetBrewer)
library(colorspace)

# Load spatial data
data <- st_read('C:\\Users\\MOUAD\\Documents\\data\\kontur_population_MA_20231101.gpkg')

RSK <- getMultiRegion("Rabat-Sale-Kenitra")

# Set CRS for Rabat-sale-kenitra
RSK <- st_set_crs(RSK, 4326)

RSK <- st_transform(RSK, crs = st_crs(data))

# Plot RSK for visualization (optional)
RSK %>%
  ggplot() +
  geom_sf()

# Perform intersection
st_RSK <- st_sf(geometry = st_intersection(data$geom, RSK$coordinates))
st_RSK <- st_join(st_RSK, data %>% select(population), by = c("geometry" = "geom"))

# Calculate bounding box and create points for corners
bb <- st_bbox(st_RSK)
bottom_left <- st_point(c(bb[["xmin"]], bb[["ymin"]])) %>%
  st_sfc(crs = st_crs(data))
bottom_right <- st_point(c(bb[["xmax"]], bb[["ymax"]])) %>%
  st_sfc(crs = st_crs(data))
width <- st_distance(bottom_left, bottom_right)
top_left <- st_point(c(bb[["xmin"]], bb[["ymax"]])) %>%
  st_sfc(crs = st_crs(data))
top_right <- st_point(c(bb[["xmax"]], bb["ymin"])) %>%
  st_sfc(crs = st_crs(data))
height <- st_distance(top_left, bottom_left)

# Visualize bounding box
# RSK %>%
#   ggplot() +
#   geom_sf() +
#   geom_sf(data = bottom_left) +
#   geom_sf(data = bottom_right, color = 'red') +
#   geom_sf(data = top_left, color = "green") +
#   geom_sf(data = top_right, color = 'blue')

# Calculate width and height ratios
if (width > height) {
  w_ratio <- 1
  h_ratio <- height / width
} else {
  h_ratio <- 1
  w_ratio <- width / height
}
size <- 3000

# Rasterize  data
RSK_rasterize <- st_rasterize(st_RSK,
                              nx = floor(size * w_ratio),
                              ny = floor(size * h_ratio))

mat <- matrix(RSK_rasterize$population,
              nrow = floor(size * w_ratio),
              ncol = floor(size * h_ratio))

#Color palette
c1 <- met.brewer("Greek")
c1 <- c(c1, "#ffeac1")

swatchplot(rev(c1))

texture <- grDevices::colorRampPalette(rev(c1), bias = 1.2)(256)
swatchplot(texture)

# Visualize the 3D plot using rayshader
mat %>%
  height_shade(texture = texture) %>%
  plot_3d(heightmap = mat,
          zscale = 50,
          solid = FALSE,
          shadowdepth = 0)

# Set rendering camera parameters

render_camera(theta = -100, phi = 60, zoom = 0.8)
# render_camera(theta = 0, phi = 60, zoom = 0.8)


# Render the 3D plot to a high-quality image

outfile <- "RSKFinal.png"

result <- render_highquality(
  filename = outfile,
  interactive = FALSE,
  lightdirection = 0,
  lightaltitude = c(60, 70),
  lightcolor = c("#ffeaa9", "white"),
  lightintensity = c(300, 200),
  #samples = 450,
  width = 3000,
  height = 3000
)
if (startsWith(result, "Error")) {
  print(result)
} else {
  print("Rendering successful!")
}
