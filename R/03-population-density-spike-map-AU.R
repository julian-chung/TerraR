# Population Density Spike Map of Australia

# libraries we need
libs <- c(
  "tidyverse", "R.utils",
  "httr", "sf", "stars",
  "rayshader"
)

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}

# load libraries
invisible(lapply(libs, library, character.only = T))

# Ensure the output directory exists
if (!dir.exists("outputs/images")) {
  dir.create("outputs/images", recursive = TRUE)
}

### 1. LOAD DATA
### -------------
# Using local file instead of downloading
file_path <- "data/raw/kontur_population_AU_20231101.gpkg"

# Australia-appropriate projection
# Lambert Conformal Conic for Australia (GDA2020 / Australia Albers)
crsAU <- "+proj=aea +lat_0=0 +lon_0=132 +lat_1=-18 +lat_2=-36 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

get_population_data <- function() {
  pop_df <- sf::st_read(file_path) |>
    sf::st_transform(crs = crsAU)
  
  return(pop_df)
}

pop_sf <- get_population_data()

# Define file path
output_file <- "outputs/images/03-population-density-spike-map-AU_raw-population-points.png"

head(pop_sf)

# Save the plot
ggplot() +
  geom_sf(data = pop_sf, color = "grey10", fill = "grey10") +
  theme_void() +  # or theme_classic() if you want axes
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave(filename = output_file, width = 10, height = 8, dpi = 300, bg = "white")

### 3. SHP TO RASTER
### ----------------

bb <- sf::st_bbox(pop_sf)

get_raster_size <- function() {
  height <- sf::st_distance(
    sf::st_point(c(bb[["xmin"]], bb[["ymin"]])),
    sf::st_point(c(bb[["xmin"]], bb[["ymax"]]))
  )
  width <- sf::st_distance(
    sf::st_point(c(bb[["xmin"]], bb[["ymin"]])),
    sf::st_point(c(bb[["xmax"]], bb[["ymin"]]))
  )
  
  if (height > width) {
    height_ratio <- 1
    width_ratio <- width / height
  } else {
    width_ratio <- 1
    height_ratio <- height / width
  }
  
  return(list(width_ratio, height_ratio))
}
width_ratio <- get_raster_size()[[1]]
height_ratio <- get_raster_size()[[2]]

size <- 3000
width <- round((size * width_ratio), 0)
height <- round((size * height_ratio), 0)

get_population_raster <- function() {
  pop_rast <- stars::st_rasterize(
    pop_sf |>
      dplyr::select(population, geom),
    nx = width, ny = height
  )
  
  return(pop_rast)
}

pop_rast <- get_population_raster()

# Define output filename
output_file <- "outputs/images/03-population-density-spike-map-AU_raw-population-raster-preview.png"

# Open PNG device
png(filename = output_file, width = 2000, height = 1500, res = 300)

# Plot the raster
plot(pop_rast, main = "Population Raster", axes = FALSE, box = FALSE)

# Close the device
dev.off()

# Convert the stars object to a matrix for rayshader
pop_mat <- pop_rast |>
  as("Raster") |>
  rayshader::raster_to_matrix()

# Australian-themed colors
#cols <- c(
#  "#00843D", "#FFCD00", 
#  "#FF8000", "#FF4500"
#)

cols <- c(
  "#00441B",  # dark green
  "#238B45",  # medium green
  "#66C2A4",  # light green
  "#FFFFB2",  # pale yellow
  "#FE9929",  # orange
  "#EC7014",  # deeper orange
  "#CC4C02",  # brown-orange
  "#800026"   # red
)

texture <- grDevices::colorRampPalette(cols)(2048)

# True Density transformation
# This is a more complex transformation that takes into account the density of points
# in the raster. It can be useful for visualizing population density.

# Calculate the density of points in the raster
cell_area_km2 <- 0.16
density_raster <- pop_mat / cell_area_km2 # Now we have density in people per km2

# Check the distribution of values to determine a reasonable Z scale
summary(as.vector(density_raster))

# Create the initial 3D object using the density raster
density_raster |>
  rayshader::height_shade(texture = texture) |>
  rayshader::plot_3d(
    heightmap = density_raster,
    solid = F,
    soliddepth = 0,
    zscale = 100,  # We need to play with this value with the transformed data
    shadowdepth = 0,
    shadow_darkness = .95,
    windowsize = c(800, 800),
    phi = 65,
    zoom = .65,
    theta = -30,
    background = "white"
  )

# Use this to adjust the view after building the window object
rayshader::render_camera(phi = 55, zoom = .55, theta = 7)

# Define the output file path
output_file <- "outputs/images/03-population-density-spike-map-AU.png"

# Render the high-quality image
rayshader::render_highquality(
  filename = output_file,
  preview = TRUE,
  light = TRUE,
  lightdirection = 225,
  lightaltitude = 60,
  lightintensity = 400,
  interactive = FALSE,
  width = width,
  height = height
)

# Close the 3D device
rgl::close3d()

# Log transformation of population density
pop_mat_log <- log(pop_mat + 1) # Adding 1 to avoid log(0)

# Check the distribution of values to determine a reasonable Z scale
summary(as.vector(pop_mat_log))

# Create the initial 3D object using the transformed matrix
pop_mat_log |>
  rayshader::height_shade(texture = texture) |>
  rayshader::plot_3d(
    heightmap = pop_mat_log,
    solid = F,
    soliddepth = 0,
    zscale = 0.25,  # We need to play with this value with the transformed data
    shadowdepth = 0,
    shadow_darkness = .95,
    windowsize = c(800, 800),
    phi = 65,
    zoom = .65,
    theta = -30,
    background = "white"
  )

# Use this to adjust the view after building the window object
rayshader::render_camera(phi = 75, zoom = .7, theta = 0)

# Define the output file path
output_file <- "outputs/images/03-population-density-spike-map-AU-log.png"

# Render the high-quality image
rayshader::render_highquality(
  filename = output_file,
  preview = TRUE,
  light = TRUE,
  lightdirection = 225,
  lightaltitude = 60,
  lightintensity = 400,
  interactive = FALSE,
  width = width,
  height = height
)

# Square root transformation of population density
pop_mat_sqrt <- sqrt(pop_mat)

# Check the distribution of values to determine a reasonable Z scale
summary(as.vector(pop_mat_sqrt))

# Create the initial 3D object using the transformed matrix
pop_mat_sqrt |>
  rayshader::height_shade(texture = texture) |>
  rayshader::plot_3d(
    heightmap = pop_mat_sqrt,
    solid = F,
    soliddepth = 0,
    zscale = 0.25,  # We need to play with this value with the transformed data
    shadowdepth = 0,
    shadow_darkness = .95,
    windowsize = c(800, 800),
    phi = 65,
    zoom = .65,
    theta = -30,
    background = "white"
  )

# Use this to adjust the view after building the window object
rayshader::render_camera(phi = 75, zoom = .7, theta = 0)

# Define the output file path
output_file <- "outputs/images/03-population-density-spike-map-AU-sqrt.png"

# Render the high-quality image
rayshader::render_highquality(
  filename = output_file,
  preview = TRUE,
  light = TRUE,
  lightdirection = 225,
  lightaltitude = 60,
  lightintensity = 400,
  interactive = FALSE,
  width = width,
  height = height
)

# Close the 3D device
rgl::close3d()

# Power transformation of population density
pop_mat_power <- pop_mat^0.7

# Check the distribution of values to determine a reasonable Z scale
summary(as.vector(pop_mat_power))

# Create the initial 3D object using the transformed matrix
pop_mat_power |>
  rayshader::height_shade(texture = texture) |>
  rayshader::plot_3d(
    heightmap = pop_mat_power,
    solid = F,
    soliddepth = 0,
    zscale = 0.25,  # We need to play with this value with the transformed data
    shadowdepth = 0,
    shadow_darkness = .95,
    windowsize = c(800, 800),
    phi = 65,
    zoom = .65,
    theta = -30,
    background = "white"
  )

# Use this to adjust the view after building the window object
rayshader::render_camera(phi = 75, zoom = .7, theta = 0)

# Define the output file path
output_file <- "outputs/images/03-population-density-spike-map-AU-power.png"

# Render the high-quality image
rayshader::render_highquality(
  filename = output_file,
  preview = TRUE,
  light = TRUE,
  lightdirection = 225,
  lightaltitude = 60,
  lightintensity = 400,
  interactive = FALSE,
  width = width,
  height = height
)

# Close the 3D device
rgl::close3d()
