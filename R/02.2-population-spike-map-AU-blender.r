#######################################################
#                 Making crisp spike maps with R
#                 Adapted for Australia
#                 2025/05/16
########################################################
# install rayshader & rayrender from the source
# devtools::install_github("tylermorganwall/rayshader")
# devtools::install_github("tylermorganwall/rayrender")

# libraries we need
libs <- c(
    "tidyverse", "R.utils",
    "httr", "sf", "stars",
    "rayshader", "rgl"  # Added rgl for export functionality
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
# Ensure the output directory for 3D models exists
if (!dir.exists("outputs/models")) {
  dir.create("outputs/models", recursive = TRUE)
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
output_file <- "outputs/images/02-population-spike-map-AU_raw-population-points.png"

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

size <- 6000
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
output_file <- "outputs/images/02-population-spike-map-AU_raw-population-raster-preview.png"

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

# Check values in the matrix
summary(as.vector(pop_mat))

# Australian-themed colors
cols <- rev(c(
    "#00843D", "#FFCD00", 
    "#FF8000", "#FF4500"
))

# Create a texture using the defined colors
texture <- grDevices::colorRampPalette(cols)(1024)

# Check the texture
plot(1:1024, col = texture, pch = 15, cex = 2, main = "Texture Colors")

# Check the dimensions of the matrix
dim(pop_mat)

#######################################################
# MODIFICATION 1: Add base elevation and solid depth
# to make the continent "pop out" from the background
#######################################################

# Define base elevation for the continent
base_elevation <- 20.0  # Adjust this value to control how much the continent is elevated

# Create a mask to identify valid data points (non-NA values)
is_valid_data <- !is.na(pop_mat)

# Create an adjusted matrix with base elevation
pop_mat_elevated <- pop_mat
pop_mat_elevated[is_valid_data] <- pop_mat_elevated[is_valid_data] + base_elevation

# Create the initial 3D object with solid depth
pop_mat_elevated |>
    rayshader::height_shade(texture = texture) |>
    rayshader::plot_3d(
        heightmap = pop_mat_elevated,
        solid = TRUE,  # Changed to TRUE to enable solid base
        soliddepth = -150,  # Added negative soliddepth to create thickness below the surface
        zscale = 10,  # Adjust this value as needed
        shadowdepth = 0,
        shadow_darkness = .95,
        windowsize = c(800, 800),
        phi = 65,
        zoom = .65,
        theta = -30,
        background = "white"
    )

# Use this to adjust the view after building the window object
rayshader::render_camera(phi = 60, zoom = .6, theta = -10)

# Define the output file path
output_file <- "outputs/images/02-population-spike-map-AU-solid.png"

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

#######################################################
# MODIFICATION 2: Export 3D model for Blender
# We'll export to OBJ format which Blender can import
#######################################################

# Define the output file path for the 3D model
model_output_file <- "outputs/models/australia_population_3d"

# Export the current 3D scene to OBJ format
# The rgl.snapshot function may be needed first to ensure the scene is ready
rgl::rgl.snapshot(filename = paste0(model_output_file, "_preview.png"))

# Export to OBJ with material file
rgl::writeOBJ(
  paste0(model_output_file, ".obj"),
  pointsAsSpherex = FALSE
)

# Add informative message to console
message("3D model exported to: ", paste0(model_output_file, ".obj"))
message("You can now import this file into Blender.")

# Additional export to PLY format (which may work better for some Blender workflows)
# PLY often preserves color information better than OBJ
rgl::writePLY(
  paste0(model_output_file, ".ply"),
  pointsAsSpherex = FALSE
)
message("Additional PLY format exported to: ", paste0(model_output_file, ".ply"))

# If you want to export specifically for Blender, rayshader has a specialized function:
rayshader::save_3dprint(
  filename = paste0(model_output_file, "_rayshader.stl"),
  rotate = TRUE  # Set to TRUE to rotate the model to a flat orientation for 3D printing
)
message("STL format (optimized for 3D workflow) exported to: ", paste0(model_output_file, "_rayshader.stl"))

#######################################################
# Continue with the transformations as in original code
# but with solid depth added to all visualizations
########################################################

# 1. Log transformation with solid depth
pop_mat_log <- log(pop_mat + 1) # Adding 1 to avoid log(0)

# Apply the same base elevation to log-transformed data
pop_mat_log_elevated <- pop_mat_log
pop_mat_log_elevated[is_valid_data] <- pop_mat_log_elevated[is_valid_data] + base_elevation

# Create the 3D object using the transformed matrix and solid depth
pop_mat_log_elevated |>
    rayshader::height_shade(texture = texture) |>
    rayshader::plot_3d(
        heightmap = pop_mat_log_elevated,
        solid = TRUE,
        soliddepth = -150,
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
output_file <- "outputs/images/02-population-spike-map-AU-log-solid.png"

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

# Export log-transformed model
model_output_file_log <- "outputs/models/australia_population_3d_log"
rgl::writeOBJ(
  paste0(model_output_file_log, ".obj"),
  pointsAsSpherex = FALSE
)

# 2. Square root transformation
pop_mat_sqrt <- sqrt(pop_mat)

# Apply the same base elevation to sqrt-transformed data
pop_mat_sqrt_elevated <- pop_mat_sqrt
pop_mat_sqrt_elevated[is_valid_data] <- pop_mat_sqrt_elevated[is_valid_data] + base_elevation

# Create the 3D object using the transformed matrix and solid depth
pop_mat_sqrt_elevated |>
    rayshader::height_shade(texture = texture) |>
    rayshader::plot_3d(
        heightmap = pop_mat_sqrt_elevated,
        solid = TRUE,
        soliddepth = -150,
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
output_file <- "outputs/images/02-population-spike-map-AU-sqrt-solid.png"

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

# Export sqrt-transformed model
model_output_file_sqrt <- "outputs/models/australia_population_3d_sqrt"
rgl::writeOBJ(
  paste0(model_output_file_sqrt, ".obj"),
  pointsAsSpherex = FALSE
)

# 3. Power transformation
pop_mat_power <- pop_mat^0.7 # Adjust the exponent as needed 0.7 seems to work well

# Apply the same base elevation to power-transformed data
pop_mat_power_elevated <- pop_mat_power
pop_mat_power_elevated[is_valid_data] <- pop_mat_power_elevated[is_valid_data] + base_elevation

# Create the 3D object using the transformed matrix and solid depth
pop_mat_power_elevated |>
    rayshader::height_shade(texture = texture) |>
    rayshader::plot_3d(
        heightmap = pop_mat_power_elevated,
        solid = TRUE,
        soliddepth = -150,
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
output_file <- "outputs/images/02-population-spike-map-AU-power-solid.png"

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

# Export power-transformed model
model_output_file_power <- "outputs/models/australia_population_3d_power"
rgl::writeOBJ(
  paste0(model_output_file_power, ".obj"),
  pointsAsSpherex = FALSE
)

# Close rgl device
rgl::close3d()

# Print instructions for importing into Blender
cat("\n")
cat("============================================================================\n")
cat("INSTRUCTIONS FOR IMPORTING INTO BLENDER:\n")
cat("============================================================================\n")
cat("1. Open Blender\n")
cat("2. File > Import > Wavefront (.obj) or File > Import > Stanford (.ply)\n")
cat("3. Navigate to one of these files:\n")
cat("   - ", paste0(model_output_file, ".obj"), "\n")
cat("   - ", paste0(model_output_file, ".ply"), "\n")
cat("   - ", paste0(model_output_file_log, ".obj"), " (log transformation)\n")
cat("   - ", paste0(model_output_file_sqrt, ".obj"), " (sqrt transformation)\n")
cat("   - ", paste0(model_output_file_power, ".obj"), " (power transformation)\n")
cat("4. The STL file may work better if you encounter issues with OBJ or PLY:\n")
cat("   - ", paste0(model_output_file, "_rayshader.stl"), "\n")
cat("5. You may need to adjust scale and orientation after import\n")
cat("============================================================================\n")
