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
output_file <- "outputs/images/01-population-spike-map-AU_raw-population-points.png"

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
output_file <- "outputs/images/01-population-spike-map-AU_raw-population-raster-preview.png"

# Open PNG device
png(filename = output_file, width = 2000, height = 1500, res = 300)

# Plot the raster
plot(pop_rast, main = "Population Raster", axes = FALSE, box = FALSE)

# Close the device
dev.off()


pop_mat <- pop_rast |>
    as("Raster") |>
    rayshader::raster_to_matrix()

# Summarize the population data
summary(as.vector(pop_mat))

# Australian-themed colors
cols <- rev(c(
    "#00843D", "#FFCD00", 
    "#FF8000", "#FF4500"
))

texture <- grDevices::colorRampPalette(cols)(256)

# Create the initial 3D object
pop_mat |>
    rayshader::height_shade(texture = texture) |>
    rayshader::plot_3d(
        heightmap = pop_mat,
        solid = F,
        soliddepth = 0,
        zscale = 15,
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
output_file <- "outputs/images/01-population-spike-map-AU.png"

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

