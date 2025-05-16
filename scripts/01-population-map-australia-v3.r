#######################################################
#                 Australian Population Spikemap
#                 Simplified Approach
########################################################

# libraries we need
libs <- c(
    "tidyverse", "sf", "stars",
    "rayshader", "rnaturalearth", "rnaturalearthdata"
)

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
    install.packages(libs[!installed_libs])
}

# load libraries
invisible(lapply(libs, library, character.only = T))

# Helper function for aspect ratio - MOVED HERE BEFORE IT'S USED
st_bbox_aspect_ratio <- function(bbox) {
  height <- sf::st_distance(
    sf::st_point(c(bbox[["xmin"]], bbox[["ymin"]])),
    sf::st_point(c(bbox[["xmin"]], bbox[["ymax"]]))
  )
  width <- sf::st_distance(
    sf::st_point(c(bbox[["xmin"]], bbox[["ymin"]])),
    sf::st_point(c(bbox[["xmax"]], bbox[["ymin"]]))
  )

  if (height > width) {
    height_ratio <- 1
    width_ratio <- width / height
  } else {
    width_ratio <- 1
    height_ratio <- height / width
  }

  return(list(as.numeric(width_ratio), as.numeric(height_ratio)))
}

### 1. LOAD DATA
### -------------
file_name <- "data/raw/kontur_population_AU_20231101.gpkg"
crsALBERS_AU <- "+proj=aea +lat_0=0 +lon_0=132 +lat_1=-18 +lat_2=-36 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

# Load population data
pop_sf <- sf::st_read(file_name) |>
    sf::st_transform(crs = crsALBERS_AU)

# Get Australia outline
australia <- rnaturalearth::ne_countries(country = "Australia", returnclass = "sf") |>
  st_transform(crs = crsALBERS_AU)

### 2. PREPARE RASTER DIMENSIONS
### ----------------------------
bb <- sf::st_bbox(pop_sf)
width_height <- st_bbox_aspect_ratio(bb)
width_ratio <- width_height[[1]]
height_ratio <- width_height[[2]]

size <- 4000  # Balanced resolution
width <- round((size * width_ratio), 0)
height <- round((size * height_ratio), 0)

### 3. CREATE BASE LAYER (AUSTRALIA OUTLINE)
### ---------------------------------------
# Create a raster of Australia's landmass with a base height
aus_rast <- stars::st_rasterize(
  australia |> dplyr::select(geometry),
  nx = width, ny = height
)
aus_mat <- aus_rast |> as("Raster") |> rayshader::raster_to_matrix()

# Convert to a matrix where land = 0.1 (base height) and ocean = NA
aus_mat[!is.na(aus_mat)] <- 0.1

### 4. CREATE POPULATION LAYER
### -------------------------
# Rasterize population data
pop_rast <- stars::st_rasterize(
  pop_sf |> dplyr::select(population, geom),
  nx = width, ny = height
)
pop_mat_raw <- pop_rast |> as("Raster") |> rayshader::raster_to_matrix()

# Create combined matrix - base layer + population spikes
combined_mat <- aus_mat  # Start with the base Australia layer
pop_mat_raw[is.na(pop_mat_raw)] <- 0  # Replace NA with 0 in population matrix

# Find cells with population > 0 and apply log transformation for better visibility
pop_cells <- which(pop_mat_raw > 0, arr.ind = TRUE)
if(length(pop_cells) > 0) {
  for(i in 1:nrow(pop_cells)) {
    row <- pop_cells[i, 1]
    col <- pop_cells[i, 2]
    # Apply log transformation to make small values more visible
    # while keeping large values from dominating
    pop_value <- log1p(pop_mat_raw[row, col])
    combined_mat[row, col] <- 0.1 + pop_value * 0.03  # Increased from 0.01 to 0.03
  }
}

### 5. COLOR AND RENDER
### ------------------
# Simple color scheme - tan base with green-to-yellow gradient for population
aus_colors <- c(
  "#e8d9b5", # Tan base for the land
  "#c9b17d", # Darker tan
  "#7f802b", # Olive green for low population
  "#4f8e57", # Green for medium population 
  "#ffcc00"  # Yellow for highest population density
)

# Apply coloring
texture <- grDevices::colorRampPalette(aus_colors)(256)
combined_mat_colored <- rayshader::height_shade(combined_mat, texture = texture)

# Render 3D plot
combined_mat_colored |>
  rayshader::plot_3d(
    heightmap = combined_mat,
    solid = TRUE,
    soliddepth = 0,  # Base layer at ocean level
    zscale = 300,  # Increased from 200 to 300 for more dramatic height
    shadowdepth = 0,
    windowsize = c(1000, 800),
    phi = 50,        # Lower angle for more dramatic view
    zoom = 0.5,
    theta = -20,
    background = "#87CEEB"  # Sky blue for the ocean/background
  )

# Adjust camera for best view
rayshader::render_camera(phi = 45, zoom = 0.4, theta = -15)

# Create output directory if it doesn't exist
if (!dir.exists("images")) {
  dir.create("images", recursive = TRUE)
}

# Render high-quality version
rayshader::render_highquality(
  filename = "images/australia_population_simple.png",
  preview = TRUE,
  light = TRUE,
  lightdirection = 225, 
  lightaltitude = 40,
  lightintensity = 700,
  interactive = FALSE,
  width = width, height = height
)
