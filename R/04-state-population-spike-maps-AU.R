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

# Load and transform Australian state boundaries
aus_states <- sf::st_read("data/boundaries/STE_2021_AUST_GDA2020.shp") |>
  sf::st_transform(crs = crsAU)

# Filter for the states/territories we want
filtered_states <- aus_states |>
  dplyr::filter(STE_NAME21 %in% c(
    "New South Wales", "Victoria", "Queensland", "South Australia",
    "Western Australia", "Tasmania", "Northern Territory", "Australian Capital Territory"
  ))

# Create a list of state geometries by name (using the filtered dataframe)
state_boundaries <- split(filtered_states, filtered_states$STE_NAME21)

# Create a named list of clipped population data per state
state_pops <- lapply(state_boundaries, function(state_geom) {
  # Get the geometry column name for state_geom
  state_geom_col <- attr(state_geom, "sf_column")
  
  # Select only necessary columns from pop_sf
  pop_subset <- pop_sf %>% dplyr::select(population, geom)
  
  # Select only necessary columns from state_geom
  state_subset <- state_geom %>% dplyr::select(STE_NAME21, all_of(state_geom_col))
  
  # Perform intersection with reduced attributes
  sf::st_intersection(pop_subset, state_subset)
})

### The above code frequently hangs, try another approach

# Create a named list of clipped population data per state
#state_pops <- lapply(state_boundaries, function(state_geom) {
  # Simply use the sf objects directly
#  sf::st_intersection(
#    pop_sf %>% dplyr::select(population),  # Keep only population column from pop_sf
#    state_geom %>% dplyr::select(STE_NAME21) # Keep only STE_NAME21 from state_geom
#  )
#})

# Now let's focus on NSW
nsw_pop_sf <- state_pops[["New South Wales"]]

# Prepare the bounding box for NSW
bb <- sf::st_bbox(nsw_pop_sf)

get_raster_size <- function(bbox) {
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
  
  return(list(width_ratio, height_ratio))
}

# Calculate dimensions for NSW
size <- 3000
width_ratio <- get_raster_size(bb)[[1]]
height_ratio <- get_raster_size(bb)[[2]]
width <- round((size * width_ratio), 0)
height <- round((size * height_ratio), 0)

# Generate raster for NSW
nsw_rast <- stars::st_rasterize(
  nsw_pop_sf |>
    dplyr::select(population, geom),
  nx = width, ny = height
)

# Convert to matrix
nsw_mat <- nsw_rast |>
  as("Raster") |>
  rayshader::raster_to_matrix()

# Apply log transformation to enhance visibility of smaller population centers
nsw_mat_log <- log10(nsw_mat + 1)  # +1 to avoid log(0)

# Australian-themed colors
cols <- rev(c(
  "#00843D", "#FFCD00", 
  "#FF8000", "#FF4500"
))

texture <- grDevices::colorRampPalette(cols)(256)

# Create the 3D object for NSW
nsw_mat_log |>
  rayshader::height_shade(texture = texture) |>
  rayshader::plot_3d(
    heightmap = nsw_mat_log,
    solid = FALSE,
    soliddepth = 0,
    zscale = 10,  # Adjusted for log transformation
    shadowdepth = 0,
    shadow_darkness = .95,
    windowsize = c(800, 800),
    phi = 65,
    zoom = .65,
    theta = -30,
    background = "white"
  )

# Adjust view
rayshader::render_camera(phi = 75, zoom = .7, theta = 0)

# Define the output file path
output_file <- "outputs/images/04-state-population-spike-map-NSW.png"

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

