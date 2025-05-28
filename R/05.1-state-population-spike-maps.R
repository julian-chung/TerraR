#######################################################
#                 Making crisp spike maps with R
#                 Adapted for Australia
#                 2025/05/16
########################################################
# install rayshader & rayrender from the source
devtools::install_github("tylermorganwall/rayshader")
devtools::install_github("tylermorganwall/rayrender")

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
###### WARNING: This operation literally takes several hours to run
###### It is recommended to save the results after the first run, then comment out this function, and load the saved results for future tasks as required
###### There is likely a more efficient way to do this, but I haven't found it yet

state_pops <- lapply(state_boundaries, function(state_geom) {
  # Get the geometry column name for state_geom
  state_geom_col <- attr(state_geom, "sf_column")
  
  # Select only necessary columns from pop_sf
  pop_subset <- pop_sf %>% dplyr::select(population, geom)
  
  # Select only necessary columns from state_geom
  state_subset <- state_geom %>% dplyr::select(STE_NAME21, all_of(state_geom_col))
  
  #   # Perform intersection with reduced attributes
  sf::st_intersection(pop_subset, state_subset)
})

# IMPORTANT: Save the resulting list for future use (recommended)
saveRDS(state_pops, file = "data/processed/state_pops.rds")

# Later, we can reload with:
state_pops <- readRDS("data/processed/state_pops.rds")

#######################################################
#                 WORKING AREA FOR STATE SETTINGS
# Use this section to experiment with settings for
# individual states before using the modular approach
#######################################################

# Extract individual state population datasets
nsw_pop_sf <- state_pops[["New South Wales"]]
vic_pop_sf <- state_pops[["Victoria"]]
qld_pop_sf <- state_pops[["Queensland"]]
sa_pop_sf <- state_pops[["South Australia"]]
wa_pop_sf <- state_pops[["Western Australia"]]
tas_pop_sf <- state_pops[["Tasmania"]]
nt_pop_sf <- state_pops[["Northern Territory"]]
act_pop_sf <- state_pops[["Australian Capital Territory"]]

# Define custom aesthetic-inspired palette
cols <- c(
  "#f8f4e3",  # bone white
  "#fceea7",  # soft yellow
  "#f5c94c",  # mustard
  "#e67e22",  # burnt orange
  "#c0392b"   # rich red
)

# Create a smooth gradient texture
texture <- grDevices::colorRampPalette(cols)(1024)

# Process New South Wales
# ------------------------
# Get NSW bounding box
bb_nsw <- sf::st_bbox(nsw_pop_sf)

# Calculate dimensions
height_nsw <- sf::st_distance(
  sf::st_point(c(bb_nsw[["xmin"]], bb_nsw[["ymin"]])),
  sf::st_point(c(bb_nsw[["xmin"]], bb_nsw[["ymax"]]))
)
width_nsw <- sf::st_distance(
  sf::st_point(c(bb_nsw[["xmin"]], bb_nsw[["ymin"]])),
  sf::st_point(c(bb_nsw[["xmax"]], bb_nsw[["ymin"]]))
)

if (height_nsw > width_nsw) {
  height_ratio_nsw <- 1
  width_ratio_nsw <- width_nsw / height_nsw
} else {
  width_ratio_nsw <- 1
  height_ratio_nsw <- height_nsw / width_nsw
}

size <- 3000
width_nsw <- round((size * width_ratio_nsw), 0)
height_nsw <- round((size * height_ratio_nsw), 0)

# Generate raster for NSW
nsw_rast <- stars::st_rasterize(
  nsw_pop_sf |>
    dplyr::select(population, geom),
  nx = width_nsw, ny = height_nsw
)

# Convert to matrix
nsw_mat <- nsw_rast |>
  as("Raster") |>
  rayshader::raster_to_matrix()

# Create transformations for NSW
nsw_mat_log <- log10(nsw_mat + 1)  # +1 to avoid log(0)
nsw_mat_sqrt <- sqrt(nsw_mat)
nsw_mat_power <- nsw_mat^0.7


## --- START MODIFICATIONS ---
  
# Define parameters for landmass elevation and NA value handling
# base_elevation: How much to raise the entire NSW landmass.
# Adjust this value based on the scale of your nsw_mat population data.
# If population values are large, base_elevation might need to be larger.
# This value is in the same units as your nsw_mat data before zscale.
base_elevation <- 50.0  # Example: raise landmass by 50 units
epsilon <- 0.1          # Tiny offset to distinguish pop=0 from NA

# Create a definitive mask for the NSW state boundary
# This uses the nsw_rast as a template for grid alignment
nsw_boundary_sf <- state_boundaries[["New South Wales"]] # Assuming state_boundaries is loaded
template_stars_nsw <- nsw_rast # nsw_rast is the stars object from which nsw_mat was derived

nsw_definitive_boundary_rast <- stars::st_rasterize(
  nsw_boundary_sf,
  template = template_stars_nsw
)
nsw_definitive_mask_matrix_raw <- as(nsw_definitive_boundary_rast, "Raster")
nsw_definitive_mask_matrix <- rayshader::raster_to_matrix(nsw_definitive_mask_matrix_raw)
is_within_nsw_matrix <- !is.na(nsw_definitive_mask_matrix)

# Prepare the final height matrix for rendering
nsw_mat_final <- matrix(NA_real_, nrow = nrow(nsw_mat), ncol = ncol(nsw_mat))

for (r in 1:nrow(nsw_mat)) {
  for (c in 1:ncol(nsw_mat)) {
    if (is_within_nsw_matrix[r, c]) { # If the cell is within NSW boundary
      if (!is.na(nsw_mat[r, c])) { # If there's population data
        # Add population data on top of base_elevation, plus epsilon
        nsw_mat_final[r, c] <- base_elevation + nsw_mat[r, c] + epsilon
      } else { # If it's NA within NSW (e.g., a lake, no data area)
        # Set to base_elevation, will be colored bone white
        nsw_mat_final[r, c] <- base_elevation
      }
    } else { # Outside NSW boundary
      nsw_mat_final[r, c] <- NA_real_
    }
  }
}

# NSW visualization settings (phi, theta, zoom might need tweaking with the new base height)
nsw_zscale <- 10        # Adjust as needed
nsw_phi <- 60           # Elevation angle
nsw_theta <- 25        # Azimuth angle
nsw_zoom <- 0.60        # Zoom level

# The existing 'texture' variable should work well with this new height scheme:
# Bone white for base_elevation (NA-within-NSW)
# Soft yellow onwards for base_elevation + epsilon + population data
# texture <- grDevices::colorRampPalette(cols)(1024) # This is already defined in your script

# --- END MODIFICATIONS ---

# Create the 3D object for NSW
nsw_mat_final |> 
  rayshader::height_shade(texture = texture) |> 
  rayshader::plot_3d(
    heightmap = nsw_mat_final, 
    solid = TRUE,             
    soliddepth = -150,        
    zscale = nsw_zscale, # zscale scales the model here
    shadowdepth = 0,          
    shadow_darkness = .95,
    windowsize = c(800, 800),
    phi = nsw_phi,       # Initial RGL window phi
    zoom = nsw_zoom,     # Initial RGL window zoom
    theta = nsw_theta,   # Initial RGL window theta
    background = "grey10"
  )

# Adjust view - Remember to go back and update the camera settings for the state variables above interactively - modify values as needed
rayshader::render_camera(phi = nsw_phi, zoom = nsw_zoom, theta = nsw_theta) # Use defined variables

# Define the output file path
output_file <- "outputs/images/04-state-population-spike-map-NSW-grey10-elevated-camera-adjusted.png"

# Render the high-quality image
rayshader::render_highquality(
  filename = output_file,
  preview = TRUE,
  light = TRUE,
  ambient_light = FALSE,
  backgroundhigh = "#1A1A1A", 
  backgroundlow = "#1A1A1A",
  ground_material = rayrender::diffuse(color = "#1A1A1A"),
  lightdirection = 225,
  lightaltitude = 60,
  lightintensity = 400,
  interactive = FALSE,
  width = width_nsw,
  height = height_nsw
)

###### Attempt to Render the high-quality image with different camera settings

# Create the 3D object for NSW (same as before)
nsw_mat_final |> 
  rayshader::height_shade(texture = texture) |> 
  rayshader::plot_3d(
    heightmap = nsw_mat_final, 
    solid = TRUE,             
    soliddepth = -150,        
    zscale = nsw_zscale,
    shadowdepth = 0,          
    shadow_darkness = .95,
    windowsize = c(800, 800),
    phi = nsw_phi,
    zoom = nsw_zoom,
    theta = nsw_theta,
    background = "grey10"
  )

# Use pan3d to shift the view
# pan3d(button, dev = cur3d(), subscene = currentSubscene3d(dev))
# You can call this interactively or programmatically

# For programmatic panning, you can simulate mouse movements:
# Pan right and up
rgl::pan3d(c(0.3, -0.2, 0))  # x, y, z shifts

# Or use multiple small pans for fine control
# rgl::pan3d(c(0.1, 0, 0))  # Pan right
# rgl::pan3d(c(0, -0.1, 0)) # Pan up

# Now render with this panned view
output_file_panned <- "outputs/images/04-state-population-spike-map-NSW-grey10-elevated-PANNED.png"

rayshader::render_highquality(
  filename = output_file_panned,
  # No camera arguments - use the RGL window state
  preview = TRUE,
  light = TRUE,
  ambient_light = FALSE,
  backgroundhigh = "#1A1A1A",
  backgroundlow = "#1A1A1A",
  ground_material = rayrender::diffuse(color = "#1A1A1A"),
  lightdirection = 225,
  lightaltitude = 60,
  lightintensity = 400,
  interactive = FALSE,
  width = width_nsw,
  height = height_nsw
)

# Process Victoria
# ------------------------
# Get Victoria bounding box
bb_vic <- sf::st_bbox(vic_pop_sf)

# Calculate dimensions
height_vic <- sf::st_distance(
  sf::st_point(c(bb_vic[["xmin"]], bb_vic[["ymin"]])),
  sf::st_point(c(bb_vic[["xmin"]], bb_vic[["ymax"]]))
)
width_vic <- sf::st_distance(
  sf::st_point(c(bb_vic[["xmin"]], bb_vic[["ymin"]])),
  sf::st_point(c(bb_vic[["xmax"]], bb_vic[["ymin"]]))
)

if (height_vic > width_vic) {
  height_ratio_vic <- 1
  width_ratio_vic <- width_vic / height_vic
} else {
  width_ratio_vic <- 1
  height_ratio_vic <- height_vic / width_vic
}

size <- 3000
width_vic <- round((size * width_ratio_vic), 0)
height_vic <- round((size * height_ratio_vic), 0)

# Generate raster for Victoria
vic_rast <- stars::st_rasterize(
  vic_pop_sf |>
    dplyr::select(population, geom),
  nx = width_vic, ny = height_vic
)

# Convert to matrix
vic_mat <- vic_rast |>
  as("Raster") |>
  rayshader::raster_to_matrix()

# Create transformations for Victoria
vic_mat_log <- log10(vic_mat + 1)  # +1 to avoid log(0)
vic_mat_sqrt <- sqrt(vic_mat)
vic_mat_power <- vic_mat^0.7

# Victoria visualization settings
vic_zscale <- 10        # Adjust as needed
vic_phi <- 65           # Elevation angle
vic_theta <- -10        # Azimuth angle
vic_zoom <- 0.65        # Zoom level

# Create the 3D object for Victoria
vic_mat |>          # We can use any transformation here
  rayshader::height_shade(texture = texture) |>
  rayshader::plot_3d(
    heightmap = vic_mat,  # We can use any transformation here
    solid = TRUE,
    soliddepth = -150,
    zscale = vic_zscale,
    shadowdepth = 0,
    shadow_darkness = .95,
    windowsize = c(800, 800),
    phi = vic_phi,
    zoom = vic_zoom,
    theta = vic_theta,
    background = "grey10"
  )

# Adjust view - Remember to go back and update the camera settings for the state variables above
rayshader::render_camera(phi = 65, zoom = 0.65, theta = 0)

# Define the output file path
output_file <- "outputs/images/04-state-population-spike-map-VIC.png"

# Render the high-quality image
rayshader::render_highquality(
  filename = output_file,
  preview = TRUE,
  light = TRUE,
  ambient_light = FALSE,
  backgroundhigh = "#1A1A1A", 
  backgroundlow = "#1A1A1A",
  ground_material = rayrender::diffuse(color = "#1A1A1A"),
  lightdirection = 225,
  lightaltitude = 60,
  lightintensity = 400,
  interactive = FALSE,
  width = width_vic,
  height = height_vic
)

# Process Queensland
# ------------------------
# Get Queensland bounding box
bb_qld <- sf::st_bbox(qld_pop_sf)

# Calculate dimensions
height_qld <- sf::st_distance(
  sf::st_point(c(bb_qld[["xmin"]], bb_qld[["ymin"]])),
  sf::st_point(c(bb_qld[["xmin"]], bb_qld[["ymax"]]))
)
width_qld <- sf::st_distance(
  sf::st_point(c(bb_qld[["xmin"]], bb_qld[["ymin"]])),
  sf::st_point(c(bb_qld[["xmax"]], bb_qld[["ymin"]]))
)

if (height_qld > width_qld) {
  height_ratio_qld <- 1
  width_ratio_qld <- width_qld / height_qld
} else {
  width_ratio_qld <- 1
  height_ratio_qld <- height_qld / width_qld
}

size <- 3000
width_qld <- round((size * width_ratio_qld), 0)
height_qld <- round((size * height_ratio_qld), 0)

# Generate raster for Queensland
qld_rast <- stars::st_rasterize(
  qld_pop_sf |>
    dplyr::select(population, geom),
  nx = width_qld, ny = height_qld
)

# Convert to matrix
qld_mat <- qld_rast |>
  as("Raster") |>
  rayshader::raster_to_matrix()

# Create transformations for Queensland
qld_mat_log <- log10(qld_mat + 1)  # +1 to avoid log(0)
qld_mat_sqrt <- sqrt(qld_mat)
qld_mat_power <- qld_mat^0.7

# Queensland visualization settings
qld_zscale <- 10        # Adjust as needed
qld_phi <- 60           # Elevation angle
qld_theta <- -15        # Azimuth angle
qld_zoom <- 0.65        # Zoom level

# Create the 3D object for Queensland
qld_mat |>          # We can use any transformation here
  rayshader::height_shade(texture = texture) |>
  rayshader::plot_3d(
    heightmap = qld_mat,  # We can use any transformation here
    solid = TRUE,
    soliddepth = -150,
    zscale = qld_zscale,
    shadowdepth = 0,
    shadow_darkness = .95,
    windowsize = c(800, 800),
    phi = qld_phi,
    zoom = qld_zoom,
    theta = qld_theta,
    background = "grey10"
  )

# Adjust view - Remember to go back and update the camera settings for the state variables above
rayshader::render_camera(phi = 55, zoom = 0.68, theta = -20)

# Define the output file path
output_file <- "outputs/images/04-state-population-spike-map-QLD.png"

# Render the high-quality image
rayshader::render_highquality(
  filename = output_file,
  preview = TRUE,
  light = TRUE,
  ambient_light = FALSE,
  backgroundhigh = "#1A1A1A", 
  backgroundlow = "#1A1A1A",
  ground_material = rayrender::diffuse(color = "#1A1A1A"),
  lightdirection = 225,
  lightaltitude = 60,
  lightintensity = 400,
  interactive = FALSE,
  width = width_qld,
  height = height_qld
)

# Process South Australia
# ------------------------
# Get South Australia bounding box
bb_sa <- sf::st_bbox(sa_pop_sf)

# Calculate dimensions
height_sa <- sf::st_distance(
  sf::st_point(c(bb_sa[["xmin"]], bb_sa[["ymin"]])),
  sf::st_point(c(bb_sa[["xmin"]], bb_sa[["ymax"]]))
)
width_sa <- sf::st_distance(
  sf::st_point(c(bb_sa[["xmin"]], bb_sa[["ymin"]])),
  sf::st_point(c(bb_sa[["xmax"]], bb_sa[["ymin"]]))
)

if (height_sa > width_sa) {
  height_ratio_sa <- 1
  width_ratio_sa <- width_sa / height_sa
} else {
  width_ratio_sa <- 1
  height_ratio_sa <- height_sa / width_sa
}

size <- 3000
width_sa <- round((size * width_ratio_sa), 0)
height_sa <- round((size * height_ratio_sa), 0)

# Generate raster for South Australia
sa_rast <- stars::st_rasterize(
  sa_pop_sf |>
    dplyr::select(population, geom),
  nx = width_sa, ny = height_sa
)

# Convert to matrix
sa_mat <- sa_rast |>
  as("Raster") |>
  rayshader::raster_to_matrix()

# Create transformations for South Australia
sa_mat_log <- log10(sa_mat + 1)  # +1 to avoid log(0)
sa_mat_sqrt <- sqrt(sa_mat)
sa_mat_power <- sa_mat^0.7

# South Australia visualization settings
sa_zscale <- 10        # Adjust as needed
sa_phi <- 65           # Elevation angle
sa_theta <- -30        # Azimuth angle
sa_zoom <- 0.65        # Zoom level

# Create the 3D object for South Australia
sa_mat |>           # We can use any transformation here
  rayshader::height_shade(texture = texture) |>
  rayshader::plot_3d(
    heightmap = sa_mat,   # We can use any transformation here
    solid = TRUE,
    soliddepth = -150,
    zscale = sa_zscale,
    shadowdepth = 0,
    shadow_darkness = .95,
    windowsize = c(800, 800),
    phi = sa_phi,
    zoom = sa_zoom,
    theta = sa_theta,
    background = "grey10"
  )

# Adjust view - Remember to go back and update the camera settings for the state variables above
rayshader::render_camera(phi = 65, zoom = 0.6, theta = -10)

# Define the output file path
output_file <- "outputs/images/04-state-population-spike-map-SA.png"

# Render the high-quality image
rayshader::render_highquality(
  filename = output_file,
  preview = TRUE,
  light = TRUE,
  ambient_light = FALSE,
  backgroundhigh = "#1A1A1A", 
  backgroundlow = "#1A1A1A",
  ground_material = rayrender::diffuse(color = "#1A1A1A"),
  lightdirection = 225,
  lightaltitude = 60,
  lightintensity = 400,
  interactive = FALSE,
  width = width_sa,
  height = height_sa
)

# Process Western Australia
# ------------------------
# Get Western Australia bounding box
bb_wa <- sf::st_bbox(wa_pop_sf)

# Calculate dimensions
height_wa <- sf::st_distance(
  sf::st_point(c(bb_wa[["xmin"]], bb_wa[["ymin"]])),
  sf::st_point(c(bb_wa[["xmin"]], bb_wa[["ymax"]]))
)
width_wa <- sf::st_distance(
  sf::st_point(c(bb_wa[["xmin"]], bb_wa[["ymin"]])),
  sf::st_point(c(bb_wa[["xmax"]], bb_wa[["ymin"]]))
)

if (height_wa > width_wa) {
  height_ratio_wa <- 1
  width_ratio_wa <- width_wa / height_wa
} else {
  width_ratio_wa <- 1
  height_ratio_wa <- height_wa / width_wa
}

size <- 3000
width_wa <- round((size * width_ratio_wa), 0)
height_wa <- round((size * height_ratio_wa), 0)

# Generate raster for Western Australia
wa_rast <- stars::st_rasterize(
  wa_pop_sf |>
    dplyr::select(population, geom),
  nx = width_wa, ny = height_wa
)

# Convert to matrix
wa_mat <- wa_rast |>
  as("Raster") |>
  rayshader::raster_to_matrix()

# Create transformations for Western Australia
wa_mat_log <- log10(wa_mat + 1)  # +1 to avoid log(0)
wa_mat_sqrt <- sqrt(wa_mat)
wa_mat_power <- wa_mat^0.7

# Western Australia visualization settings
wa_zscale <- 10        # Adjust as needed
wa_phi <- 60           # Elevation angle
wa_theta <- 0        # Azimuth angle
wa_zoom <- 0.75        # Zoom level

# Create the 3D object for Western Australia
wa_mat |>           # We can use any transformation here
  rayshader::height_shade(texture = texture) |>
  rayshader::plot_3d(
    heightmap = wa_mat,   # We can use any transformation here
    solid = TRUE,
    soliddepth = -150,
    zscale = wa_zscale,
    shadowdepth = 0,
    shadow_darkness = .95,
    windowsize = c(800, 800),
    phi = wa_phi,
    zoom = wa_zoom,
    theta = wa_theta,
    background = "grey10"
  )

# Adjust view - Remember to go back and update the camera settings for the state variables above
rayshader::render_camera(phi = 60, zoom = 0.75, theta = -0)

# Define the output file path
output_file <- "outputs/images/04-state-population-spike-map-WA.png"

# Render the high-quality image
rayshader::render_highquality(
  filename = output_file,
  preview = TRUE,
  light = TRUE,
  ambient_light = FALSE,
  backgroundhigh = "#1A1A1A", 
  backgroundlow = "#1A1A1A",
  ground_material = rayrender::diffuse(color = "#1A1A1A"),
  lightdirection = 225,
  lightaltitude = 60,
  lightintensity = 400,
  interactive = FALSE,
  width = width_wa,
  height = height_wa
)

# Process Tasmania
# ------------------------
# Get Tasmania bounding box
bb_tas <- sf::st_bbox(tas_pop_sf)

# Calculate dimensions
height_tas <- sf::st_distance(
  sf::st_point(c(bb_tas[["xmin"]], bb_tas[["ymin"]])),
  sf::st_point(c(bb_tas[["xmin"]], bb_tas[["ymax"]]))
)
width_tas <- sf::st_distance(
  sf::st_point(c(bb_tas[["xmin"]], bb_tas[["ymin"]])),
  sf::st_point(c(bb_tas[["xmax"]], bb_tas[["ymin"]]))
)

if (height_tas > width_tas) {
  height_ratio_tas <- 1
  width_ratio_tas <- width_tas / height_tas
} else {
  width_ratio_tas <- 1
  height_ratio_tas <- height_tas / width_tas
}

size <- 3000
width_tas <- round((size * width_ratio_tas), 0)
height_tas <- round((size * height_ratio_tas), 0)

# Generate raster for Tasmania
tas_rast <- stars::st_rasterize(
  tas_pop_sf |>
    dplyr::select(population, geom),
  nx = width_tas, ny = height_tas
)

# Convert to matrix
tas_mat <- tas_rast |>
  as("Raster") |>
  rayshader::raster_to_matrix()

# Create transformations for Tasmania
tas_mat_log <- log10(tas_mat + 1)  # +1 to avoid log(0)
tas_mat_sqrt <- sqrt(tas_mat)
tas_mat_power <- tas_mat^0.7

# Tasmania visualization settings
tas_zscale <- 10        # Adjust as needed
tas_phi <- 45           # Elevation angle
tas_theta <- 0        # Azimuth angle
tas_zoom <- 0.7        # Zoom level

# Create the 3D object for Tasmania
tas_mat |>          # We can use any transformation here
  rayshader::height_shade(texture = texture) |>
  rayshader::plot_3d(
    heightmap = tas_mat, # We can use any transformation here
    solid = TRUE,
    soliddepth = -150,
    zscale = tas_zscale,
    shadowdepth = 0,
    shadow_darkness = .95,
    windowsize = c(800, 800),
    phi = tas_phi,
    zoom = tas_zoom,
    theta = tas_theta,
    background = "grey10"
  )

# Adjust view - Remember to go back and update the camera settings for the state variables above
rayshader::render_camera(phi = 45, zoom = 0.7, theta = 0)

# Define the output file path
output_file <- "outputs/images/04-state-population-spike-map-TAS.png"

# Render the high-quality image
rayshader::render_highquality(
  filename = output_file,
  preview = TRUE,
  light = TRUE,
  ambient_light = FALSE,
  backgroundhigh = "#1A1A1A", 
  backgroundlow = "#1A1A1A",
  ground_material = rayrender::diffuse(color = "#1A1A1A"),
  lightdirection = 225,
  lightaltitude = 60,
  lightintensity = 400,
  interactive = FALSE,
  width = width_tas,
  height = height_tas
)

# Process Northern Territory
# ------------------------
# Get Northern Territory bounding box
bb_nt <- sf::st_bbox(nt_pop_sf)

# Calculate dimensions
height_nt <- sf::st_distance(
  sf::st_point(c(bb_nt[["xmin"]], bb_nt[["ymin"]])),
  sf::st_point(c(bb_nt[["xmin"]], bb_nt[["ymax"]]))
)
width_nt <- sf::st_distance(
  sf::st_point(c(bb_nt[["xmin"]], bb_nt[["ymin"]])),
  sf::st_point(c(bb_nt[["xmax"]], bb_nt[["ymin"]]))
)

if (height_nt > width_nt) {
  height_ratio_nt <- 1
  width_ratio_nt <- width_nt / height_nt
} else {
  width_ratio_nt <- 1
  height_ratio_nt <- height_nt / width_nt
}

size <- 3000
width_nt <- round((size * width_ratio_nt), 0)
height_nt <- round((size * height_ratio_nt), 0)

# Generate raster for Northern Territory
nt_rast <- stars::st_rasterize(
  nt_pop_sf |>
    dplyr::select(population, geom),
  nx = width_nt, ny = height_nt
)

# Convert to matrix
nt_mat <- nt_rast |>
  as("Raster") |>
  rayshader::raster_to_matrix()

# Create transformations for Northern Territory
nt_mat_log <- log10(nt_mat + 1)  # +1 to avoid log(0)
nt_mat_sqrt <- sqrt(nt_mat)
nt_mat_power <- nt_mat^0.7

# Northern Territory visualization settings
nt_zscale <- 10        # Adjust as needed
nt_phi <- 55           # Elevation angle
nt_theta <- 0        # Azimuth angle
nt_zoom <- 0.65        # Zoom level

# Create the 3D object for Northern Territory
nt_mat |>           
  rayshader::height_shade(texture = texture) |>
  rayshader::plot_3d(
    heightmap = nt_mat,   
    solid = TRUE,
    soliddepth = -150,
    zscale = nt_zscale,
    shadowdepth = 0,
    shadow_darkness = .95,
    windowsize = c(800, 800),
    phi = nt_phi,
    zoom = nt_zoom,
    theta = nt_theta,
    background = "grey10"
  )

# Adjust view - Remember to go back and update the camera settings for the state variables above
rayshader::render_camera(phi = 55, zoom = 0.75, theta = 0)

# Define the output file path
output_file <- "outputs/images/04-state-population-spike-map-NT.png"

# Render the high-quality image
rayshader::render_highquality(
  filename = output_file,
  preview = TRUE,
  light = TRUE,
  ambient_light = FALSE,
  backgroundhigh = "#1A1A1A", 
  backgroundlow = "#1A1A1A",
  ground_material = rayrender::diffuse(color = "#1A1A1A"),
  lightdirection = 225,
  lightaltitude = 60,
  lightintensity = 400,
  interactive = FALSE,
  width = width_nt,
  height = height_nt
)

# Process Australian Capital Territory
# ------------------------
# Get ACT bounding box
bb_act <- sf::st_bbox(act_pop_sf)

# Calculate dimensions
height_act <- sf::st_distance(
  sf::st_point(c(bb_act[["xmin"]], bb_act[["ymin"]])),
  sf::st_point(c(bb_act[["xmin"]], bb_act[["ymax"]]))
)
width_act <- sf::st_distance(
  sf::st_point(c(bb_act[["xmin"]], bb_act[["ymin"]])),
  sf::st_point(c(bb_act[["xmax"]], bb_act[["ymin"]]))
)

if (height_act > width_act) {
  height_ratio_act <- 1
  width_ratio_act <- width_act / height_act
} else {
  width_ratio_act <- 1
  height_ratio_act <- height_act / width_act
}

size <- 3000
width_act <- round((size * width_ratio_act), 0)
height_act <- round((size * height_ratio_act), 0)

# Generate raster for ACT
act_rast <- stars::st_rasterize(
  act_pop_sf |>
    dplyr::select(population, geom),
  nx = width_act, ny = height_act
)

# Convert to matrix
act_mat <- act_rast |>
  as("Raster") |>
  rayshader::raster_to_matrix()

# Create transformations for ACT
act_mat_log <- log10(act_mat + 1)  # +1 to avoid log(0)
act_mat_sqrt <- sqrt(act_mat)
act_mat_power <- act_mat^0.7

# ACT visualization settings
act_zscale <- 10        # Adjust as needed
act_phi <- 60           # Elevation angle
act_theta <- 0        # Azimuth angle
act_zoom <- 0.7        # Zoom level

# Create the 3D object for ACT
act_mat |>          # Changed from act_mat_log to act_mat
  rayshader::height_shade(texture = texture) |>
  rayshader::plot_3d(
    heightmap = act_mat,  # Changed from act_mat_log to act_mat
    solid = TRUE,
    soliddepth = -150,
    zscale = act_zscale,
    shadowdepth = 0,
    shadow_darkness = .95,
    windowsize = c(800, 800),
    phi = act_phi,
    zoom = act_zoom,
    theta = act_theta,
    background = "grey10"
  )

# Adjust view - Remember to go back and update the camera settings for the state variables above
rayshader::render_camera(phi = 60, zoom = 0.70, theta = 0)

# Define the output file path
output_file <- "outputs/images/04-state-population-spike-map-ACT.png"

# Render the high-quality image
rayshader::render_highquality(
  filename = output_file,
  preview = TRUE,
  light = TRUE,
  ambient_light = FALSE,
  backgroundhigh = "#1A1A1A", 
  backgroundlow = "#1A1A1A",
  ground_material = rayrender::diffuse(color = "#1A1A1A"),
  lightdirection = 225,
  lightaltitude = 60,
  lightintensity = 400,
  interactive = FALSE,
  width = width_act,
  height = height_act
)

# When done with a state visualization, close the window
rgl::close3d()

#######################################################
#                 MODULAR APPROACH BELOW
# Once we've determined the best settings above,
# we can use them in the modular functions below
#######################################################

# Function to process state data and create matrices
process_state_data <- function(state_pop_sf, state_name, base_size = 3000) {
  # Get state bounding box
  bb <- sf::st_bbox(state_pop_sf)
  
  # Calculate dimensions
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
  
  size <- base_size
  width <- round((size * width_ratio), 0)
  height <- round((size * height_ratio), 0)
  
  # Generate raster for the state
  rast <- stars::st_rasterize(
    state_pop_sf |>
      dplyr::select(population, geom),
    nx = width, ny = height
  )
  
  # Convert to matrix
  mat <- rast |>
    as("Raster") |>
    rayshader::raster_to_matrix()
  
  # Create transformations
  mat_log <- log10(mat + 1)  # +1 to avoid log(0)
  mat_sqrt <- sqrt(mat)
  mat_power <- mat^0.7
  
  return(list(
    mat = mat,
    mat_log = mat_log,
    mat_sqrt = mat_sqrt,
    mat_power = mat_power,
    width = width,
    height = height
  ))
}

# Function to create and save a 3D visualization for a state
visualize_state <- function(state_data, state_name, transformation = "log", 
                            output_dir = "outputs/images", preview = TRUE) {
  # Extract the correct matrix based on transformation type
  if (transformation == "log") {
    heightmap <- state_data$mat_log
  } else if (transformation == "sqrt") {
    heightmap <- state_data$mat_sqrt
  } else {
    heightmap <- state_data$mat_power
  }
  
  # Create the 3D object
  heightmap |>
    rayshader::height_shade(texture = texture) |>
    rayshader::plot_3d(
      heightmap = heightmap,
      solid = FALSE,
      soliddepth = 0,
      zscale = 10,  # Adjust as needed
      shadowdepth = 0,
      shadow_darkness = .95,
      windowsize = c(800, 800),
      phi = 65,     # Elevation angle
      zoom = 0.65,  # Zoom level
      theta = -30,  # Azimuth angle
      background = "white"
    )
  
  # Adjust view - Remember to go back and update the camera settings for the state variables above interactively - modify values as needed
  rayshader::render_camera(phi = 75, zoom = 0.7, theta = 0)
  
  # Define the output file path
  output_file <- file.path(output_dir, paste0("04-state-population-spike-map-", gsub(" ", "-", state_name), ".png"))
  
  # Render the high-quality image
  rayshader::render_highquality(
    filename = output_file,
    preview = preview,
    light = TRUE,
    lightdirection = 225,
    lightaltitude = 60,
    lightintensity = 400,
    interactive = FALSE,
    width = state_data$width,
    height = state_data$height
  )
}

# Process all states
state_names <- c("New South Wales", "Victoria", "Queensland", "South Australia",
                 "Western Australia", "Tasmania", "Northern Territory", "Australian Capital Territory")

# Initialize a list to store state data
all_states_data <- list()

# Loop through each state, process the data, and visualize
for (state_name in state_names) {
  # Extract the state population sf object
  state_pop_sf <- state_pops[[state_name]]
  
  # Process the state data
  state_data <- process_state_data(state_pop_sf, state_name)
  
  # Save to list
  all_states_data[[state_name]] <- state_data
  
  # Visualize and save the spike map (using log transformation as default)
  visualize_state(state_data, state_name, transformation = "log")
}

# When done with all visualizations, close the window
rgl::close3d()