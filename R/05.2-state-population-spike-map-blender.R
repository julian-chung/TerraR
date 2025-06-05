#######################################################
#                 Making crisp spike maps with R
#                 Adapted for Australia - Blender Export Version
#                 2025/06/05
########################################################
# install rayshader & rayrender from the source
# devtools::install_github("tylermorganwall/rayshader")
# devtools::install_github("tylermorganwall/rayrender")

# libraries we need
libs <- c(
  "tidyverse", "R.utils",
  "httr", "sf", "stars",
  "rayshader", "rgl"  # Added rgl for 3D export functionality
)

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}

# load libraries
invisible(lapply(libs, library, character.only = T))

# Ensure the output directories exist
if (!dir.exists("outputs/images")) {
  dir.create("outputs/images", recursive = TRUE)
}
# NEW: Ensure the output directory for 3D models exists
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

# Load the preprocessed state population data
# (Assuming this has already been created from the original script)
state_pops <- readRDS("data/processed/state_pops.rds")

#######################################################
#                 WORKING AREA FOR STATE SETTINGS - BLENDER EXPORT VERSION
# Use this section to experiment with settings for
# individual states and export to Blender-compatible formats
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

#######################################################
# NEW FUNCTION: Export state to Blender-compatible formats
#######################################################
export_state_to_blender <- function(heightmap, state_name, transformation = "", 
                                   export_formats = c("obj", "ply", "stl")) {
  # Create clean filename
  clean_state_name <- gsub(" ", "_", tolower(state_name))
  transformation_suffix <- ifelse(transformation == "", "", paste0("_", transformation))
  
  base_filename <- paste0("state_", clean_state_name, transformation_suffix)
  
  # Export to different formats
  exported_files <- c()
  
  if ("obj" %in% export_formats) {
    obj_file <- file.path("outputs/models", paste0(base_filename, ".obj"))
    rgl::writeOBJ(obj_file, pointsAsSpherex = FALSE)
    exported_files <- c(exported_files, obj_file)
    message("OBJ exported: ", obj_file)
  }
  
  if ("ply" %in% export_formats) {
    ply_file <- file.path("outputs/models", paste0(base_filename, ".ply"))
    rgl::writePLY(ply_file, pointsAsSpherex = FALSE)
    exported_files <- c(exported_files, ply_file)
    message("PLY exported: ", ply_file)
  }
  
  if ("stl" %in% export_formats) {
    stl_file <- file.path("outputs/models", paste0(base_filename, ".stl"))
    rayshader::save_3dprint(filename = stl_file, rotate = TRUE)
    exported_files <- c(exported_files, stl_file)
    message("STL exported: ", stl_file)
  }
  
  return(exported_files)
}

# Process New South Wales - BLENDER EXPORT VERSION
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

## --- START MODIFICATIONS (same as original) ---
# Define parameters for landmass elevation and NA value handling
base_elevation <- 50.0  # Example: raise landmass by 50 units
epsilon <- 0.1          # Tiny offset to distinguish pop=0 from NA

# Create a definitive mask for the NSW state boundary
nsw_boundary_sf <- state_boundaries[["New South Wales"]]
template_stars_nsw <- nsw_rast

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

# NSW visualization settings
nsw_zscale <- 10        # Adjust as needed
nsw_phi <- 60           # Elevation angle
nsw_theta <- 25        # Azimuth angle
nsw_zoom <- 0.60        # Zoom level
## --- END MODIFICATIONS ---

# Create the 3D object for NSW and export to Blender
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

# Adjust view for optimal export
rayshader::render_camera(phi = nsw_phi, zoom = nsw_zoom, theta = nsw_theta)

# NEW: Export NSW to Blender formats
nsw_exported_files <- export_state_to_blender(nsw_mat_final, "New South Wales", "elevated")

# Process Victoria - BLENDER EXPORT VERSION
# ------------------------
rgl::close3d()  # Close previous window

# Get Victoria bounding box
bb_vic <- sf::st_bbox(vic_pop_sf)

# Calculate dimensions (same logic as original)
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

# Victoria visualization settings
vic_zscale <- 10
vic_phi <- 65
vic_theta <- -10
vic_zoom <- 0.65

# Create the 3D object for Victoria
vic_mat |>
  rayshader::height_shade(texture = texture) |>
  rayshader::plot_3d(
    heightmap = vic_mat,
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

# Adjust view
rayshader::render_camera(phi = 65, zoom = 0.65, theta = 0)

# NEW: Export Victoria to Blender formats
vic_exported_files <- export_state_to_blender(vic_mat, "Victoria")

#######################################################
# NEW: AUTOMATED PROCESSING FUNCTION FOR ALL REMAINING STATES
#######################################################

process_and_export_state <- function(state_pop_sf, state_name, state_boundaries) {
  message("Processing ", state_name, "...")
  
  # Close any existing 3D window
  rgl::close3d()
  
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
  
  size <- 3000
  width <- round((size * width_ratio), 0)
  height <- round((size * height_ratio), 0)
  
  # Generate raster
  rast <- stars::st_rasterize(
    state_pop_sf |>
      dplyr::select(population, geom),
    nx = width, ny = height
  )
  
  # Convert to matrix
  mat <- rast |>
    as("Raster") |>
    rayshader::raster_to_matrix()
  
  # Apply the same elevation treatment as NSW (if desired)
  # For simplicity, we'll use the basic matrix here, but you can apply the same
  # boundary-based elevation logic from NSW if needed
  
  # State-specific visualization settings (using sensible defaults)
  zscale <- 10
  phi <- 60
  theta <- 0
  zoom <- 0.65
  
  # Create the 3D object
  mat |>
    rayshader::height_shade(texture = texture) |>
    rayshader::plot_3d(
      heightmap = mat,
      solid = TRUE,
      soliddepth = -150,
      zscale = zscale,
      shadowdepth = 0,
      shadow_darkness = .95,
      windowsize = c(800, 800),
      phi = phi,
      zoom = zoom,
      theta = theta,
      background = "grey10"
    )
  
  # Adjust view
  rayshader::render_camera(phi = phi, zoom = zoom, theta = theta)
  
  # Export to Blender formats
  exported_files <- export_state_to_blender(mat, state_name)
  
  return(exported_files)
}

# Process remaining states automatically
remaining_states <- c("Queensland", "South Australia", "Western Australia", 
                     "Tasmania", "Northern Territory", "Australian Capital Territory")

all_exported_files <- list()

for (state_name in remaining_states) {
  state_pop_sf <- state_pops[[state_name]]
  exported_files <- process_and_export_state(state_pop_sf, state_name, state_boundaries)
  all_exported_files[[state_name]] <- exported_files
}

# Close final 3D window
rgl::close3d()

#######################################################
# SUMMARY AND BLENDER IMPORT INSTRUCTIONS
#######################################################

cat("\n")
cat("============================================================================\n")
cat("AUSTRALIAN STATE POPULATION SPIKE MAPS - BLENDER EXPORT COMPLETE\n")
cat("============================================================================\n")
cat("The following 3D models have been exported for Blender:\n\n")

# Print all exported files
cat("NEW SOUTH WALES (with elevated boundary treatment):\n")
for (file in nsw_exported_files) {
  cat("  - ", file, "\n")
}

cat("\nVICTORIA:\n")
for (file in vic_exported_files) {
  cat("  - ", file, "\n")
}

for (state_name in remaining_states) {
  cat("\n", toupper(state_name), ":\n")
  for (file in all_exported_files[[state_name]]) {
    cat("  - ", file, "\n")
  }
}

cat("\n============================================================================\n")
cat("INSTRUCTIONS FOR IMPORTING INTO BLENDER:\n")
cat("============================================================================\n")
cat("1. Open Blender\n")
cat("2. Delete the default cube (select it and press X > Delete)\n")
cat("3. File > Import > Wavefront (.obj) for OBJ files\n")
cat("   OR File > Import > Stanford (.ply) for PLY files\n")
cat("   OR File > Import > STL (.stl) for STL files\n")
cat("4. Navigate to the 'outputs/models/' directory\n")
cat("5. Select the desired state file\n")
cat("6. Adjust import settings as needed:\n")
cat("   - Scale: You may need to scale down (try 0.1 or 0.01)\n")
cat("   - Orientation: The model may need rotation\n")
cat("7. After import, you can:\n")
cat("   - Apply materials and textures\n")
cat("   - Add lighting\n")
cat("   - Set up camera angles\n")
cat("   - Render high-quality images or animations\n")
cat("\nRECOMMENDATION:\n")
cat("- Start with OBJ files as they're most universally supported\n")
cat("- PLY files may preserve vertex colors better\n")
cat("- STL files are optimized for 3D printing workflows\n")
cat("============================================================================\n")

# NEW: Save a summary of all exported files to a text file
summary_file <- "outputs/models/export_summary.txt"
writeLines(c(
  "Australian State Population Spike Maps - Blender Export Summary",
  paste("Generated on:", Sys.time()),
  "",
  "Exported Files:",
  paste("New South Wales:", paste(nsw_exported_files, collapse = ", ")),
  paste("Victoria:", paste(vic_exported_files, collapse = ", ")),
  sapply(remaining_states, function(state) {
    paste(state, ":", paste(all_exported_files[[state]], collapse = ", "))
  }),
  "",
  "Import these files into Blender using File > Import and selecting the appropriate format."
), summary_file)

message("Export summary saved to: ", summary_file)