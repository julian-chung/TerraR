#######################################################
#                 Making crisp spike maps with R
#                 Adapted for Australia
#                 2025/05/16
########################################################

#######################################################
# SECTION 1: SETUP & DEPENDENCIES
# Load necessary libraries and create output directories
#######################################################

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

#######################################################
# SECTION 2: DATA LOADING & PREPARATION
# Load population and boundary data, prepare geographic data
#######################################################

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

#######################################################
# SECTION 3: STATE-SPECIFIC DATA PROCESSING
# Create per-state datasets by clipping population to state boundaries
#######################################################

# Create a list of state geometries by name (using the filtered dataframe)
state_boundaries <- split(filtered_states, filtered_states$STE_NAME21)

# WARNING - Creating the state_pops potentially takes hours - used saved data in data/processed/state_pops.rds 
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

# Optional: Save the resulting list for future use (recommended if this takes a long time)
# saveRDS(state_pops, file = "data/processed/state_pops.rds")

# Later, we can reload with:
state_pops <- readRDS("data/processed/state_pops.rds")

# Extract individual state population datasets
nsw_pop_sf <- state_pops[["New South Wales"]]
vic_pop_sf <- state_pops[["Victoria"]]
qld_pop_sf <- state_pops[["Queensland"]]
sa_pop_sf <- state_pops[["South Australia"]]
wa_pop_sf <- state_pops[["Western Australia"]]
tas_pop_sf <- state_pops[["Tasmania"]]
nt_pop_sf <- state_pops[["Northern Territory"]]
act_pop_sf <- state_pops[["Australian Capital Territory"]]

#######################################################
# SECTION 4: UTILITY FUNCTIONS
# Helper functions for data processing and visualization
#######################################################

# Helper function to calculate raster dimensions
# Maintains aspect ratio while setting optimal width and height
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

# Color transformation helper function
# Applies various mathematical transformations to highlight different aspects of the data
apply_color_transform <- function(mat, method = "sqrt") {
  switch(method,
         "log" = log10(mat + 1),
         "sqrt" = sqrt(mat),
         "power07" = mat^0.7,
         "power03" = mat^0.3,
         "eq" = ecdf(mat)(mat),  # Histogram equalization
         "none" = mat,
         mat)
}

# Function to process state data and create matrices
# Creates raw and transformed matrices for visualization
process_state_data <- function(state_pop_sf, state_name, base_size = 3000) {
  # Create state-specific bounding box
  bb <- sf::st_bbox(state_pop_sf)
  
  # Calculate dimensions
  ratios <- get_raster_size(bb)
  width_ratio <- ratios[[1]]
  height_ratio <- ratios[[2]]
  width <- round((base_size * width_ratio), 0)
  height <- round((base_size * height_ratio), 0)
  
  # Generate raster
  state_rast <- stars::st_rasterize(
    state_pop_sf |>
      dplyr::select(population, geom),
    nx = width, ny = height
  )
  
  # Convert to matrix
  state_mat <- state_rast |>
    as("Raster") |>
    rayshader::raster_to_matrix()
  
  # Create derived transformations
  state_mat_log <- log10(state_mat + 1)
  state_mat_sqrt <- sqrt(state_mat)
  state_mat_power <- state_mat^0.7
  
  # Return all results in a list
  return(list(
    bb = bb,
    width = width,
    height = height,
    rast = state_rast,
    mat = state_mat,
    mat_log = state_mat_log,
    mat_sqrt = state_mat_sqrt,
    mat_power = state_mat_power
  ))
}

#######################################################
# SECTION 5: DATA PROCESSING
# Create raster matrices for each state/territory
#######################################################

# Process all states and territories
nsw_data <- process_state_data(nsw_pop_sf, "New South Wales")
vic_data <- process_state_data(vic_pop_sf, "Victoria")
qld_data <- process_state_data(qld_pop_sf, "Queensland")
xsa_data <- process_state_data(sa_pop_sf, "South Australia")
wa_data <- process_state_data(wa_pop_sf, "Western Australia")
tas_data <- process_state_data(tas_pop_sf, "Tasmania")
nt_data <- process_state_data(nt_pop_sf, "Northern Territory")
act_data <- process_state_data(act_pop_sf, "Australian Capital Territory")

#######################################################
# SECTION 6: INTERACTIVE TESTING ENVIRONMENT
# Testing block for experimenting with visualization parameters
# Set TESTING_MODE to TRUE to use this section
#######################################################

# Set to TRUE to enable interactive testing
TESTING_MODE <- FALSE

if (TESTING_MODE) {
  # Select the state to test (uncomment one)
  test_data <- nsw_data
  test_name <- "New South Wales"
  # test_data <- vic_data
  # test_name <- "Victoria"
  # test_data <- qld_data
  # test_name <- "Queensland"
  # test_data <- sa_data
  # test_name <- "South Australia"
  # test_data <- wa_data
  # test_name <- "Western Australia"
  # test_data <- tas_data
  # test_name <- "Tasmania"
  # test_data <- nt_data
  # test_name <- "Northern Territory"
  # test_data <- act_data
  # test_name <- "Australian Capital Territory"
  
  # Select the transformation to test
  test_transformation <- "log" # Options: "raw", "log", "sqrt", "power"
  
  # Select the matrix based on transformation
  if (test_transformation == "log") {
    test_mat <- test_data$mat_log
    test_zscale <- 0.05  # Try different values: 0.01-0.1
  } else if (test_transformation == "sqrt") {
    test_mat <- test_data$mat_sqrt
    test_zscale <- 0.1   # Try different values: 0.05-0.2
  } else if (test_transformation == "power") {
    test_mat <- test_data$mat_power
    test_zscale <- 0.1   # Try different values: 0.05-0.2
  } else if (test_transformation == "raw") {
    test_mat <- test_data$mat
    test_zscale <- 2.0   # Try different values: 1-5
  }
  
  # Camera/view parameters to test
  test_phi <- 65    # Elevation angle (try 45-75)
  test_theta <- 0   # Azimuth angle (try -45 to 45)
  test_zoom <- 0.6  # Zoom level (try 0.5-0.8)
  
  # Australian-themed colors
  cols <- c("#00843D", "#FFCD00", "#FF8000", "#FF4500")
  texture <- grDevices::colorRampPalette(cols)(256)
  
  # Create the 3D preview
  test_mat |>
    rayshader::height_shade(texture = texture) |>
    rayshader::plot_3d(
      heightmap = test_mat,
      solid = FALSE,
      soliddepth = 0,
      zscale = test_zscale,
      shadowdepth = 0,
      shadow_darkness = .95,
      windowsize = c(800, 800),
      phi = test_phi,
      zoom = test_zoom,
      theta = test_theta,
      background = "white"
    )
  
  # Manual camera adjustment function - run this multiple times to find the right view
  adjust_camera <- function(phi = NULL, theta = NULL, zoom = NULL) {
    if (!is.null(phi)) test_phi <<- phi
    if (!is.null(theta)) test_theta <<- theta
    if (!is.null(zoom)) test_zoom <<- zoom
    
    rayshader::render_camera(phi = test_phi, zoom = test_zoom, theta = test_theta)
    
    cat("Current camera settings:\n")
    cat("  phi =", test_phi, "\n")
    cat("  theta =", test_theta, "\n")
    cat("  zoom =", test_zoom, "\n")
  }
  
  # Call this to adjust the camera interactively
  adjust_camera()
  
  # Once you're happy with the settings, you can save them for this state
  save_test_settings <- function() {
    # Create a data frame to store settings
    if (!exists("state_settings")) {
      state_settings <<- data.frame(
        state = character(),
        transformation = character(),
        zscale = numeric(),
        phi = numeric(),
        theta = numeric(),
        zoom = numeric(),
        stringsAsFactors = FALSE
      )
    }
    
    # Add current settings
    new_settings <- data.frame(
      state = test_name,
      transformation = test_transformation,
      zscale = test_zscale,
      phi = test_phi,
      theta = test_theta,
      zoom = test_zoom,
      stringsAsFactors = FALSE
    )
    
    # Update or append settings
    idx <- which(state_settings$state == test_name & 
                 state_settings$transformation == test_transformation)
    if (length(idx) > 0) {
      state_settings[idx,] <<- new_settings
    } else {
      state_settings <<- rbind(state_settings, new_settings)
    }
    
    cat("Settings saved for", test_name, "with", test_transformation, "transformation\n")
    
    # Optional: save to a file
    # write.csv(state_settings, "data/processed/state_visualization_settings.csv", row.names = FALSE)
  }
  
  # Call this when we're happy with our settings
  # save_test_settings()
  
  # Render a quick test snapshot (not high quality)
  quick_test_render <- function(filename = NULL) {
    if (is.null(filename)) {
      clean_state_name <- gsub(" ", "", test_name)
      filename <- paste0("outputs/images/test_", clean_state_name, "_", 
                         test_transformation, ".png")
    }
    
    rayshader::render_snapshot(filename)
    cat("Test snapshot saved to:", filename, "\n")
  }
  
  # Call this to save a quick test image
  # quick_test_render()
  
  # Close the 3D window when done testing
  # rgl::close3d()
}

#######################################################
# SECTION 7: RENDERING FUNCTIONS
# Functions for creating 3D visualizations
#######################################################

# Simple function for quick map rendering with separate color and height transformations
# This function provides fine-grained control over visualization parameters
render_spike_map <- function(mat, filename, zscale = 15, phi = 75, theta = 0, zoom = 0.7, 
                             preview = TRUE, cols = NULL, height_transform = "none",
                             color_transform = "sqrt") {
  if (is.null(cols)) {
    # Australian-themed colors
    cols <- c("#00843D", "#FFCD00", "#FF8000", "#FF4500")
  }
  texture <- grDevices::colorRampPalette(cols)(256)
  
  # Apply color transformation separately from height transformation
  color_mat <- apply_color_transform(mat, color_transform)

  # Apply different height transformation if specified (for 3D shape)
  height_mat <- if(height_transform != "none") {
    apply_color_transform(mat, height_transform)
  } else {
    mat
  }

  # Create the visualization with separate transformations for color and height
  color_mat |>
    rayshader::height_shade(texture = texture) |>
    rayshader::plot_3d(
      heightmap = height_mat, # Use height transformation for 3D shape
      solid = FALSE,
      soliddepth = 0,
      zscale = zscale,
      shadowdepth = 0,
      shadow_darkness = .95,
      windowsize = c(800, 800),
      phi = 65,
      zoom = .65,
      theta = -30,
      background = "white"
    )

  # Adjust camera position
  rayshader::render_camera(phi = phi, zoom = zoom, theta = theta)

  # Render the high-quality output
  rayshader::render_highquality(
    filename = filename,
    preview = preview,
    light = TRUE,
    lightdirection = 225,
    lightaltitude = 60,
    lightintensity = 400,
    interactive = FALSE,
    width = ncol(mat),
    height = nrow(mat)
  )
  
  # Close 3D device
  rgl::close3d()
}

# Function to create and save a 3D visualization for a state with dual transformations
# This is a higher-level function that handles state-specific data and settings
visualize_state <- function(state_data, state_name, height_transform = "log", 
                            color_transform = "sqrt", output_dir = "outputs/images", 
                            preview = TRUE, custom_settings = NULL) {
  # Look up custom settings if available
  if (is.null(custom_settings) && exists("state_settings")) {
    idx <- which(state_settings$state == state_name & 
                 state_settings$transformation == height_transform)
    if (length(idx) > 0) {
      custom_settings <- state_settings[idx,]
    }
  }
  
  # Select the appropriate matrix - always use the raw matrix and transform on demand
  mat <- state_data$mat
  
  # Set zscale based on transformation
  if (height_transform == "log") {
    zscale <- ifelse(!is.null(custom_settings), custom_settings$zscale, 0.05)
  } else if (height_transform == "sqrt") {
    zscale <- ifelse(!is.null(custom_settings), custom_settings$zscale, 0.1)
  } else if (height_transform == "power07") {
    zscale <- ifelse(!is.null(custom_settings), custom_settings$zscale, 0.1)
  } else if (height_transform == "none" || height_transform == "raw") {
    zscale <- ifelse(!is.null(custom_settings), custom_settings$zscale, 2.0)
  } else {
    # Default case
    height_transform <- "log"
    zscale <- 0.05
    warning("Unknown transformation type: ", height_transform, 
            ". Using 'log' transformation instead.")
  }
  
  # Set camera parameters
  phi <- ifelse(!is.null(custom_settings), custom_settings$phi, 65)
  theta <- ifelse(!is.null(custom_settings), custom_settings$theta, 0)
  zoom <- ifelse(!is.null(custom_settings), custom_settings$zoom, 0.6)
  
  # Define the output file path
  clean_state_name <- gsub(" ", "", state_name)
  output_file <- paste0(output_dir, "/05-state-population-spike-map-", 
                       clean_state_name, "-", height_transform, 
                       "-", color_transform, ".png")
  
  # Use the enhanced render_spike_map function
  render_spike_map(
    mat = mat,
    filename = output_file,
    zscale = zscale,
    phi = phi,
    theta = theta,
    zoom = zoom,
    preview = preview,
    height_transform = height_transform,
    color_transform = color_transform
  )
  
  return(output_file)
}

# Legacy function - can be removed after confirming the dual transform approach works
# Function to create and save a 3D visualization for a state
visualize_state <- function(state_data, state_name, transformation = "log", 
                            output_dir = "outputs/images", preview = TRUE,
                            custom_settings = NULL) {
  # Look up custom settings if available
  if (is.null(custom_settings) && exists("state_settings")) {
    idx <- which(state_settings$state == state_name & 
                 state_settings$transformation == transformation)
    if (length(idx) > 0) {
      custom_settings <- state_settings[idx,]
    }
  }
  
  # Select the appropriate matrix based on transformation
  if (transformation == "log") {
    mat <- state_data$mat_log
    zscale <- ifelse(!is.null(custom_settings), custom_settings$zscale, 0.05)
  } else if (transformation == "sqrt") {
    mat <- state_data$mat_sqrt
    zscale <- ifelse(!is.null(custom_settings), custom_settings$zscale, 0.1)
  } else if (transformation == "power") {
    mat <- state_data$mat_power
    zscale <- ifelse(!is.null(custom_settings), custom_settings$zscale, 0.1)
  } else if (transformation == "raw") {
    mat <- state_data$mat
    zscale <- ifelse(!is.null(custom_settings), custom_settings$zscale, 2.0)
  } else {
    # Default case
    mat <- state_data$mat_log
    zscale <- 0.05
    warning("Unknown transformation type: ", transformation, 
            ". Using 'log' transformation instead.")
  }
  
  # Set camera parameters
  phi <- ifelse(!is.null(custom_settings), custom_settings$phi, 65)
  theta <- ifelse(!is.null(custom_settings), custom_settings$theta, 0)
  zoom <- ifelse(!is.null(custom_settings), custom_settings$zoom, 0.6)
  
  # Australian-themed colors
  cols <- c("#00843D", "#FFCD00", "#FF8000", "#FF4500")
  texture <- grDevices::colorRampPalette(cols)(256)
  
  # Create the 3D object
  mat |>
    rayshader::height_shade(texture = texture) |>
    rayshader::plot_3d(
      heightmap = mat,
      solid = FALSE,
      soliddepth = 0,
      zscale = zscale,
      shadowdepth = 0,
      shadow_darkness = .95,
      windowsize = c(800, 800),
      phi = phi,
      zoom = zoom,
      theta = theta,
      background = "white"
    )
  
  # Adjust view
  rayshader::render_camera(phi = phi, zoom = zoom, theta = theta)
  
  # Define the output file path
  clean_state_name <- gsub(" ", "", state_name)
  output_file <- paste0(output_dir, "/04-state-population-spike-map-", 
                       clean_state_name, "-", transformation, ".png")
  
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
  
  # Close the 3D device
  rgl::close3d()
  
  # Return the output file path
  return(output_file)
}

#######################################################
# SECTION 8: BATCH RENDERING
# Create visualizations for all states with specified transformations
#######################################################

# Visualize all states with all transformations - OPTION 1: Individually
output_files <- list()

# NSW
output_files[["NSW_raw"]] <- visualize_state(nsw_data, "New South Wales", "raw")
output_files[["NSW_log"]] <- visualize_state(nsw_data, "New South Wales", "log")
output_files[["NSW_sqrt"]] <- visualize_state(nsw_data, "New South Wales", "sqrt")
output_files[["NSW_power"]] <- visualize_state(nsw_data, "New South Wales", "power")

# Victoria
output_files[["Victoria_raw"]] <- visualize_state(vic_data, "Victoria", "raw")
output_files[["Victoria_log"]] <- visualize_state(vic_data, "Victoria", "log")
output_files[["Victoria_sqrt"]] <- visualize_state(vic_data, "Victoria", "sqrt")
output_files[["Victoria_power"]] <- visualize_state(vic_data, "Victoria", "power")

# Queensland
output_files[["Queensland_raw"]] <- visualize_state(qld_data, "Queensland", "raw")
output_files[["Queensland_log"]] <- visualize_state(qld_data, "Queensland", "log")
output_files[["Queensland_sqrt"]] <- visualize_state(qld_data, "Queensland", "sqrt")
output_files[["Queensland_power"]] <- visualize_state(qld_data, "Queensland", "power")

# South Australia
output_files[["SouthAustralia_raw"]] <- visualize_state(sa_data, "South Australia", "raw")
output_files[["SouthAustralia_log"]] <- visualize_state(sa_data, "South Australia", "log")
output_files[["SouthAustralia_sqrt"]] <- visualize_state(sa_data, "South Australia", "sqrt")
output_files[["SouthAustralia_power"]] <- visualize_state(sa_data, "South Australia", "power")

# Western Australia
output_files[["WesternAustralia_raw"]] <- visualize_state(wa_data, "Western Australia", "raw")
output_files[["WesternAustralia_log"]] <- visualize_state(wa_data, "Western Australia", "log")
output_files[["WesternAustralia_sqrt"]] <- visualize_state(wa_data, "Western Australia", "sqrt")
output_files[["WesternAustralia_power"]] <- visualize_state(wa_data, "Western Australia", "power")

# Tasmania
output_files[["Tasmania_raw"]] <- visualize_state(tas_data, "Tasmania", "raw")
output_files[["Tasmania_log"]] <- visualize_state(tas_data, "Tasmania", "log")
output_files[["Tasmania_sqrt"]] <- visualize_state(tas_data, "Tasmania", "sqrt")
output_files[["Tasmania_power"]] <- visualize_state(tas_data, "Tasmania", "power")

# Northern Territory
output_files[["NorthernTerritory_raw"]] <- visualize_state(nt_data, "Northern Territory", "raw")
output_files[["NorthernTerritory_log"]] <- visualize_state(nt_data, "Northern Territory", "log")
output_files[["NorthernTerritory_sqrt"]] <- visualize_state(nt_data, "Northern Territory", "sqrt")
output_files[["NorthernTerritory_power"]] <- visualize_state(nt_data, "Northern Territory", "power")

# Australian Capital Territory
output_files[["AustralianCapitalTerritory_raw"]] <- visualize_state(act_data, "Australian Capital Territory", "raw")
output_files[["AustralianCapitalTerritory_log"]] <- visualize_state(act_data, "Australian Capital Territory", "log")
output_files[["AustralianCapitalTerritory_sqrt"]] <- visualize_state(act_data, "Australian Capital Territory", "sqrt")
output_files[["AustralianCapitalTerritory_power"]] <- visualize_state(act_data, "Australian Capital Territory", "power")

# Display all output file paths
output_files

#######################################################
# SECTION 9: ALTERNATIVE BATCH PROCESSING WITH DUAL TRANSFORMATIONS
# Process all states with separate height and color transformations
# Uncomment to use this approach instead of the manual approach above
#######################################################

# OPTION 2: Loop through all states with all height+color transformation combinations
# Uncomment to use this approach instead of the manual approach above

# all_state_data <- list(
#   list(nsw_data, "New South Wales"),
#   list(vic_data, "Victoria"),
#   list(qld_data, "Queensland"),
#   list(sa_data, "South Australia"),
#   list(wa_data, "Western Australia"),
#   list(tas_data, "Tasmania"),
#   list(nt_data, "Northern Territory"),
#   list(act_data, "Australian Capital Territory")
# )
# 
# height_transformations <- c("log", "sqrt", "power07", "none")
# color_transformations <- c("sqrt", "eq")
# 
# for (state_info in all_state_data) {
#   for (height_trans in height_transformations) {
#     for (color_trans in color_transformations) {
#       visualize_state(
#         state_info[[1]], 
#         state_info[[2]], 
#         height_transform = height_trans,
#         color_transform = color_trans
#       )
#     }
#   }
# }

#######################################################
# SECTION 10: EXAMPLES OF DIRECT FUNCTION USAGE
# Examples showing how to use render_spike_map directly
# Uncomment individual examples to run them
#######################################################

# Below are some examples of using render_spike_map function directly (commented out)
# These can be uncommented and used for quick exploration

# Example 1: Simple rendering of NSW with raw population data
# render_spike_map(
#   mat = nsw_data$mat,
#   filename = "outputs/images/quick_nsw_raw.png",
#   zscale = 100,  # Higher zscale for raw data
#   phi = 70,
#   theta = -15,
#   zoom = 0.6
# )

# Example 2: Log-transformed data for Victoria
# render_spike_map(
#   mat = vic_data$mat_log,
#   filename = "outputs/images/quick_vic_log.png",
#   zscale = 0.05,  # Lower zscale for log-transformed data
#   phi = 65,
#   theta = 0,
#   zoom = 0.7
# )

# Example 3: Sqrt-transformed data for Queensland with custom colors
# custom_colors <- c("#1E88E5", "#FFC107", "#D81B60", "#004D40")
# render_spike_map(
#   mat = qld_data$mat_sqrt,
#   filename = "outputs/images/quick_qld_sqrt_custom.png",
#   zscale = 0.1,
#   phi = 60,
#   theta = 10,
#   zoom = 0.65,
#   cols = custom_colors
# )

# Example 4: Quick comparison of different transformations for one state
# transformations <- list(
#   list(wa_data$mat, "raw", 100),
#   list(wa_data$mat_log, "log", 0.05),
#   list(wa_data$mat_sqrt, "sqrt", 0.1),
#   list(wa_data$mat_power, "power", 0.1)
# )
# 
# for (i in seq_along(transformations)) {
#   trans <- transformations[[i]]
#   render_spike_map(
#     mat = trans[[1]],
#     filename = paste0("outputs/images/quick_wa_", trans[[2]], ".png"),
#     zscale = trans[[3]],
#     phi = 65,
#     theta = 0,
#     zoom = 0.7
#   )
# }

#######################################################
# SECTION 11: DUAL TRANSFORMATION EXAMPLES
# Examples showing separate height and color transformations
#######################################################

# Example 5: Dual transformation - log heights with histogram equalized colors
# render_spike_map(
#   mat = nsw_data$mat,
#   filename = "outputs/images/dual_transform_nsw_log_eq.png",
#   zscale = 0.05,
#   phi = 65,
#   theta = 0,
#   zoom = 0.7,
#   height_transform = "log",     # Log transform for heights
#   color_transform = "eq"        # Equalize colors for better contrast
# )

# Example 6: Different power transforms for height vs color
# render_spike_map(
#   mat = vic_data$mat,
#   filename = "outputs/images/dual_transform_vic_power07_power03.png",
#   zscale = 0.1,
#   phi = 65,
#   theta = 0,
#   zoom = 0.7,
#   height_transform = "power07", # Mild power transform for heights
#   color_transform = "power03"   # Stronger power transform for colors
# )

# Example 7: Raw heights with sqrt colors
# render_spike_map(
#   mat = qld_data$mat,
#   filename = "outputs/images/dual_transform_qld_raw_sqrt.png",
#   zscale = 100,
#   phi = 65,
#   theta = 0,
#   zoom = 0.7,
#   height_transform = "none",    # Raw data for heights
#   color_transform = "sqrt"      # Sqrt transform for colors
# )

# Example 8: Test all color transformations with one height transformation
# height_trans <- "log"
# color_transforms <- c("none", "log", "sqrt", "power07", "power03", "eq")
# 
# for (color_trans in color_transforms) {
#   render_spike_map(
#     mat = wa_data$mat,
#     filename = paste0("outputs/images/color_test_wa_", height_trans, "_", color_trans, ".png"),
#     zscale = 0.05,
#     phi = 65,
#     theta = 0, 
#     zoom = 0.7,
#     height_transform = height_trans,
#     color_transform = color_trans
#   )
# }