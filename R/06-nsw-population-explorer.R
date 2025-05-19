# NSW Population Visualization
# This script loads the processed state population data for NSW
# and provides functionality for experimenting with different visualizations

#######################################################
# SECTION 1: SETUP & DEPENDENCIES
#######################################################

# Required libraries
libs <- c("tidyverse", "sf", "stars", "rayshader", "rgl")

# Install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}

# Load libraries
invisible(lapply(libs, library, character.only = T))

# Create output directory if needed
if (!dir.exists("outputs/explorations")) {
  dir.create("outputs/explorations", recursive = TRUE)
}

#######################################################
# SECTION 2: DATA LOADING & PREPARATION
#######################################################

# Load the state population data
state_pops <- readRDS("data/processed/state_pops.rds")

# Extract NSW data
nsw_pop_sf <- state_pops[["New South Wales"]]

# Get NSW bounding box
bb_nsw <- sf::st_bbox(nsw_pop_sf)

# Calculate dimensions with aspect ratio preservation
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

# Set size parameters
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

#######################################################
# SECTION 3: COLOR PALETTES & TRANSFORMATIONS
#######################################################

# Color palettes for experimentation
# Standard green/gold Australian colors
aus_colors <- c("#00843D", "#FFCD00", "#FF8000", "#FF4500")
texture_aus <- grDevices::colorRampPalette(aus_colors)(256)

# Blue-focused palette
blue_colors <- c("#001D34", "#00477D", "#0074D9", "#7FDBFF")
texture_blue <- grDevices::colorRampPalette(blue_colors)(256)

# Terrain-like palette
terrain_colors <- c("#006837", "#1A9850", "#66BD63", "#A6D96A", "#D9EF8B", 
                    "#FFFFBF", "#FEE08B", "#FDAE61", "#F46D43", "#D73027", "#A50026")
texture_terrain <- grDevices::colorRampPalette(terrain_colors)(256)

# Color transformation helper function
apply_transform <- function(mat, method = "sqrt") {
  switch(method,
         "log" = log10(mat + 1),
         "sqrt" = sqrt(mat),
         "power07" = mat^0.7,
         "power03" = mat^0.3,
         "eq" = ecdf(mat)(mat),  # Histogram equalization
         "none" = mat,
         mat)
}

#######################################################
# SECTION 4: BOUNDARY & ANNOTATION FUNCTIONS
#######################################################

# Add state boundary to the current 3D plot
add_state_boundary <- function(state_sf, color = "#000000", linewidth = 3, 
                               elevation_offset = 0.001, simplify_tolerance = NULL) {
  # Get current heightmap info
  current_scene <- rayshader::get_scene_parameters()
  
  # Extract the boundary as line
  state_boundary <- sf::st_boundary(sf::st_union(state_sf))
  
  # Optionally simplify the boundary to improve performance
  if (!is.null(simplify_tolerance)) {
    state_boundary <- sf::st_simplify(state_boundary, dTolerance = simplify_tolerance)
  }
  
  # Extract coordinates
  boundary_coords <- sf::st_coordinates(state_boundary)
  
  # Convert to matrix for rayshader
  x_coords <- boundary_coords[, "X"]
  y_coords <- boundary_coords[, "Y"]
  
  # Convert to relative coordinates within the visualization window
  # by mapping the bounding box to matrix dimensions
  state_bbox <- sf::st_bbox(state_sf)
  x_norm <- (x_coords - state_bbox$xmin) / (state_bbox$xmax - state_bbox$xmin) * ncol(current_scene$heightmap)
  y_norm <- (y_coords - state_bbox$ymin) / (state_bbox$ymax - state_bbox$ymin) * nrow(current_scene$heightmap)
  
  # Get z values from the heightmap and add a small offset to ensure visibility
  z_values <- numeric(length(x_norm))
  
  for (i in seq_along(x_norm)) {
    x_idx <- min(max(1, round(x_norm[i])), ncol(current_scene$heightmap))
    y_idx <- min(max(1, round(y_norm[i])), nrow(current_scene$heightmap))
    z_values[i] <- current_scene$heightmap[y_idx, x_idx] * current_scene$zscale + elevation_offset
  }
  
  # Draw the line segments
  for (i in 1:(length(x_norm)-1)) {
    if (boundary_coords[i, "L1"] == boundary_coords[i+1, "L1"]) {  # Check if part of same line
      rgl::lines3d(
        x = c(x_norm[i], x_norm[i+1]),
        y = c(y_norm[i], y_norm[i+1]),
        z = c(z_values[i], z_values[i+1]),
        color = color,
        lwd = linewidth
      )
    }
  }
}

# Add boundary as an overlay (alternative approach)
add_boundary_overlay <- function(state_sf, color = "#FFFFFF", linewidth = 2, alpha = 0.7) {
  # Create a matrix of the same dimensions with zeros
  overlay_mat <- matrix(0, nrow = nrow(nsw_mat), ncol = ncol(nsw_mat))
  
  # Extract boundary and convert to SpatRaster with same extents as our data
  boundary <- sf::st_boundary(sf::st_union(state_sf))
  
  # Rasterize the boundary
  boundary_rast <- stars::st_rasterize(
    sf::st_set_crs(sf::st_as_sf(boundary), sf::st_crs(state_sf)), 
    nx = ncol(nsw_mat), 
    ny = nrow(nsw_mat),
    template = nsw_rast
  )
  
  # Convert to matrix
  boundary_mat <- boundary_rast |>
    as("Raster") |>
    rayshader::raster_to_matrix()
  
  # Create a binary mask where boundary exists
  boundary_mat[boundary_mat > 0] <- 1
  
  # Apply the overlay to the current plot
  rayshader::add_overlay(
    boundary_mat,
    alphalayer = alpha, 
    colorintensity = 1, 
    alphacolor = color
  )
}

# Add a point of interest to the current 3D plot
add_point <- function(x, y, z = NULL, label = NULL, color = "#FF0000", size = 10) {
  # If z not provided, extract from current heightmap at x,y location
  if (is.null(z)) {
    # Get current heightmap
    current_scene <- rayshader::get_scene_parameters()
    if (!is.null(current_scene$heightmap)) {
      # Ensure x and y are within bounds
      x_idx <- min(max(1, round(x)), ncol(current_scene$heightmap))
      y_idx <- min(max(1, round(y)), nrow(current_scene$heightmap))
      z <- current_scene$heightmap[y_idx, x_idx] * current_scene$zscale * 1.1 # Slightly above surface
    } else {
      z <- 0
    }
  }
  
  # Add point
  rgl::points3d(x, y, z, color = color, size = size)
  
  # Add label if provided
  if (!is.null(label)) {
    rayshader::render_label(label, x = x + 50, y = y, z = z, 
                           textsize = 1.0, 
                           alpha = 1.0,
                           color = color)
  }
}

#######################################################
# SECTION 5: VISUALIZATION FUNCTIONS
#######################################################

# Enhanced visualization function with separate height and color transformations
visualize_nsw <- function(base_mat = nsw_mat, 
                          height_transform = "none",
                          color_transform = "none",
                          color_palette = aus_colors,
                          zscale = 10,
                          phi = 65,
                          theta = -30,
                          zoom = 0.65,
                          title = "NSW Population",
                          save_preview = FALSE,
                          filename = NULL,
                          add_boundary = FALSE,
                          boundary_color = "#000000",
                          boundary_width = 3,
                          boundary_simplify = 1000,
                          use_overlay = FALSE,
                          overlay_alpha = 0.7) {
  
  # Apply transformations
  height_mat <- apply_transform(base_mat, height_transform)
  color_mat <- apply_transform(base_mat, color_transform)
  
  # Create color texture from palette
  color_texture <- grDevices::colorRampPalette(color_palette)(256)
  
  # Update title with transformation info if not custom
  if (title == "NSW Population") {
    height_label <- if (height_transform != "none") paste0(" (Height: ", height_transform, ")") else ""
    color_label <- if (color_transform != "none") paste0(" (Color: ", color_transform, ")") else ""
    title <- paste0("NSW Population", height_label, color_label)
  }
  
  # Create the 3D object
  color_mat |>
    rayshader::height_shade(texture = color_texture) |>
    rayshader::plot_3d(
      heightmap = height_mat,
      solid = FALSE,
      soliddepth = 0,
      zscale = zscale,
      shadowdepth = 0,
      shadow_darkness = 0.95,
      windowsize = c(800, 800),
      phi = phi,
      zoom = zoom,
      theta = theta,
      background = "white"
    )
  
  # Add state boundary if requested
  if (add_boundary) {
    if (use_overlay) {
      add_boundary_overlay(nsw_pop_sf, 
                          color = boundary_color, 
                          linewidth = boundary_width,
                          alpha = overlay_alpha)
    } else {
      add_state_boundary(nsw_pop_sf, 
                         color = boundary_color, 
                         linewidth = boundary_width,
                         simplify_tolerance = boundary_simplify)
    }
  }
  
  # Add a title
  rayshader::render_label(title, x = 300, y = 50, z = 0, 
                         textsize = 2.0, 
                         alpha = 1.0,
                         color = "black")
  
  # Save preview if requested
  if (save_preview && !is.null(filename)) {
    output_path <- paste0("outputs/explorations/", filename, ".png")
    rayshader::render_snapshot(output_path)
    cat("Preview saved to", output_path, "\n")
  }
  
  # Return the rgl window ID for potential later use
  return(rgl::cur3d())
}

# Function to render high-quality output
render_high_quality <- function(filename, 
                               lightdirection = 225,
                               lightaltitude = 60,
                               lightintensity = 400,
                               width = width_nsw,
                               height = height_nsw) {
  
  output_file <- paste0("outputs/explorations/", filename, "_hq.png")
  
  rayshader::render_highquality(
    filename = output_file,
    preview = TRUE,
    light = TRUE,
    lightdirection = lightdirection,
    lightaltitude = lightaltitude,
    lightintensity = lightintensity,
    interactive = FALSE,
    width = width,
    height = height
  )
  
  cat("High-quality render saved to", output_file, "\n")
}

#######################################################
# SECTION 6: EXAMPLES & USAGE
# Uncomment these to explore different visualizations
#######################################################

#----------------
# Example 1: Basic visualization with various transforms
#----------------

# # Raw data visualization
# visualize_nsw(save_preview = TRUE, filename = "nsw_raw")
# rgl::close3d()

# # Log height with sqrt color (good balance)
# visualize_nsw(
#   height_transform = "log",
#   color_transform = "sqrt",
#   zscale = 0.05,
#   save_preview = TRUE, 
#   filename = "nsw_log_height_sqrt_color"
# )
# rgl::close3d()

# # Raw height with equalized color (shows density patterns)
# visualize_nsw(
#   height_transform = "none",
#   color_transform = "eq",
#   zscale = 15,
#   color_palette = blue_colors,
#   save_preview = TRUE, 
#   filename = "nsw_raw_height_eq_color_blue"
# )
# rgl::close3d()

# # Power height with power03 color (more contrast)
# visualize_nsw(
#   height_transform = "power07",
#   color_transform = "power03",
#   zscale = 0.1,
#   color_palette = terrain_colors,
#   save_preview = TRUE,
#   filename = "nsw_power07_height_power03_color_terrain"
# )
# rgl::close3d()

#----------------
# Example 2: Explore different color transformations with same height
#----------------

# transformations <- c("none", "log", "sqrt", "power07", "power03", "eq")
# current_height_transform <- "log"
# 
# for (transform in transformations) {
#   visualize_nsw(
#     height_transform = current_height_transform,
#     color_transform = transform,
#     zscale = 0.05,
#     save_preview = TRUE,
#     filename = paste0("nsw_color_test_", transform)
#   )
#   
#   # Close window after each render to avoid memory issues
#   rgl::close3d()
# }

#----------------
# Example 3: Visualizations with boundaries
#----------------

# # Example with state boundary (3D lines)
# visualize_nsw(
#   height_transform = "log",
#   color_transform = "sqrt",
#   zscale = 0.05,
#   save_preview = TRUE, 
#   filename = "nsw_with_boundary",
#   add_boundary = TRUE,
#   boundary_color = "#000000",
#   boundary_width = 4
# )
# rgl::close3d()
# 
# # Example with state boundary (overlay approach)
# visualize_nsw(
#   height_transform = "log",
#   color_transform = "sqrt",
#   zscale = 0.05,
#   save_preview = TRUE, 
#   filename = "nsw_with_boundary_overlay",
#   add_boundary = TRUE,
#   use_overlay = TRUE,
#   boundary_color = "#FFFFFF",
#   overlay_alpha = 0.7
# )
# rgl::close3d()
# 
# # Example with colored boundary
# visualize_nsw(
#   height_transform = "log",
#   color_transform = "power03",
#   zscale = 0.05,
#   color_palette = terrain_colors,
#   save_preview = TRUE, 
#   filename = "nsw_terrain_with_red_boundary",
#   add_boundary = TRUE,
#   boundary_color = "#FF0000",
#   boundary_width = 3
# )
# rgl::close3d()

#----------------
# Example 4: Adding cities/points of interest
#----------------

# # Create base map
# visualize_nsw(
#   height_transform = "log",
#   color_transform = "sqrt",
#   zscale = 0.05,
#   add_boundary = TRUE,
#   boundary_color = "#FFFFFF",
#   use_overlay = TRUE
# )
# 
# # Add major NSW cities
# # Note: Coordinates are approximate - adjust as needed
# add_point(1700, 1500, label = "Sydney", color = "#FF0000", size = 15)
# add_point(1550, 1000, label = "Wollongong", color = "#0000FF", size = 12)
# add_point(1850, 1850, label = "Newcastle", color = "#00FF00", size = 12)
# add_point(950, 1600, label = "Dubbo", color = "#FFA500", size = 10)
# add_point(500, 900, label = "Wagga Wagga", color = "#800080", size = 10)
# 
# # Save the result
# rayshader::render_snapshot("outputs/explorations/nsw_with_cities.png")
# rgl::close3d()

#----------------
# Example 5: High Quality Render
#----------------

# # Create visualization
# visualize_nsw(
#   height_transform = "log",
#   color_transform = "sqrt",
#   zscale = 0.05,
#   add_boundary = TRUE,
#   boundary_color = "#FFFFFF",
#   boundary_width = 3,
#   use_overlay = TRUE
# )
# 
# # Render high-quality version
# render_high_quality("nsw_population_final", 
#                    lightdirection = 225,
#                    lightaltitude = 60,
#                    lightintensity = 400)
# 
# # Close the window
# rgl::close3d()

# End of script