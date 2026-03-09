# =============================================================
# TerraR: Simple Spike Map Pipeline (old approach)
# Script 04: Tune + Render
# =============================================================
#
# A simplified alternative to the 02 + 03 workflow, preserved
# for comparison. Key differences from the 02/03 approach:
#
#   - No base_elevation or floating island effect
#   - No spike_scale or smoothing parameters
#   - Log transform applied directly to the raw population matrix
#   - NA cells outside state boundary render as transparent
#
# TUNING WORKFLOW:
#   Run line by line in RStudio (Ctrl+Enter).
#   Sections 1-5 are setup — run once per session.
#   Section 6 opens the 3D scene — run once per state.
#   Section 7 is the tuning loop — edit and re-run until happy.
#   Section 8 prints settings to paste into Section 9.
#   Section 9 is the batch render — run when all states are dialled in.
#
# PREREQUISITES:
#   Run 01_prepare_data.R first to generate data/processed/state_pops.rds
#
# OUTPUTS:
#   outputs/images/simple-spike-map-{State-Name}.png
# =============================================================

# =============================================================
# 1. Dependencies
# =============================================================

library(tidyverse)
library(sf)
library(stars)
library(rayshader)
library(rgl)

dir.create("outputs", showWarnings = FALSE)

# =============================================================
# 2. Config — set your target state and zscale here
#    (changing these requires re-running sections 5 and 6)
# =============================================================

tune_state  <- "Victoria"
tune_zscale <- 10

# =============================================================
# 3. Load data
# =============================================================

state_pops <- readRDS("data/processed/state_pops.rds")

crsAU <- "+proj=aea +lat_0=0 +lon_0=132 +lat_1=-18 +lat_2=-36 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

aus_states <- sf::st_read("data/boundaries/STE_2021_AUST_GDA2020.shp", quiet = TRUE) |>
  sf::st_transform(crs = crsAU)

palette_cols <- c("#f8f4e3", "#fceea7", "#f5c94c", "#e67e22", "#c0392b")
texture      <- grDevices::colorRampPalette(palette_cols)(1024)

# =============================================================
# 4. Helper functions
# =============================================================

get_raster_dims <- function(bbox, base_size = 1000) {
  height_dist <- sf::st_distance(
    sf::st_point(c(bbox[["xmin"]], bbox[["ymin"]])),
    sf::st_point(c(bbox[["xmin"]], bbox[["ymax"]]))
  )
  width_dist <- sf::st_distance(
    sf::st_point(c(bbox[["xmin"]], bbox[["ymin"]])),
    sf::st_point(c(bbox[["xmax"]], bbox[["ymin"]]))
  )
  if (height_dist > width_dist) {
    list(nx = round(base_size * as.numeric(width_dist / height_dist), 0), ny = base_size)
  } else {
    list(nx = base_size, ny = round(base_size * as.numeric(height_dist / width_dist), 0))
  }
}

# Simple matrix: rasterize population, return raw values.
# No base_elevation — NA cells outside state render transparent.
make_state_matrix <- function(state_pop_sf) {
  dims <- get_raster_dims(sf::st_bbox(state_pop_sf))

  rast <- stars::st_rasterize(
    state_pop_sf |> dplyr::select(population, geom),
    nx = dims$nx,
    ny = dims$ny
  )

  mat <- rast |> as("Raster") |> rayshader::raster_to_matrix()

  list(mat = mat, nx = dims$nx, ny = dims$ny)
}

# =============================================================
# 5. Build matrix — run once per state (takes a moment)
# =============================================================

message("Processing: ", tune_state)

state_pop_sf <- state_pops[[tune_state]]

data       <- make_state_matrix(state_pop_sf)
color_mat  <- log10(data$mat + 1)  # log scale for colour gradient
height_mat <- data$mat              # raw population for spike height

# =============================================================
# 6. Open 3D scene — run once (re-run if you close the window)
# =============================================================

color_mat |>
  rayshader::height_shade(texture = texture) |>
  rayshader::plot_3d(
    heightmap       = height_mat,
    solid           = TRUE,
    solidcolor      = "#1A1A1A",
    soliddepth      = -150,
    zscale          = tune_zscale,
    shadowdepth     = 0,
    shadow_darkness = 0.95,
    windowsize      = c(800, 800),
    background      = "grey10"
  )

# =============================================================
# 7. Tune camera — edit values and re-run this whole block
#    Preview saves to outputs/tuning_preview_simple.png
# =============================================================
#
# Parameter cheat sheet:
#
#   phi    — vertical angle of the camera (degrees)
#              0  = side-on (horizon level)
#              90 = directly overhead (bird's eye)
#              ~45–70 is typical for spike maps
#
#   theta  — horizontal rotation of the scene (degrees)
#              0   = north facing up
#              +ve = rotate scene counter-clockwise
#              -ve = rotate scene clockwise
#
#   zoom   — magnification of the camera
#              higher = more zoomed in (larger scene)
#              lower  = more zoomed out (more context)
#              typical range: 0.5–0.9
#
#   zscale — vertical exaggeration of the height matrix
#   (set in section 2; changing it requires re-running section 6)
#              lower  = taller spikes
#              higher = flatter terrain
#
# =============================================================

tune_phi   <- 65
tune_theta <- 0
tune_zoom  <- 0.70

rayshader::render_camera(phi = tune_phi, zoom = tune_zoom, theta = tune_theta)
rayshader::render_snapshot("outputs/tuning_preview_simple.png")

# =============================================================
# 8. Print final settings — copy output into Section 9 below
# =============================================================

cat(sprintf(
  '  "%s" = list(\n    phi = %g, theta = %g, zoom = %g, zscale = %g\n  ),\n',
  tune_state, tune_phi, tune_theta, tune_zoom, tune_zscale
))

# =============================================================
# 9. Batch render — run when all states are dialled in
# =============================================================

state_settings <- list(
  "New South Wales" = list(
    phi = 50, theta = -0.5, zoom = 0.70, zscale = 15
  ),
  "Victoria" = list(
    phi = 65, theta = -0.5, zoom = 0.70, zscale = 10
  ),
  "Queensland" = list(
    phi = 55, theta = -20,  zoom = 0.68, zscale = 10
  ),
  "South Australia" = list(
    phi = 65, theta = -10,  zoom = 0.60, zscale = 10
  ),
  "Western Australia" = list(
    phi = 60, theta =  0,   zoom = 0.75, zscale = 10
  ),
  "Tasmania" = list(
    phi = 45, theta =  0,   zoom = 0.70, zscale = 10
  ),
  "Northern Territory" = list(
    phi = 55, theta =  0,   zoom = 0.75, zscale = 10
  ),
  "Australian Capital Territory" = list(
    phi = 60, theta =  0,   zoom = 0.70, zscale = 10
  )
)

render_state <- function(state_name, state_pop_sf, settings, texture,
                         output_dir = "outputs/images") {
  message("Rendering: ", state_name)

  # Use full resolution (3000px) for final renders
  dims <- get_raster_dims(sf::st_bbox(state_pop_sf), base_size = 3000)
  rast <- stars::st_rasterize(
    state_pop_sf |> dplyr::select(population, geom),
    nx = dims$nx, ny = dims$ny
  )
  mat        <- rast |> as("Raster") |> rayshader::raster_to_matrix()
  color_mat  <- log10(mat + 1)  # log scale for colour gradient
  height_mat <- mat             # raw population for spike height

  color_mat |>
    rayshader::height_shade(texture = texture) |>
    rayshader::plot_3d(
      heightmap       = height_mat,
      solid           = TRUE,
      solidcolor      = "#1A1A1A",
      soliddepth      = -150,
      zscale          = settings$zscale,
      shadowdepth     = 0,
      shadow_darkness = 0.95,
      windowsize      = c(800, 800),
      phi             = settings$phi,
      zoom            = settings$zoom,
      theta           = settings$theta,
      background      = "grey10"
    )

  rayshader::render_camera(
    phi   = settings$phi,
    zoom  = settings$zoom,
    theta = settings$theta
  )

  filename <- file.path(
    output_dir,
    paste0("simple-spike-map-", gsub(" ", "-", state_name), ".png")
  )

  rayshader::render_highquality(
    filename        = filename,
    preview         = FALSE,
    light           = TRUE,
    ambient_light   = FALSE,
    backgroundhigh  = "#1A1A1A",
    backgroundlow   = "#1A1A1A",
    ground_material = rayrender::diffuse(color = "#1A1A1A"),
    lightdirection  = 225,
    lightaltitude   = 60,
    lightintensity  = 400,
    interactive     = FALSE,
    width           = dims$nx,
    height          = dims$ny
  )

  rgl::close3d()
  message("Saved: ", filename)
  return(filename)
}

output_files <- lapply(names(state_settings), function(state_name) {
  render_state(
    state_name   = state_name,
    state_pop_sf = state_pops[[state_name]],
    settings     = state_settings[[state_name]],
    texture      = texture
  )
})

message("All done. Images saved to outputs/images/")
