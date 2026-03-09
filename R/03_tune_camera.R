# =============================================================
# TerraR: Camera Tuning Script
# Script 03: Interactive camera tuning for spike maps
# =============================================================
#
# Run line by line in RStudio (Cmd+Enter).
# Sections 1-5 are setup — run once per session.
# Section 6 opens the 3D scene — run once per state.
# Section 7 is the tuning loop — edit values and re-run
#   repeatedly until happy, then copy output to 02_render_maps.R.
#
# Preview images are saved to outputs/tuning_preview.png
# and can be opened in RStudio's Files pane.
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
# 2. Config — set your target state, zscale, and spike_scale here
#    (changing any of these requires re-running sections 5 and 6)
# =============================================================

tune_state       <- "Victoria"
tune_zscale      <- 10
tune_spike_scale <- 10
tune_smooth_r    <- 3    # smoothing window radius in raster cells (0 = off)

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

make_state_matrix <- function(state_pop_sf, state_boundary_sf, base_elevation = 50, spike_scale = 10, smooth_r = 0) {
  dims <- get_raster_dims(sf::st_bbox(state_pop_sf))

  pop_rast <- stars::st_rasterize(
    state_pop_sf |> dplyr::select(population, geom),
    nx = dims$nx, ny = dims$ny
  )

  boundary_rast <- stars::st_rasterize(state_boundary_sf, template = pop_rast)

  # Optional Gaussian smoothing to soften hexagonal grid artifacts
  pop_raster <- as(pop_rast, "Raster")
  if (smooth_r > 0) {
    pop_terra  <- terra::rast(pop_raster)
    pop_terra  <- terra::focal(pop_terra, w = smooth_r * 2 + 1, fun = "mean", na.rm = TRUE)
    pop_raster <- raster::raster(pop_terra)
  }

  pop_mat      <- pop_raster    |> rayshader::raster_to_matrix()
  boundary_mat <- boundary_rast |> as("Raster") |> rayshader::raster_to_matrix()

  within    <- !is.na(boundary_mat)
  final_mat <- matrix(NA_real_, nrow = nrow(pop_mat), ncol = ncol(pop_mat))
  color_mat <- matrix(NA_real_, nrow = nrow(pop_mat), ncol = ncol(pop_mat))
  log_pop   <- log10(pop_mat + 1) * spike_scale
  final_mat[within & !is.na(pop_mat)] <- base_elevation + log_pop[within & !is.na(pop_mat)]
  final_mat[within &  is.na(pop_mat)] <- base_elevation
  # colour matrix uses plain log10 so the texture gradient spans the full population range
  color_mat[within & !is.na(pop_mat)] <- log10(pop_mat[within & !is.na(pop_mat)] + 1)
  color_mat[within &  is.na(pop_mat)] <- 0

  list(mat = final_mat, color_mat = color_mat, nx = dims$nx, ny = dims$ny)
}

# =============================================================
# 5. Build matrix — run once per state (takes a moment)
# =============================================================

message("Processing: ", tune_state)

state_pop_sf      <- state_pops[[tune_state]]
state_boundary_sf <- aus_states |> dplyr::filter(STE_NAME21 == tune_state)

data       <- make_state_matrix(state_pop_sf, state_boundary_sf, spike_scale = tune_spike_scale, smooth_r = tune_smooth_r)
height_mat <- data$mat        # drives spike geometry
color_mat  <- data$color_mat  # drives colour gradient

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
#    Preview saves to outputs/tuning_preview.png
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
#   spike_scale — multiplier on log10(population) before adding to base
#   (set in section 2; changing it requires re-running sections 5 and 6)
#              higher = taller spikes relative to the base platform
#              lower  = shorter spikes
#              default 10 puts max spike height roughly equal to base_elevation
#
#   smooth_r  — smoothing window radius in raster cells
#   (set in section 2; changing it requires re-running sections 5 and 6)
#              0       = no smoothing (raw hexagonal grid visible)
#              3–5     = gentle smoothing, softens hex artifacts
#              7–10    = heavy smoothing, very organic shapes
#
# =============================================================

tune_phi   <- 65
tune_theta <- 0
tune_zoom  <- 0.70

rayshader::render_camera(phi = tune_phi, zoom = tune_zoom, theta = tune_theta)
rayshader::render_snapshot("outputs/tuning_preview.png")

# =============================================================
# 8. Print final settings — copy output into 02_render_maps.R
# =============================================================

cat(sprintf(
  '  "%s" = list(\n    phi = %g, theta = %g, zoom = %g, zscale = %g, spike_scale = %g, smooth_r = %g\n  ),\n',
  tune_state, tune_phi, tune_theta, tune_zoom, tune_zscale, tune_spike_scale, tune_smooth_r
))
