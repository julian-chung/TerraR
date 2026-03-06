# =============================================================
# TerraR: Camera Tuning Script
# Script 03: Interactive camera tuning for spike maps
# =============================================================
#
# Opens an rgl preview window for a single state so you can
# dial in phi, theta, zoom, and zscale interactively.
#
# USAGE:
#   1. Set tune_state and tune_zscale below
#   2. Source this script — an rgl window will open
#   3. Call adjust_camera(phi=, theta=, zoom=) to try angles
#   4. Call print_settings() to copy values into state_settings
#      in 02_render_maps.R
# =============================================================

options(rgl.useNULL = FALSE)  # ensure rgl window opens

library(tidyverse)
library(sf)
library(stars)
library(rayshader)
library(rgl)

# --- Config: change these to tune a different state ---

tune_state     <- "Victoria"
tune_zscale    <- 10
base_elevation <- 50

# --- Load data ---

state_pops <- readRDS("data/processed/state_pops.rds")

crsAU <- "+proj=aea +lat_0=0 +lon_0=132 +lat_1=-18 +lat_2=-36 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

aus_states <- sf::st_read("data/boundaries/STE_2021_AUST_GDA2020.shp", quiet = TRUE) |>
  sf::st_transform(crs = crsAU)

# --- Colour palette (mirrors 02_render_maps.R) ---

palette_cols <- c("#f8f4e3", "#fceea7", "#f5c94c", "#e67e22", "#c0392b")
texture      <- grDevices::colorRampPalette(palette_cols)(1024)

# --- Helpers (mirrors 02_render_maps.R) ---

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

make_state_matrix <- function(state_pop_sf, state_boundary_sf, base_elevation = 50) {
  dims <- get_raster_dims(sf::st_bbox(state_pop_sf))

  pop_rast <- stars::st_rasterize(
    state_pop_sf |> dplyr::select(population, geom),
    nx = dims$nx, ny = dims$ny
  )

  boundary_rast <- stars::st_rasterize(state_boundary_sf, template = pop_rast)

  pop_mat      <- pop_rast      |> as("Raster") |> rayshader::raster_to_matrix()
  boundary_mat <- boundary_rast |> as("Raster") |> rayshader::raster_to_matrix()

  within    <- !is.na(boundary_mat)
  final_mat <- matrix(NA_real_, nrow = nrow(pop_mat), ncol = ncol(pop_mat))
  final_mat[within & !is.na(pop_mat)] <- base_elevation + pop_mat[within & !is.na(pop_mat)] + 0.1
  final_mat[within &  is.na(pop_mat)] <- base_elevation

  list(mat = final_mat, nx = dims$nx, ny = dims$ny)
}

# --- Build matrix and open preview ---
# Uses base_size = 1000 for fast interactive rendering (vs 3000 for final output)

message("Processing: ", tune_state)

state_pop_sf      <- state_pops[[tune_state]]
state_boundary_sf <- aus_states |> dplyr::filter(STE_NAME21 == tune_state)

data       <- make_state_matrix(state_pop_sf, state_boundary_sf, base_elevation)
height_mat <- log10(data$mat + 1)

# Initial camera values — edit these or adjust interactively below
.tune_phi   <- 65
.tune_theta <- 0
.tune_zoom  <- 0.70

height_mat |>
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
    phi             = .tune_phi,
    zoom            = .tune_zoom,
    theta           = .tune_theta,
    background      = "grey10"
  )

# --- Interactive helpers ---

# Call this repeatedly to adjust the view. All args are optional —
# omit any you don't want to change.
adjust_camera <- function(phi = .tune_phi, theta = .tune_theta, zoom = .tune_zoom) {
  .tune_phi   <<- phi
  .tune_theta <<- theta
  .tune_zoom  <<- zoom
  rayshader::render_camera(phi = phi, zoom = zoom, theta = theta)
  cat(sprintf("phi = %g, theta = %g, zoom = %g\n", phi, theta, zoom))
}

# Call this when happy with the view to get copy-pasteable settings.
print_settings <- function() {
  cat(sprintf(
    '  "%s" = list(\n    phi = %g, theta = %g, zoom = %g, zscale = %g\n  ),\n',
    tune_state, .tune_phi, .tune_theta, .tune_zoom, tune_zscale
  ))
}

message("Ready. rgl window is open.")
message("  adjust_camera(phi=, theta=, zoom=)  — update the view")
message("  print_settings()                    — copy values to 02_render_maps.R")
