# =============================================================
# TerraR: Camera Tuning Script
# Script 03: Interactive camera tuning for spike maps
# =============================================================
#
# Run line by line in RStudio.
# Sections 1-5 are setup — run once per session.
# Section 6 opens the 3D scene — run once per state.
# Section 7 is the tuning loop — edit values and re-run
#   repeatedly until happy, then copy output to 02_render_maps.R.
#
# Preview images are saved to outputs/tuning_preview.png
#
# NOTE: The preview uses render_highquality at 1/5 resolution
# (~1000px) so lighting, shadows, and spike heights match the
# final render exactly. Each preview takes ~30 seconds.
# =============================================================

# =============================================================
# 1. Dependencies
# NOTE: rgl.useNULL is intentionally NOT set here so the
#       interactive 3D window opens for visual reference.
# =============================================================

library(tidyverse)
library(sf)
library(stars)
library(rayshader)
library(rgl)

dir.create("outputs", showWarnings = FALSE)

# =============================================================
# 2. Config — set your target state here
#    (changing this requires re-running sections 5 and 6)
# =============================================================

tune_state <- "Tasmania"

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
# 4. Helper functions (mirrors 02_render_maps.R exactly)
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

make_state_matrix <- function(state_pop_sf, state_boundary_sf,
                               base_elevation = 150, spike_scale = 8, base_size = 1000) {
  polys       <- sf::st_cast(state_boundary_sf, "POLYGON")
  mainland_sf <- polys[which.max(sf::st_area(polys)), ]

  dims <- get_raster_dims(sf::st_bbox(mainland_sf), base_size = base_size)

  pop_rast <- stars::st_rasterize(
    state_pop_sf |> dplyr::select(population, geom),
    template = stars::st_as_stars(sf::st_bbox(mainland_sf), nx = dims$nx, ny = dims$ny, values = NA_real_)
  )

  boundary_rast <- stars::st_rasterize(mainland_sf, template = pop_rast)

  pop_mat      <- pop_rast      |> as("Raster") |> rayshader::raster_to_matrix()
  boundary_mat <- boundary_rast |> as("Raster") |> rayshader::raster_to_matrix()

  within    <- !is.na(boundary_mat)
  final_mat <- matrix(NA_real_, nrow = nrow(pop_mat), ncol = ncol(pop_mat))
  color_mat <- matrix(NA_real_, nrow = nrow(pop_mat), ncol = ncol(pop_mat))

  spike_heights <- sqrt(pop_mat) * spike_scale
  final_mat[within & !is.na(pop_mat)] <- base_elevation + spike_heights[within & !is.na(pop_mat)]
  final_mat[within &  is.na(pop_mat)] <- base_elevation
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

# Build at low resolution for fast tuning previews
data       <- make_state_matrix(state_pop_sf, state_boundary_sf, base_size = 1000)
height_mat <- data$mat
color_mat  <- data$color_mat

# =============================================================
# 6. Open 3D scene — run once (re-run if you close the window)
#    Use this window as a rough visual guide only.
# =============================================================

color_mat |>
  rayshader::height_shade(texture = texture) |>
  rayshader::plot_3d(
    heightmap       = height_mat,
    solid           = TRUE,
    solidcolor      = "#1A1A1A",
    soliddepth      = -150,
    zscale          = 1,
    shadowdepth     = 0,
    shadow_darkness = 0.95,
    windowsize      = c(data$nx, data$ny),
    background      = "grey10"
  )

# =============================================================
# 7. Tune camera — edit values and re-run this whole block
#
#   Parameter cheat sheet:
#
#   phi    — vertical angle of the camera (degrees)
#              0  = side-on (horizon level)
#              90 = directly overhead (bird's eye)
#              ~45-70 is typical for spike maps
#
#   theta  — horizontal rotation of the scene (degrees)
#              0   = north facing up
#              +ve = rotate counter-clockwise
#              -ve = rotate clockwise
#
#   zoom   — magnification (lower = more zoomed out)
#              typical range: 0.5-0.9
#
# The preview uses render_highquality at ~1/5 final resolution
# so lighting and shadows match the actual output exactly.
# Each preview takes ~30 seconds.
# =============================================================

tune_phi   <- 45
tune_theta <- 0
tune_zoom  <- 0.70

rayshader::render_camera(phi = tune_phi, zoom = tune_zoom, theta = tune_theta)

rayshader::render_highquality(
  filename        = "outputs/tuning_preview.png",
  preview         = FALSE,
  light           = TRUE,
  ambient_light   = FALSE,
  backgroundhigh  = "#1A1A1A",
  backgroundlow   = "#1A1A1A",
  ground_material = rayrender::diffuse(color = "#1A1A1A"),
  lightdirection  = 315,
  lightaltitude   = 60,
  lightintensity  = 400,
  interactive     = FALSE,
  width           = data$nx,
  height          = data$ny
)

message("Preview saved to outputs/tuning_preview.png — open in RStudio Files pane to review.")

# =============================================================
# 8. Print final settings — copy output into 02_render_maps.R
# =============================================================

cat(sprintf(
  '  "%s" = list(\n    phi = %g, theta = %g, zoom = %g, zscale = 1, spike_scale = 8\n  ),\n',
  tune_state, tune_phi, tune_theta, tune_zoom
))

# =============================================================
# Tuned Settings (archive)
# =============================================================
# Victoria
# phi = 65, theta = -0.5, zoom = 0.70
#
# New South Wales
# phi = 50, theta = -0.5, zoom = 0.70
#
# Queensland
# phi = 55, theta = -20, zoom = 0.68
#
# Tasmania
# phi = 45, theta = 0, zoom = 0.70
