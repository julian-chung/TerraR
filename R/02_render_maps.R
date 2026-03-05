# =============================================================
# TerraR: Australian Population Spike Maps
# Script 02: Render State Spike Maps
# =============================================================
#
# Reads the cached state population data produced by 01_prepare_data.R
# and renders a 3D population spike map for each Australian state
# and territory using rayshader.
#
# PREREQUISITES:
#   Run 01_prepare_data.R first to generate data/processed/state_pops.rds
#
# OUTPUTS:
#   outputs/images/state-spike-map-{State-Name}.png  (one per state/territory)
# =============================================================

# --- Dependencies ---

libs <- c("tidyverse", "sf", "stars", "rayshader", "rgl")

installed_libs <- libs %in% rownames(installed.packages())
if (any(!installed_libs)) install.packages(libs[!installed_libs])
invisible(lapply(libs, library, character.only = TRUE))

dir.create("outputs/images", recursive = TRUE, showWarnings = FALSE)

# --- Load cached state populations ---

state_pops <- readRDS("data/processed/state_pops.rds")

# --- Colour palette ---
# Warm neutral ramp on a dark background: bone white for low density,
# stepping through yellow and orange to deep red at peak population.

palette_cols <- c("#f8f4e3", "#fceea7", "#f5c94c", "#e67e22", "#c0392b")
texture      <- grDevices::colorRampPalette(palette_cols)(1024)

# --- Per-state camera and rendering settings ---
# phi:    elevation angle (degrees above horizon)
# theta:  azimuth rotation (degrees; negative = clockwise)
# zoom:   magnification (lower = more zoomed out)
# zscale: vertical exaggeration applied after log transform
#         (lower value = taller spikes)

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

# =============================================================
# Helper functions
# =============================================================

# Compute raster pixel dimensions that preserve the geographic aspect ratio.
get_raster_dims <- function(bbox, base_size = 3000) {
  height_dist <- sf::st_distance(
    sf::st_point(c(bbox[["xmin"]], bbox[["ymin"]])),
    sf::st_point(c(bbox[["xmin"]], bbox[["ymax"]]))
  )
  width_dist <- sf::st_distance(
    sf::st_point(c(bbox[["xmin"]], bbox[["ymin"]])),
    sf::st_point(c(bbox[["xmax"]], bbox[["ymin"]]))
  )

  if (height_dist > width_dist) {
    nx <- round(base_size * as.numeric(width_dist / height_dist), 0)
    ny <- base_size
  } else {
    nx <- base_size
    ny <- round(base_size * as.numeric(height_dist / width_dist), 0)
  }

  list(nx = nx, ny = ny)
}

# Rasterize a state's population sf object and convert to a matrix.
make_state_matrix <- function(state_pop_sf) {
  dims <- get_raster_dims(sf::st_bbox(state_pop_sf))

  rast <- stars::st_rasterize(
    state_pop_sf |> dplyr::select(population, geom),
    nx = dims$nx,
    ny = dims$ny
  )

  mat <- rast |>
    as("Raster") |>
    rayshader::raster_to_matrix()

  list(mat = mat, nx = dims$nx, ny = dims$ny)
}

# Render and save a spike map for one state.
render_state <- function(state_name, state_pop_sf, settings, texture,
                         output_dir = "outputs/images", preview = FALSE) {
  message("Rendering: ", state_name)

  data <- make_state_matrix(state_pop_sf)

  # Log transform compresses extreme city spikes so smaller towns remain visible
  height_mat <- log10(data$mat + 1)

  height_mat |>
    rayshader::height_shade(texture = texture) |>
    rayshader::plot_3d(
      heightmap       = height_mat,
      solid           = TRUE,
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
    paste0("state-spike-map-", gsub(" ", "-", state_name), ".png")
  )

  rayshader::render_highquality(
    filename        = filename,
    preview         = preview,
    light           = TRUE,
    ambient_light   = FALSE,
    backgroundhigh  = "#1A1A1A",
    backgroundlow   = "#1A1A1A",
    ground_material = rayrender::diffuse(color = "#1A1A1A"),
    lightdirection  = 225,
    lightaltitude   = 60,
    lightintensity  = 400,
    interactive     = FALSE,
    width           = data$nx,
    height          = data$ny
  )

  rgl::close3d()
  message("Saved: ", filename)
  return(filename)
}

# =============================================================
# Batch render all states
# =============================================================

output_files <- lapply(names(state_settings), function(state_name) {
  render_state(
    state_name   = state_name,
    state_pop_sf = state_pops[[state_name]],
    settings     = state_settings[[state_name]],
    texture      = texture
  )
})

message("All done. Images saved to outputs/images/")
