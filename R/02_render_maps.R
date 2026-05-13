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

# Suppress the rgl popup window during batch rendering.
# Must be set before library(rgl) is called.
options(rgl.useNULL = TRUE)

library(tidyverse)
library(sf)
library(stars)
library(rayshader)
library(rgl)

dir.create("outputs/images", recursive = TRUE, showWarnings = FALSE)

# --- Load cached state populations ---

state_pops <- readRDS("data/processed/state_pops.rds")

# --- Colour palette ---
# Warm neutral ramp on a dark background: bone white for low density,
# stepping through yellow and orange to deep red at peak population.

palette_cols <- c("#f8f4e3", "#fceea7", "#f5c94c", "#e67e22", "#c0392b")
texture      <- grDevices::colorRampPalette(palette_cols)(1024)

# --- Render quality ---
# DRAFT: fast iteration (minutes per state) — use when checking camera angles.
# FINAL: high-quality output (hours per state) — use for finished renders.
#
#                       DRAFT          FINAL
SAMPLES            <- 2048        #    128            2048
SAMPLE_METHOD      <- "sobol"     #    "sobol_blue"   "sobol"
AMBIENT_LIGHT      <- TRUE        #    FALSE          TRUE
AMBIENT_BACKGROUND <- "#2A2A2A"   #    "#1A1A1A"      "#2A2A2A"

# --- Per-state camera and rendering settings ---
# phi:         elevation angle (degrees above horizon)
# theta:       azimuth rotation (degrees; negative = clockwise)
# zoom:        magnification (lower = more zoomed out)
# zscale:      vertical exaggeration of the height matrix
#              (lower value = taller spikes; set to 1 for 1:1 pixel-to-unit ratio)
# spike_scale: linear multiplier applied to sqrt(population) before building the
#              height matrix — controls overall spike prominence without changing
#              the relative distribution between cities and rural areas

state_settings <- list(
  "New South Wales" = list(
    phi = 55, theta = -4, zoom = 0.70, zscale = 1, spike_scale = 8
  ),
  "Victoria" = list(
    phi = 55, theta = 0, zoom = 0.75, zscale = 1, spike_scale = 8
  ),
  "Queensland" = list(
    phi = 55, theta = -2,  zoom = 0.69, zscale = 1, spike_scale = 8
  ),
  "South Australia" = list(
    phi = 55, theta = 0,  zoom = 0.65, zscale = 1, spike_scale = 8
  ),
  "Western Australia" = list(
    phi = 60, theta =  0,   zoom = 0.75, zscale = 1, spike_scale = 8
  ),
  "Tasmania" = list(
    phi = 45, theta =  0,   zoom = 0.70, zscale = 1, spike_scale = 8
  ),
  "Northern Territory" = list(
    phi = 55, theta =  0,   zoom = 0.75, zscale = 1, spike_scale = 8
  ),
  "Australian Capital Territory" = list(
    phi = 60, theta =  0,   zoom = 0.70, zscale = 1, spike_scale = 8
  )
)

# --- Load state boundaries ---
# Used to create an elevated landmass cutout: cells inside the state boundary
# are raised by base_elevation so the state "floats" above the dark background.

crsAU <- "+proj=aea +lat_0=0 +lon_0=132 +lat_1=-18 +lat_2=-36 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

aus_states <- sf::st_read("data/boundaries/STE_2021_AUST_GDA2020.shp", quiet = TRUE) |>
  sf::st_transform(crs = crsAU) |>
  dplyr::filter(STE_NAME21 %in% names(state_settings))

# =============================================================
# Helper functions
# =============================================================

# Compute raster pixel dimensions that preserve the geographic aspect ratio.
# base_size controls the longer edge in pixels; increase for higher-resolution output.
get_raster_dims <- function(bbox, base_size = 6000) {
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
# Applies an elevated landmass cutout: cells inside the state boundary are
# raised by base_elevation, cells outside remain NA (transparent in render).
make_state_matrix <- function(state_pop_sf, state_boundary_sf, base_elevation = 150, spike_scale = 8, base_size = 5000) {
  # Use only the largest polygon (mainland) to avoid offshore islands such as
  # Lord Howe Island (NSW) skewing the bounding box and appearing as ghost spikes.
  # st_cast on an sf object returns an sf object (one row per polygon).
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
  # sqrt gives far more dynamic range than log10:
  # Sydney (50k) is 224x taller than a 1-person hex vs only 15x with log10.
  spike_heights <- sqrt(pop_mat) * spike_scale
  final_mat[within & !is.na(pop_mat)] <- base_elevation + spike_heights[within & !is.na(pop_mat)]
  final_mat[within &  is.na(pop_mat)] <- base_elevation
  # colour matrix uses log10 so the palette gradient spans the full population range
  color_mat[within & !is.na(pop_mat)] <- log10(pop_mat[within & !is.na(pop_mat)] + 1)
  color_mat[within &  is.na(pop_mat)] <- 0

  list(mat = final_mat, color_mat = color_mat, nx = dims$nx, ny = dims$ny)
}

# Render and save a spike map for one state.
render_state <- function(state_name, state_pop_sf, state_boundary_sf, settings, texture,
                         output_dir = "outputs/images", preview = FALSE, base_size = 5000,
                         samples = SAMPLES, sample_method = SAMPLE_METHOD,
                         ambient_light = AMBIENT_LIGHT, ambient_background = AMBIENT_BACKGROUND) {
  message("Rendering: ", state_name, " [", samples, " samples]")

  data <- make_state_matrix(state_pop_sf, state_boundary_sf, spike_scale = settings$spike_scale, base_size = base_size)

  height_mat <- data$mat        # base_elevation + log(pop)*spike_scale — drives spike geometry
  color_mat  <- data$color_mat  # plain log10(pop) — spreads colour gradient across full population range

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
    paste0("state-spike-map-", gsub(" ", "-", state_name), ".png")
  )

  rayshader::render_highquality(
    filename        = filename,
    preview         = preview,
    light           = TRUE,
    ambient_light   = ambient_light,
    backgroundhigh  = ambient_background,
    backgroundlow   = ambient_background,
    ground_material = rayrender::diffuse(color = "#1A1A1A"),
    lightdirection  = 315,
    lightaltitude   = 60,
    lightintensity  = 400,
    samples         = samples,
    sample_method   = sample_method,
    interactive     = FALSE,
    width           = data$nx,
    height          = data$ny
  )

  rgl::close3d()
  gc()  # release rayrender scene + matrix memory; prevents OOM crash on large batch runs
  message("Saved: ", filename)
  return(filename)
}

# =============================================================
# Render each state
# Comment out individual blocks to skip / re-run a single state.
# =============================================================

# --- New South Wales ---
render_state(
  state_name        = "New South Wales",
  state_pop_sf      = state_pops[["New South Wales"]],
  state_boundary_sf = aus_states |> dplyr::filter(STE_NAME21 == "New South Wales"),
  settings          = state_settings[["New South Wales"]],
  texture           = texture
)

# --- Victoria ---
render_state(
  state_name        = "Victoria",
  state_pop_sf      = state_pops[["Victoria"]],
  state_boundary_sf = aus_states |> dplyr::filter(STE_NAME21 == "Victoria"),
  settings          = state_settings[["Victoria"]],
  texture           = texture
)

# --- Queensland ---
render_state(
  state_name        = "Queensland",
  state_pop_sf      = state_pops[["Queensland"]],
  state_boundary_sf = aus_states |> dplyr::filter(STE_NAME21 == "Queensland"),
  settings          = state_settings[["Queensland"]],
  texture           = texture
)

# --- South Australia ---
render_state(
  state_name        = "South Australia",
  state_pop_sf      = state_pops[["South Australia"]],
  state_boundary_sf = aus_states |> dplyr::filter(STE_NAME21 == "South Australia"),
  settings          = state_settings[["South Australia"]],
  texture           = texture
)

# --- Western Australia ---
render_state(
  state_name        = "Western Australia",
  state_pop_sf      = state_pops[["Western Australia"]],
  state_boundary_sf = aus_states |> dplyr::filter(STE_NAME21 == "Western Australia"),
  settings          = state_settings[["Western Australia"]],
  texture           = texture
)

# --- Tasmania ---
render_state(
  state_name        = "Tasmania",
  state_pop_sf      = state_pops[["Tasmania"]],
  state_boundary_sf = aus_states |> dplyr::filter(STE_NAME21 == "Tasmania"),
  settings          = state_settings[["Tasmania"]],
  texture           = texture
)

# --- Northern Territory ---
render_state(
  state_name        = "Northern Territory",
  state_pop_sf      = state_pops[["Northern Territory"]],
  state_boundary_sf = aus_states |> dplyr::filter(STE_NAME21 == "Northern Territory"),
  settings          = state_settings[["Northern Territory"]],
  texture           = texture
)

# --- Australian Capital Territory ---
render_state(
  state_name        = "Australian Capital Territory",
  state_pop_sf      = state_pops[["Australian Capital Territory"]],
  state_boundary_sf = aus_states |> dplyr::filter(STE_NAME21 == "Australian Capital Territory"),
  settings          = state_settings[["Australian Capital Territory"]],
  texture           = texture
)

message("All done. Images saved to outputs/images/")
