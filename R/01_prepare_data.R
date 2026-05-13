# =============================================================
# TerraR: Australian Population Spike Maps
# Script 01: Data Preparation
# =============================================================
#
# Loads and clips data from two sources:
#
#   1. Kontur Population Dataset (kontur_population_AU_20231101.gpkg)
#      High-resolution 400m hexagonal population grid for Australia.
#      Download from: https://data.humdata.org/dataset/kontur-population-australia
#      Place at: data/raw/kontur_population_AU_20231101.gpkg
#
#   2. ABS State/Territory Boundaries (STE_2021_AUST_GDA2020.shp)
#      2021 Census boundaries from the Australian Bureau of Statistics.
#      Included in data/boundaries/
#      Download from: https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/access-and-downloads/digital-boundary-files
#
# OUTPUT:
#   data/processed/state_pops.rds
#
# NOTE: The st_intersection() clipping step takes several hours on a
# full-Australia dataset. This script checks for a cached result first
# and skips the operation if state_pops.rds already exists.
# Run this script once, then use 02_render_maps.R for all rendering.
# =============================================================

# --- Dependencies ---

library(tidyverse)
library(sf)
library(stars)

# --- Paths ---

kontur_path     <- "data/raw/kontur_population_AU_20231101.gpkg"
boundaries_path <- "data/boundaries/STE_2021_AUST_GDA2020.shp"
cache_path      <- "data/processed/state_pops.rds"

# GDA2020 / Australia Albers equal-area projection
crsAU <- "+proj=aea +lat_0=0 +lon_0=132 +lat_1=-18 +lat_2=-36 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

# --- Load source data ---

message("Loading Kontur population data...")
pop_sf <- sf::st_read(kontur_path) |>
  sf::st_transform(crs = crsAU)

message("Loading ABS state boundaries...")
state_boundaries <- sf::st_read(boundaries_path) |>
  sf::st_transform(crs = crsAU) |>
  dplyr::filter(STE_NAME21 %in% c(
    "New South Wales", "Victoria", "Queensland", "South Australia",
    "Western Australia", "Tasmania", "Northern Territory",
    "Australian Capital Territory"
  )) |>
  (\(x) split(x, x$STE_NAME21))()

# --- Clip population to state boundaries (expensive — cached) ---

if (file.exists(cache_path)) {
  message("Cached data found. Loading from ", cache_path)
  state_pops <- readRDS(cache_path)
} else {
  message("No cache found. Clipping population data to state boundaries...")
  message("WARNING: This may take several hours. The result will be saved to:")
  message("  ", cache_path)

  dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

  state_pops <- lapply(state_boundaries, function(state_geom) {
    geom_col     <- attr(state_geom, "sf_column")
    pop_subset   <- pop_sf |> dplyr::select(population, geom)
    state_subset <- state_geom |> dplyr::select(STE_NAME21, dplyr::all_of(geom_col))
    sf::st_intersection(pop_subset, state_subset)
  })

  saveRDS(state_pops, file = cache_path)
  message("Saved to ", cache_path)
}

message("Data ready. Run 02_render_maps.R to generate visualizations.")

