# --- Libraries ---
libs <- c("tidyverse", "sf", "stars", "rayshader", "rgl", "magick", "rayimage", "rnaturalearth", "rnaturalearthdata")
installed_libs <- libs %in% rownames(installed.packages())
if (any(!installed_libs)) install.packages(libs[!installed_libs])
invisible(lapply(libs, library, character.only = TRUE))

# --- Load Population Data ---
file_name <- "data/raw/kontur_population_AU_20231101.gpkg"
crsAUS <- "+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
pop_sf <- sf::st_read(file_name, quiet = TRUE) |> sf::st_transform(crs = crsAUS)

# --- Download Landmass Polygon for Overlay ---
aus_outline <- rnaturalearth::ne_countries(
  country = "Australia",
  scale = "medium",
  returnclass = "sf"
) |> st_transform(crs = crsAUS)

# --- Raster Size ---
bb <- st_bbox(pop_sf)
get_raster_size <- function() {
  height <- st_distance(st_point(c(bb$xmin, bb$ymin)), st_point(c(bb$xmin, bb$ymax)))
  width  <- st_distance(st_point(c(bb$xmin, bb$ymin)), st_point(c(bb$xmax, bb$ymin)))
  if (height > width) c(1, width / height) else c(height / width, 1)
}
ratios <- get_raster_size()
size <- 4000
width <- round(size * ratios[1])
height <- round(size * ratios[2])

# --- Rasterize Population Layer ---
pop_rast <- st_rasterize(pop_sf |> select(population, geom), nx = width, ny = height)
pop_rast[is.na(pop_rast)] <- 0
pop_mat <- pop_rast |> as("Raster") |> raster_to_matrix()

# --- Rasterize Overlay Shape ---
aus_rast <- st_rasterize(aus_outline |> select(geometry), template = pop_rast)
aus_rast[!is.na(aus_rast)] <- 1
aus_mask <- aus_rast |> as("Raster") |> raster_to_matrix()

# --- Generate Overlay Image (landmass shading) ---
overlay_img <- matrix("#000000", nrow = nrow(aus_mask), ncol = ncol(aus_mask))
overlay_img[aus_mask == 1] <- "#00000000"  # Transparent
overlay_img[aus_mask == 0] <- "#00000015"  # Very light grey

# --- Define Palette ---
cols <- c("#dac4dd", "#b68ab3", "#8a60a3", "#5e3d97", "#2e1065")
texture <- colorRampPalette(cols)(256)

# --- Create RGBA Overlay Array for Land/Ocean Distinction ---
# Define colors and alpha for the ocean part of the overlay
# Using a light blue, fully opaque
ocean_r <- 173/255  # R for light blue (hex #ADD8E6)
ocean_g <- 216/255  # G for light blue
ocean_b <- 230/255  # B for light blue
ocean_a <- 1.0      # Alpha for opaque ocean

# Define colors and alpha for the land part of the overlay
# Making it fully transparent so the height_shade texture and spikes are not affected
land_r <- 0.0
land_g <- 0.0
land_b <- 0.0
land_a <- 0.0       # Alpha for fully transparent land

# Initialize the overlay array
overlay_array <- array(0, dim = c(nrow(aus_mask), ncol(aus_mask), 4))

# Populate the array:
# - If aus_mask == 1 (land), use transparent overlay settings.
# - If aus_mask == 0 (ocean), use light blue opaque overlay settings.
overlay_array[,,1] <- ifelse(aus_mask == 1, land_r, ocean_r) # Red channel
overlay_array[,,2] <- ifelse(aus_mask == 1, land_g, ocean_g) # Green channel
overlay_array[,,3] <- ifelse(aus_mask == 1, land_b, ocean_b) # Blue channel
overlay_array[,,4] <- ifelse(aus_mask == 1, land_a, ocean_a) # Alpha channel

# --- Preview Render ---
rgl::close3d()

height_shade(pop_mat, texture = texture) %>%
  add_overlay(overlay_array) %>%
  plot_3d(
    heightmap = pop_mat,
    solid = FALSE,
    zscale = 15,
    shadowdepth = 0,
    shadow_darkness = 0.9,
    windowsize = c(1200, 1000),
    phi = 65,
    zoom = 0.65,
    theta = -30,
    background = "white"
  )

# --- High-Quality Final Render ---
if (!dir.exists("outputs/images")) dir.create("outputs/images", recursive = TRUE)

render_highquality(
  filename = "outputs/images/australia_population_spike_overlay.png",
  preview = TRUE,
  light = TRUE,
  lightdirection = 225,
  lightaltitude = 60,
  lightintensity = 400,
  interactive = FALSE,
  width = width,
  height = height
)


cat("✅ Done! Overlay render with landmass outline saved.\n")
