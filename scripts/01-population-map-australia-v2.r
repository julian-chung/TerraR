#######################################################
#                 Making crisp spike maps with R
#                 Adapted for Australia
#                 Original by Milos Popovic
#                 2023/03/12
########################################################

# libraries we need
libs <- c(
    "tidyverse", "sf", "stars",
    "rayshader", "grDevices", "rnaturalearth", "rnaturalearthdata", "rgl" # Added rnaturalearth, rnaturalearthdata, rgl
)

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == FALSE)) {
    install.packages(libs[!installed_libs])
}

# load libraries
invisible(lapply(libs, library, character.only = TRUE))

### 1. LOAD DATA
### -------------
australia_pop_file <- "data/raw/kontur_population_AU_20231101.gpkg"

# Australian Albers projection
crsAUS <- "+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

pop_sf <- sf::st_read(australia_pop_file) |>
    sf::st_transform(crs = crsAUS)

print("Population data loaded and transformed.")

# --- Download Landmass Polygon for Australia ---
aus_outline_sf <- rnaturalearth::ne_countries(
  country = "Australia",
  scale = "large", # or "medium" for less detail
  returnclass = "sf"
) |> sf::st_transform(crs = crsAUS)

print("Australia outline loaded and transformed.")

# Optional: Quick plot to check data
# ggplot() +
#     geom_sf(data = pop_sf, aes(fill = population), color = NA) +
#     geom_sf(data = aus_outline_sf, fill = NA, color = "red") +
#     theme_minimal()

### 2. DATA TO RASTER
### ----------------

bb <- sf::st_bbox(pop_sf)

get_raster_size <- function(bbox_obj) {
    height <- sf::st_distance(
        sf::st_point(c(bbox_obj[["xmin"]], bbox_obj[["ymin"]])),
        sf::st_point(c(bbox_obj[["xmin"]], bbox_obj[["ymax"]]))
    )
    width <- sf::st_distance(
        sf::st_point(c(bbox_obj[["xmin"]], bbox_obj[["ymin"]])),
        sf::st_point(c(bbox_obj[["xmax"]], bbox_obj[["ymin"]]))
    )

    if (height > width) {
        height_ratio <- 1
        width_ratio <- width / height
    } else {
        width_ratio <- 1
        height_ratio <- height / width
    }
    return(list(as.numeric(width_ratio), as.numeric(height_ratio)))
}

ratios <- get_raster_size(bb)
width_ratio <- ratios[[1]]
height_ratio <- ratios[[2]]

raster_size_param <- 3000
raster_width <- round((raster_size_param * width_ratio), 0)
raster_height <- round((raster_size_param * height_ratio), 0)

print(paste("Raster dimensions: width =", raster_width, ", height =", raster_height))

# Rasterize population data
pop_rast_stars <- stars::st_rasterize(
    pop_sf |> dplyr::select(population, geom),
    nx = raster_width, ny = raster_height
)
pop_rast_stars[[1]][is.na(pop_rast_stars[[1]])] <- 0 # Ensure NAs (e.g. ocean) are 0
pop_mat <- rayshader::raster_to_matrix(as(pop_rast_stars, "Raster")) # Convert stars to Raster then to matrix

print("Population data rasterized to matrix.")

# --- Create Australia Land Mask ---
# Use the population raster as a template for the outline raster
aus_mask_rast_stars <- stars::st_rasterize(
    aus_outline_sf |> dplyr::select(geometry), # Only need geometry for mask
    template = pop_rast_stars # Use the stars object itself as the template
)
# Convert mask to matrix: 1 for land, 0 for ocean
aus_mask_mat <- matrix(0, nrow = nrow(pop_mat), ncol = ncol(pop_mat)) # Initialize with 0s
# Convert stars raster to a simple matrix for masking
temp_mask_raster <- as(aus_mask_rast_stars, "Raster")
aus_mask_values <- raster::getValues(temp_mask_raster)
# Where the mask raster is not NA (i.e., it's land), set our matrix to 1
aus_mask_mat[!is.na(aus_mask_values)] <- 1

print("Australia land mask created.")

# --- REMOVE/COMMENT OUT: Modify Population Matrix to include Base Land Elevation ---
# Define a small base elevation for land areas with no population
# This value will be amplified by zscale.
# If the population values are small integers, 1 might be too high.
# If min(pop_mat[pop_mat > 0]) is e.g. 5, then 0.5 or 1 is okay.
# Testing a small value first.
# base_land_elevation <- 0.1
# if (min(pop_mat[pop_mat > 0], na.rm = TRUE) > 10) { # Heuristic
#     base_land_elevation <- 1
# }


# Create the final elevation matrix
# Start with population data (which is 0 for ocean and unpopulated land)
# final_elevation_mat <- pop_mat
# Where it's land (from mask) AND population is currently 0, set to base_land_elevation
# land_no_pop_indices <- which(aus_mask_mat == 1 & pop_mat == 0)
# final_elevation_mat[land_no_pop_indices] <- base_land_elevation

# print(paste("Base land elevation of", base_land_elevation, "applied to unpopulated land areas."))
# print(paste("Min/Max of final_elevation_mat:", min(final_elevation_mat), max(final_elevation_mat)))

# --- Create RGBA Overlay Array for Land/Ocean Distinction ---
# aus_mask_mat is 1 for land, 0 for ocean.

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
# Dimensions should match pop_mat (and aus_mask_mat)
overlay_array <- array(0, dim = c(nrow(pop_mat), ncol(pop_mat), 4))
overlay_array <- array(as.numeric(overlay_array), dim = dim(overlay_array))

# Populate the array:
# - If aus_mask_mat == 1 (land), use transparent overlay settings.
# - If aus_mask_mat == 0 (ocean), use light blue opaque overlay settings.
overlay_array[,,1] <- ifelse(aus_mask_mat == 1, land_r, ocean_r) # Red channel
overlay_array[,,2] <- ifelse(aus_mask_mat == 1, land_g, ocean_g) # Green channel
overlay_array[,,3] <- ifelse(aus_mask_mat == 1, land_b, ocean_b) # Blue channel
overlay_array[,,4] <- ifelse(aus_mask_mat == 1, land_a, ocean_a) # Alpha channel

print("RGBA overlay array created for land/ocean distinction.")


### 3. RENDER 3D SPIKE MAP
### -----------------------

cols <- rev(c(
  "#0b1354",  # deep ocean
  "#1d2c6b",  # dark indigo
  "#324c89",  # mid purple-blue
  "#4f5eaa",  # softened blue
  "#8365b1",  # warm purple
  "#a865b7",  # magenta tint
  "#c863b3",  # bright pink
  "#e09ed5"   # desaturated pastel
))
texture <- grDevices::colorRampPalette(cols)(256)


stopifnot(all(dim(pop_mat) == dim(overlay_array)[1:2]))


rgl::close3d()

# Use the original pop_mat for height_shade and plot_3d, and add the overlay
pop_mat |>
    rayshader::height_shade(texture = texture) |>
    rayshader::add_overlay(overlay_array) |>
    rayshader::plot_3d(
        heightmap = pop_mat, # Use the original population matrix
        solid = FALSE,
        soliddepth = 0, # Set to a very small negative if you see gaps under spikes, e.g. -1
        zscale = 25, 
        shadowdepth = 0, # Shadow for spikes, not for the overlay
        shadow_darkness = .95,
        windowsize = c(1000, 800),
        phi = 75,
        zoom = .6,
        theta = 0,
        fov = 0,
        background = "white" # Ocean color is now handled by the overlay
    )

# Adjust the camera if needed

rayshader::render_camera(phi = 45, zoom = .6, theta = 0, fov = 0) # Example adjustment

if (!dir.exists("outputs/images")) {
    dir.create("outputs/images", recursive = TRUE)
}
output_filename <- "outputs/images/australia_population_spikemap_overlay_v4.png" # New filename
print(paste("Rendering high-quality image to:", output_filename))

rayshader::render_highquality(
    filename = output_filename,
    preview = TRUE,
    light = TRUE,
    lightdirection = 225,
    lightaltitude = 60,
    lightintensity = 400,
    interactive = FALSE,
    width = raster_width,
    height = raster_height
)

print(paste("✅ Done! Australia spike map with overlay saved to", output_filename))