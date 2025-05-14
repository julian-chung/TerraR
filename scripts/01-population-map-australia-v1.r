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

print("Data loaded and transformed:")
# print(head(pop_sf)) # Keep this commented unless debugging

# --- Download Landmass Polygon for Australia ---
australia_outline_sf <- rnaturalearth::ne_countries(
  country = "Australia",
  scale = "large", # or "medium" for less detail
  returnclass = "sf"
) |> sf::st_transform(crs = crsAUS)

print("Australia outline loaded and transformed.")


### 2. DATA TO RASTER
### ----------------

bb <- sf::st_bbox(pop_sf) # Bbox from population data to define raster extent

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

    # Ensure ratios are numeric
    return(list(as.numeric(width_ratio), as.numeric(height_ratio)))
}

ratios <- get_raster_size(bb)
width_ratio <- ratios[[1]]
height_ratio <- ratios[[2]]

raster_size_param <- 3000 
raster_width <- round((raster_size_param * width_ratio), 0)
raster_height <- round((raster_size_param * height_ratio), 0)

print(paste("Raster dimensions: width =", raster_width, ", height =", raster_height))

# Create a template stars object based on population data extent and desired resolution
template_stars <- stars::st_as_stars(bb, nx = raster_width, ny = raster_height, values = 0) # Values don't matter here

# Rasterize population data using the template
pop_rast_stars <- stars::st_rasterize(
    pop_sf |> dplyr::select(population, geom), 
    template = template_stars
)
pop_rast_stars[[1]][is.na(pop_rast_stars[[1]])] <- 0 # Replace NA with 0 for ocean/no data
pop_mat <- rayshader::raster_to_matrix(as(pop_rast_stars, "Raster"))
print("Population data rasterized to matrix.")

# --- Create Australia Land Mask from outline ---
aus_mask_rast_stars <- stars::st_rasterize(
    australia_outline_sf |> dplyr::select(geometry), 
    template = template_stars # Use the same template for consistent grid
)
# Convert mask to matrix: 1 for land, 0 for ocean/outside
aus_mask_mat <- matrix(0, nrow = nrow(pop_mat), ncol = ncol(pop_mat))
temp_mask_raster <- as(aus_mask_rast_stars, "Raster")
aus_mask_values <- raster::getValues(temp_mask_raster)
aus_mask_mat[!is.na(aus_mask_values)] <- 1 # Land is 1
print("Australia land mask created.")

# --- Create RGBA Overlay Array for Land/Ocean Distinction ---
ocean_r <- 173/255; ocean_g <- 216/255; ocean_b <- 230/255; ocean_a <- 1.0 # Light blue opaque
land_r <- 0.0; land_g <- 0.0; land_b <- 0.0; land_a <- 0.0 # Fully transparent land

overlay_array <- array(0, dim = c(nrow(pop_mat), ncol(pop_mat), 4))
overlay_array[,,1] <- ifelse(aus_mask_mat == 1, land_r, ocean_r)
overlay_array[,,2] <- ifelse(aus_mask_mat == 1, land_g, ocean_g)
overlay_array[,,3] <- ifelse(aus_mask_mat == 1, land_b, ocean_b)
overlay_array[,,4] <- ifelse(aus_mask_mat == 1, land_a, ocean_a)
print("RGBA overlay array created.")

### 3. RENDER 3D SPIKE MAP
### -----------------------

cols <- rev(c(
    "#0b1354", "#283680",
    "#6853a9", "#c863b3"
)) 
texture <- grDevices::colorRampPalette(cols)(256)

rgl::close3d() 

pop_mat |>
    rayshader::height_shade(texture = texture) |>
    rayshader::add_overlay(overlay_array) |> # Add the overlay here
    rayshader::plot_3d(
        heightmap = pop_mat,
        solid = FALSE,
        soliddepth = 0, 
        zscale = 15,    
        shadowdepth = 0,
        shadow_darkness = 0.7, # Reduced shadow darkness
        windowsize = c(1000, 800), 
        phi = 80,                  
        zoom = .75,                 
        theta = -15,               
        background = "white" # Ocean color now primarily from overlay
    )

# Use this to adjust the view after building the window object if needed
# rayshader::render_camera(phi = 35, zoom = .65, theta = -60) # Example adjustment

if (!dir.exists("outputs/images")) {
    dir.create("outputs/images", recursive = TRUE)
}

output_filename <- "outputs/images/australia_population_spikemap_overlay_v5.png" # Updated filename
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

print(paste("✅ Done! Australia spike map saved to", output_filename))