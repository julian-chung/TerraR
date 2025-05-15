#######################################################
#                 Creating Spike Map of Australia
#                 Adapted from Milos Popovic
########################################################

# Load necessary libraries
libs <- c("tidyverse", "sf", "stars", "rayshader", "rnaturalearth", "rnaturalearthdata", "rgl")
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == FALSE)) {
    install.packages(libs[!installed_libs])
}
invisible(lapply(libs, library, character.only = TRUE))

# Load population data
load_population_data <- function(file_path) {
    # Use the Australian Albers projection instead of WGS84
    # This gives a more natural view of Australia
    crsAUS <- "+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=134 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
    
    pop_sf <- sf::st_read(file_path) %>%
        sf::st_transform(crs = crsAUS)
    return(pop_sf)
}

# Create raster from population data - with better size handling
create_population_raster <- function(pop_sf, raster_size = 1500) { # Reduced from 3000
    bb <- sf::st_bbox(pop_sf)
    
    # Calculate width and height based on actual distances
    width <- sf::st_distance(
        sf::st_point(c(bb[["xmin"]], bb[["ymin"]])), 
        sf::st_point(c(bb[["xmax"]], bb[["ymin"]]))
    )
    height <- sf::st_distance(
        sf::st_point(c(bb[["xmin"]], bb[["ymin"]])),
        sf::st_point(c(bb[["xmin"]], bb[["ymax"]]))
    )
    
    # Calculate aspect ratio
    if (as.numeric(height) > as.numeric(width)) {
        height_ratio <- 1
        width_ratio <- as.numeric(width) / as.numeric(height)
    } else {
        width_ratio <- 1
        height_ratio <- as.numeric(height) / as.numeric(width)
    }
    
    # Calculate raster dimensions
    raster_height <- round(raster_size * height_ratio)
    raster_width <- round(raster_size * width_ratio)
    
    print(paste("Dimensions of matrix are:", raster_height, "x", raster_width))
    
    # Create template stars object
    template_stars <- stars::st_as_stars(bb, nx = raster_width, ny = raster_height, values = 0)
    
    # Rasterize population data
    pop_rast <- stars::st_rasterize(
        pop_sf |> dplyr::select(population, geom), 
        template = template_stars
    )
    pop_rast[[1]][is.na(pop_rast[[1]])] <- 0 # Set NAs to 0
    
    return(pop_rast)
}

# Create land mask
create_land_mask <- function(outline_sf, pop_rast_stars) {
    # Transform outline to match population CRS
    outline_sf <- sf::st_transform(outline_sf, st_crs(pop_rast_stars))
    
    # Rasterize outline using stars object as template
    mask_rast_stars <- stars::st_rasterize(
        outline_sf |> dplyr::select(geometry), 
        template = pop_rast_stars
    )
    
    # Convert to matrix
    mask_raster <- as(mask_rast_stars, "Raster")
    mask_mat <- matrix(0, nrow = nrow(mask_raster), ncol = ncol(mask_raster))
    mask_values <- raster::getValues(mask_raster)
    mask_mat[!is.na(mask_values)] <- 1
    return(mask_mat)
}

# Render spike map with safety checks
render_spike_map <- function(pop_mat, land_mask) {
    # Safety check - limit matrix size
    if (ncol(pop_mat) * nrow(pop_mat) > 10000000) {
        warning("Matrix size exceeds safe limit. Consider reducing raster_size parameter.")
        return(FALSE)
    }
    
    rgl::close3d() # Close any existing rgl windows
    
    # Use a nice color palette for Australia
    cols <- rev(c("#0b1354", "#1d2c6b", "#4f5eaa", "#e09ed5"))
    texture <- grDevices::colorRampPalette(cols)(256)

    # Create the overlay array
    overlay_array <- array(0, dim = c(nrow(pop_mat), ncol(pop_mat), 4))
    overlay_array[,,1] <- ifelse(land_mask == 1, 0, 173/255) # Red channel for ocean
    overlay_array[,,2] <- ifelse(land_mask == 1, 0, 216/255) # Green channel for ocean
    overlay_array[,,3] <- ifelse(land_mask == 1, 0, 230/255) # Blue channel for ocean
    overlay_array[,,4] <- ifelse(land_mask == 1, 0, 1)       # Alpha for ocean
    
    # Normalize population values to avoid extreme spikes
    pop_mat_norm <- pop_mat
    if (max(pop_mat, na.rm = TRUE) > 0) {
        # Cap extreme outliers if needed
        quantile_99 <- quantile(pop_mat[pop_mat > 0], 0.99, na.rm = TRUE)
        pop_mat_norm[pop_mat > quantile_99*3] <- quantile_99*3
    }

    # Create the visualization
    tryCatch({
        pop_mat_norm |>
            rayshader::height_shade(texture = texture) |>
            rayshader::add_overlay(overlay_array) |>
            rayshader::plot_3d(
                heightmap = pop_mat_norm,
                solid = FALSE,
                zscale = 15,
                windowsize = c(800, 600), # Reduced window size
                phi = 45,
                zoom = 0.7,
                theta = 30,
                background = "white"
            )
        return(TRUE)
    }, error = function(e) {
        message("Error in rendering: ", e$message)
        return(FALSE)
    })
}

# Main function to create spike map
create_spike_map <- function() {
    # Set a smaller raster size as default, with option to specify
    raster_size <- 1500 # Reduced from original 3000
    
    population_file <- "data/raw/kontur_population_AU_20231101.gpkg"
    outline_sf <- rnaturalearth::ne_countries(country = "Australia", scale = "large", returnclass = "sf")

    # Load and process data
    message("Loading population data...")
    pop_sf <- load_population_data(population_file)
    
    message("Creating raster...")
    pop_rast_stars <- create_population_raster(pop_sf, raster_size = raster_size)
    
    # Convert to matrix only when needed for rayshader
    pop_raster <- as(pop_rast_stars, "Raster")
    pop_mat <- rayshader::raster_to_matrix(pop_raster)

    message("Creating land mask...")
    land_mask <- create_land_mask(outline_sf, pop_rast_stars)

    message("Rendering spike map...")
    render_success <- render_spike_map(pop_mat, land_mask)
    
    if (render_success) {
        # Create directory if it doesn't exist
        if (!dir.exists("outputs/images")) {
            dir.create("outputs/images", recursive = TRUE)
        }

        output_filename <- "outputs/images/australia_population_spikemap.png"
        message("Rendering high-quality image...")
        
        tryCatch({
            rayshader::render_highquality(
                filename = output_filename,
                preview = TRUE,
                light = TRUE,
                lightdirection = 225,
                lightaltitude = 60,
                lightintensity = 400,
                interactive = FALSE,
                width = min(2000, ncol(pop_mat)), # Cap width for memory safety
                height = min(2000, nrow(pop_mat)) # Cap height for memory safety
            )
            message(paste("Spike map saved to:", output_filename))
        }, error = function(e) {
            message("Error in high-quality rendering: ", e$message)
            message("Try using rayshader::render_snapshot() instead for a basic screenshot.")
        })
    } else {
        message("Rendering was not successful. Try reducing the raster_size parameter.")
    }
}

# Run the spike map creation
create_spike_map()