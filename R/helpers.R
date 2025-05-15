# filepath: /australia-spikemap/australia-spikemap/R/helpers.R

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

create_overlay_array <- function(mask_matrix, land_color = c(0, 0, 0, 0), ocean_color = c(173/255, 216/255, 230/255, 1)) {
    overlay_array <- array(0, dim = c(nrow(mask_matrix), ncol(mask_matrix), 4))
    overlay_array[,,1] <- ifelse(mask_matrix == 1, land_color[1], ocean_color[1]) # Red channel
    overlay_array[,,2] <- ifelse(mask_matrix == 1, land_color[2], ocean_color[2]) # Green channel
    overlay_array[,,3] <- ifelse(mask_matrix == 1, land_color[3], ocean_color[3]) # Blue channel
    overlay_array[,,4] <- ifelse(mask_matrix == 1, land_color[4], ocean_color[4]) # Alpha channel
    return(overlay_array)
}

transform_coordinates <- function(data, crs) {
    return(sf::st_transform(data, crs))
}