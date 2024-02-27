#' AUTHOR: Cole Brookson
#' DATE: 2024-02-21

# make standard map ============================================================

plot_study_area <- function(geo_data, site_data, to_save = TRUE) {
    # turn shapefile into something workable
    geo_data_sf_bc <- sf::st_as_sf(geo_data)

    bc_utm <- st_transform(geo_data_sf_bc,
        crs = "+proj=utm +zone=9 +datum=NAD83 +unit=m"
    )
    bc_cropped <- sf::st_crop(bc_utm,
        xmin = 644000,
        xmax = 726000,
        ymin = 5600000,
        ymax = 5660000
    )

    # put the farm locations into the proper utm format for the mapping
    farms_sf <- sf::st_as_sf(farm_locs, coords = c("long", "lat"))
    sf::st_crs(farms_sf) <- 4326 # set the coordinates for WGS84
    farms_utm <- sf::st_transform(farms_sf,
        crs = "+proj=utm +zone=9 +datum=NAD83 +unit=m"
    ) %>%
        dplyr::mutate(
            name = farm_name,
            area = as.character(NA),
            type = "farm"
        ) %>%
        dplyr::select(name, area, type, geometry)

    # do the same with the sampling locations
    sampling_sf <- sf::st_as_sf(sampling_locs,
        coords = c("lon_site_dd", "lat_site_dd")
    )
    sf::st_crs(sampling_sf) <- 4326 # set the coordinates for WGS84
    sampling_utm <- sf::st_transform(sampling_sf,
        crs = "+proj=utm +zone=9 +datum=NAD83 +unit=m"
    ) %>%
        dplyr::mutate(
            type = "sampling",
            name = site_name
        ) %>%
        dplyr::select(name, area, type, geometry)

    # put the points together and map them
    all_locs <- rbind(farms_utm, sampling_utm)
    basic_map <- ggplot() +
        geom_sf(data = bc_cropped) +
        geom_sf(data = all_locs, aes(fill = type, shape = type), size = 2) +
        coord_sf(datum = "+proj=utm +zone=9 +datum=NAD83 +unit=m") +
        scale_shape_manual("Location", values = c(21, 22)) +
        scale_fill_manual("Location", values = c("#EA738D", "#89ABE3")) +
        theme_base() +
        theme(
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            legend.position = c(0.85, 0.8)
        )
    if (to_save) {
        ggplot2::ggsave(
            here::here("./figs/maps/basic-map.png"),
            basic_map
        )
    }
}
