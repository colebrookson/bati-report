#' AUTHOR: Cole Brookson
#' DATE: 2024-02-21

# make standard map ============================================================

plot_study_area <- function(
    geo_data, farm_locs, sampling_locs,
    to_save = TRUE) {
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

    return(basic_map)
}

time_series_lice <- function(fish_data, sampling_locs) {

    fish_summarized <- fish_data_region %>%
        dplyr::select(year, month, date, all_leps, region) %>%
        dplyr::group_by(year, month, region) %>%
        dplyr::summarize(
            mean_leps = mean(all_leps, na.rm = TRUE)
        ) %>%
        dplyr::mutate(
            date_ym = lubridate::ym(
                paste(year, month, sep = "-")
            )
        )

    # plot a timeseries coloured by the region
    ggplot(data = fish_summarized) +
        geom_line(aes(x = date_ym, y = mean_leps, colour = region),
            linewidth = 1, linetype = "dashed", alpha = 0.8
        ) +
        geom_point(aes(x = date_ym, y = mean_leps, fill = region),
            shape = 21, size = 2
        ) +
        theme_base()

    # show a timeseries of each type of the lice stages over time 
    lice_by_stage <- fish_data_region %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(

    )

    ggplot(data = 

}