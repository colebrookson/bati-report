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
            type = "farm",
            names_code = farm_name
        ) %>%
        dplyr::select(name, names_code, area, type, geometry)

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
            name = site_name,
            names_code = site_code
        ) %>%
        dplyr::select(name, names_code, area, type, geometry)

    # put the points together and map them
    all_locs <- rbind(farms_utm, sampling_utm) %>%
        dplyr::mutate(
            long = sf::st_coordinates(.)[, 1],
            lat = sf::st_coordinates(.)[, 2]
        ) %>%
        dplyr::mutate(
            # use this so in the plot I can make the labels different fonts
            ff = ifelse(type == "farm", "bold", "plain")
        )

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
    named_map <- ggplot() +
        geom_sf(data = bc_cropped) +
        geom_sf(data = all_locs, aes(fill = type, shape = type), size = 2) +
        coord_sf(datum = "+proj=utm +zone=9 +datum=NAD83 +unit=m") +
        scale_shape_manual("Location", values = c(21, 22)) +
        scale_fill_manual("Location", values = c("#EA738D", "#89ABE3")) +
        theme_base() +
        ggrepel::geom_text_repel(
            data = all_locs,
            aes(
                x = long, y = lat,
                label = names_code, fontface = ff
            ),
            size = 3,
            max.overlaps = 20
        ) +
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
        ggplot2::ggsave(
            here::here("./figs/maps/named-map.png"),
            named_map
        )
    }

    return(basic_map)
}

time_series_lice <- function(fish_data, sampling_locs) {
    # plot a timeseries coloured by the region
    regions_all_leps <- ggplot(data = fish_data %>%
        dplyr::select(year, month, date, all_leps, region) %>%
        dplyr::group_by(year, month, region) %>%
        dplyr::summarize(
            mean_leps = mean(all_leps, na.rm = TRUE)
        ) %>%
        dplyr::mutate(
            date_ym = lubridate::ym(
                paste(year, month, sep = "-")
            )
        )) +
        geom_line(aes(x = date_ym, y = mean_leps, colour = region),
            linewidth = 1, linetype = "dashed", alpha = 0.8
        ) +
        geom_point(aes(x = date_ym, y = mean_leps, fill = region),
            shape = 21, size = 2
        ) +
        theme_base() +
        labs(x = "Year", y = "Mean lice per year/month", title = "All Leps")
    ggplot2::ggsave(
        here::here("./figs/all-leps-on-wild-fish-by-region-month-&-year.png"),
        regions_all_leps
    )

    # plot by region but use just yearly mean not monthly as well
    regions_all_leps_yr <- ggplot(data = fish_data %>%
        dplyr::select(year, month, date, all_leps, region) %>%
        dplyr::group_by(year, region) %>%
        dplyr::summarize(
            mean_leps = mean(all_leps, na.rm = TRUE)
        )) +
        geom_line(aes(x = year, y = mean_leps, colour = region),
            linewidth = 1, linetype = "dashed", alpha = 0.8
        ) +
        geom_point(aes(x = year, y = mean_leps, fill = region),
            shape = 21, size = 2
        ) +
        theme_base() +
        labs(x = "Year", y = "Mean lice per year", title = "All Leps")
    ggplot2::ggsave(
        here::here("./figs/all-leps-on-wild-fish-by-region-year.png"),
        regions_all_leps_yr
    )

    # caligus through time
    all_cals_region <- ggplot(data = fish_data %>%
        dplyr::select(year, month, date, all_cals, region) %>%
        dplyr::group_by(year, month, region) %>%
        dplyr::summarize(
            mean_cals = mean(all_cals, na.rm = TRUE)
        ) %>%
        dplyr::mutate(
            date_ym = lubridate::ym(
                paste(year, month, sep = "-")
            )
        )) +
        geom_line(aes(x = date_ym, y = mean_cals, colour = region),
            linewidth = 1, linetype = "dashed", alpha = 0.8
        ) +
        geom_point(aes(x = date_ym, y = mean_cals, fill = region),
            shape = 21, size = 2
        ) +
        theme_base() +
        labs(x = "Year", y = "Mean lice per year/month", title = "All Cals")
    ggplot2::ggsave(
        here::here("./figs/all-cals-on-wild-fish-by-region-month-&-year.png"),
        all_cals_region
    )

    # plot by region but use just yearly mean not monthly as well
    all_cals_region_yr <- ggplot(data = fish_data %>%
        dplyr::select(year, month, date, all_cals, region) %>%
        dplyr::group_by(year, region) %>%
        dplyr::summarize(
            mean_cals = mean(all_cals, na.rm = TRUE)
        )) +
        geom_line(aes(x = year, y = mean_cals, colour = region),
            linewidth = 1, linetype = "dashed", alpha = 0.8
        ) +
        geom_point(aes(x = year, y = mean_cals, fill = region),
            shape = 21, size = 2
        ) +
        theme_base() +
        labs(x = "Year", y = "Mean lice per year", title = "All Cals")
    ggplot2::ggsave(
        here::here("./figs/all-cals-on-wild-fish-by-region-year.png"),
        all_cals_region_yr
    )

    # show a timeseries of each type of the lice stages over time
    # lice_by_stage <- fish_data_region %>%
    # dplyr::rowwise() %>%
    # dplyr::mutate(

    # )

    #
}

headwater_distances <- function(head_dists, fish_data) {
    fish_data_headwater <- fish_data %>%
        dplyr::mutate(site_code = as.factor(site_code)) %>%
        dplyr::group_by(site_code) %>%
        dplyr::summarize(
            mean_leps = mean(all_leps, na.rm = TRUE),
            mean_cals = mean(all_cals, na.rm = TRUE),
            mean_all = mean(all_lice, na.rm = TRUE),
            mean_chal_cope = mean(all_chal_cope, na.rm = TRUE),
            mean_lep_adults = mean(adult_leps, na.rm = TRUE),
            mean_cal_adults = mean(adult_cals, na.rm = TRUE),
            mean_all_adults = mean(all_adults, na.rm = TRUE),
            mean_lep_juvs = mean(juv_leps, na.rm = TRUE),
            se_leps = std_err(all_leps),
            se_cals = std_err(all_cals),
            se_all = std_err(all_lice),
            se_chal_cope = std_err(all_chal_cope),
            se_lep_adults = std_err(adult_leps),
            se_cal_adults = std_err(adult_cals),
            se_all_adults = std_err(all_adults),
            se_lep_juvs = std_err(juv_leps)
        )

    head_dists_lice <- dplyr::left_join(
        x = head_dists,
        y = fish_data_headwater,
        by = "site_code"
    )
    just_farms <- head_dists[which(
        head_dists_lice$type == "farm"
    ), ]

    # Knight passage 1 =========================================================

    knight_1_all <- ggplot() +
        geom_point(
            data = head_dists_lice,
            aes(x = (knight_head_1 / 1000), y = mean_all)
        ) +
        geom_errorbar(
            data = head_dists_lice,
            aes(
                x = (knight_head_1 / 1000), ymin = (mean_all - se_all),
                ymax = (mean_all + se_all)
            )
        ) +
        geom_col(data = just_farms, aes(
            x = (knight_head_1 / 1000),
            y = max(head_dists_lice$mean_all, na.rm = TRUE) * 1.25
        ), fill = "lightblue") +
        theme_base() +
        labs(
            x = "Distance from Knight Headwaters (km)",
            y = "Mean number of lice per fish at each sampling location",
            title = "All Species / Stages"
        )
    ggplot2::ggsave(
        here::here("./figs/knight-1-corridor-headwater-distance-all-lice.png"),
        knight_1_all
    )
    knight_1_juvs <- ggplot() +
        geom_point(
            data = head_dists_lice,
            aes(x = (knight_head_1 / 1000), y = mean_chal_cope)
        ) +
        geom_errorbar(
            data = head_dists_lice,
            aes(
                x = (knight_head_1 / 1000),
                ymin = (mean_chal_cope - se_chal_cope),
                ymax = (mean_chal_cope + se_chal_cope)
            )
        ) +
        geom_col(data = just_farms, aes(
            x = (knight_head_1 / 1000),
            y = max(head_dists_lice$mean_chal_cope, na.rm = TRUE) * 1.25
        ), fill = "lightblue") +
        theme_base() +
        labs(
            x = "Distance from Knight Headwaters (km)",
            y = "Mean number of lice per fish at each sampling location",
            title = "Chalimus / Cope -- All Species"
        )
    ggplot2::ggsave(
        here::here("./figs/knight-1-corridor-headwater-distance-all-juvs.png"),
        knight_1_juvs
    )
    knight_1_adult_leps <- ggplot() +
        geom_point(
            data = head_dists_lice,
            aes(x = (knight_head_1 / 1000), y = mean_lep_adults)
        ) +
        geom_errorbar(
            data = head_dists_lice,
            aes(
                x = (knight_head_1 / 1000),
                ymin = (mean_lep_adults - se_lep_adults),
                ymax = (mean_lep_adults + se_lep_adults)
            )
        ) +
        geom_col(data = just_farms, aes(
            x = (knight_head_1 / 1000),
            y = max(head_dists_lice$mean_lep_adults, na.rm = TRUE) * 1.25
        ), fill = "lightblue") +
        theme_base() +
        labs(
            x = "Distance from Knight Headwaters (km)",
            y = "Mean number of lice per fish at each sampling location",
            title = "Adult Leps"
        )
    ggplot2::ggsave(
        here::here("./figs/knight-1-corridor-headwater-distance-adult-leps.png"),
        knight_1_adult_leps
    )

    knight_1_juv_leps <- ggplot() +
        geom_point(
            data = head_dists_lice,
            aes(x = (knight_head_1 / 1000), y = mean_lep_juvs)
        ) +
        geom_errorbar(
            data = head_dists_lice,
            aes(
                x = (knight_head_1 / 1000),
                ymin = (mean_lep_juvs - se_lep_juvs),
                ymax = (mean_lep_juvs + se_lep_juvs)
            )
        ) +
        geom_col(data = just_farms, aes(
            x = (knight_head_1 / 1000),
            y = max(head_dists_lice$mean_lep_juvs, na.rm = TRUE) * 1.25
        ), fill = "lightblue") +
        theme_base() +
        labs(
            x = "Distance from Knight Headwaters (km)",
            y = "Mean number of lice per fish at each sampling location",
            title = "Juvenile leps"
        )
    ggplot2::ggsave(
        here::here("./figs/knight-1-corridor-headwater-distance-juv-leps.png"),
        knight_1_juv_leps
    )

    # Knight passage 2 =========================================================

    knight_2_all <- ggplot() +
        geom_point(
            data = head_dists_lice,
            aes(x = (knight_head_2 / 1000), y = mean_all)
        ) +
        geom_errorbar(
            data = head_dists_lice,
            aes(
                x = (knight_head_2 / 1000), ymin = (mean_all - se_all),
                ymax = (mean_all + se_all)
            )
        ) +
        geom_col(data = just_farms, aes(
            x = (knight_head_2 / 1000),
            y = max(head_dists_lice$mean_all, na.rm = TRUE) * 1.25
        ), fill = "lightblue") +
        theme_base() +
        labs(
            x = "Distance from Knight Headwaters (km)",
            y = "Mean number of lice per fish at each sampling location",
            title = "All Species / Stages"
        )
    ggplot2::ggsave(
        here::here("./figs/knight-2-corridor-headwater-distance-all-lice.png"),
        knight_2_all
    )
    knight_2_juvs <- ggplot() +
        geom_point(
            data = head_dists_lice,
            aes(x = (knight_head_2 / 1000), y = mean_chal_cope)
        ) +
        geom_errorbar(
            data = head_dists_lice,
            aes(
                x = (knight_head_2 / 1000),
                ymin = (mean_chal_cope - se_chal_cope),
                ymax = (mean_chal_cope + se_chal_cope)
            )
        ) +
        geom_col(data = just_farms, aes(
            x = (knight_head_2 / 1000),
            y = max(head_dists_lice$mean_chal_cope, na.rm = TRUE) * 1.25
        ), fill = "lightblue") +
        theme_base() +
        labs(
            x = "Distance from Knight Headwaters (km)",
            y = "Mean number of lice per fish at each sampling location",
            title = "Chalimus / Cope -- All Species"
        )
    ggplot2::ggsave(
        here::here("./figs/knight-2-corridor-headwater-distance-all-juvs.png"),
        knight_2_juvs
    )
    knight_2_adult_leps <- ggplot() +
        geom_point(
            data = head_dists_lice,
            aes(x = (knight_head_2 / 1000), y = mean_lep_adults)
        ) +
        geom_errorbar(
            data = head_dists_lice,
            aes(
                x = (knight_head_2 / 1000),
                ymin = (mean_lep_adults - se_lep_adults),
                ymax = (mean_lep_adults + se_lep_adults)
            )
        ) +
        geom_col(data = just_farms, aes(
            x = (knight_head_2 / 1000),
            y = max(head_dists_lice$mean_lep_adults, na.rm = TRUE) * 1.25
        ), fill = "lightblue") +
        theme_base() +
        labs(
            x = "Distance from Knight Headwaters (km)",
            y = "Mean number of lice per fish at each sampling location",
            title = "Adult Leps"
        )
    ggplot2::ggsave(
        here::here("./figs/knight-2-corridor-headwater-distance-adult-leps.png"),
        knight_2_adult_leps
    )

    knight_2_juv_leps <- ggplot() +
        geom_point(
            data = head_dists_lice,
            aes(x = (knight_head_2 / 1000), y = mean_lep_juvs)
        ) +
        geom_errorbar(
            data = head_dists_lice,
            aes(
                x = (knight_head_2 / 1000),
                ymin = (mean_lep_juvs - se_lep_juvs),
                ymax = (mean_lep_juvs + se_lep_juvs)
            )
        ) +
        geom_col(data = just_farms, aes(
            x = (knight_head_2 / 1000),
            y = max(head_dists_lice$mean_lep_juvs, na.rm = TRUE) * 1.25
        ), fill = "lightblue") +
        theme_base() +
        labs(
            x = "Distance from Knight Headwaters (km)",
            y = "Mean number of lice per fish at each sampling location",
            title = "Juvenile leps"
        )
    ggplot2::ggsave(
        here::here("./figs/knight-2-corridor-headwater-distance-juv-leps.png"),
        knight_2_juv_leps
    )

    # Wakeman ==================================================================

    wakeman_all <- ggplot() +
        geom_point(
            data = head_dists_lice,
            aes(x = (wakeman_head / 1000), y = mean_all)
        ) +
        geom_errorbar(
            data = head_dists_lice,
            aes(
                x = (knight_head_2 / 1000), ymin = (mean_all - se_all),
                ymax = (mean_all + se_all)
            )
        ) +
        geom_col(data = just_farms, aes(
            x = (knight_head_2 / 1000),
            y = max(head_dists_lice$mean_all, na.rm = TRUE) * 1.25
        ), fill = "lightblue") +
        theme_base() +
        labs(
            x = "Distance from Knight Headwaters (km)",
            y = "Mean number of lice per fish at each sampling location",
            title = "All Species / Stages"
        )
    ggplot2::ggsave(
        here::here("./figs/knight-2-corridor-headwater-distance-all-lice.png"),
        knight_2_all
    )
    knight_2_juvs <- ggplot() +
        geom_point(
            data = head_dists_lice,
            aes(x = (knight_head_2 / 1000), y = mean_chal_cope)
        ) +
        geom_errorbar(
            data = head_dists_lice,
            aes(
                x = (knight_head_2 / 1000),
                ymin = (mean_chal_cope - se_chal_cope),
                ymax = (mean_chal_cope + se_chal_cope)
            )
        ) +
        geom_col(data = just_farms, aes(
            x = (knight_head_2 / 1000),
            y = max(head_dists_lice$mean_chal_cope, na.rm = TRUE) * 1.25
        ), fill = "lightblue") +
        theme_base() +
        labs(
            x = "Distance from Knight Headwaters (km)",
            y = "Mean number of lice per fish at each sampling location",
            title = "Chalimus / Cope -- All Species"
        )
    ggplot2::ggsave(
        here::here("./figs/knight-2-corridor-headwater-distance-all-juvs.png"),
        knight_2_juvs
    )
    knight_2_adult_leps <- ggplot() +
        geom_point(
            data = head_dists_lice,
            aes(x = (knight_head_2 / 1000), y = mean_lep_adults)
        ) +
        geom_errorbar(
            data = head_dists_lice,
            aes(
                x = (knight_head_2 / 1000),
                ymin = (mean_lep_adults - se_lep_adults),
                ymax = (mean_lep_adults + se_lep_adults)
            )
        ) +
        geom_col(data = just_farms, aes(
            x = (knight_head_2 / 1000),
            y = max(head_dists_lice$mean_lep_adults, na.rm = TRUE) * 1.25
        ), fill = "lightblue") +
        theme_base() +
        labs(
            x = "Distance from Knight Headwaters (km)",
            y = "Mean number of lice per fish at each sampling location",
            title = "Adult Leps"
        )
    ggplot2::ggsave(
        here::here("./figs/knight-2-corridor-headwater-distance-adult-leps.png"),
        knight_2_adult_leps
    )

    knight_2_juv_leps <- ggplot() +
        geom_point(
            data = head_dists_lice,
            aes(x = (knight_head_2 / 1000), y = mean_lep_juvs)
        ) +
        geom_errorbar(
            data = head_dists_lice,
            aes(
                x = (knight_head_2 / 1000),
                ymin = (mean_lep_juvs - se_lep_juvs),
                ymax = (mean_lep_juvs + se_lep_juvs)
            )
        ) +
        geom_col(data = just_farms, aes(
            x = (knight_head_2 / 1000),
            y = max(head_dists_lice$mean_lep_juvs, na.rm = TRUE) * 1.25
        ), fill = "lightblue") +
        theme_base() +
        labs(
            x = "Distance from Knight Headwaters (km)",
            y = "Mean number of lice per fish at each sampling location",
            title = "Juvenile leps"
        )
    ggplot2::ggsave(
        here::here("./figs/knight-2-corridor-headwater-distance-juv-leps.png"),
        knight_2_juv_leps
    )
}
