final_timeseries <- function(fish_data, title = "Sea Lice on Wild Fish") {
    timeseries <- ggplot(data = fish_data %>%
        dplyr::select(year, month, date, all_lice, region) %>%
        dplyr::group_by(year, region) %>%
        dplyr::summarize(
            mean_lice = mean(all_lice, na.rm = TRUE)
        )) +
        geom_line(aes(x = year, y = mean_lice, colour = region),
            linewidth = 1, linetype = "dashed", alpha = 0.3
        ) +
        geom_point(aes(x = year, y = mean_lice, fill = region),
            shape = 21, size = 3, stroke = 1.2
        ) +
        theme_base() +
        labs(
            x = "Year", y = "Mean lice per year",
            title = title
        ) +
        scale_fill_manual(
            "Region",
            values = c("#785ef0", "#ffb000", "#dc267f")
        ) +
        scale_colour_manual(
            "Region",
            values = c("#785ef0", "#ffb000", "#dc267f")
        )
    ggplot2::ggsave(
        here::here("./figs/final/timeseries.png"),
        timeseries,
        width = 8, height = 6,
        dpi = 600
    )
    return(timeseries)
}

k1_headwater_distances <- function(knight_1_pred_df, head_dists, inventory) {
    knight1_headwater <- knight_1_pred_df %>%
        # dplyr::mutate(site_code = as.factor(site_code)) %>%
        dplyr::group_by(site_code, year) %>%
        dplyr::summarize(
            mean_lep_copes = mean(pred_lep_copes, na.rm = TRUE),
            se_lep_copes = mean(pred_se_copes, na.rm = TRUE),
            mean_lep_adults = mean(pred_lep_adults, na.rm = TRUE),
            se_lep_adults = mean(pred_se_adults, na.rm = TRUE)
        ) %>%
        dplyr::rowwise()

    knight1_head_dists_lice <- dplyr::left_join(
        x = head_dists %>% dplyr::mutate(site_code = as.factor(site_code)),
        y = knight1_headwater,
        by = "site_code"
    )
    knight1_just_farms <- knight1_head_dists_lice[which(
        knight1_head_dists_lice$type == "farm"
    ), c("site_code", "knight_head_1")]
    # join with yearly inventory to get the years that the different farms
    # are present and stocked
    knight1_just_farms <- dplyr::left_join(
        x = yearly_inventory %>% dplyr::select(
            farm_name, year, inventory, lep_tot
        ),
        y = knight1_just_farms,
        by = join_by("farm_name" == "site_code")
    )

    # Knight passage 1 =========================================================
    for (yr in c(2019:2023)) {
        temp_line_df <- knight1_just_farms[which(
            !is.na(knight1_just_farms$knight_head_1) &
                knight1_just_farms$year == yr &
                !is.na(knight1_just_farms$inventory)
        ), ]
        temp_line_df$type <- "farm"
        temp_lice <- knight1_head_dists_lice[which(
            knight1_head_dists_lice$type == "sampling" &
                knight1_head_dists_lice$year == yr &
                !is.na(knight1_head_dists_lice$knight_head_1)
        ), ]

        # make and save plots yearly
        k1_adults <- ggplot() +
            geom_linerange(data = temp_line_df, aes(
                x = (knight_head_1 / 1000), linewidth = type,
                ymin = 0, ymax =
                    (max(temp_lice$mean_lep_adults +
                        temp_lice$se_lep_adults) * 1.1)
            ), colour = "red", linetype = "dashed", alpha = 0.4) +
            geom_errorbar(
                data = temp_lice,
                aes(
                    x = (knight_head_1 / 1000),
                    ymin = ifelse((mean_lep_adults - se_lep_adults) > 0,
                        (mean_lep_adults - se_lep_adults), 0
                    ),
                    ymax = (mean_lep_adults + se_lep_adults),
                ), position = position_dodge(width = 2),
                size = 1.5, colour = "#22c1b8", width = 0
            ) +
            geom_point(
                data = temp_lice,
                aes(
                    x = (knight_head_1 / 1000), y = mean_lep_adults,
                    group = year
                ),
                shape = 21, position = position_dodge(width = 2), size = 3,
                fill = "#22c1b8", stroke = 1.2
            ) +
            theme_base() +
            labs(
                x = "Distance from headwaters (km)",
                y = rlang::expr(paste(
                    "Mean ", "adult ", italic("L. salmonis "),
                    " per fish at each sampling location"
                ))
            ) +
            ggtitle(
                rlang::expr(paste("Knight Inlet (route 1), ", !!yr))
            ) +
            ylim(
                c(
                    -0.000001,
                    max(temp_lice$mean_lep_adults +
                        temp_lice$se_lep_adults) * 1.1
                )
            ) +
            scale_linewidth_manual(
                "Farm Locations",
                values = c(1),
                labels = c("")
            ) +
            theme(
                legend.position = c(0.18, 0.8),
                legend.background = element_rect(colour = "grey80")
            ) +
            guides(
                linewidth = guide_legend(
                    override.aes = list(
                        size = 3,
                        alpha = 1
                    ), ,
                    title.position = "right"
                )
            )
        ggplot2::ggsave(
            paste0(here::here("./figs/final/kn1-adults-"), yr, ".png"),
            k1_adults,
            dpi = 300,
            height = 8, width = 8,
        )

        k1_copes <- ggplot() +
            geom_linerange(data = temp_line_df, aes(
                x = (knight_head_1 / 1000), linewidth = type,
                ymin = 0, ymax = 0.37
                #     (max(temp_lice$mean_lep_adults +
                # temp_lice$se_lep_adults) * 1.08)
            ), colour = "red", linetype = "dashed", alpha = 0.4) +
            geom_point(
                data = temp_lice,
                aes(
                    x = (knight_head_1 / 1000), y = mean_lep_copes,
                ),
                shape = 21, position = position_dodge(width = 2), size = 3,
                fill = "#9b044f", stroke = 1.2, colour = "black"
            ) +
            geom_errorbar(
                data = temp_lice,
                aes(
                    x = (knight_head_1 / 1000),
                    ymin = ifelse((mean_lep_copes - se_lep_copes) > 0,
                        (mean_lep_copes - se_lep_copes), 0
                    ),
                    ymax = (mean_lep_copes + se_lep_copes),
                ), position = position_dodge(width = 2), alpha = 0.8,
                size = 1.5, colour = "#9b044f", width = 0
            ) +
            theme_base() +
            scale_color_viridis("Year", discrete = TRUE, option = "D") +
            scale_fill_viridis("Year", discrete = TRUE, option = "D") +
            labs(
                x = "Distance from headwaters (km)",
                y = rlang::expr(paste(
                    "Mean ", "juvenile ", italic("L. salmonis "),
                    " per fish at each sampling location"
                ))
            ) +
            ggtitle(
                rlang::expr(paste("Knight Inlet (route 1), ", !!yr))
            ) +
            ylim(
                c(-0.000001, 0.37)
                # max(temp_lice$mean_lep_adults +
                # temp_lice$se_lep_adults) * 1.1)
            ) +
            scale_linewidth_manual(
                "Farm Locations",
                values = c(1),
                labels = c("")
            ) +
            theme(
                legend.position = c(0.18, 0.8),
                legend.background = element_rect(colour = "grey80")
            ) +
            guides(
                linewidth = guide_legend(
                    override.aes = list(
                        size = 3,
                        alpha = 1
                    ), ,
                    title.position = "right"
                )
            )
        ggplot2::ggsave(
            paste0(here::here("./figs/final/kn1-copes-"), yr, ".png"),
            k1_copes,
            dpi = 300,
            height = 8, width = 8,
        )
    }
}
k2_headwater_distances <- function(knight_2_pred_df, head_dists, inventory) {
    knight2_headwater <- knight_2_pred_df %>%
        # dplyr::mutate(site_code = as.factor(site_code)) %>%
        dplyr::group_by(site_code, year) %>%
        dplyr::summarize(
            mean_lep_copes = mean(pred_lep_copes, na.rm = TRUE),
            se_lep_copes = mean(pred_se_copes, na.rm = TRUE),
            mean_lep_adults = mean(pred_lep_adults, na.rm = TRUE),
            se_lep_adults = mean(pred_se_adults, na.rm = TRUE)
        ) %>%
        dplyr::rowwise()

    knight2_head_dists_lice <- dplyr::left_join(
        x = head_dists %>% dplyr::mutate(site_code = as.factor(site_code)),
        y = knight2_headwater,
        by = "site_code"
    )
    knight2_just_farms <- knight2_head_dists_lice[which(
        knight2_head_dists_lice$type == "farm"
    ), c("site_code", "knight_head_2")]
    # join with yearly inventory to get the years that the different farms
    # are present and stocked
    knight2_just_farms <- dplyr::left_join(
        x = yearly_inventory %>% dplyr::select(
            farm_name, year, inventory, lep_tot
        ),
        y = knight2_just_farms,
        by = join_by("farm_name" == "site_code")
    )

    # Knight passage 1 =========================================================
    for (yr in c(2019:2023)) {
        temp_line_df <- knight2_just_farms[which(
            !is.na(knight2_just_farms$knight_head_2) &
                knight2_just_farms$year == yr &
                !is.na(knight2_just_farms$inventory)
        ), ]
        temp_line_df$type <- "farm"
        temp_lice <- knight2_head_dists_lice[which(
            knight2_head_dists_lice$type == "sampling" &
                knight2_head_dists_lice$year == yr &
                !is.na(knight2_head_dists_lice$knight_head_2)
        ), ]

        # make and save plots yearly
        k2_adults <- ggplot() +
            geom_linerange(data = temp_line_df, aes(
                x = (knight_head_2 / 1000), linewidth = type,
                ymin = 0, ymax = 0.37
                #     (max(temp_lice$mean_lep_adults +
                # temp_lice$se_lep_adults) * 1.1)
            ), colour = "red", linetype = "dashed", alpha = 0.4) +
            geom_errorbar(
                data = temp_lice,
                aes(
                    x = (knight_head_2 / 1000),
                    ymin = ifelse((mean_lep_adults - se_lep_adults) > 0,
                        (mean_lep_adults - se_lep_adults), 0
                    ),
                    ymax = (mean_lep_adults + se_lep_adults),
                ), position = position_dodge(width = 2),
                size = 1.5, colour = "#22c1b8", width = 0
            ) +
            geom_point(
                data = temp_lice,
                aes(
                    x = (knight_head_2 / 1000), y = mean_lep_adults,
                    group = year
                ),
                shape = 21, position = position_dodge(width = 2), size = 3,
                fill = "#22c1b8", stroke = 1.2
            ) +
            theme_base() +
            labs(
                x = "Distance from headwaters (km)",
                y = rlang::expr(paste(
                    "Mean ", "adult ", italic("L. salmonis "),
                    " per fish at each sampling location"
                ))
            ) +
            ggtitle(
                rlang::expr(paste("Knight Inlet (route 2), ", !!yr))
            ) +
            ylim(
                c(-0.000001, 0.37)
                # max(temp_lice$mean_lep_adults +
                # temp_lice$se_lep_adults) * 1.1)
            ) +
            scale_linewidth_manual(
                "Farm Locations",
                values = c(1),
                labels = c("")
            ) +
            theme(
                legend.position = c(0.18, 0.8),
                legend.background = element_rect(colour = "grey80")
            ) +
            guides(
                linewidth = guide_legend(
                    override.aes = list(
                        size = 3,
                        alpha = 1
                    ), ,
                    title.position = "right"
                )
            )
        ggplot2::ggsave(
            paste0(here::here("./figs/final/kn2-adults-"), yr, ".png"),
            k2_adults,
            dpi = 300,
            height = 8, width = 8,
        )

        k2_copes <- ggplot() +
            geom_linerange(data = temp_line_df, aes(
                x = (knight_head_2 / 1000), linewidth = type,
                ymin = 0, ymax =
                    (max(temp_lice$mean_lep_adults +
                        temp_lice$se_lep_adults) * 1.08)
            ), colour = "red", linetype = "dashed", alpha = 0.4) +
            geom_point(
                data = temp_lice,
                aes(
                    x = (knight_head_2 / 1000), y = mean_lep_copes,
                ),
                shape = 21, position = position_dodge(width = 2), size = 3,
                fill = "#9b044f", stroke = 1.2, colour = "black"
            ) +
            geom_errorbar(
                data = temp_lice,
                aes(
                    x = (knight_head_2 / 1000),
                    ymin = ifelse((mean_lep_copes - se_lep_copes) > 0,
                        (mean_lep_copes - se_lep_copes), 0
                    ),
                    ymax = (mean_lep_copes + se_lep_copes),
                ), position = position_dodge(width = 2), alpha = 0.8,
                size = 1.5, colour = "#9b044f", width = 0
            ) +
            theme_base() +
            scale_color_viridis("Year", discrete = TRUE, option = "D") +
            scale_fill_viridis("Year", discrete = TRUE, option = "D") +
            labs(
                x = "Distance from headwaters (km)",
                y = rlang::expr(paste(
                    "Mean ", "juvenile ", italic("L. salmonis "),
                    " per fish at each sampling location"
                ))
            ) +
            ggtitle(
                rlang::expr(paste("Knight Inlet (route 2), ", !!yr))
            ) +
            ylim(
                c(
                    -0.000001,
                    max(temp_lice$mean_lep_adults +
                        temp_lice$se_lep_adults) * 1.1
                )
            ) +
            scale_linewidth_manual(
                "Farm Locations",
                values = c(1),
                labels = c("")
            ) +
            theme(
                legend.position = c(0.18, 0.8),
                legend.background = element_rect(colour = "grey80")
            ) +
            guides(
                linewidth = guide_legend(
                    override.aes = list(
                        size = 3,
                        alpha = 1
                    ), ,
                    title.position = "right"
                )
            )
        ggplot2::ggsave(
            paste0(here::here("./figs/final/kn2-copes-"), yr, ".png"),
            k2_copes,
            dpi = 300,
            height = 8, width = 8,
        )
    }
}
wake_headwater_distances <- function(wakeman_pred_df, head_dists, inventory) {
    wakeman_headwater <- wakeman_pred_df %>%
        # dplyr::mutate(site_code = as.factor(site_code)) %>%
        dplyr::group_by(site_code, year) %>%
        dplyr::summarize(
            mean_lep_copes = mean(pred_lep_copes, na.rm = TRUE),
            se_lep_copes = mean(pred_se_copes, na.rm = TRUE),
            mean_lep_adults = mean(pred_lep_adults, na.rm = TRUE),
            se_lep_adults = mean(pred_se_adults, na.rm = TRUE)
        ) %>%
        dplyr::rowwise()

    wakeman_head_dists_lice <- dplyr::left_join(
        x = head_dists %>% dplyr::mutate(site_code = as.factor(site_code)),
        y = wakeman_headwater,
        by = "site_code"
    )
    wakeman_just_farms <- wakeman_head_dists_lice[which(
        wakeman_head_dists_lice$type == "farm"
    ), c("site_code", "wakeman_head")]
    # join with yearly inventory to get the years that the different farms
    # are present and stocked
    wakeman_just_farms <- dplyr::left_join(
        x = yearly_inventory %>% dplyr::select(
            farm_name, year, inventory, lep_tot
        ),
        y = wakeman_just_farms,
        by = join_by("farm_name" == "site_code")
    )

    # wakeman passage 1 =========================================================
    for (yr in c(2019:2023)) {
        temp_line_df <- wakeman_just_farms[which(
            !is.na(wakeman_just_farms$wakeman_head) &
                wakeman_just_farms$year == yr &
                !is.na(wakeman_just_farms$inventory)
        ), ]
        temp_line_df$type <- "farm"
        temp_lice <- wakeman_head_dists_lice[which(
            wakeman_head_dists_lice$type == "sampling" &
                wakeman_head_dists_lice$year == yr &
                !is.na(wakeman_head_dists_lice$wakeman_head)
        ), ]

        # make and save plots yearly
        wake_adults <- ggplot() +
            geom_linerange(data = temp_line_df, aes(
                x = (wakeman_head / 1000), linewidth = type,
                ymin = 0, ymax =
                    (max(temp_lice$mean_lep_adults +
                        temp_lice$se_lep_adults) * 1.1)
            ), colour = "red", linetype = "dashed", alpha = 0.4) +
            geom_errorbar(
                data = temp_lice,
                aes(
                    x = (wakeman_head / 1000),
                    ymin = ifelse((mean_lep_adults - se_lep_adults) > 0,
                        (mean_lep_adults - se_lep_adults), 0
                    ),
                    ymax = (mean_lep_adults + se_lep_adults),
                ), position = position_dodge(width = 2),
                size = 1.5, colour = "#22c1b8", width = 0
            ) +
            geom_point(
                data = temp_lice,
                aes(
                    x = (wakeman_head / 1000), y = mean_lep_adults,
                    group = year
                ),
                shape = 21, position = position_dodge(width = 2), size = 3,
                fill = "#22c1b8", stroke = 1.2
            ) +
            theme_base() +
            labs(
                x = "Distance from headwaters (km)",
                y = rlang::expr(paste(
                    "Mean ", "adult ", italic("L. salmonis "),
                    " per fish at each sampling location"
                ))
            ) +
            ggtitle(
                rlang::expr(paste("Wakeman, ", !!yr))
            ) +
            ylim(
                c(
                    -0.000001,
                    max(temp_lice$mean_lep_adults +
                        temp_lice$se_lep_adults) * 1.1
                )
            ) +
            scale_linewidth_manual(
                "Farm Locations",
                values = c(1),
                labels = c("")
            ) +
            theme(
                legend.position = c(0.18, 0.8),
                legend.background = element_rect(colour = "grey80")
            ) +
            guides(
                linewidth = guide_legend(
                    override.aes = list(
                        size = 3,
                        alpha = 1
                    ), ,
                    title.position = "right"
                )
            )
        ggplot2::ggsave(
            paste0(here::here("./figs/final/wake-adults-"), yr, ".png"),
            wake_adults,
            dpi = 300,
            height = 8, width = 8,
        )

        wake_copes <- ggplot() +
            geom_linerange(data = temp_line_df, aes(
                x = (wakeman_head / 1000), linewidth = type,
                ymin = 0, ymax =
                    (max(temp_lice$mean_lep_adults +
                        temp_lice$se_lep_adults) * 1.08)
            ), colour = "red", linetype = "dashed", alpha = 0.4) +
            geom_point(
                data = temp_lice,
                aes(
                    x = (wakeman_head / 1000), y = mean_lep_copes,
                ),
                shape = 21, position = position_dodge(width = 2), size = 3,
                fill = "#9b044f", stroke = 1.2, colour = "black"
            ) +
            geom_errorbar(
                data = temp_lice,
                aes(
                    x = (wakeman_head / 1000),
                    ymin = ifelse((mean_lep_copes - se_lep_copes) > 0,
                        (mean_lep_copes - se_lep_copes), 0
                    ),
                    ymax = (mean_lep_copes + se_lep_copes),
                ), position = position_dodge(width = 2), alpha = 0.8,
                size = 1.5, colour = "#9b044f", width = 0
            ) +
            theme_base() +
            scale_color_viridis("Year", discrete = TRUE, option = "D") +
            scale_fill_viridis("Year", discrete = TRUE, option = "D") +
            labs(
                x = "Distance from headwaters (km)",
                y = rlang::expr(paste(
                    "Mean ", "juvenile ", italic("L. salmonis "),
                    " per fish at each sampling location"
                ))
            ) +
            ggtitle(
                rlang::expr(paste("Wakeman, ", !!yr))
            ) +
            ylim(
                c(
                    -0.000001,
                    max(temp_lice$mean_lep_adults +
                        temp_lice$se_lep_adults) * 1.1
                )
            ) +
            scale_linewidth_manual(
                "Farm Locations",
                values = c(1),
                labels = c("")
            ) +
            theme(
                legend.position = c(0.18, 0.8),
                legend.background = element_rect(colour = "grey80")
            ) +
            guides(
                linewidth = guide_legend(
                    override.aes = list(
                        size = 3,
                        alpha = 1
                    ), ,
                    title.position = "right"
                )
            )
        ggplot2::ggsave(
            paste0(here::here("./figs/final/wake-copes-"), yr, ".png"),
            wake_copes,
            dpi = 300,
            height = 8, width = 8,
        )
    }
}
maps_thru_time <- function() {
    # get farm data to an easy-to-plot way
    mean_farm <- inventory %>%
        dplyr::group_by(year, farm_name) %>%
        dplyr::summarize(
            inventory = mean(inventory, na.rm = TRUE),
            lice = mean(lep_tot, na.rm = TRUE)
        ) %>%
        tidyr::pivot_longer(
            values_to = "vals",
            cols = c("inventory", "lice"),
            names_to = "type"
        )
    farms_locs_inventory <- dplyr::left_join(
        x = mean_farm,
        y = farm_locs,
        by = "farm_name"
    )
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
    farms_sf <- sf::st_as_sf(farms_locs_inventory, coords = c("long", "lat"))
    sf::st_crs(farms_sf) <- 4326 # set the coordinates for WGS84
    farms_utm <- sf::st_transform(farms_sf,
        crs = "+proj=utm +zone=9 +datum=NAD83 +unit=m"
    )
    farms_utm$long <- sf::st_coordinates(farms_utm)[, 1]
    farms_utm$lat <- sf::st_coordinates(farms_utm)[, 2]

    yr <- 2019
    temp_farms <- farms_utm[which(farms_utm$year == yr &
        farms_utm$type == "lice"), ]
    p_2019 <- ggplot() +
        geom_sf(data = bc_cropped, fill = "grey95") +
        geom_sf(data = temp_farms, aes(
            size = vals,
            fill = vals
        ), shape = 22, colour = "black", stroke = 1.2) +
        coord_sf(datum = "+proj=utm +zone=9 +datum=NAD83 +unit=m") +
        theme_base() +
        scale_fill_viridis_c("Lice on Farms - 2019",
            guide = "legend", option = "C",
            labels = function(x) format(x, big.mark = ",", scientific = FALSE),
            breaks = c(200000, 475000, 750000)
        ) +
        scale_size_continuous("Lice on Farms - 2019",
            labels = function(x) format(x, big.mark = ",", scientific = FALSE),
            breaks = c(200000, 475000, 750000)
        ) +
        labs(x = "", y = "") +
        theme(
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            legend.position = c(0.8, 0.8)
        )
    ggplot2::ggsave(
        here::here("./figs/final/farm-2019.png"),
        p_2019,
        dpi = 300,
        height = 6.5, width = 8.5,
    )

    ## fish data ===============================================================
    # plot sampling locs with data
    fish_data_summed <- fish_data %>%
        dplyr::group_by(site_code, year) %>%
        dplyr::summarize(
            mean_leps = mean(all_leps, na.rm = TRUE),
            mean_lep_adults = mean(adult_leps, na.rm = TRUE)
        )
    fish_data_locs <- dplyr::left_join(
        x = fish_data_summed,
        y = sampling_locs,
        by = "site_code"
    )
    # do the same with the sampling locations
    fish_sf <- sf::st_as_sf(fish_data_locs,
        coords = c("lon_site_dd", "lat_site_dd")
    )
    sf::st_crs(fish_sf) <- 4326 # set the coordinates for WGS84
    fish_utm <- sf::st_transform(fish_sf,
        crs = "+proj=utm +zone=9 +datum=NAD83 +unit=m"
    )
    fish_utm$long <- sf::st_coordinates(fish_utm)[, 1]
    fish_utm$lat <- sf::st_coordinates(fish_utm)[, 2]

    temp_fish <- fish_utm[which(fish_utm$year == yr), ]
    pwild_2019 <- ggplot() +
        geom_sf(data = bc_cropped, fill = "grey95") +
        geom_sf(data = temp_fish, aes(
            size = mean_leps,
            fill = mean_leps
        ), shape = 21, colour = "black", stroke = 1.2) +
        coord_sf(datum = "+proj=utm +zone=9 +datum=NAD83 +unit=m") +
        theme_base() +
        scale_fill_viridis_c("Lice per fish - 2019",
            guide = "legend"
        ) +
        scale_size_continuous("Lice per fish - 2019") +
        labs(x = "", y = "") +
        theme(
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            legend.position = c(0.8, 0.8)
        )
    ggplot2::ggsave(
        here::here("./figs/final/wild-2019.png"),
        pwild_2019,
        dpi = 300,
        height = 6.5, width = 8.5,
    )
    # 2023
    yr <- 2023
    temp_farms <- farms_utm[which(farms_utm$year == yr &
        farms_utm$type == "lice"), ]
    p_2023 <- ggplot() +
        geom_sf(data = bc_cropped, fill = "grey95") +
        geom_sf(data = temp_farms, aes(
            size = vals,
            fill = vals
        ), shape = 22, colour = "black", stroke = 1.2) +
        coord_sf(datum = "+proj=utm +zone=9 +datum=NAD83 +unit=m") +
        theme_base() +
        scale_fill_viridis_c("Lice on Farms - 2023",
            guide = "legend", option = "C",
            labels = function(x) format(x, big.mark = ",", scientific = FALSE)
        ) +
        scale_size_continuous("Lice on Farms - 2023",
            labels = function(x) format(x, big.mark = ",", scientific = FALSE)
        ) +
        labs(x = "", y = "") +
        theme(
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            legend.position = c(0.8, 0.8)
        )
    ggplot2::ggsave(
        here::here("./figs/final/farm-2023.png"),
        p_2023,
        dpi = 300,
        height = 6.5, width = 8.5,
    )
    temp_fish <- fish_utm[which(fish_utm$year == yr), ]
    pwild_2023 <- ggplot() +
        geom_sf(data = bc_cropped, fill = "grey95") +
        geom_sf(data = temp_fish, aes(
            size = mean_leps,
            fill = mean_leps
        ), shape = 21, colour = "black", stroke = 1.2) +
        coord_sf(datum = "+proj=utm +zone=9 +datum=NAD83 +unit=m") +
        theme_base() +
        scale_fill_viridis_c("Lice per fish - 2023",
            guide = "legend"
        ) +
        scale_size_continuous("Lice per fish - 2023") +
        labs(x = "", y = "") +
        theme(
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            legend.position = c(0.8, 0.8)
        )
    ggplot2::ggsave(
        here::here("./figs/final/wild-2023.png"),
        pwild_2023,
        dpi = 300,
        height = 6.5, width = 8.5,
    )

    library(patchwork)
    stacked <- (p_2019 + pwild_2019) / (p_2023 + pwild_2023)
    ggplot2::ggsave(
        here::here("./figs/final/all-4-maps.png"),
        stacked,
        dpi = 300,
        height = 14, width = 19,
    )
}
