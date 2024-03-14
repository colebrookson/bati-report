#' ---
#' title: "Working figs for BATI Report"
#' output:
#'   pdf_document:
#'     keep_tex: true
#' ---

# set up =======================================================================

library(here)
library(readr)
library(dplyr)
library(ggplot2)
library(sf)

options(readr.show_col_types = FALSE)
options(dplyr.warnings = FALSE)

# read in the functions files
source(here::here("./R/00_global_funs.R"))
source(here::here("./R/01_plots_functions.R"))

fish_data_raw <- readr::read_csv(
    here::here("./data/bati_fish_data_2023-10-04(corrected).csv")
) %>%
    standardize_names()
fish_data_raw1 <- readr::read_csv(
    here::here("./data/bati_fish_data_2023-10-04.csv")
) %>%
    standardize_names()

seaway_data <- readr::read_csv(
    here::here("./data/bati_seaway_distances_to_farms.csv")
) %>% standardize_names()
site_data <- readr::read_csv(here::here("./data/bati_site_data_2023-10-04.csv"))
farm_activity <- readxl::read_xlsx(here::here("./data/farm_activity.xlsx"))
geo_data <- sf::read_sf(here::here("./data/geo-spatial/shapefile/"))
farm_locs <- readr::read_csv(
    here::here("./data/geo-spatial/farm-locations.csv")
)
sampling_locs <- readr::read_csv(
    here::here("./data/geo-spatial/bati_site_master_list.csv")
)
head_dists <- readr::read_csv(
    here::here("./data/headwater-distances.csv")
)
inventory <- readr::read_csv(
    here::here("./data/old-farm-data.csv")
) %>% dplyr::mutate(
    date = lubridate::ym(
        paste(year, month, sep = "-")
    )
)

# some data cleaning ===========================================================
fish_data1 <- lice_data_clean(fish_data_raw1, sampling_locs)
fish_data <- lice_data_clean(fish_data_raw, sampling_locs)

fish_data %>%
    dplyr::group_by(year, month) %>%
    dplyr::summarize(
        mean_leps = mean(all_leps, na.rm = TRUE),
        mean_lice = mean(all_lice, na.rm = TRUE),
        mean_cals = mean(all_cals, na.rm = TRUE)
    )
fish_data1 %>%
    dplyr::group_by(year, month) %>%
    dplyr::summarize(
        mean_leps = mean(all_leps, na.rm = TRUE),
        mean_lice = mean(all_lice, na.rm = TRUE),
        mean_cals = mean(all_cals, na.rm = TRUE)
    )

inventory <- inventory[which(inventory$month %in% c(3:6)), ]

colnames(seaway_data)[1] <- "sampling_sites"

# run models directly in main ==================================================
head_dists_lice <- dplyr::left_join(
        x = head_dists %>% dplyr::mutate(site_code = as.factor(site_code)),
        y = fish_data,
        by = "site_code"
)

knight1_df <- head_dists_lice %>% 
dplyr::rowwise() %>%  
dplyr::mutate(
    route = ifelse(is.na(knight_head_1), 0, 1)
) %>% 
dplyr::select(site_code, knight_head_1, route)

knight_leps <- glmmTMB::glmmTMB(
    all_leps ~ 
)

# put some initial plots =======================================================
plot_study_area(
    geo_data, farm_locs, sampling_locs,
    to_save = TRUE
)

time_series_lice(
    fish_data, sampling_locs, inventory
)

maps_with_data(
    inventory, fish_data, sampling_locs, farm_locs, geo_data
)

headwater_distances(
    head_dists, fish_data
)
