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
library(glmmTMB)
remotes::install_version("Matrix",
    version = "1.6-2",
    repos = "http://cran.us.r-project.org"
)

installed.packages() |>
    as.data.frame() |>
    subset(Package == "TMB", select = c(LibPath, Version))

options(readr.show_col_types = FALSE)
options(dplyr.warnings = FALSE)

# read in the functions files
source(here::here("./R/00_global_funs.R"))
source(here::here("./R/01_plots_functions.R"))

fish_data_raw <- readr::read_csv(
    here::here("./data/bati_fish_data_2023-10-04(corrected).csv")
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
inventory_old <- readr::read_csv(
    here::here("./data/old-farm-data.csv")
)
new_inventory_data <- readr::read_csv(
    here::here("./data/re-formatted-inventory.csv")
)

# some data cleaning ===========================================================
fish_data <- lice_data_clean(fish_data_raw, sampling_locs)

# inventory cleaning
inventory_old <- inventory_old[which(inventory_old$month %in% c(3:6)), ] %>%
    dplyr::select(-c(cal_av, cal_tot)) %>%
    dplyr::select(
        farm_name, year, month, inventory, lep_av, lep_tot, farm_num,
        ktc, hump_sarg_doc
    )

new_inventory_clean <- new_inventory_data %>%
    # dplyr::mutate(year = as.factor(year), month = as.factor(month)) %>%
    dplyr::group_by(year, month, farm_name, farm_num, ktc, hump_sarg_doc) %>%
    dplyr::summarize(
        inventory = mean(inventory, na.rm = TRUE),
        lep_tot = mean(lep_tot, na.rm = TRUE)
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
        lep_av = inventory / lep_tot
    ) %>%
    dplyr::select(
        farm_name, year, month, inventory, lep_av, lep_tot, farm_num,
        ktc, hump_sarg_doc
    )

# join the old and new inventory
inventory <- rbind(inventory_old, new_inventory_clean) %>% dplyr::mutate(
    date = lubridate::ym(
        paste(year, month, sep = "-")
    )
)

# seaway cleaning
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
        route = as.factor(ifelse(is.na(knight_head_1), 0, 1)),
        year = as.factor(year),
        season = as.factor(season),
        site_code = as.factor(site_code)
    )
knight_leps <- lme4::glmer(
    all_leps ~ route * year + season + (1 | site_code),
    data = knight1_df,
    family = poisson(link = "log")
)
knight_leps <- glmmTMB::glmmTMB(
    all_leps ~ route * year + season + (1 | site_code),
    data = knight1_df,
    family = poisson(link = "log")
)
hist(knight1_df$all_leps)
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
