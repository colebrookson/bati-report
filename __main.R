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
library(viridis)
library(ggnewscale)

# remotes::install_version("Matrix",
#     version = "1.6-2",
#     repos = "http://cran.us.r-project.org"
# )
#
# installed.packages() |>
#     as.data.frame() |>
#     subset(Package == "TMB", select = c(LibPath, Version))

options(readr.show_col_types = FALSE)
options(dplyr.warnings = FALSE)

# read in the functions files
source(here::here("./R/00_global_funs.R"))
source(here::here("./R/01_plots_functions.R"))
source(here::here("./R/03_final_plots.R"))

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
yearly_inventory <- as_tibble(inventory %>%
    dplyr::group_by(farm_name, year, farm_num, ktc, hump_sarg_doc) %>%
    dplyr::summarize(
        inventory = mean(inventory, na.rm = TRUE),
        lep_tot = mean(lep_tot, na.rm = TRUE),
        lep_av = mean(lep_av, na.rm = TRUE)
    ))
all <- yearly_inventory %>%
    tidyr::expand(year, farm_name)
yearly_inventory <- yearly_inventory %>%
    dplyr::right_join(all) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
        lep_av = lep_tot / inventory
    )

# seaway cleaning
colnames(seaway_data)[1] <- "sampling_sites"

# set up models directly in main ===============================================
head_dists_lice <- dplyr::left_join(
    x = head_dists %>% dplyr::mutate(site_code = as.factor(site_code)),
    y = fish_data,
    by = "site_code"
)

knight1_df <- head_dists_lice %>%
    dplyr::rowwise() %>%
    dplyr::filter(type == "sampling") %>%
    dplyr::mutate(
        route = as.factor(ifelse(is.na(knight_head_1), "no", "yes")),
        year = as.factor(year),
        season = as.factor(season),
        site_code = as.factor(site_code)
    )
knight2_df <- head_dists_lice %>%
    dplyr::rowwise() %>%
    dplyr::filter(type == "sampling") %>%
    dplyr::mutate(
        route = as.factor(ifelse(is.na(knight_head_2), "no", "yes")),
        year = as.factor(year),
        season = as.factor(season),
        site_code = as.factor(site_code)
    )
wakeman_df <- head_dists_lice %>%
    dplyr::rowwise() %>%
    dplyr::filter(type == "sampling") %>%
    dplyr::mutate(
        route = as.factor(ifelse(is.na(wakeman_head), "no", "yes")),
        year = as.factor(year),
        season = as.factor(season),
        site_code = as.factor(site_code)
    )
re_fit <- FALSE
if (re_fit) {
    source(here::here("./R/02_models.R"))
} else {
    knight1_lice <- readRDS(here::here("./outputs/knight1-all-lice-model.rds"))
    knight1_leps <- readRDS(here::here("./outputs/knight1-all-leps-model.rds"))
    knight1_lep_adults <- readRDS(
        here::here("./outputs/knight1-lep-adults-model.rds")
    )
    knight1_lep_copes <- readRDS(
        here::here("./outputs/knight1-lep-copes-model.rds")
    )

    knight2_lice <- readRDS(here::here("./outputs/knight2-all-lice-model.rds"))
    knight2_leps <- readRDS(here::here("./outputs/knight2-all-leps-model.rds"))
    knight2_lep_adults <- readRDS(
        here::here("./outputs/knight2-lep-adults-model.rds")
    )
    knight2_lep_copes <- readRDS(
        here::here("./outputs/knight2-lep-copes-model.rds")
    )

    wakeman_lice <- readRDS(here::here("./outputs/wakeman-all-lice-model.rds"))
    wakeman_leps <- readRDS(here::here("./outputs/wakeman-all-leps-model.rds"))
    wakeman_lep_adults <- readRDS(
        here::here("./outputs/wakeman-lep-adults-model.rds")
    )
    wakeman_lep_copes <- readRDS(
        here::here("./outputs/wakeman-lep-copes-model.rds")
    )
}

## re-impute the values off the predictions ====================================
knight_1_pred_df <- knight1_df
knight_2_pred_df <- knight2_df
wakeman_pred_df <- wakeman_df
knight_1_pred_df[, c("pred_lep_adults", "pred_se_adults")] <- data.frame(
    stats::predict(knight1_lep_adults,
        type = "response", se.fit = TRUE
    )
)
knight_1_pred_df[, c("pred_leps", "pred_se_leps")] <- data.frame(
    stats::predict(knight1_leps,
        type = "response", se.fit = TRUE
    )
)
knight_1_pred_df[, c("pred_lep_copes", "pred_se_copes")] <- data.frame(
    stats::predict(knight1_lep_copes,
        type = "response", se.fit = TRUE
    )
)
knight_1_pred_df[, c("pred_lice", "pred_se_lice")] <- data.frame(
    stats::predict(knight1_lice,
        type = "response", se.fit = TRUE
    )
)
knight_2_pred_df[, c("pred_lep_adults", "pred_se_adults")] <- data.frame(
    stats::predict(knight2_lep_adults,
        type = "response", se.fit = TRUE
    )
)
knight_2_pred_df[, c("pred_leps", "pred_se_leps")] <- data.frame(
    stats::predict(knight2_leps,
        type = "response", se.fit = TRUE
    )
)
knight_2_pred_df[, c("pred_lep_copes", "pred_se_copes")] <- data.frame(
    stats::predict(knight2_lep_copes,
        type = "response", se.fit = TRUE
    )
)
knight_2_pred_df[, c("pred_lice", "pred_se_lice")] <- data.frame(
    stats::predict(knight2_lice,
        type = "response", se.fit = TRUE
    )
)
wakeman_pred_df[, c("pred_lep_adults", "pred_se_adults")] <- data.frame(
    stats::predict(wakeman_lep_adults,
        type = "response", se.fit = TRUE
    )
)
wakeman_pred_df[, c("pred_lep_copes", "pred_se_copes")] <- data.frame(
    stats::predict(wakeman_lep_copes,
        type = "response", se.fit = TRUE
    )
)
wakeman_pred_df[, c("pred_lice", "pred_se_lice")] <- data.frame(
    stats::predict(wakeman_lice,
        type = "response", se.fit = TRUE
    )
)
wakeman_pred_df[, c("pred_leps", "pred_se_leps")] <- data.frame(
    stats::predict(wakeman_leps,
        type = "response", se.fit = TRUE
    )
)

# final plots ==================================================================

## time series with all the lice through time ==================================

final_timeseries(fish_data, title = "Sea Lice on Wild Salmon")

## save the map plots ==========================================================
plot_study_area(geo_data, farm_locs, sampling_locs, to_save = TRUE)

## wakeman plots of copes and headwater distances ==============================
wake_headwater_distances(wakeman_pred_df, head_dists, inventory)

## for myself ==================================================================
sampling_counts <- fish_data %>%
    dplyr::group_by(year, month, day, site_name) %>%
    dplyr::summarize(
        n = n()
    )
hist(sampling_counts$n)
