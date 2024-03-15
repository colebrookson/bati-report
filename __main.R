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
knight2_df <- head_dists_lice %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    route = as.factor(ifelse(is.na(knight_head_2), 0, 1)),
    year = as.factor(year),
    season = as.factor(season),
    site_code = as.factor(site_code)
  )
wakeman_df <- head_dists_lice %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    route = as.factor(ifelse(is.na(wakeman_head), 0, 1)),
    year = as.factor(year),
    season = as.factor(season),
    site_code = as.factor(site_code)
  )

if(re_fit) {
  ## knight 1 models ===========================================================
  knight1_leps <- glmmTMB::glmmTMB(
    all_leps ~ route * year + season + (1 | site_code),
    data = knight1_df,
    family = poisson(link = "log")
  )
  saveRDS(knight1_leps, here::here("./outputs/knight1-all-leps-model.rds"))
  
  knight1_lep_adults <- glmmTMB::glmmTMB(
    adult_leps ~ route * year + season + (1 | site_code),
    data = knight1_df,
    family = poisson(link = "log")
  )
  saveRDS(knight1_lep_adults, 
          here::here("./outputs/knight1-lep-adults-model.rds"))
  
  knight1_lep_copes <- glmmTMB::glmmTMB(
    lep_cope ~ route * year + season + (1 | site_code),
    data = knight1_df,
    family = poisson(link = "log")
  )
  saveRDS(knight1_lep_copes, 
          here::here("./outputs/knight1-lep-copes-model.rds"))
  
  ## knight 2 models ===========================================================
  knight2_leps <- glmmTMB::glmmTMB(
    all_leps ~ route * year + season + (1 | site_code),
    data = knight2_df,
    family = poisson(link = "log")
  )
  saveRDS(knight2_leps, here::here("./outputs/knight2-all-leps-model.rds"))
  
  knight2_lep_adults <- glmmTMB::glmmTMB(
    adult_leps ~ route * year + season + (1 | site_code),
    data = knight2_df,
    family = poisson(link = "log")
  )
  saveRDS(knight2_lep_adults, 
          here::here("./outputs/knight2-lep-adults-model.rds"))
  
  knight2_lep_copes <- glmmTMB::glmmTMB(
    lep_cope ~ route * year + season + (1 | site_code),
    data = knight2_df,
    family = poisson(link = "log")
  )
  saveRDS(knight2_lep_copes, 
          here::here("./outputs/knight2-lep-copes-model.rds"))
  
  ## wakeman models ============================================================
  wakeman_leps <- glmmTMB::glmmTMB(
    all_leps ~ route * year + season + (1 | site_code),
    data = wakeman_df,
    family = poisson(link = "log")
  )
  saveRDS(wakeman_leps, here::here("./outputs/wakeman-all-leps-model.rds"))
  
  wakeman_lep_adults <- glmmTMB::glmmTMB(
    adult_leps ~ route * year + season + (1 | site_code),
    data = wakeman_df,
    family = poisson(link = "log")
  )
  saveRDS(wakeman_lep_adults, 
          here::here("./outputs/wakeman-lep-adults-model.rds"))
  
  wakeman_lep_copes <- glmmTMB::glmmTMB(
    lep_cope ~ route * year + season + (1 | site_code),
    data = wakeman_df,
    family = poisson(link = "log")
  )
  saveRDS(wakeman_lep_copes, 
          here::here("./outputs/wakeman-lep-copes-model.rds"))
}

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
