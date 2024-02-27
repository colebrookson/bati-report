#' AUTHOR: Cole Brookson
#' DATE: 2024-02-21

# set up =======================================================================

library(here)
library(readr)
library(dplyr)
library(ggplot2)
library(sf)

# read in the functions files
source(here::here("./R/00_global_funs.R"))
source(here::here("./R/01_plots_functions.R"))

fish_data <- readr::read_csv(here::here("./data/bati_fish_data_2023-10-04.csv"))
seaway_data <- readr::read_csv(
    here::here("./data/bati_seaway_distances_to_farms.csv")
)
site_data <- readr::read_csv(here::here("./data/bati_site_data_2023-10-04.csv"))
site_master <- readr::read_csv(here::here("./data/bati_site_master_list.csv"))
farm_activity <- readxl::read_xlsx(here::here("./data/farm_activity.xlsx"))
geo_data <- sf::read_sf(here::here("./data/geo-spatial/shapefile/"))
farm_locs <- readr::read_csv(
    here::here("./data/geo-spatial/farm-locations.csv")
)
sampling_locs <- readr::read_csv(
    here::here("./data/geo-spatial/bati_site_master_list.csv")
)
# put some initial plots =======================================================
