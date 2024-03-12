---
title: "Working figs for BATI Report"
output:
  pdf_document:
    keep_tex: true
---


```r
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

fish_data <- readr::read_csv(
    here::here("./data/bati_fish_data_2023-10-04.csv")
) %>%
    standardize_names()
seaway_data <- readr::read_csv(
    here::here("./data/bati_seaway_distances_to_farms.csv")
)
```

```
## New names:
## • `` -> `...1`
```

```r
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
fish_data <- lice_data_clean(fish_data, sampling_locs)

inventory <- inventory[which(inventory$month %in% c(3:6)), ]

# put some initial plots =======================================================
plot_study_area(
    geo_data, farm_locs, sampling_locs,
    to_save = TRUE
)
```

```
## Warning: attribute variables are assumed to be spatially constant throughout
## all geometries
```

```
## Saving 7 x 7 in image
## Saving 7 x 7 in image
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png)

```r
time_series_lice(
    fish_data, sampling_locs, inventory
)
```

```
## `summarise()` has grouped output by 'year', 'month'. You can override using the `.groups` argument.
## Saving 7 x 7 in image
```

```
## Warning: Removed 5 rows containing missing values (`geom_point()`).
```

```
## `summarise()` has grouped output by 'year'. You can override using the `.groups` argument.
## Saving 7 x 7 in image
## `summarise()` has grouped output by 'year', 'month'. You can override using the `.groups` argument.
## Saving 7 x 7 in image
```

```
## Warning: Removed 5 rows containing missing values (`geom_point()`).
```

```
## `summarise()` has grouped output by 'year'. You can override using the `.groups` argument.
## Saving 7 x 7 in image
## `summarise()` has grouped output by 'year'. You can override using the `.groups` argument.
## Saving 7 x 7 in image
```

```
## Warning: Removed 2 rows containing non-finite values (`stat_smooth()`).
```

```
## Warning: Removed 2 rows containing missing values (`geom_point()`).
```

```
## Saving 7 x 7 in image
## `summarise()` has grouped output by 'year', 'month'. You can override using the `.groups` argument.
## `summarise()` has grouped output by 'year'. You can override using the `.groups` argument.
## `summarise()` has grouped output by 'year'. You can override using the `.groups` argument.
## Saving 7 x 7 in image
```

```
## Warning: Removed 1 rows containing non-finite values (`stat_smooth()`).
```

```
## Warning: Removed 1 rows containing missing values (`geom_point()`).
```

```
## Saving 7 x 7 in image
```

```
## Warning: Removed 1 rows containing non-finite values (`stat_smooth()`).
## Removed 1 rows containing missing values (`geom_point()`).
```

```
## [[1]]
```

```
## Warning: Removed 5 rows containing missing values (`geom_point()`).
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-2.png)

```
## 
## [[2]]
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-3.png)

```
## 
## [[3]]
```

```
## Warning: Removed 5 rows containing missing values (`geom_point()`).
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-4.png)

```
## 
## [[4]]
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-5.png)

```
## 
## [[5]]
```

```
## Warning: Removed 2 rows containing non-finite values (`stat_smooth()`).
```

```
## Warning: Removed 2 rows containing missing values (`geom_point()`).
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-6.png)

```
## 
## [[6]]
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-7.png)

```
## 
## [[7]]
```

```
## Warning: Removed 1 rows containing non-finite values (`stat_smooth()`).
```

```
## Warning: Removed 1 rows containing missing values (`geom_point()`).
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-8.png)

```
## 
## [[8]]
```

```
## Warning: Removed 1 rows containing non-finite values (`stat_smooth()`).
## Removed 1 rows containing missing values (`geom_point()`).
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-9.png)

```r
maps_with_data(
    inventory, fish_data, sampling_locs, farm_locs, geo_data
)
```

```
## `summarise()` has grouped output by 'year'. You can override using the
## `.groups` argument.
```

```
## Warning: attribute variables are assumed to be spatially constant throughout
## all geometries
```

```
## Saving 7 x 7 in image
## Saving 7 x 7 in image
## Saving 7 x 7 in image
## `summarise()` has grouped output by 'site_code'. You can override using the `.groups` argument.
## Saving 7 x 7 in image
```

```
## Warning: Removed 10 rows containing missing values (`geom_sf()`)
```

```
## Saving 7 x 7 in image
```

```
## Warning: Removed 10 rows containing missing values (`geom_sf()`)
```

```
## Saving 7 x 7 in image
## Saving 7 x 7 in image
## Saving 7 x 7 in image
## Saving 7 x 7 in image
## Saving 7 x 7 in image
## Saving 7 x 7 in image
```

```
## Warning: Removed 10 rows containing missing values (`geom_sf()`)
```

```
## Saving 7 x 7 in image
```

```
## Warning: Removed 10 rows containing missing values (`geom_sf()`)
```

```
## Saving 7 x 7 in image
```

```
## Warning: Removed 10 rows containing missing values (`geom_sf()`)
```

```
## Saving 7 x 7 in image
## Saving 7 x 7 in image
## Saving 7 x 7 in image
## Saving 7 x 7 in image
## Saving 7 x 7 in image
## Saving 7 x 7 in image
```

```
## Warning: Removed 10 rows containing missing values (`geom_sf()`)
```

```
## Saving 7 x 7 in image
```

```
## Warning: Removed 10 rows containing missing values (`geom_sf()`)
```

```
## Saving 7 x 7 in image
```

```
## Warning: Removed 10 rows containing missing values (`geom_sf()`)
```

```
## Saving 7 x 7 in image
## Saving 7 x 7 in image
## Saving 7 x 7 in image
## Saving 7 x 7 in image
## Saving 7 x 7 in image
## Saving 7 x 7 in image
```

```
## Warning: Removed 10 rows containing missing values (`geom_sf()`)
```

```
## Saving 7 x 7 in image
```

```
## Warning: Removed 9 rows containing missing values (`geom_sf()`)
```

```
## Saving 7 x 7 in image
```

```
## Warning: Removed 9 rows containing missing values (`geom_sf()`)
```

```
## Saving 7 x 7 in image
## Saving 7 x 7 in image
## Saving 7 x 7 in image
## Saving 7 x 7 in image
## Saving 7 x 7 in image
## Saving 7 x 7 in image
```

```
## Warning: Removed 9 rows containing missing values (`geom_sf()`)
```

```
## Saving 7 x 7 in image
```

```
## Warning: Removed 13 rows containing missing values (`geom_sf()`)
```

```
## Saving 7 x 7 in image
```

```
## Warning: Removed 13 rows containing missing values (`geom_sf()`)
```

```
## Saving 7 x 7 in image
## Saving 7 x 7 in image
## Saving 7 x 7 in image
## Saving 7 x 7 in image
## Saving 7 x 7 in image
## Saving 7 x 7 in image
```

```
## Warning: Removed 13 rows containing missing values (`geom_sf()`)
```

```
## $length
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-10.png)

```
## 
## [[2]]
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-11.png)

```
## 
## [[3]]
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-12.png)

```
## 
## [[4]]
```

```
## Warning: Removed 10 rows containing missing values (`geom_sf()`)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-13.png)

```
## 
## [[5]]
```

```
## Warning: Removed 10 rows containing missing values (`geom_sf()`)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-14.png)

```
## 
## [[6]]
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-15.png)

```
## 
## [[7]]
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-16.png)

```
## 
## [[8]]
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-17.png)

```
## 
## [[9]]
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-18.png)

```
## 
## [[10]]
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-19.png)

```
## 
## [[11]]
```

```
## Warning: Removed 10 rows containing missing values (`geom_sf()`)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-20.png)

```
## 
## [[12]]
```

```
## Warning: Removed 10 rows containing missing values (`geom_sf()`)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-21.png)

```
## 
## [[13]]
```

```
## Warning: Removed 10 rows containing missing values (`geom_sf()`)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-22.png)

```
## 
## [[14]]
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-23.png)

```
## 
## [[15]]
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-24.png)

```
## 
## [[16]]
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-25.png)

```
## 
## [[17]]
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-26.png)

```
## 
## [[18]]
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-27.png)

```
## 
## [[19]]
```

```
## Warning: Removed 10 rows containing missing values (`geom_sf()`)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-28.png)

```
## 
## [[20]]
```

```
## Warning: Removed 10 rows containing missing values (`geom_sf()`)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-29.png)

```
## 
## [[21]]
```

```
## Warning: Removed 10 rows containing missing values (`geom_sf()`)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-30.png)

```
## 
## [[22]]
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-31.png)

```
## 
## [[23]]
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-32.png)

```
## 
## [[24]]
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-33.png)

```
## 
## [[25]]
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-34.png)

```
## 
## [[26]]
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-35.png)

```
## 
## [[27]]
```

```
## Warning: Removed 10 rows containing missing values (`geom_sf()`)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-36.png)

```
## 
## [[28]]
```

```
## Warning: Removed 9 rows containing missing values (`geom_sf()`)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-37.png)

```
## 
## [[29]]
```

```
## Warning: Removed 9 rows containing missing values (`geom_sf()`)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-38.png)

```
## 
## [[30]]
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-39.png)

```
## 
## [[31]]
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-40.png)

```
## 
## [[32]]
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-41.png)

```
## 
## [[33]]
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-42.png)

```
## 
## [[34]]
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-43.png)

```
## 
## [[35]]
```

```
## Warning: Removed 9 rows containing missing values (`geom_sf()`)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-44.png)

```
## 
## [[36]]
```

```
## Warning: Removed 13 rows containing missing values (`geom_sf()`)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-45.png)

```
## 
## [[37]]
```

```
## Warning: Removed 13 rows containing missing values (`geom_sf()`)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-46.png)

```
## 
## [[38]]
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-47.png)

```
## 
## [[39]]
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-48.png)

```
## 
## [[40]]
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-49.png)

```
## 
## [[41]]
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-50.png)

```
## 
## [[42]]
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-51.png)

```
## 
## [[43]]
```

```
## Warning: Removed 13 rows containing missing values (`geom_sf()`)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-52.png)

```r
headwater_distances(
    head_dists, fish_data
)
```

```
## Saving 7 x 7 in image
```

```
## Warning: Removed 6 rows containing missing values (`position_stack()`).
```

```
## Warning: Removed 36 rows containing missing values (`geom_point()`).
```

```
## Saving 7 x 7 in image
```

```
## Warning: Removed 6 rows containing missing values (`position_stack()`).
## Removed 36 rows containing missing values (`geom_point()`).
```

```
## Saving 7 x 7 in image
```

```
## Warning: Removed 6 rows containing missing values (`position_stack()`).
## Removed 36 rows containing missing values (`geom_point()`).
```

```
## Saving 7 x 7 in image
```

```
## Warning: Removed 6 rows containing missing values (`position_stack()`).
```

```
## Warning: Removed 39 rows containing missing values (`geom_point()`).
```

```
## Saving 7 x 7 in image
```

```
## Warning: Removed 7 rows containing missing values (`position_stack()`).
```

```
## Warning: Removed 30 rows containing missing values (`geom_point()`).
```

```
## Saving 7 x 7 in image
```

```
## Warning: Removed 7 rows containing missing values (`position_stack()`).
## Removed 30 rows containing missing values (`geom_point()`).
```

```
## Saving 7 x 7 in image
```

```
## Warning: Removed 7 rows containing missing values (`position_stack()`).
## Removed 30 rows containing missing values (`geom_point()`).
```

```
## Saving 7 x 7 in image
```

```
## Warning: Removed 7 rows containing missing values (`position_stack()`).
```

```
## Warning: Removed 33 rows containing missing values (`geom_point()`).
```

```
## Saving 7 x 7 in image
```

```
## Warning: Removed 11 rows containing missing values (`position_stack()`).
```

```
## Warning: Removed 42 rows containing missing values (`geom_point()`).
```

```
## Saving 7 x 7 in image
```

```
## Warning: Removed 11 rows containing missing values (`position_stack()`).
## Removed 42 rows containing missing values (`geom_point()`).
```

```
## Saving 7 x 7 in image
```

```
## Warning: Removed 11 rows containing missing values (`position_stack()`).
## Removed 42 rows containing missing values (`geom_point()`).
```

```
## Saving 7 x 7 in image
```

```
## Warning: Removed 11 rows containing missing values (`position_stack()`).
```

```
## Warning: Removed 44 rows containing missing values (`geom_point()`).
```

```
## [[1]]
```

```
## Warning: Removed 6 rows containing missing values (`position_stack()`).
```

```
## Warning: Removed 36 rows containing missing values (`geom_point()`).
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-53.png)

```
## 
## [[2]]
```

```
## Warning: Removed 6 rows containing missing values (`position_stack()`).
## Removed 36 rows containing missing values (`geom_point()`).
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-54.png)

```
## 
## [[3]]
```

```
## Warning: Removed 6 rows containing missing values (`position_stack()`).
```

```
## Warning: Removed 39 rows containing missing values (`geom_point()`).
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-55.png)

```
## 
## [[4]]
```

```
## Warning: Removed 6 rows containing missing values (`position_stack()`).
```

```
## Warning: Removed 36 rows containing missing values (`geom_point()`).
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-56.png)

```
## 
## [[5]]
```

```
## Warning: Removed 7 rows containing missing values (`position_stack()`).
```

```
## Warning: Removed 30 rows containing missing values (`geom_point()`).
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-57.png)

```
## 
## [[6]]
```

```
## Warning: Removed 7 rows containing missing values (`position_stack()`).
## Removed 30 rows containing missing values (`geom_point()`).
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-58.png)

```
## 
## [[7]]
```

```
## Warning: Removed 7 rows containing missing values (`position_stack()`).
```

```
## Warning: Removed 33 rows containing missing values (`geom_point()`).
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-59.png)

```
## 
## [[8]]
```

```
## Warning: Removed 7 rows containing missing values (`position_stack()`).
```

```
## Warning: Removed 30 rows containing missing values (`geom_point()`).
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-60.png)

```
## 
## [[9]]
```

```
## Warning: Removed 11 rows containing missing values (`position_stack()`).
```

```
## Warning: Removed 42 rows containing missing values (`geom_point()`).
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-61.png)

```
## 
## [[10]]
```

```
## Warning: Removed 11 rows containing missing values (`position_stack()`).
## Removed 42 rows containing missing values (`geom_point()`).
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-62.png)

```
## 
## [[11]]
```

```
## Warning: Removed 11 rows containing missing values (`position_stack()`).
```

```
## Warning: Removed 44 rows containing missing values (`geom_point()`).
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-63.png)

```
## 
## [[12]]
```

```
## Warning: Removed 11 rows containing missing values (`position_stack()`).
```

```
## Warning: Removed 42 rows containing missing values (`geom_point()`).
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-64.png)

