#' AUTHOR: Cole Brookson
#' DATE: 21-02-2024

library(targets)

tar_option_set(packages = c("dplyr", "readr", "here"))

tar_target(all_data, {
    # Your code to generate all_data target
})

tar_target(analysis, {
    # Your code to generate analysis target
})

tar_target(plots, {
    # Your code to generate plots target
})

tar_target(report, {
    # Your code to generate report target
})
