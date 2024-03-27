#' AUTHOR: Cole B. Brookson
#' DATE OF CREATION: 2024-02-21
#'
#' This targets file contains all functions that don't relate to a specific
#' part of the analysis, but are required to perform general tasks
#'
#' All functions are documented using the roxygen2 framework and the docstring
#' library
#'

`%notin%` <- Negate(`%in%`)

#' Takes in the file and reads it for use
#' Generic function to interface with the file targets, to read them in and
#' make them available for analysis
#'
#' @param file character. The file path in the target
#'
#' @return Dataframe
#' @export
get_data_csv <- function(file) {
    readr::read_csv(file, show_col_types = FALSE)
}

#' Standardizes column names in a dataframe
#'
#' @param df dataFrame. The data at hand
#'
#' @return df, but with new names
#' @export
standardize_names <- function(df) {
    # get current set of names
    current_names <- names(df)

    # loop through, pull the name out, change " " to "_"
    for (name in seq_len(length(current_names))) {
        current_names[name] <- gsub("\\ ", "_", current_names[name])
        current_names[name] <- gsub("\\.", "_", current_names[name])
    }

    # check for any upper case letters and make those lower_case
    current_names <- tolower(current_names)

    # remove brackets
    for (name in seq_len(length(current_names))) {
        current_names[name] <- gsub("\\(", "", current_names[name])
        current_names[name] <- gsub("\\)", "", current_names[name])
    }

    # rename the dataframe
    names(df) <- current_names

    # return dataframe renamed
    return(df)
}

#' Calculate standard error
#'
#' @param x vector of values
#'
#' @return numeric value
#' @export
std_err <- function(x) {
    return(sd(x, na.rm = TRUE) / sqrt(length(x)))
}

#' Gets the standard required lice information
#'
#' @description Take in the raw data and do the standard stuff that we'll need
#' for further processing - get all the lice counts of interest
#'
#' @param fish_data the raw lice data pre- any messing
#' @param sampling_locs the raw sampling location data
lice_data_clean <- function(fish_data_raw, sampling_locs) {
    # first, make sure all the site_codes are right
    fish_data_raw$site_code[which(fish_data_raw$site_code == "KoK")] <- "KOK"

    # do a join to put the regions on the fish dataframe
    fish_data <- dplyr::left_join(
        x = fish_data_raw,
        y = sampling_locs[, c("site_name", "region")],
        by = "site_name"
    ) %>%
        dplyr::filter(liced == 1) %>% # keep only liced fish
        dplyr::filter(spp %in% c("pink", "chum")) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
            date = lubridate::ymd(
                paste(year, month, day, sep = "-")
            )
        ) %>%
        # make a column of all lice
        dplyr::mutate(
            all_leps = sum(
                lep_cope, lep_pa_male, lep_pa_female,
                lep_male, lep_nongravid, lep_gravid,
                na.rm = TRUE
            ),
            adult_leps = sum(
                lep_male, lep_nongravid, lep_gravid,
                na.rm = TRUE
            ),
            adult_cals = sum(
                cal_mot, cal_gravid,
                na.rm = TRUE
            ),
            all_cals = sum(cal_cope, cal_mot, cal_gravid, na.rm = TRUE),
            all_chals = sum(chal_a, chal_b, unid_chal, na.rm = TRUE),
            all_lice = sum(
                lep_cope, cal_cope, chal_a, chal_b, lep_pa_male,
                lep_pa_female, lep_male, lep_nongravid, lep_gravid,
                cal_mot, cal_gravid, unid_cope, unid_chal, unid_pa,
                unid_adult,
                na.rm = TRUE
            )
        )

    # now add in a "season" variable ===========================================

    # find the number of rounds a year
    fish_data %>%
        dplyr::group_by(year) %>%
        summarize(n = dplyr::n_distinct(round))
    fish_data <- rbind(
        (
            fish_data %>% dplyr::filter(year == 2019) %>%
                dplyr::rowwise() %>%
                dplyr::mutate(
                    season = ifelse(round %in% c(1, 2), "early", "late")
                )
        ),
        (
            fish_data %>% dplyr::filter(year == 2020) %>%
                dplyr::rowwise() %>%
                dplyr::mutate(
                    season = ifelse(round == 1, "early", "late")
                )
        ),
        (
            fish_data %>% dplyr::filter(year %in% c(2021, 2022, 2023)) %>%
                dplyr::rowwise() %>%
                dplyr::mutate(
                    season = ifelse(round %in% c(1, 2, 3), "early", "late")
                )
        )
    )

    return(fish_data)
}

#' Foundation Theme
#'
#' This theme is designed to be a foundation from which to build new
#' themes, and not meant to be used directly. \code{theme_foundation()}
#' is a complete theme with only minimal number of elements defined.
#' It is easier to create new themes by extending this one rather
#' than \code{\link[ggplot2]{theme_gray}()} or \code{\link[ggplot2]{theme_bw}()},
#' because those themes define elements deep in the hierarchy.
#'
#' This theme takes \code{\link[ggplot2]{theme_gray}()} and sets all
#' \code{colour} and \code{fill} values to \code{NULL}, except for the top-level
#' elements (\code{line}, \code{rect}, and \code{title}), which have
#' \code{colour = "black"}, and \code{fill = "white"}. This leaves the spacing
#' and-non colour defaults of the default \pkg{ggplot2} themes in place.
#'
#' @export
#' @inheritParams ggplot2::theme_grey
#'
#' @family themes
#' @export
#' @importFrom ggplot2 theme_grey
theme_foundation <- function(base_size = 12, base_family = "") {
    thm <- theme_grey(base_size = base_size, base_family = base_family)
    for (i in names(thm)) {
        if ("colour" %in% names(thm[[i]])) {
            thm[[i]]["colour"] <- list(NULL)
        }
        if ("fill" %in% names(thm[[i]])) {
            thm[[i]]["fill"] <- list(NULL)
        }
    }
    thm + theme(
        panel.border = element_rect(fill = NA),
        legend.background = element_rect(colour = NA),
        line = element_line(colour = "black"),
        rect = element_rect(fill = "white", colour = "black"),
        text = element_text(colour = "black")
    )
}

#' Theme Base
#'
#' Theme similar to the default settings of the \sQuote{base} R graphics.
#'
#' @inheritParams ggplot2::theme_bw
#' @export
#' @family themes
#' @example inst/examples/ex-theme_base.R
theme_base <- function(base_size = 18, base_family = "") {
    theme_foundation() +
        theme(
            line = element_line(
                colour = "black",
                lineend = "round",
                linetype = "solid"
            ),
            rect = element_rect(
                fill = "white",
                colour = "black",
                linetype = "solid",
                linewidth = 1.2
            ),
            text = element_text(
                colour = "black",
                face = "plain",
                family = base_family,
                size = base_size,
                vjust = 0.5,
                hjust = 0.5,
                lineheight = 1,
            ),
            panel.grid = element_blank(),
            strip.background = element_rect(colour = NA),
            legend.key = element_rect(colour = NA),
            title = element_text(size = rel(1)),
            plot.title = element_text(
                size = rel(1.4), face = "bold",
                hjust = 0.5
            ),
            axis.title = element_text(
                size = rel(1.15),
                face = "bold"
            ),
            axis.text = element_text(
                size = rel(1)
            ),
            strip.text = element_text(),
            axis.ticks.length = unit(0.5, "lines"),
            # add my addition here
            plot.background = element_rect(colour = NA)
        )
    # TODO: get margins right
}
