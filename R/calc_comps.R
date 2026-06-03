#' Calculate expanded discard composition
#'
#'
#' @param dir Directory location to save files.
#' @param data A data frame of WCGOP biological data
#' @param comp_bins Length or age bins to apply to the composition data.
#' @param comp_column Column name for the type of composition data to process (e.g., "length" or "age")
#'
#' @author Chantel Wetzel
#' @export
#'
#'
calc_comps <- function(
  dir = NULL,
  data,
  comp_bins,
  comp_column = "length"
) {
  # Calculate the discard length frequencies
  comp_data <- data[!is.na(data[, comp_column]), ]
  # Perhaps add a check for lengths being available
  bins <- c(comp_bins, Inf)
  comp_data[, "bin"] <- bins[findInterval(
    comp_data[, comp_column],
    bins,
    all.inside = TRUE
  )]
  comp_data[, "bin"] <- factor(comp_data[, "bin"], levels = comp_bins)
  if (comp_column == "length") {
    comp_data$n <- comp_data$n_length
    comp_data$weight <- comp_data$final_weight_length_capped
  } else {
    comp_data$n <- comp_data$n_age
    comp_data$weight <- comp_data$final_weight_age_capped
  }

  # calculate input n similar to pacfintools
  comp_weights <- comp_data |>
    dplyr::group_by(year, fleet, sex) |>
    dplyr::mutate(
      hauls = dplyr::n_distinct(haul_id),
      trips = dplyr::n_distinct(trip_id),
      fish = sum(n),
      ratio = sum(unique(fish)) / trips,
      input_n = dplyr::case_when(
        ratio < 44 ~ trips + 0.138 * sum(unique(fish)),
        .default = 7.06 * trips
      ),
      total_weight_year = sum(weight)
    ) |>
    dplyr::ungroup()

  init_count <- comp_weights |>
    dplyr::summarise(
      .by = c(
        "year",
        "fleet",
        "sex",
        "bin"
      ),
      input_n = unique(input_n),
      bin_weight = sum(weight),
      total_weight_year = unique(total_weight_year)
    ) |>
    dplyr::ungroup()

  filled_count <- init_count |>
    tidyr::complete(
      year,
      fleet,
      sex,
      bin,
      fill = list(
        input_n = 0,
        bin_weight = 0
      )
    )

  all_weights <- filled_count |>
    dplyr::filter(input_n != 0) |>
    dplyr::group_by(year, fleet) |>
    dplyr::mutate(
      prop_weighted = 100 * bin_weight / unique(total_weight_year)
    )

  # Format the composition data for SS3
  comps <- all_weights[, c(
    "year",
    "fleet",
    "sex",
    "bin",
    "input_n",
    "prop_weighted"
  )] |>
    dplyr::arrange(bin, year, fleet) |>
    tidyr::pivot_wider(
      names_from = bin,
      values_from = prop_weighted
    )

  comps_out_sexed <- comps_out_unsexed <- all_comps <- NULL
  out <- list()
  if (any(comps[, "sex"] == "U")) {
    filter_comps <- comps[comps$sex == "U", ]
    filter_comps[is.na(filter_comps)] <- 0

    comps_out_unsexed <- cbind(
      data.frame(
        year = filter_comps$year,
        month = 7,
        fleet = filter_comps$fleet,
        sex = 0,
        partition = 1,
        input_n = filter_comps$input_n
      ),
      filter_comps[filter_comps$sex == "U", 5:ncol(filter_comps)],
      0 * filter_comps[filter_comps$sex == "U", 5:ncol(comps)]
    )
    colnames(comps_out_unsexed[, 7:dim(comps_out_unsexed)[2]]) <- c(
      paste0("f", comp_bins),
      paste0("m", comp_bins)
    )
    out$unsexed <- comps_out_unsexed
  }

  if ((sum(comps$sex == "F") + sum(comps$sex == "M")) > 0) {
    comps_sexed <- cbind(
      comps[comps$sex == "F", 6:ncol(comps)],
      comps[comps$sex == "M", 6:ncol(comps)]
    ) |>
      data.frame()
    if (sum(apply(comps_sexed, 1, sum) == 0) > 0) {
      remove <- which(apply(comps_sexed, 1, sum) == 0)
      filter_comps <- comps_sexed[-remove, ]
    } else {
      filter_comps <- comps_sexed
    }

    comps_out_sexed <- cbind(
      data.frame(
        year = filter_comps$year,
        month = 7,
        fleet = filter_comps$fleet,
        sex = 0,
        partition = 1,
        input_n = filter_comps$input_n
      ),
      comps_sexed
    )
    colnames(comps_out_sexed[, 7:dim(comps_out_sexed)[2]]) <- c(
      paste0("f", comp_bins),
      paste0("m", comp_bins)
    )
    out$sexed <- comps_out_sexed
  }

  if (comp_column == "age") {
    if (!is.null(comps_out_unsexed)) {
      comps_out_unsexed <- dplyr::bind_cols(
        comps_out_unsexed[, 1:5],
        "age_error",
        -1,
        -1,
        comps_out_unsexed[, "input_n"],
        comps_out_unsexed[, 7:ncol(comps_out_unsexed)]
      )
      colnames(comps_out_unsexed)[6:9] <- c(
        "age_error",
        "age_low",
        "age_high",
        "input_n"
      )
      out$unsexed <- comps_out_unsexed
    }
    if (!is.null(comps_out_sexed)) {
      comps_out_sexed <- cbind(
        comps_out_sexed[, 1:5],
        "age_error",
        -1,
        -1,
        comps_out_sexed[, "input_n"],
        comps_out_sexed[, 7:ncol(comps_out_sexed)]
      )
      colnames(comps_out_sexed)[6:9] <- c(
        "age_error",
        "age_low",
        "age_high",
        "input_n"
      )
      out$sexed <- comps_out_sexed
    }
  }
  all_comps <- dplyr::bind_rows(comps_out_sexed, comps_out_unsexed)

  sample_size <- comp_data |>
    dplyr::mutate(
      sex_group = dplyr::case_when(
        sex == "U" ~ "unsexed",
        .default = "sexed"
      )
    ) |>
    dplyr::summarise(
      .by = c(year, fleet, sex_group),
      hauls = dplyr::n_distinct(haul_id),
      trips = dplyr::n_distinct(trip_id),
      fish = sum(n),
      ratio = sum(unique(fish)) / trips,
      input_n = round(
        dplyr::case_when(
          ratio < 44 ~ trips + 0.138 * sum(unique(fish)),
          .default = 7.06 * trips
        ),
        0
      )
    ) |>
    dplyr::select(-ratio) |>
    dplyr::arrange(fleet, year) |>
    data.frame()

  if (!is.null(dir)) {
    if (dim(sample_size)[1] > 0) {
      write.csv(
        sample_size,
        file = file.path(
          dir,
          paste0("biological_sample_sizes_", comp_column, ".csv")
        ),
        row.names = FALSE
      )
    }

    if (!is.null(all_comps)) {
      write.csv(
        all_comps,
        file = file.path(
          dir,
          paste0("biological_discard_", comp_column, "s.csv")
        ),
        row.names = FALSE
      )
    }
  }
  return(all_comps)
}
