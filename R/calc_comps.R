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
    comp_column = "length") {
  # Calculate the discard length frequencies
  comp_data <- data[!is.na(data[, comp_column]), ]
  # Perhaps add a check for lengths being available
  bins <- c(comp_bins, Inf)
  comp_data[, "bin"] <- bins[findInterval(comp_data[, comp_column], bins, all.inside = TRUE)]
  comp_data[, "bin"] <- factor(comp_data[, "bin"], levels = comp_bins)

  init_count <- comp_data |>
    dplyr::group_by(year, gear_groups, fleet_groups, sex, bin) |>
    dplyr::summarise(
      n = dplyr::n(),
      weighted = sum(wghtd_freq),
      .groups = "drop"
    ) |>
    dplyr::ungroup()

  filled_count <- init_count |>
    tidyr::complete(
      year, gear_groups, fleet_groups, sex, bin,
      fill = list(
        n = 0,
        weighted = 0
      )
    )

  all_weights <- filled_count |>
    dplyr::group_by(year, gear_groups, fleet_groups) |>
    dplyr::mutate(
      n_by_year = sum(n),
      w_by_year = sum(weighted)
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(n_by_year != 0) |>
    dplyr::mutate(
      prop_numbers = 100 * n / n_by_year,
      prop_weighted = 100 * weighted / w_by_year
    )

  # Format the composition data for SS3
  comps <- all_weights[, c("year", "gear_groups", "fleet_groups", "sex", "bin", "n_by_year", "prop_weighted")] |>
    tidyr::pivot_wider(
      names_from = bin,
      values_from = prop_weighted
    )

  comps_out_sexed <- comps_out_unsexed <- all_comps <- NULL
  out <- list()
  if (any(comps[, "sex"] == "U")) {
    sub <- comps[comps$sex == "U", ]
    keep <- which(apply(sub[, 6:ncol(sub)], 1, sum) != 0)
    filter_comps <- sub[keep, ]

    sample_size <- comp_data |>
      dplyr::filter(sex == "U") |>
      dplyr::group_by(year, gear_groups, fleet_groups) |>
      dplyr::summarise(
        fish = sum(frequency),
        hauls = length(unique(haul_id)),
        trips = length(unique(trip_id)),
        vessels = length(unique(drvid)),
        ratio = fish / trips,
        input_n = round(dplyr::case_when(
          ratio < 44 ~ trips + 0.138 * fish,
          .default = 7.06 * trips
        ), 0),
        fleet = paste0(unique(gear_groups), "-", unique(fleet_groups))
      ) |>
      data.frame()

    comps_out_unsexed <- cbind(
      sample_size[, "year"],
      "Month",
      sample_size[, "fleet"],
      0,
      1,
      sample_size[, "input_n"],
      filter_comps[filter_comps$sex == "U", 6:ncol(comps)],
      0 * filter_comps[filter_comps$sex == "U", 6:ncol(comps)]
    )
    colnames(comps_out_unsexed) <- c(
      "year", "month", "fleet", "sex", "partition", "input_n",
      paste0("f", comp_bins), paste0("m", comp_bins)
    )
    out$unsexed <- comps_out_unsexed
  }

  if ((sum(comps$sex == "F") + sum(comps$sex == "M")) > 0) {
    sample_size <- comp_data |>
      dplyr::filter(sex != "U") |>
      dplyr::group_by(year, gear_groups, fleet_groups) |>
      dplyr::summarise(
        fish = sum(frequency),
        hauls = length(unique(haul_id)),
        trips = length(unique(trip_id)),
        vessels = length(unique(drvid)),
        ratio = fish / trips,
        input_n = round(dplyr::case_when(
          ratio < 44 ~ trips + 0.138 * fish,
          .default = 7.06 * trips
        ), 0),
        fleet = paste0(unique(gear_groups), "-", unique(fleet_groups))
      ) |>
      data.frame()

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
      sample_size[, "year"],
      "Month",
      sample_size[, "fleet"],
      3,
      1,
      sample_size[, "input_n"],
      filter_comps
    )
    colnames(comps_out_sexed) <- c(
      "year", "month", "fleet", "sex", "partition", "input_n",
      paste0("f", comp_bins), paste0("m", comp_bins)
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
      colnames(comps_out_unsexed)[6:9] <- c("age_error", "age_low", "age_high", "input_n")
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
      colnames(comps_out_sexed)[6:9] <- c("age_error", "age_low", "age_high", "input_n")
      out$sexed <- comps_out_sexed
    }
  }
  all_comps <- dplyr::bind_rows(comps_out_sexed, comps_out_unsexed)

  sample_size <- comp_data |>
    dplyr::group_by(year, gear_groups, fleet_groups) |>
    dplyr::summarise(
      fish = sum(frequency),
      hauls = length(unique(haul_id)),
      trips = length(unique(trip_id)),
      vessels = length(unique(drvid)),
      ratio = fish / trips,
      input_n = round(dplyr::case_when(
        ratio < 44 ~ trips + 0.138 * fish,
        .default = 7.06 * trips
      ), 0)
    ) |>
    dplyr::select(-ratio) |>
    data.frame()

  if (!is.null(dir)) {
    if (dim(sample_size)[1] > 0) {
      write.csv(sample_size,
        file = file.path(dir, paste0("biological_sample_sizes_", comp_column, ".csv")),
        row.names = FALSE
      )
    }

    if (!is.null(all_comps)) {
      write.csv(all_comps,
        file = file.path(dir, paste0("biological_discard_", comp_column, "s.csv")),
        row.names = FALSE
      )
    }
  }
  return(all_comps)
}
