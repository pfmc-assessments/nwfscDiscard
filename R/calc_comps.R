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
  comp_data$bin <- bins[findInterval(comp_data[, comp_column], comp_bins, all.inside = TRUE)]
  comp_data$bin <- factor(comp_data$bin, levels = comp_bins)

  init_count <- comp_data |>
    dplyr::group_by(year, gear_groups, fleet_groups, bin) |>
    dplyr::summarise(
      n = dplyr::n(),
      weighted = sum(wghtd_freq),
      .groups = "drop"
    ) |>
    dplyr::ungroup()

  filled_count <- init_count |>
    tidyr::complete(
      year, gear_groups, fleet_groups, bin,
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
    dplyr::filter(n_by_year != 0)

  all_weights$prop_numbers <- 100 * all_weights$n / all_weights$n_by_year
  all_weights$prop_weighted <- 100 * all_weights$weighted / all_weights$w_by_year

  # Format the composition data for SS3
  comps <- all_weights[, c("year", "gear_groups", "fleet_groups", "bin", "n_by_year", "prop_weighted")] |>
    tidyr::pivot_wider(
      names_from = bin,
      values_from = prop_weighted
    )

  # Calculate based on all the observations rather than the ones filtered down to non-NA length or age alone.
  sample_size <- data |>
    dplyr::group_by(year, gear_groups, fleet_groups) |>
    dplyr::summarise(
      fish = sum(frequency),
      hauls = length(unique(haul_id)),
      trips = length(unique(trip_id)),
      vessels = length(unique(drvid)),
    )

  sample_size$nsamp <- round(ifelse(
    sample_size$fish / sample_size$trips < 44,
    sample_size$trips + 0.138 * sample_size$fish,
    7.06 * sample_size$hauls
  ), 0)

  fleet <- apply(comps[, c("gear_groups", "fleet_groups")], 1, paste, collapse = "-")

  comps_out <- cbind(
    comps[, "year"],
    "Month",
    fleet,
    0,
    1,
    sample_size$nsamp,
    comps[, 5:ncol(comps)]
  )
  colnames(comps_out)[c(2, 4, 5, 6)] <- c("month", "sex", "partition", "input_n")

  if (comp_column == "length") {
    remove <- which(is.na(apply(comps_out[, 7:ncol(comps_out)], 1, sum)))
    if (length(remove) > 0) {
      comps_out <- comps_out[-remove, ]
    }
  }

  if (comp_column == "age") {
    comps_out <- cbind(
      comps_out[, 1:5],
      "age_error",
      -1,
      -1,
      sample_size$nsamp,
      comps[, 5:ncol(comps)]
    )
    colnames(comps_out)[6:9] <- c("age_error", "age_low", "age_high", "nsamp")
    remove <- which(is.na(apply(comps_out[, 10:ncol(comps_out)], 1, sum)))
    if (length(remove) > 0) {
      comps_out <- comps_out[-remove, ]
    }
  }

  if (!is.null(dir)) {
    write.csv(sample_size,
      file = file.path(dir, paste0(tolower(species), "_wcgop_biological_sample_sizes_", comp_column, ".csv")),
      row.names = FALSE
    )

    write.csv(comps_out,
      file = file.path(dir, paste0(tolower(species), "_wcgop_discard_", comp_column, "s.csv")),
      row.names = FALSE
    )
  }
  return(comps_out)
}
