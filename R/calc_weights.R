#' Calculate discard, landings, and catch by gear for weighting data
#'
#'
#' @param data Data frame of the alternative GEMM data provided by the FOS
#'   team for calculating data weights using alternative areas. The gear type
#'   (minor) and the gear group (major) are used to calculate totals by area
#'   and weighting proportions.
#' @param include_catch_share TRUE/FALSE
#' @param dir Directory location to save files.
#'
#'
#'
#' @author Chantel Wetzel
#' @export
#'
#
calc_weights <- function(data, include_catch_share = TRUE, dir = NULL) {
  all_areas <- unique(data$area)
  cli::cli_alert_info(
    "The following areas are in the data: {all_areas}. Data will be grouped
    within each area."
  )
  if (length(unique(data$species)) > 1) {
    cli::cli_abort(
      "Data from more than one species in included in the data frame.  Please
      filter down to a select species."
    )
  }

  if (include_catch_share) {
    weights <- data |>
      dplyr::group_by(year, gear_type) |>
      dplyr::mutate(
        total_discard_mt = sum(gemm_dis_est_area, na.rm = TRUE),
        total_landed_mt = sum(landings_area, na.rm = TRUE),
        total_catch_mt = total_discard_mt + total_landed_mt
      ) |>
      dplyr::ungroup() |>
      dplyr::summarise(
        .by = c("year", "catch_shares", "gear_type"),
        gear_discard_mt = sum(gemm_dis_est_area, na.rm = TRUE),
        gear_landings_mt = sum(landings_area, na.rm = TRUE),
        gear_catch_mt = sum(gear_discard_mt + gear_landings_mt, na.rm = TRUE),
        prop_discard = round(gear_discard_mt / unique(total_discard_mt), 4),
        prop_landed = round(gear_landings_mt / unique(total_landed_mt), 4),
        prop_catch = round(gear_catch_mt / unique(total_catch_mt), 4)
      ) |>
      dplyr::ungroup()
  } else {
    weights <- data |>
      dplyr::summarise(
        .by = c("year", "area", "gear", "gear_type"),
        total_discard_mt = round(sum(gemm_dis_est_area, na.rm = TRUE), 4),
        total_landed_mt = round(sum(landings_area, na.rm = TRUE), 4),
        total_catch_mt = round(total_discard_mt + total_landed_mt, 4)
      ) |>
      dplyr::group_by(year, area, gear_type) |>
      dplyr::mutate(
        gear_group_discard_mt = sum(total_discard_mt),
        gear_group_landings_mt = sum(total_landed_mt),
        gear_group_catch_mt = sum(total_catch_mt)
      ) |>
      dplyr::group_by(year, area, gear) |>
      dplyr::mutate(
        prop_discard = round(
          total_discard_mt / unique(gear_group_discard_mt),
          4
        ),
        prop_landed = round(
          total_landed_mt / unique(gear_group_landings_mt),
          4
        ),
        prop_catch = round(total_catch_mt / unique(gear_group_catch_mt), 4)
      ) |>
      dplyr::ungroup()
  }
  if (!is.null(dir)) {
    write.csv(
      x = weights,
      file = file.path(dir, "weights_alt_gemm.csv"),
      row.names = FALSE
    )
  }
  return(weights)
}
