#' Calculate discard, landings, and catch by gear for weighting data
#'
#'
#' @param data Data frame of the alternative GEMM data provided by the FOS
#'   team for calculating data weights using alternative areas. The gear type
#'   (minor) and the gear group (major) are used to calculate totals by area
#'   and weighting proportions.
#' @param dir Directory location to save files.
#'
#'
#'
#' @author Chantel Wetzel
#' @export
#'
#
calc_weights <- function(data, dir = NULL) {
  all_areas <- unique(data$area)
  cli::cli_alert_info(
    "The following areas are in the data: {all_areas}. Data will be grouped
    within each area."
  )
  weights <- data |>
    dplyr::rename(
      gear = lilboy_fleet,
      gear_type = bigboy_fleet
    ) |>
    dplyr::mutate(
      gear = dplyr::case_when(
        sector == "Pink Shrimp" ~ "Shrimp Trawl",
        .default = gear
      ),
      gear = dplyr::case_when(
        gear == "bottom_trawl" ~ "Bottom Trawl",
        gear == "fixed_gear" ~ "Fixed Gears",
        gear == "hkl" ~ "Hook & Line",
        gear == "midwater_trawl" ~ "Midwater Trawl",
        gear == "pot" ~ "Pot",
        .default = gear
      )
    ) |>
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
      prop_discard = round(total_discard_mt / unique(gear_group_discard_mt), 4),
      prop_landed = round(total_landed_mt / unique(gear_group_landings_mt), 4),
      prop_catch = round(total_catch_mt / unique(gear_group_catch_mt), 4)
    ) |>
    dplyr::ungroup()
  if (!is.null(dir)) {
    write.csv(
      x = weights,
      file = file.path(dir, "weights_alt_gemm.csv"),
      row.names = FALSE
    )
  }
  return(weights)
}
