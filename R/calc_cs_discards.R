#' Function that calculates discards for the catch share vessels
#'
#' @param dir Directory location to save files.
#' @param data A data frame of WCGOP catch data filtered down to only catch share data
#' @param conf_data_check Dataframe with the number of observations, trips, and vessels by fleet.
#'
#' @author Chantel Wetzel
#' @export
#'
calc_cs_discards <- function(
    data,
    conf_data_check,
    dir = NULL) {
  if (sum(colnames(data) == "emtrip_id") == 1) {
    add_name <- "em_"
  } else {
    add_name <- ""
  }

  discards <- data |>
    dplyr::group_by(year, fleet) |>
    dplyr::summarise(
      observed_discard_mt = sum(dis_mt),
      observed_retained_mt = sum(ret_mt),
      discard_rate = observed_discard_mt / (observed_discard_mt + observed_retained_mt)
    ) |>
    dplyr::ungroup()

  # Merge the confidential data check with the discard rates
  out <- dplyr::left_join(
    x = conf_data_check,
    y = discards,
    by = c("fleet", "year")
  ) |> dplyr::select(-gear_groups, -fleet_groups)


  if (!is.null(dir)) {
    write.csv(
      x = out |> dplyr::filter(n_vessels >= 3),
      file = file.path(dir, paste0("discards_rates_", add_name, "catch_share_.csv")),
      row.names = FALSE
    )
  } else {
    cli::cli_inform("No directory provided. Catch share discard rates not saved.")
  }
  return(out)
}
