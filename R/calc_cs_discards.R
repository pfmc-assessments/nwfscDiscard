#' Function that calculates discards for the catch share vessels
#'
#' @param dir Directory where output will be saved. The directory where the file
#'   should be saved. If dir = NULL no output will be saved.
#' @param data A data frame of WCGOP catch data filtered down to only catch share data
#'   created by [get_discard_rates()].
#'   group data for bootstrapping.
#' @param conf_data_check Dataframe with the number of observations, trips, and
#'   vessels by fleet created by [check_confidential()].
#'
#' @author Chantel Wetzel
#' @export
#'
calc_cs_discards <- function(
  data,
  conf_data_check,
  dir = NULL
) {
  actual_obs <- data |>
    dplyr::summarise(
      .by = c("year", "fleet"),
      n_ret = sum(ret_mt > 0),
      n_dis = sum(dis_mt > 0)
    )

  discards <- data |>
    dplyr::summarise(
      .by = c("year", "fleet"),
      observed_discard_mt = sum(dis_mt),
      observed_retained_mt = sum(ret_mt),
      discard_rate = observed_discard_mt /
        (observed_discard_mt + observed_retained_mt)
    ) |>
    dplyr::ungroup()

  discard_and_obs <- dplyr::left_join(
    x = discards,
    y = actual_obs,
    by = c("fleet", "year")
  )

  if ("catch_shares" %in% colnames(conf_data_check)) {
    conf_data_check <- conf_data_check |>
      dplyr::filter(catch_shares == TRUE) |>
      dplyr::select(-catch_shares)
  }

  # Merge the confidential data check with the discard rates
  out <- dplyr::right_join(
    x = conf_data_check,
    y = discard_and_obs,
    by = c("fleet", "year")
  ) |>
    dplyr::filter(n_vessels >= 3)

  if (!is.null(dir)) {
    write.csv(
      x = out,
      file = file.path(
        dir,
        "wcgop_discards_rates_catch_share.csv"
      ),
      row.names = FALSE
    )
  } else {
    cli::cli_alert_info(
      "No directory provided. Catch share discard rates not saved."
    )
  }
  return(out)
}
