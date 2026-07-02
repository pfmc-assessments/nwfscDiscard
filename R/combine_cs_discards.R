#' Bootstrap uncertainty and summarize WCGOP discard data
#'
#' @param dir Directory where output will be saved. The directory where the file
#'   should be saved. If dir = NULL no output will be saved.
#' @param cs_data Data object of the processed catch share discards from the OBCatch data
#'   created by [get_discard_rates()].
#' @param em_data Data object of the processed em catch share discards from the EMCatch data
#'   created by [get_discard_rates()].
#'
#'
#' @author Chantel Wetzel
#' @export
#'
#
combine_cs_discards <- function(
  cs_data,
  em_data,
  dir = NULL
) {
  lifecycle::deprecate_stop(
    when = "2.1",
    what = "combine_cs_discards()",
    details = "This function is no longer used. The combine_catch_data() combines
    observer and EM data for use by get_discard_rates()."
  )
  data <- rbind(cs_data, em_data)

  combined_data <- data |>
    dplyr::group_by(year, fleet) |>
    dplyr::summarise(
      catch_shares = TRUE,
      n_obs = sum(n_obs),
      n_hauls = sum(n_hauls),
      n_trips = sum(n_trips),
      n_vessels = sum(n_vessels),
      observed_discard_mt = sum(observed_discard_mt),
      observed_retained_mt = sum(observed_retained_mt),
      discard_rate = round(
        observed_discard_mt / (observed_discard_mt + observed_retained_mt),
        3
      )
    ) |>
    as.data.frame()

  if (!is.null(dir)) {
    write.csv(
      combined_data,
      file = file.path(dir, "discard_rates_combined_catch_share.csv"),
      row.names = FALSE
    )
  }

  return(combined_data)
}
