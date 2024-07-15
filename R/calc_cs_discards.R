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
    dir = NULL,
    data,
    conf_data_check) {
  if (any(data$catch_shares == FALSE)) {
    data <- data[data$catch_shares == TRUE, ]
  }

  if (sum(colnames(data) == "emtrip_id") == 1) {
    add_name <- "_em"
  } else {
    add_name <- NULL
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
  )

  if (!is.null(dir)) {
    species <- tolower(unique(data[, "species"]))
    remove <- ifelse(out$n_vessels >= 3, FALSE, TRUE)
    if (any(remove)) {
      out_redacted <- out
      out_redacted[remove, c("observed_discard_mt", "observed_retained_mt", "discard_rate")] <- "confidential"
      write.csv(out_redacted,
        file = file.path(dir, paste0(tolower(species), add_name, "_catch_share_discards_redacted.csv")),
        row.names = FALSE
      )
    }

    write.csv(out,
      file = file.path(dir, paste0(tolower(species), add_name, "_catch_share_discards.csv")),
      row.names = FALSE
    )
  } else {
    warning("No directory provided. Catch share discard rates not saved.")
  }
  return(out)
}
