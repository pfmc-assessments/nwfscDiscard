#' Weight discard rates between catch share and non-catch share
#'
#' @param weight_data Data frame of the alternative GEMM or GEMM data provided by the FOS
#'   team for calculating data weights created by [get_weights()].
#' @param cs_data Data object of the processed catch share discards from catch data
#'   created by [get_discard_rates()].
#' @param ncs_data Data object of the processed non-catch share discards from the catch
#'   data created by [get_discard_rates()].
#' @param dir Directory where output will be saved. The directory where the file
#'   should be saved. If dir = NULL no output will be saved.
#' @param min_sd Numeric value that is defines minimum standard deviation to apply
#'   to catch share observations with full observer coverage.
#'
#' @author Chantel Wetzel
#' @export
#' @return dataframe
#'
#'
weight_discard_rates <- function(
  weight_data,
  ncs_data,
  cs_data,
  dir = NULL,
  min_sd = 0.015
) {
  # check for fleet name alignment
  weight_names <- unique(weight_data$fleet)
  data_names <- unique(c(unique(ncs_data$fleet), unique(cs_data$fleet)))
  if (any(!data_names %in% weight_names)) {
    cli::cli_abort(
      "The weight data has the following fleet names: {weight_names}.
       The data has the following fleet names: {data_names}.
      "
    )
  }
  min_var <- min_sd * min_sd
  # For rates that were based on <3 observations set those to 0
  ncs_filtered <- ncs_data |>
    dplyr::mutate(
      median_ratio = dplyr::case_when(
        n_ret < 3 ~ NA,
        # using retained observations for now but could switch to discard or both
        .default = median_ratio
      )
    ) |>
    dplyr::select(year, fleet, median_ratio, var_ratio) |>
    dplyr::mutate(
      catch_shares = FALSE
    ) |>
    dplyr::rename(
      discard_rate = median_ratio,
      var = var_ratio
    )
  ncs_pre_2011 <- ncs_filtered |>
    dplyr::filter(year < 2011) |>
    dplyr::mutate(
      month = 7,
      discard_rate = round(discard_rate, 4),
      sd = round(sqrt(var), 4)
    ) |>
    dplyr::select(-catch_shares, -var) |>
    dplyr::relocate(
      month,
      .after = year
    )
  ncs_post_2011 <- ncs_filtered |>
    dplyr::filter(year >= 2011) |>
    dplyr::relocate(catch_shares, .after = fleet)
  cs_filtered <- cs_data |>
    dplyr::mutate(
      discard_rate = dplyr::case_when(
        n_ret < 3 ~ NA,
        # using retained observations for now but could switch to discard or both
        .default = discard_rate
      )
    ) |>
    dplyr::select(year, fleet, discard_rate) |>
    dplyr::mutate(
      catch_shares = TRUE,
      var = min_var
    )
  bind_rates <- dplyr::bind_rows(ncs_post_2011, cs_filtered) |>
    tidyr::complete(
      year,
      fleet,
      catch_shares,
      fill = list(
        discard_rate = NA,
        var = min_var
      )
    )
  join_all <- dplyr::left_join(
    bind_rates,
    weight_data |> dplyr::select(-gear_type),
    by = c("year", "catch_shares", "fleet")
  )

  combine_rates <- join_all |>
    dplyr::group_by(year, fleet) |>
    dplyr::mutate(
      n = sum(!is.na(discard_rate))
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(discard_rate)) |>
    dplyr::mutate(
      weighted_rate = dplyr::case_when(
        n == 2 ~ discard_rate * prop_catch,
        .default = discard_rate
      ),
      weighted_var = dplyr::case_when(
        n == 2 ~ var * prop_catch * prop_catch,
        .default = var
      )
    ) |>
    dplyr::summarise(
      .by = c("year", "fleet"),
      month = 7,
      discard_rate = round(sum(weighted_rate), 4),
      sd = round(sqrt(sum(weighted_var)), 4)
    ) |>
    dplyr::relocate(month, .after = year)

  all_rates <- dplyr::bind_rows(
    ncs_pre_2011,
    combine_rates
  ) |>
    dplyr::mutate(
      sd = dplyr::case_when(
        sd < min_sd ~ min_sd,
        .default = sd
      )
    ) |>
    dplyr::arrange(fleet, year)

  if (!is.null(dir)) {
    write.csv(
      all_rates,
      file = file.path(
        dir,
        "wcgop_discard_rates_weighted.csv"
      ),
      row.names = FALSE
    )
  }
  return(all_rates)
}
