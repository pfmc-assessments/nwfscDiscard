#' Weight discard rates between catch share and non-catch share
#'
#' @param weight_data Data frame of the alternative GEMM data provided by the FOS
#'   team for calculating data weights using alternative areas. The gear type
#'   (minor) and the gear group (major) are used to calculate totals by area
#'   and weighting proportions.
#' @param cs_data Data object of the processed catch share discards from the OBCatch data.
#'   This should be the combined observer and EM catch share data from [combine_cs_discards()].
#' @param ncs_data Data object of the processed non0catch share discards from the OBCatch data.
#' @param dir Directory location to save files.
#' @param min_sd The minimum standard deviation to apply to catch share observations with full
#'   observer coverage.
#'
#' @author Chantel Wetzel
#' @export
#' @return list
#'
#'
weight_discard_rates <- function(
  weight_data,
  ncs_data,
  cs_data,
  dir = NULL,
  min_sd = 0.015
) {
  min_var <- min_sd * min_sd
  ncs_filtered <- ncs_data |>
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
    dplyr::select(year, fleet, catch_shares, discard_rate) |>
    dplyr::mutate(
      var = min_var
    )
  bind_rates <- dplyr::bind_rows(ncs_post_2011, cs_filtered) |>
    tidyr::complete(
      year,
      fleet,
      catch_shares,
      fill = list(
        discard_rate = 0,
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
      n = sum(discard_rate > 0)
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(discard_rate != 0) |>
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
        paste0("wcgop_discard_rates_weighted.csv")
      ),
      row.names = FALSE
    )
  }
  return(all_rates)
}
