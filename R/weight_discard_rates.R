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
  dir = NULL
) {
  ncs_filtered <- ncs_data |>
    dplyr::select(year, fleet, median_ratio, sd_ratio) |>
    dplyr::mutate(
      catch_shares = FALSE
    ) |>
    dplyr::rename(
      discard_rate = median_ratio,
      sd = sd_ratio
    )
  ncs_pre_2011 <- ncs_filtered |>
    dplyr::filter(year < 2011)
  ncs_post_2011 <- ncs_filtered |>
    dplyr::filter(year >= 2011) |>
    dplyr::relocate(catch_shares, .after = fleet)
  cs_filtered <- cs_data |>
    dplyr::select(year, fleet, catch_shares, discard_rate) |>
    dplyr::mutate(
      sd = 0.015
    )
  bind_rates <- dplyr::bind_rows(ncs_post_2011, cs_filtered) |>
    tidyr::complete(
      year,
      fleet,
      catch_shares,
      fill = list(
        discard_rate = 0,
        sd = 0.015
      )
    )
  join_all <- dplyr::join_left(
    bind_rates,
    weight_data |> dplyr::select(year, fleet, catch_shares)
  )

  ncs_post_2011 <- ncs_filtered |>
    dplyr::filter(year >= 2011) |>
    dplyr::rename(
      ncs_discard_rate = discard_rate,
      ncs_sd = sd
    )
  cs_filtered <- cs_data |>
    dplyr::select(year, fleet, catch_shares, discard_rate) |>
    dplyr::mutate(
      sd = 0.015
    ) |>
    dplyr::rename(
      cs_discard_rate = discard_rate,
      cs_sd = sd
    )
  combined_rates <- dplyr::full_join(ncs_post_2011, cs_filtered) |>
    dplyr::mutate(
      cs_rate =
    )
  #month
  #fleet
}
