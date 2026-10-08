#' Function to combine EM catch data
#'
#' Review rates for EM data was reduced below 100% starting in 2024.  This change
#' requires data these data to not be treated as a census but rather use bootstrapping
#' to estimate discard rates and uncertainty. `em_catch_data` starting in 2024 are added
#' to `catch_data` and are categorized as non-catch share for bootstrapping
#'
#' @param em_catch_data_census A data frame of WCGOP catch data that includes all species for
#'   the years of 2015-2023 where there was 100% review rate.
#' @param em_catch_data_subset A data frame of WCGOP EM catch data that includes all species
#'   for the years of 2024+ where the video review rate was reduced to 10% of hauls.
#'
#' @author Chantel Wetzel
#' @export
#' @return dataframe
#'
#'
combine_catch_data <- function(
  em_catch_data_census,
  em_catch_data_subset
) {
  port_groups <- get(utils::data(
    "port_groups",
    overwrite = TRUE,
    package = "nwfscDiscard"
  ))

  port_groups_format <- port_groups |>
    dplyr::mutate(
      r_state = dplyr::if_else(
        agency_code == "C",
        "CA",
        dplyr::if_else(agency_code == "O", "OR", "CA")
      )
    ) |>
    dplyr::rename(
      lb_return_port = pacfin_port_code,
      r_port_group = wcgop_port_group_code
    )

  em_joined <- dplyr::left_join(
    x = em_catch_data_subset |> dplyr::rename_with(tolower),
    y = port_groups_format |>
      dplyr::select(lb_return_port, r_port_group, r_state),
    by = dplyr::join_by(lb_return_port),
    relationship = "many-to-many"
  )

  cols_to_keep <- tolower(c(
    "EMTRIP_ID",
    "TRIP_ID",
    "HAUL_ID",
    "DRVID",
    "gear",
    "R_PORT_GROUP",
    "RYEAR",
    "YEAR",
    "R_STATE",
    "AREA",
    "sector",
    "AVG_LAT",
    "CATCH_DISPOSITION",
    "species",
    "DIS_MT",
    "RET_MT"
  ))
  em_cols <- cols_to_keep[
    tolower(cols_to_keep) %in% tolower(colnames(em_catch_data_census))
  ]

  # Need to add r_port_group to new em
  combined_data <- dplyr::bind_rows(
    em_joined |>
      dplyr::rename(
        emtrip_id = trip_id
      ) |>
      dplyr::mutate(
        area = dplyr::if_else(avg_lat >= 40.1667, "NORTH", "SOUTH")
      ) |>
      dplyr::select(tidyr::all_of(em_cols)),
    em_catch_data_census |>
      dplyr::rename_with(tolower) |>
      dplyr::select(tidyr::all_of(em_cols))
  )
  return(combined_data)
}
