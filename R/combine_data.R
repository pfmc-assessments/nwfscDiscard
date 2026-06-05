#' Function to combine catch data with EM catch data
#'
#' Review rates for EM data was reduced below 100% starting in 2024.  This change
#' requires data these data to not be treated as a census but rather use bootstrapping
#' to estimate discard rates and uncertainty. `em_catch_data` starting in 2024 are added
#' to `catch_data` and are categorized as non-catch share for bootstrapping
#'
#' @param catch_data A data frame of WCGOP catch data that includes all species.
#'   This data frame will be used to check confidentiality.
#' @param em_catch_data A data frame of WCGOP EM catch data that includes all species.
#'
#' @author Chantel Wetzel
#' @export
#' @return list
#'
#'
combine_data <- function(
  catch_data,
  em_catch_data
) {
  cols_to_keep <- c(
    "TRIP_ID",
    "EMTRIP_ID",
    "HAUL_ID",
    "DRVID",
    "gear",
    "R_PORT_GROUP",
    "RYEAR",
    "YEAR",
    "R_STATE",
    "AREA",
    "SECTOR",
    "sector",
    "AVG_LAT",
    "CATCH_DISPOSITION",
    "species",
    "DIS_MT",
    "RET_MT"
  )
  em_cols <- cols_to_keep[cols_to_keep %in% colnames(em_catch_data)]
  em_late <- em_catch_data |>
    dplyr::select(tidyr::all_of(em_cols)) |>
    dplyr::rename(
      TRIP_ID = EMTRIP_ID,
      RYEAR = YEAR
    ) |>
    # !!!!!!!!!!  CHANGE THIS BEFORE FINISHING !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    dplyr::filter(RYEAR >= 2023) |>
    dplyr::mutate(
      sector = dplyr::case_when(
        sector == "Catch Shares EM" ~ "Catch Shares EM Low Review Rates",
        sector == "Midwater Hake EM" ~ "Midwter Hake EM Low Review Rates",
        sector == "Midwater Rockfish EM" ~
          "Midwater Rockfish EM Low Review Rates"
      )
    )

  em_early <- em_catch_data |>
    dplyr::select(tidyr::all_of(em_cols)) |>
    dplyr::filter(YEAR < 2024) |>
    dplyr::rename(RYEAR = YEAR)

  cd_cols <- cols_to_keep[cols_to_keep %in% colnames(catch_data)]
  catch_data_select <- catch_data |>
    dplyr::select(tidyr::all_of(cd_cols)) |>
    dplyr::select(-YEAR) |>
    dplyr::mutate(
      TRIP_ID = as.character(TRIP_ID),
      HAUL_ID = as.character(HAUL_ID)
    )

  catch_data_combined <- dplyr::bind_rows(
    tibble::tibble(catch_data_select),
    tibble::tibble(em_late)
  ) |>
    as.data.frame()
  data_list <- list()
  data_list$catch_data <- catch_data_combined
  data_list$em_catch_data <- em_early
  return(data_list)
}
