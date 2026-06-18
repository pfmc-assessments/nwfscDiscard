#' Bootstrap uncertainty and summarize WCGOP discard data
#'
#' @param dir Directory location to save files.
#' @param data A data frame of WCGOP catch data that is from [combine_catch_data()]
#' @param species_name species name that should match the name in the ob data file, can be single species or multiple names (e.g. c("Gopher Rockfish", "Black and Yellow Rockfish"))
#' @param boot_number The number of bootstraps to conduct
#' @param gear_groups List of gear types to group together
#' (example: list(c("Bottom Trawl", "Midwater Trawl"), c("Hook & Line", "Pot", "Shrimp Trawl"))).
#' @param gear_names Vector of gear group names (example: c("trawl", "fixed gear")).
#' @param fleet_colname Column to use to determine areas for fleets (example: "r_state.x")
#' @param fleet_groups List of fleet groups to use (example: list(c("WA", "OR", "CA"))).
#' @param fleet_names Vector of fleet names (example: c("coastwide")).
#' @param seed_number The seed number.
#'
#'
#' @author Chantel Wetzel, Allan Hicks, and Jason Jannot
#' @export
#'
#
get_discard_rates <- function(
  data,
  species_name,
  gear_groups,
  gear_names,
  fleet_colname,
  fleet_groups,
  fleet_names,
  dir = NULL,
  boot_number = 10000,
  seed_number = 1
) {
  nwfscSurvey::check_dir(dir = dir)
  if (!species_name %in% data[, "species"]) {
    cli::cli_abort("{species_name} not found in the data.")
  }

  data <- data |>
    dplyr::rename(
      gear_to_use = gear
    ) |>
    dplyr::rename_with(
      tolower
    ) |>
    dplyr::rename(
      year = ryear
    )

  # Add the gear and area groupings to the data
  data <- create_groups(
    data = data,
    gear_groups = gear_groups,
    gear_names = gear_names,
    fleet_colname = fleet_colname,
    fleet_groups = fleet_groups,
    fleet_names = fleet_names
  )

  data_conf_check <- check_confidential(
    data = data,
    gear_groups = gear_groups,
    gear_names = gear_names,
    fleet_colname = fleet_colname,
    fleet_groups = fleet_groups,
    fleet_names = fleet_names,
    by_catch_share = FALSE
  )

  species_data <- data |> dplyr::filter(species == species_name)
  cs_data <- species_data |> dplyr::filter(catch_shares == TRUE)
  ncs_data <- species_data |> dplyr::filter(catch_shares == FALSE)

  # calculate catch shares discard quantities
  if (nrow(cs_data) > 0) {
    cs_data_out <- calc_cs_discards(
      dir = dir,
      data = cs_data,
      conf_data_check = data_conf_check
    )
  } else {
    cli::cli_alert_info("No catch share records found in the data.")
  }

  if (nrow(ncs_data) > 0) {
    # calculate catch shares discard quantities
    ncs_data_out <- boostrap_discard(
      dir = dir,
      data = ncs_data,
      conf_data_check = data_conf_check,
      boot_number = boot_number,
      boot_variable = "r_port_group",
      seed_number = seed_number
    )
  } else {
    cli::cli_alert_info("No non-catch share records found in the data.")
  }

  if (!is.null(dir)) {
    ncs <- ncs_data_out
    cs <- cs_data_out
  }
  return(list(cs = cs_data_out, ncs = ncs_data_out))
}
