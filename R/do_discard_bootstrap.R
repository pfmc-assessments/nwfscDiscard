#' Bootstrap uncertainty and summarize WCGOP discard data
#'
#' @param dir Directory location to save files.
#' @param data A data frame of WCGOP catch data
#' @param species_name species name that should match the name in the ob data file, can be single species or multiple names (e.g. c("Gopher Rockfish", "Black and Yellow Rockfish"))
#' @param boot_number The number of bootstraps to conduct
#' @param gear_groups List of gear types to group together
#' (example: list(c("Bottom Trawl", "Midwater Trawl"), c("Hook & Line", "Pot", "Shrimp Trawl"))).
#' @param gear_names Vector of gear group names (example: c("trawl", "fixed gear")).
#' @param fleet_colname Column to use to determine areas for fleets (example: "r_state.x")
#' @param fleet_groups List of fleet groups to use (example: list(c("WA", "OR", "CA"))).
#' @param fleet_names Vector of fleet names (example: c("coastwide")).
#' @param seed_number The seed number.
#' @param rm_em_data Logical indicating whether EM records should be removed from the observer
#' catch records (OBCatch) data file.
#'
#'
#' @author Chantel Wetzel, Allan Hicks, and Jason Jannot
#' @export
#'
#
do_discard_bootstrap <- function(
    data,
    species_name,
    boot_number,
    gear_groups,
    gear_names,
    fleet_colname,
    fleet_groups,
    fleet_names,
    dir = NULL,
    seed_number = 1,
    rm_em_data = FALSE) {
  nwfscSurvey::check_dir(dir = dir)
  if (!species_name %in% data[, "species"]) {
    cli::cli_abort("{species_name} not found in the data.")
  }

  # Format the observer catch data column names
  if (sum(colnames(data) == "TRIP_ID") == 1) {
    data <- data |> dplyr::select(-MT, -SPGRFTOB1, -SCIENTIFIC_NAME)
  }

  data <- data |>
    dplyr::rename(
      gear_to_use = gear
    ) |>
    dplyr::rename_with(
      tolower
    )
  # This column is not in the EM data, hence the need for a check:
  if ("ryear" %in% colnames(data)) {
    data[, "year"] <- data[, "ryear"]
  }

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
    dir = dir,
    data = data,
    gear_groups = gear_groups,
    gear_names = gear_names,
    fleet_colname = fleet_colname,
    fleet_groups = fleet_groups,
    fleet_names = fleet_names
  )

  data <- data |> dplyr::filter(species == species_name)
  ncs_data_out <- cs_data_out <- em_data_out <- NULL

  if (rm_em_data & sum(colnames(data) == "trip_id") == 1) {
    # Only do this if we are also processing the EM catch data file to avoid using records twice
    # Doing this after the confidentiality check since if they are removed the confidentiality check
    # should still include these vessels since the the discard rates from EM and non-EM catch share
    # vessels would likely be combined external to the data processing.
    data <- data |> dplyr::filter(sector != "Catch Shares EM")
  }

  if (sum(colnames(data) == "emtrip_id") == 1) {
    conf_data_check <- data_conf_check |> dplyr::filter(catch_shares == TRUE)
    em_data_out <- calc_cs_discards(
      dir = dir,
      data = data,
      conf_data_check = conf_data_check
    )
  }

  if (sum(colnames(data) == "trip_id") == 1) {
    cs_data <- data |> dplyr::filter(catch_shares == TRUE)
    ncs_data <- data |> dplyr::filter(catch_shares == FALSE)

    # calculate catch shares discard quantities
    if (nrow(cs_data) > 0) {
      conf_data_check <- data_conf_check |> dplyr::filter(catch_shares == TRUE)
      cs_data_out <- calc_cs_discards(
        dir = dir,
        data = cs_data,
        conf_data_check = data_conf_check
      )
    } else {
      cli::cli_inform("No catch share records found in the data.")
    }

    if (nrow(ncs_data) > 0) { # calculate catch shares discard quantities
      ncs_data_out <- boostrap_discard(
        dir = dir,
        data = ncs_data,
        boot_number = boot_number,
        boot_variable = "r_port_group",
        seed_number = seed_number
      )
    } else {
      cli::cli_inform("No non-catch share records found in the data.")
    }
  }

  if (!is.null(em_data_out)) {
    em <- em_data_out
    #if (!is.null(dir)) {
      #save(em, file = file.path(dir, "discards_em.rdata"))
    #}
    return(list(em = em))
  } else {
    if (!is.null(dir)) {
      ncs <- ncs_data_out
      cs <- cs_data_out
      #save(ncs, file = file.path(dir, "discards_noncatch_shares.rdata"))
      #save(cs, file = file.path(dir, "discards_catch_shares.rdata"))
    }
    return(list(cs = cs_data_out, ncs = ncs_data_out))
  }
}
