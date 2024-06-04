#' Bootstrap uncertainty and summarize WCGOP discard data
#'
#' @param dir Directory location to save files.
#' @param data A data frame of WCGOP catch data
#' @param species species name that should match the name in the ob data file, can be single species or multiple names (e.g. c("Gopher Rockfish", "Black and Yellow Rockfish"))
#' @param boot_number The number of bootstraps to conduct
#' @param gear_groups List of gear types to group together
#' (example: list(c("Bottom Trawl", "Midwater Trawl"), c("Hook & Line", "Pot", "Shrimp Trawl"))).
#' @param gear_name Vector of gear group names (example: c("trawl", "fixed gear")).
#' @param fleet_colname Column to use to determine areas for fleets (example: "r_state.x")
#' @param fleet_groups List of fleet groups to use (example: list(c("WA", "OR", "CA"))).
#' @param fleet_names Vector of fleet names (example: c("coastwide")).
#' @param ratio_type
#'
#'
#' @author Chantel Wetzel, Allan Hicks, and Jason Jannot
#' @export
#'
#
do_discard_bootstrap <- function(
  dir = NULL,
  data,
  species,
  boot_number,
  gear_groups,
  gear_names,
  fleet_colname,
  fleet_groups,
  fleet_names,
  seed_number = 1,
  redact = TRUE) {


  # Format the observer catch data column names
  if (sum(colnames(data) == "TRIP_ID") == 1) {
    data <- data[, which(!colnames(data) %in% c("MT", "SPGRFTOB1", "SCIENTIFIC_NAME"))]
    colnames(data)[which(colnames(data) == "gear")] <- "gear_to_use"
    colnames(data) <- tolower(colnames(data))
    if ("ryear" %in% colnames(data)) {
      data$year <- data$ryear
    }
  }
  # To Do: Add formatting for EM catch data


  if (species %in% data[, "species"]) {
    data <- data[data$species %in% species, ]
  } else {
    stop(glue::glue("{species} not found in the data."))
  }

  # Add the gear and area groupings to the data
  data <- create_groups(
    data = data,
    gear_groups = gear_groups,
    gear_names = gear_names,
    fleet_colname = fleet_colname,
    fleet_groups = fleet_groups,
    fleet_names = fleet_names)

  data_conf_check <- check_confidential(
    dir = dir,
    data = data,
    species = species,
    gear_groups = gear_groups,
    gear_names = gear_names,
    fleet_colname = fleet_colname,
    fleet_groups = fleet_groups,
    fleet_names = fleet_names)


  ind <- which(data$catch_shares == TRUE)
  cs_data <- data[ind, ]
  ncs_data <- data[-ind, ]

  # calculate catch shares discard quantities
  if(nrow(cs_data) > 0) {
    conf_data_check <- data_conf_check[[2]]
    conf_data_check <- conf_data_check[conf_data_check$catch_shares == TRUE, ]
    cs_data_out <- calc_cs_discards(
      dir = dir,
      data = cs_data,
      conf_data_check = conf_data_check,
      redact = redact)

  } else {
    message("No catch share records found in the data.")
    cs_data_out <- NULL
  }

  if(nrow(ncs_data) > 0) { #calculate catch shares discard quantities
    ncs_data_out <- boostrap_discard(
      dir = dir,
      data = ncs_data,
      boot_number = boot_number,
      boot_variable = "r_port_group",
      seed = seed_number,
      redact = redact)
  } else {
    message("No noncatch share records found in the data.")
    ncs_data_out <- NULL
  }

  return(list(cs = cs_data_out, ncs = ncs_data_out))
}
