#' Check confidentiality using the catch data
#'
#'
#' @param dir Directory location to save files.
#' @param data A data frame of WCGOP catch data
#' @param gear_groups List of gear types to group together
#' (example: list(c("Bottom Trawl", "Midwater Trawl"), c("Hook & Line", "Pot", "Shrimp Trawl"))).
#' @param gear_names Vector of gear group names (example: c("trawl", "fixed gear")).
#' @param fleet_colname Column to use to determine areas for fleets (example: "r_state.x")
#' @param fleet_groups List of fleet groups to use (example: list(c("WA", "OR", "CA"))).
#' @param fleet_names Vector of fleet names (example: c("coastwide")).
#'
#' @author Chantel Wetzel
#' @export
#'
#'
check_confidential <- function(
    data,
    gear_groups,
    gear_names,
    fleet_colname,
    fleet_groups,
    fleet_names,
    dir = NULL) {
  nwfscSurvey::check_dir(dir = dir)
  if (sum(colnames(data) == "emtrip_id") == 1) {
    data[, "trip_id"] <- data[, "emtrip_id"]
    add_name <- "_em"
  } else {
    add_name <- ""
  }

  if (sum(c("fleet_groups", "gear_groups") %in% colnames(data)) != 2) {
    if (sum(colnames(data) == "TRIP_ID") == 1) {
      # Remove duplicate columns
      data <- data |> dplyr::select(-MT, -SPGRFTOB1, -SCIENTIFIC_NAME)
    }
    data <- data |>
      dplyr::rename(
        gear_to_use = gear
      ) |>
      dplyr::rename_with(tolower)

    if ("ryear" %in% colnames(data)) {
      data[, "year"] <- data[, "ryear"]
    }

    if (fleet_colname == "r_state.x") {
      fleet_colname <- "r_state"
    }

    data <- create_groups(
      data = data,
      gear_groups = gear_groups,
      gear_names = gear_names,
      fleet_colname = fleet_colname,
      fleet_groups = fleet_groups,
      fleet_names = fleet_names
    )
  }

  vessels_by_year_cs <- data |>
    dplyr::group_by(year, gear_groups, fleet_groups, fleet, catch_shares) |>
    dplyr::reframe(
      n_obs = dplyr::n(),
      n_hauls = length(unique(haul_id)),
      n_trips = length(unique(trip_id)),
      n_vessels = length(unique(drvid))
    ) |>
    dplyr::ungroup()

  vessels_by_year_cs <- as.data.frame(vessels_by_year_cs)

  if (any(vessels_by_year_cs[, "n_vessels"] < 3)) {
    bad_fleet_group <- vessels_by_year_cs[which(vessels_by_year_cs[, "n_vessels"] < 3), "fleet"]
    bad_year <- vessels_by_year_cs[which(vessels_by_year_cs[, "n_vessels"] < 3), "year"]
    bad_cs <- vessels_by_year_cs[which(vessels_by_year_cs[, "n_vessels"] < 3), "catch_shares"]
    warn <- paste0(bad_year, "-", bad_fleet_group, "-", bad_cs)
    glue::glue("The fleet grouping does not meet confidentiality for {warn}.")
  }

  if (!is.null(dir)) {
    write.csv(vessels_by_year_cs,
      file = file.path(dir, paste0("confidentiality", add_name, "_by_catch_share.csv")),
      row.names = FALSE
    )
  }
  vessels_by_year_cs
  return(vessels_by_year_cs)
}
