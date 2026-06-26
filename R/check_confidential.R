#' Check confidentiality using the catch data
#'
#'
#' @param data A data frame of WCGOP catch data
#' @param gear_groups List of gear types to group together
#' (example: list(c("Bottom Trawl", "Midwater Trawl"), c("Hook & Line", "Pot", "Shrimp Trawl"))).
#' @param gear_names Vector of gear group names (example: c("trawl", "fixed gear")).
#' @param fleet_colname Column to use to determine areas for fleets (example: "r_state.x")
#' @param fleet_groups List of fleet groups to use (example: list(c("WA", "OR", "CA"))).
#' @param fleet_names Vector of fleet names (example: c("coastwide")).
#' @param dir Directory location to save files.
#' @param  by_catch_share Logical. Calculate the number of unique vessels by catch
#'   share for each year and fleet.
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
  by_catch_share = FALSE,
  dir = NULL
) {
  nwfscSurvey::check_dir(dir = dir)

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

  if (by_catch_share) {
    var <- c("year", "fleet", "catch_shares")
    add_name <- "_by_catch_shares"
  } else {
    var <- c("year", "fleet")
    add_name <- ""
  }
  vessels_by_year <- data |>
    dplyr::group_by_at(var) |>
    dplyr::reframe(
      n_obs = dplyr::n(),
      n_hauls = length(unique(haul_id)),
      n_trips = length(unique(trip_id)),
      n_vessels = length(unique(drvid))
    ) |>
    dplyr::ungroup() |>
    as.data.frame()

  if (any(vessels_by_year[, "n_vessels"] < 3)) {
    bad_fleet_group <- vessels_by_year[
      which(vessels_by_year[, "n_vessels"] < 3),
      "fleet"
    ]
    bad_year <- vessels_by_year[
      which(vessels_by_year[, "n_vessels"] < 3),
      "year"
    ]
    if ("catch_shares" %in% colnames(vessels_by_year)) {
      bad_cs <- paste0(
        "catch share:",
        vessels_by_year[
          which(vessels_by_year[, "n_vessels"] < 3),
          "catch_shares"
        ]
      )
    } else {
      bad_cs <- ""
    }
    warn <- paste0(bad_year, "-", bad_fleet_group, "-", bad_cs)
    cli::cli_alert_warning(
      "The fleet grouping does not meet confidentiality for {warn}."
    )
  }

  if (!is.null(dir)) {
    write.csv(
      vessels_by_year,
      file = file.path(
        dir,
        paste0("wcgop_confidentiality", add_name, ".csv")
      ),
      row.names = FALSE
    )
  }
  vessels_by_year
  return(vessels_by_year)
}
