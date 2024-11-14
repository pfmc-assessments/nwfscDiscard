#' Calculate mean weights for discarded fish
#'
#'
#' @param dir Directory location to save files.
#' @param data A data frame of WCGOP catch data
#' @param species Species that you want composition data for.
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
get_mean_weights <- function(
    dir = NULL,
    data,
    species,
    gear_groups,
    gear_names,
    fleet_colname,
    fleet_groups,
    fleet_names) {
  nwfscSurvey::check_dir(dir = dir)
  # Remove duplicate columns
  data <- data[, which(!colnames(data) %in% c("MT", "SPGRFTOB1", "SCIENTIFIC_NAME"))]
  colnames(data)[which(colnames(data) == "gear")] <- "gear_to_use"
  colnames(data) <- tolower(colnames(data))
  data$year <- data$ryear

  if (fleet_colname == "r_state.x") {
    fleet_colname <- "r_state"
  }

  # Assign gear and fleet groups
  data <- create_groups(
    data = data,
    gear_groups = gear_groups,
    gear_names = gear_names,
    fleet_colname = fleet_colname,
    fleet_groups = fleet_groups,
    fleet_names = fleet_names
  )

  # Check confidentiality
  ci_check <- check_confidential(
    dir = dir,
    data = data,
    gear_groups = gear_groups,
    gear_names = gear_names,
    fleet_colname = fleet_colname,
    fleet_groups = fleet_groups,
    fleet_names = fleet_names
  )$vessels_by_year

  # Remove years where there are < 3 vessels
  ci_not_met <- ci_check[ci_check$n_vessels < 3, ]
  if (dim(ci_not_met)[1] > 0) {
    remove <- NULL
    for (f in unique(ci_not_met$fleet)) {
      remove <- c(remove, which(data$fleet == f & data$year %in% ci_not_met[ci_not_met$fleet == f, "year"]))
    }
    data <- data[-remove, ]
    cli::cli_inform(
      "The following number of records due to not meeting confidentiality: {length(remove)}"
    )
  }

  if (species %in% data[, "species"]) {
    data <- data[data$species == species & data$catch_disposition == "D", ]
  } else {
    cli::cli_abort("{species} not found in the data.")
  }

  if (sum(is.na(data$exp_sp_wt)) > 0) {
    data$exp_sp_wt[is.na(data$exp_sp_wt)] <- 0
  }
  if (sum(is.na(data$exp_sp_wt)) > 0) {
    data$exp_sp_wt[is.na(data$exp_sp_wt)] <- 0
    data$exp_sp_ct[is.na(ob$exp_sp_wt)] <- 1
  }
  if (sum(is.na(data$species_number)) > 0) {
    data <- data[!is.na(data$species_number), ]
  }

  data$species_weight_kg <- 0.453592 * data$species_weight
  data$average_weight <- data$species_weight_kg / data$species_number
  data$exp_average_weight <- data$average_weight * data$exp_sp_ct
  data$weighted_average <- stats::weighted.mean(data$average_weight, data$exp_sp_ct)

  mean_weights <- data |>
    dplyr::group_by(year, gear_groups, fleet_groups) |>
    dplyr::summarise(
      n = n(),
      weighted_ave_w = (sum(exp_average_weight)) / sum(exp_sp_ct),
      v = sum(exp_sp_ct * (average_weight - weighted_average)^2) / sum(exp_sp_ct),
      max_count = max(exp_sp_ct),
      total_count = sum(exp_sp_ct),
      weighted_ave_w_sd = sqrt(v / ((total_count / max_count) - 1)),
      weighted_ave_w_cv = weighted_ave_w_sd / weighted_ave_w
    ) |>
    dplyr::filter(n >= 30)

  mean_bodyweight <- data.frame(
    year = mean_weights[, "year"],
    month = "Month",
    fleet = apply(mean_weights[, c("gear_groups", "fleet_groups")], 1, paste, collapse = "-"),
    partition = 2,
    obs = mean_weights[, "weighted_ave_w"],
    cv = mean_weights[, "weighted_ave_w_cv"]
  )

  colnames(mean_bodyweight)[5:6] <- c("obs", "cv")
  if (!is.null(dir)) {
    write.csv(mean_bodyweight,
      file = file.path(dir, paste0(tolower(species), "_wcgop_mean_body_weights.csv")),
      row.names = FALSE
    )
  }
  return(mean_bodyweight)
}
