#' Calculate expanded discard composition
#'
#' @param dir Directory location to save files.
#' @param data A data frame of WCGOP biological data
#' @param species Species that you want composition data for.
#' @param len_bins Length composition bins (example: seq(20, 90, 2)).
#' @param age_bins Age composition bins (example: 1:50).
#' @param gear_groups List of gear types to group together
#' (example: list(c("Bottom Trawl", "Midwater Trawl"), c("Hook & Line", "Pot", "Shrimp Trawl"))).
#' @param gear_name Vector of gear group names (example: c("trawl", "fixed gear")).
#' @param fleet_colname Column to use to determine areas for fleets (example: "r_state.x")
#' @param fleet_groups List of fleet groups to use (example: list(c("WA", "OR", "CA"))).
#' @param fleet_names Vector of fleet names (example: c("coastwide")).
#'
#' @author Chantel Wetzel
#' @export
#'
#'
get_biological_data <- function(
    dir,
    data,
    species,
    len_bins,
    age_bins,
    gear_groups,
    gear_names,
    fleet_colname,
    fleet_groups,
    fleet_names) {
  if (length(gear_names) != length(gear_groups)) {
    stop("The gear groups and names are not of the same length.")
  }

  # Remove duplicate columns
  data <- data[, which(colnames(data) != "SCIENTIFIC_NAME")]
  colnames(data)[which(colnames(data) == "gear")] <- "gear_to_use"
  colnames(data) <- tolower(colnames(data))
  data$year <- data$ryear
  data$r_state <- data$r_state.x
  data <- data[which(data$common_name == species & data$catch_disposition == "D"), ]

  # Assign gear and fleet groups
  data <- create_groups(
    data = data,
    gear_groups = gear_groups,
    gear_names = gear_names,
    fleet_colname = fleet_colname,
    fleet_groups = fleet_groups,
    fleet_names = fleet_names
  )


  # Calculate weighting
  data$exp1 <- data[, "species_number"] / data[, "bio_specimen_count"]
  data$exp_sp_wt[is.na(data$exp_sp_wt)] <-
    (data$species_weight[is.na(data$exp_sp_wt)] /
      data$hooks_sampled[is.na(data$exp_sp_wt)]) *
      data$total_hooks[is.na(data$exp_sp_wt)]

  data$exp2 <- data$exp_sp_wt / data$species_weight
  data$wghtd_freq <- data$frequency * data$exp1 * data$exp2

  NA_wgts <- sum(is.na(data$wghtd_freq))
  glue::glue("Converting {NA_wgts} weighted frequency out of {nrow(data)} to zero for arithmetic.")
  data$wghtd_freq[is.na(data$wghtd_freq)] <- 0

  if (sum(!is.na(data[, "length"])) > 0) {
    calc_comps(
      dir = dir,
      data = data,
      comp_bins = len_bins,
      comp_column = "length"
    )
  }

  if (sum(!is.na(data[, "age"])) > 0) {
    calc_comps(
      dir = dir,
      data = data,
      comp_bins = age_bins,
      comp_column = "age"
    )
  }
}
