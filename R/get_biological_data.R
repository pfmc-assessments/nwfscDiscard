#' Calculate expanded discard composition
#'
#' @param dir Directory location to save files.
#' @param data A data frame of WCGOP biological data that includes all species.
#' @param catch_data A data frame of WCGOP catch data that includes all species.
#'   This data frame will be used to check confidentiality.
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
    dir = NULL,
    data,
    catch_data,
    species,
    len_bins,
    age_bins,
    gear_groups,
    gear_names,
    fleet_colname,
    fleet_groups,
    fleet_names) {
  if (length(gear_names) != length(gear_groups)) {
    cli::cli_abort("The gear groups and names are not of the same length.")
  }

  # Remove duplicate columns
  data <- data[, which(colnames(data) != "SCIENTIFIC_NAME")]
  colnames(data)[which(colnames(data) == "gear")] <- "gear_to_use"
  colnames(data) <- tolower(colnames(data))
  data[, "year"] <- data[, "ryear"]
  data[, "r_state"] <- data[, "r_state.x"]

  # Assign gear and fleet groups
  data <- create_groups(
    data = data,
    gear_groups = gear_groups,
    gear_names = gear_names,
    fleet_colname = fleet_colname,
    fleet_groups = fleet_groups,
    fleet_names = fleet_names
  )

  # Check confidentiality with is based on the number of vessels observed (catch data),
  # not the number of vessels with biological samples (bio data)
  catch_data_mod <- create_groups(
    data = catch_data,
    gear_groups = gear_groups,
    gear_names = gear_names,
    fleet_colname = fleet_colname,
    fleet_groups = fleet_groups,
    fleet_names = fleet_names
  )
  ci_check <- check_confidential(
    dir = dir,
    data = catch_data_mod,
    gear_groups = gear_groups,
    gear_names = gear_names,
    fleet_colname = fleet_colname,
    fleet_groups = fleet_groups,
    fleet_names = fleet_names
  )$vessels_by_year

  # Remove years where there are < 3 vessels observed:
  ci_not_met <- ci_check |> dplyr::filter(n_vessels < 3, )
  if (dim(ci_not_met)[1] > 0) {
    remove <- NULL
    for (f in unique(ci_not_met[, "fleet"])) {
      remove <- c(remove, which(data$fleet == f & data$year %in% ci_not_met[ci_not_met$fleet == f, "year"]))
    }
    data <- data[-remove, ]
    cli::cli_inform(
      "The following number of records due to not meeting confidentiality: {length(remove)}"
    )
  }

  data <- data[which(data$common_name == species & data$catch_disposition == "D"), ]

  # Calculate weighting
  data$exp1 <- data[, "species_number"] / data[, "bio_specimen_count"]
  data$exp_sp_wt[is.na(data$exp_sp_wt)] <-
    (data$species_weight[is.na(data$exp_sp_wt)] /
      data$hooks_sampled[is.na(data$exp_sp_wt)]) *
      data$total_hooks[is.na(data$exp_sp_wt)]

  data$exp2 <- data$exp_sp_wt / data$species_weight
  data$wghtd_freq <- data$frequency * data$exp1 * data$exp2

  NA_wgts <- sum(is.na(data$wghtd_freq))
  cli::cli_inform(
    "Converting {NA_wgts} weighted frequency out of {nrow(data)} to zero for arithmetic."
  )
  data$wghtd_freq[is.na(data$wghtd_freq)] <- 0

  if (sum(!is.na(data[, "length"])) > 0) {
    comps <- calc_comps(
      dir = dir,
      data = data,
      comp_bins = len_bins,
      comp_column = "length"
    )
  }

  if (sum(!is.na(data[, "age"])) > 0) {
    comps <- calc_comps(
      dir = dir,
      data = data,
      comp_bins = age_bins,
      comp_column = "age"
    )
  }
  return(comps)
}
