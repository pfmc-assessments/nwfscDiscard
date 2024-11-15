#' Run all the analysis together
#'
#' @param species_name Species that you want composition data for. Should match
#'   the species names in the WCGOP data.
#' @param data_grouping A list of the groupings to apply to the data. The list should
#'   formatted as: list(gear_groups, gear_names, fleet_groups, fleet_names, fleet_colname).
#' @param len_bins Length composition bins (example: seq(20, 90, 2)).
#' @param age_bins Age composition bins (example: 1:50).
#' @param catch_data A data frame of WCGOP catch data that includes all species.
#'   This data frame will be used to check confidentiality.
#' @param biological_data A data frame of WCGOP biological data that includes all species.
#' @param em_catch_data A data frame of WCGOP EM catch data that includes all species.
#' @param save_loc Directory location to save files.
#' @param n_boot Number of bootstraps to run.
#'
#' @author Chantel Wetzel
#' @export
#'
#'
run <- function(
    species_name,
    data_grouping,
    len_bins = seq(10, 60, 2),
    age_bins = 1:30,
    catch_data,
    biological_data,
    em_catch_data = NULL,
    save_loc = NULL,
    n_boot = 10000
    ) {
  if(!is.null(em_data)) {
    do_em <- TRUE
  } else {
    do_em <- FALSE
  }

  gear_groups <- data_grouping[[1]]
  gear_names <- data_grouping[[2]]
  fleet_groups <- data_grouping[[3]]
  fleet_names <- data_grouping[[4]]
  fleet_colname <- data_grouping[[5]]

  # Process the biological data
  comps <- get_biological_data(
    dir = save_loc,
    data = biological_data,
    catch_data = catch_data,
    species_name = species_name,
    len_bins = len_bins,
    age_bins = age_bins,
    gear_groups = gear_groups,
    gear_names = gear_names,
    fleet_colname = fleet_colname,
    fleet_groups = fleet_groups,
    fleet_names = fleet_names
  )

  mean_weights <- get_mean_weights(
    dir = save_loc,
    data = catch_data,
    species_name = species_name,
    gear_groups = gear_groups,
    gear_names = gear_names,
    fleet_colname = fleet_colname,
    fleet_groups = fleet_groups,
    fleet_names = fleet_names
  )

  # Calculate the discard total and rates, including the EM data:
  ob_out <- do_discard_bootstrap(
    dir = save_loc,
    data = catch_data,
    species_name = species_name,
    boot_number = n_boot,
    gear_groups = gear_groups,
    gear_names = gear_names,
    fleet_colname = fleet_colname,
    fleet_groups = fleet_groups,
    fleet_names = fleet_names,
    seed_number = 1,
    rm_em_data = do_em
  )

  if (do_em) {
    em_out <- do_discard_bootstrap(
      dir = save_loc,
      data = em_catch_data,
      species_name = species_name,
      gear_groups = gear_groups,
      gear_names = gear_names,
      fleet_colname = fleet_colname,
      fleet_groups = fleet_groups,
      fleet_names = fleet_names
    )

    combine_cs_discards(
      cs_data = ob_out$cs,
      em_data = em_out$em
    )
  }
}
