#' Run all the analysis together
#'
#' @param em_catch_data A data frame of WCGOP EM catch data that includes all species.
#' @param format_gemm_data Data frame of GEMM data created by [format_gemm()] or [format_gemm_alt()]
#' @inheritParams get_biological_data
#' @inheritParams get_discard_rates
#'
#' @author Chantel Wetzel
#' @export
#'
#'
run <- function(
  species_name,
  catch_data,
  em_catch_data,
  biological_data,
  format_gemm_data,
  data_grouping,
  length_bins = seq(10, 60, 2),
  age_bins = 1:30,
  dir = NULL,
  n_boot = 10000
) {
  if (!is.null(em_catch_data)) {
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
  weights <- get_weights(
    data = format_gemm_data |> dplyr::filter(gear %in% unlist(gear_groups)),
    include_catch_share = FALSE,
    dir = dir
  )
  comps <- get_biological_data(
    dir = dir,
    biological_data = biological_data,
    catch_data = catch_data,
    weight_data = weights,
    species_name = species_name,
    length_bins = length_bins,
    age_bins = age_bins,
    gear_groups = gear_groups,
    gear_names = gear_names,
    fleet_colname = fleet_colname,
    fleet_groups = fleet_groups,
    fleet_names = fleet_names
  )

  # Mean discard body weight
  mean_weights <- get_mean_weights(
    dir = dir,
    catch_data = catch_data,
    weight_data = weights,
    species_name = species_name,
    gear_groups = gear_groups,
    gear_names = gear_names,
    fleet_colname = fleet_colname,
    fleet_groups = fleet_groups,
    fleet_names = fleet_names
  )

  # Discard Rates
  weight_cs <- get_weights(
    data = format_gemm_data,
    include_catch_share = TRUE,
    dir = dir
  )
  data_combined <- combine_catch_data(
    catch_data = catch_data,
    em_catch_data = em_catch_data
  )
  ob_out <- get_discard_rates(
    dir = dir,
    data = data_combined,
    species_name = species_name,
    boot_number = n_boot,
    gear_groups = gear_groups,
    gear_names = gear_names,
    fleet_colname = fleet_colname,
    fleet_groups = fleet_groups,
    fleet_names = fleet_names,
    seed_number = 1
  )
  rates <- weight_discard_rates(
    weight_data = weight_cs,
    ncs_data = ob_out$ncs,
    cs_data = ob_out$cs,
    dir = dir
  )

}
