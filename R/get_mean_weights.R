#' Calculate mean weights for discarded fish
#'
#'
#' @param weight_data A data frame created by `[get_weights()]` that will be used
#'   to weight the biological data samples.
#' @param min_sample_size An integer to filter annual estimates to retain only year
#'   and fleets of data that have at least this number of samples to inform estimates.
#' @inheritParams get_biological_data
#'
#' @author Chantel Wetzel
#' @export
#'
#'
get_mean_weights <- function(
  catch_data,
  weight_data,
  species_name,
  gear_groups,
  gear_names,
  fleet_colname,
  fleet_groups,
  fleet_names,
  min_sample_size = 20,
  dir = NULL
) {
  nwfscSurvey::check_dir(dir = dir)
  if (!species_name %in% data[, "species"]) {
    cli::cli_abort("{species_name} not found in the data.")
  }
  # Remove duplicate columns
  data <- catch_data |>
    dplyr::select(-MT, -SPGRFTOB1, -SCIENTIFIC_NAME, -YEAR) |>
    dplyr::rename(gear_to_use = gear, year = RYEAR) |>
    dplyr::rename_with(tolower)

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
  )

  # Remove years where there are < 3 vessels
  ci_not_met <- ci_check[ci_check$n_vessels < 3, ]
  if (dim(ci_not_met)[1] > 0) {
    remove <- NULL
    for (f in unique(ci_not_met$fleet)) {
      remove <- c(
        remove,
        which(
          data$fleet == f &
            data$year %in% ci_not_met[ci_not_met$fleet == f, "year"]
        )
      )
    }
    data <- data[-remove, ]
    cli::cli_alert_info(
      "The following number of records due to not meeting confidentiality: {length(remove)}"
    )
  }

  data_filtered <- data |>
    dplyr::filter(
      species == species_name,
      catch_disposition == "D"
    )

  combined_data <- dplyr::left_join(
    data_filtered,
    weight_data |> dplyr::select(year, fleet, prop_discard, prop_catch),
    by = c("year", "fleet"),
    relationship = "many-to-many"
  )

  data_weights <- combined_data |>
    dplyr::mutate(
      exp_sp_wt = dplyr::case_when(
        is.na(exp_sp_wt) ~ 0,
        .default = exp_sp_wt
      )
    ) |>
    dplyr::filter(
      !is.na(exp_sp_ct),
      exp_sp_ct > 0,
      !is.na(species_number),
      species_number > 0
    ) |>
    dplyr::mutate(
      period = dplyr::case_when(
        year < 2011 ~ "pre-catch shares",
        .default = "post-catch shares"
      ),
      species_weight_kg = 0.453592 * species_weight,
      average_weight = species_weight_kg / species_number,
      exp_average_weight = average_weight * exp_sp_ct,
    ) |>
    dplyr::group_by(period, fleet) |>
    dplyr::mutate(
      weighted_average = stats::weighted.mean(average_weight, exp_sp_ct)
    ) |>
    dplyr::ungroup()

  mean_body_weights <- data_weights |>
    dplyr::summarise(
      .by = c("year", "fleet"),
      weighted_mean = round(sum(exp_average_weight) / sum(exp_sp_ct), 4),
      v = sum(exp_sp_ct * (average_weight - weighted_average)^2) /
        sum(exp_sp_ct),
      max_count = max(exp_sp_ct),
      total_count = sum(exp_sp_ct),
      total_species_number = sum(species_number),
      sd = round(sqrt(v / ((total_count / max_count) - 1)), 4),
      cv = round(sd / weighted_mean, 4)
    ) |>
    dplyr::filter(total_species_number >= min_sample_size) |>
    dplyr::mutate(
      month = 7,
      partition = 1,
      type = 2
    ) |>
    dplyr::rename(
      obs = weighted_mean
    ) |>
    dplyr::select(year, month, fleet, partition, type, obs, cv) |>
    dplyr::arrange(fleet, year)

  if (!is.null(dir)) {
    write.csv(
      mean_body_weights,
      file = file.path(dir, "wcgop_discard_mean_body_weights.csv"),
      row.names = FALSE
    )
  }
  return(mean_body_weights)
}
