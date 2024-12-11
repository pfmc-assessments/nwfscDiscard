#' Calculate expanded discard composition
#'
#' @param dir Directory location to save files.
#' @param data A data frame of WCGOP biological data that includes all species.
#' @param catch_data A data frame of WCGOP catch data that includes all species.
#'   This data frame will be used to check confidentiality.
#' @param species_name Species that you want composition data for.
#' @param len_bins Length composition bins (example: seq(20, 90, 2)).
#' @param age_bins Age composition bins (example: 1:50).
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
get_biological_data <- function(
    dir = NULL,
    data,
    catch_data,
    species_name,
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
  if (any(!"LENGTH" %in% colnames(data))) {
    cli::cli_abort("The LENGTH/length column is not present in the data.")
  }
  if (any(!"AGE" %in% colnames(data))) {
    cli::cli_abort("The AGE/age column is not present in the data.")
  }
  present_data <- data |>
    dplyr::filter(species == species_name) |>
    dplyr::summarise(
      do_lengths = sum(!is.na(LENGTH)),
      do_ages = sum(!is.na(AGE))
    )
  if (sum(present_data) == 0) {
    cli::cli_abort("There are no length or age samples in the data for {species_name}.")
  }

  # Remove duplicate columns
  data <- data |>
    dplyr::select(-SCIENTIFIC_NAME) |>
    dplyr::rename(gear_to_use = gear) |>
    dplyr::rename_with(tolower) |>
    dplyr::rename(year = ryear, r_state = r_state.x)

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
  )

  # Remove years where there are < 3 vessels observed:
  ci_not_met <- ci_check |> dplyr::filter(n_vessels < 3, )
  if (dim(ci_not_met)[1] > 0) {
    remove <- NULL
    for (f in 1:dim(ci_not_met)[1]) {
      remove <- c(
        remove,
        which(
          data[, "fleet"] == ci_not_met[f, "fleet"] &
            data[, "year"] == ci_not_met[f, "year"] &
            data[, "catch_shares"] == ci_not_met[f, "catch_shares"]
        )
      )
    }
    data <- data[-remove, ]
    cli::cli_inform(
      "The following number of records due to not meeting confidentiality: {length(remove)}"
    )
  }

  expansions <- data |>
    dplyr::filter(
      species == species_name,
      catch_disposition == "D"
    ) |>
    dplyr::mutate(
      sex = nwfscSurvey::codify_sex(sex)
    ) |>
    dplyr::mutate(
      exp1 = dplyr::case_when(
        !is.na(species_number) | !is.na(bio_specimen_count) ~ species_number / bio_specimen_count,
        .default = 0
      ),
      exp_weight = dplyr::case_when(
        is.na(exp_sp_wt) ~ (species_weight / hooks_sampled) * total_hooks,
        .default = exp_sp_wt
      ),
      exp2 = dplyr::case_when(
        !is.na(species_weight) ~ exp_weight / species_weight,
        .default = 0
      ),
      wghtd_freq = frequency * exp1 * exp2
    )

  if (sum(!is.na(expansions[, "length"])) > 0) {
    comps <- calc_comps(
      dir = dir,
      data = expansions,
      comp_bins = len_bins,
      comp_column = "length"
    )
  }

  if (sum(!is.na(data[, "age"])) > 0) {
    comps <- calc_comps(
      dir = dir,
      data = expansions,
      comp_bins = age_bins,
      comp_column = "age"
    )
  }
  return(comps)
}
