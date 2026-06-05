#' Calculate expanded discard composition
#'
#' @param dir Directory location to save files.
#' @param data A data frame of WCGOP biological data that includes all species.
#' @param weight_data A data frame created by `[calc_weights()]` that will be used
#'   to weight the biological data samples.
#' @param catch_data A data frame of WCGOP catch data that includes all species.
#'   This data frame will be used to check confidentiality.
#' @param species_name Species that you want composition data for.
#' @param len_bins Length composition bins (example: seq(20, 90, 2)).
#' @param age_bins Age composition bins (example: 1:50).
#' @param gear_groups List of gear types to group together
#'   (example: list(c("Bottom Trawl", "Midwater Trawl"), c("Hook & Line", "Pot", "Shrimp Trawl"))).
#' @param gear_names Vector of gear group names (example: c("trawl", "fixed gear")).
#' @param fleet_colname Column to use to determine areas for fleets (example: "r_state.x")
#' @param fleet_groups List of fleet groups to use (example: list(c("WA", "OR", "CA"))).
#' @param fleet_names Vector of fleet names (example: c("coastwide")).
#' @param min_sample_size Numeric value to only retain years of data by gear that are >= to
#'   this value.
#' @param expand Logical statement on whether to expand the compositions samples.  Default is
#'   TRUE. If set to FALSE, then raw samples will be returned that are filtered for
#'   confidentiality.
#'
#' @author Chantel Wetzel
#' @export
#'
#'
get_biological_data <- function(
  dir = NULL,
  data,
  weight_data,
  catch_data,
  species_name,
  len_bins,
  age_bins,
  gear_groups,
  gear_names,
  fleet_colname,
  fleet_groups,
  fleet_names,
  min_sample_size = 20,
  expand = TRUE
) {
  if (length(gear_names) != length(gear_groups)) {
    cli::cli_abort("The gear groups and names are not of the same length.")
  }
  if (any(!"LENGTH" %in% colnames(data))) {
    cli::cli_abort("The LENGTH column is not present in the data.")
  }
  if (any(!"AGE" %in% colnames(data))) {
    cli::cli_abort("The AGE column is not present in the data.")
  }
  present_data <- data |>
    dplyr::filter(species == species_name) |>
    dplyr::summarise(
      do_lengths = sum(!is.na(LENGTH)),
      do_ages = sum(!is.na(AGE))
    )
  if (sum(present_data) == 0) {
    cli::cli_abort(
      "There are no length or age samples in the data for {species_name}."
    )
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
    if (length(remove) > 0) {
      data <- data[-remove, ]
      cli::cli_inform(
        "The following number of records due to not meeting confidentiality: {length(remove)}"
      )
    }
  }

  if (expand) {
    # Need to add filter check for records with unique(bio_specimen_item_id)
    # This column is NA for fish that were only lengthed
    data_filtered <- data |>
      dplyr::filter(
        species == species_name,
        catch_disposition == "D"
      ) |>
      dplyr::mutate(
        sex = nwfscSurvey::codify_sex(sex),
        n_age = dplyr::case_when(
          !is.na(age) ~ frequency,
          .default = 0
        ),
        n_length = dplyr::case_when(
          !is.na(length) ~ frequency,
          .default = 0
        )
      ) |>
      dplyr::group_by(year, gear_to_use, haul_id) |>
      dplyr::mutate(
        n_sampled_haul = sum(frequency),
        n_caught_haul = sum(unique(species_number)),
      ) |>
      dplyr::group_by(year, fleet) |>
      dplyr::mutate(
        n_length_sampled_year = sum(n_length, na.rm = TRUE),
        n_age_sampled_year = sum(n_age, na.rm = TRUE),
        n_length_haul_year = dplyr::case_when(
          n_length > 0 ~ dplyr::n_distinct(haul_id),
          .default = 0
        ),
        n_age_haul_year = dplyr::case_when(
          n_age > 0 ~ dplyr::n_distinct(haul_id),
          .default = 0
        ),
        n_length_trip_year = dplyr::case_when(
          n_length > 0 ~ dplyr::n_distinct(trip_id),
          .default = 0
        ),
        n_age_trip_year = dplyr::case_when(
          n_age > 0 ~ dplyr::n_distinct(trip_id),
          .default = 0
        )
      )
    # join the weights with the data for second stage expansion
    data_and_weights <- dplyr::left_join(
      x = data_filtered,
      y = weight_data |>
        dplyr::select(
          year,
          gear,
          fleet,
          total_discard_mt,
          total_catch_mt,
          gear_group_catch_mt,
          prop_discard,
          prop_catch
        ) |>
        dplyr::rename(gear_to_use = gear),
      by = c("year", "gear_to_use", "fleet")
    )
    # first stage expansion
    # species_number: Total number of individuals of given species. This is the
    #   individual species number from the species_composition_items table.  It
    #   is the same as TOTAL_SPECIES_SAMPLE_COUNT.
    # bio_specimen_count: A count of the number of data records in the
    #   BIO_SPECIMEN_ID sample. This is the total number bio specimens items and
    #   a sum of the length frequencies collected for a species.
    # species_weight: Weight of an individual species within a species composition
    #   item id
    # exp_sp_wt: Weight of catch for data record/line, expanded to the haul-level
    #   based on sampling protocol
    # frequency: Number of individual fish in given length bin (from LF table
    #   in database), or is equal to one if data record is from the Biological
    #   Specimen Item table
    expansions <- data_and_weights |>
      dplyr::mutate(
        exp1 = dplyr::case_when(
          !is.na(species_number) | !is.na(bio_specimen_count) ~
            species_number / bio_specimen_count,
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
        sample_weight_length = n_length * exp1 * exp2,
        sample_weight_age = n_age * exp1 * exp2,
      ) |>
      dplyr::group_by(year, gear_to_use) |>
      dplyr::mutate(
        discard_numerator = unique(total_discard_mt),
        catch_numerator = unique(total_catch_mt),
        calc_total_sample_weight_mt = sum(exp_weight, na.rm = TRUE) / 2204.62,
        total_sample_weight_mt = dplyr::case_when(
          calc_total_sample_weight_mt > discard_numerator ~ discard_numerator,
          .default = calc_total_sample_weight_mt
        ),
        wghtd_catch = prop_catch * (catch_numerator / total_sample_weight_mt),
        # intentionally use the proportion of catch here since that is a better indicator
        # of how to weight the second stage expansion
        wghtd_discard = dplyr::case_when(
          catch_shares == FALSE ~
            prop_catch * discard_numerator / total_sample_weight_mt,
          .default = 1
        ),
        final_weight_length = sample_weight_length * wghtd_discard,
        final_weight_age = sample_weight_age * wghtd_discard
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        length_cap = quantile(final_weight_length, 0.95),
        age_cap = quantile(final_weight_age, 0.95),
        final_weight_length_capped = dplyr::case_when(
          final_weight_length > length_cap ~ length_cap,
          .default = final_weight_length
        ),
        final_weight_age_capped = dplyr::case_when(
          final_weight_age > age_cap ~ age_cap,
          .default = final_weight_age
        )
      ) |>
      dplyr::relocate(frequency, .after = prop_catch) |>
      dplyr::select(
        fleet,
        year,
        sex,
        length,
        age,
        n_length_haul_year,
        n_length_trip_year,
        n_length_sampled_year,
        length,
        final_weight_length_capped,
        n_age_haul_year,
        n_age_trip_year,
        n_age_sampled_year,
        final_weight_age_capped
      )

    if (sum(expansions[, "final_weight_length_capped"]) > 0) {
      comps <- calc_comps(
        dir = dir,
        data = expansions |>
          dplyr::select(
            year,
            fleet,
            sex,
            length,
            n_length_haul_year,
            n_length_trip_year,
            n_length_sampled_year,
            final_weight_length_capped
          ),
        comp_bins = len_bins,
        comp_column_name = "length"
      )
    }

    if (sum(expansions[, "final_weight_age_capped"]) > 0) {
      comps <- calc_comps(
        dir = dir,
        data = expansions |>
          dplyr::select(
            year,
            fleet,
            sex,
            age,
            n_age_haul_year,
            n_age_trip_year,
            n_age_sampled_year,
            final_weight_age_capped
          ),
        comp_bins = age_bins,
        comp_column_name = "age"
      )
    }
  } else {
    comps <- data |>
      dplyr::filter(
        species == species_name,
        catch_disposition == "D"
      ) |>
      dplyr::mutate(
        sex = nwfscSurvey::codify_sex(sex)
      ) |>
      tidyr::uncount(frequency) |>
      dplyr::mutate(
        frequency = 1
      )
  }

  return(comps)
}
