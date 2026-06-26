#' Calculate biological sample size
#'
#' @param dir Directory location to save files.
#' @param data A data frame of WCGOP biological data
#' @param species_name Species that you want composition data for.
#' @param gear_names Vector of gear group names (example: c("trawl", "fixed gear")).
#' @param fleet_colname Column to use to determine areas for fleets (example: "r_state.x")
#' @param fleet_groups List of fleet groups to use (example: list(c("WA", "OR", "CA"))).
#' @param fleet_names Vector of fleet names (example: c("coastwide")).
#'
#' @author Chantel Wetzel
#' @export
#'
#'
bio_samples_by_year <- function(
  data,
  species_name,
  gear_groups,
  gear_names,
  fleet_groups,
  fleet_names,
  fleet_colname = "r_state",
  dir = NULL
) {
  nwfscSurvey::check_dir(dir = dir)
  data_filtered <- data |>
    dplyr::select(-SCIENTIFIC_NAME) |>
    dplyr::rename(gear_to_use = gear) |>
    dplyr::rename_with(tolower) |>
    dplyr::rename(year = ryear, r_state = r_state.x) |>
    dplyr::filter(species == species_name, catch_disposition == "D")

  if (!any(missing(gear_groups))) {
    data_grouped <- create_groups(
      data = data_filtered,
      gear_groups = gear_groups,
      gear_names = gear_names,
      fleet_colname = tolower(fleet_colname),
      fleet_groups = fleet_groups,
      fleet_names = fleet_names
    )
  } else {
    data_grouped <- data_filtered
    data_grouped$fleet_groups <- data_grouped[, tolower(fleet_colname)]
    data_grouped$gear_groups <- data_grouped[, "gear_to_use"]
    data_grouped$fleet <- apply(
      data_grouped[, c("gear_groups", "fleet_groups")],
      1,
      paste,
      collapse = "-"
    )
  }

  if (grepl("/", species_name)) {
    species_name_mod <- gsub("/", " ", species_name)
    data_grouped[
      which(data_grouped[, "species"] == species_name),
      "species"
    ] <- species_name_mod
  }

  samples_by_year <- data_grouped |>
    dplyr::mutate(
      sex = nwfscSurvey::codify_sex(sex),
      n_age = dplyr::case_when(
        !is.na(age) ~ frequency,
        .default = 0
      ),
      n_length = dplyr::case_when(
        !is.na(length) ~ frequency,
        .default = 0
      ),
      sex_group = dplyr::case_when(
        sex == "U" ~ "u",
        .default = "fm"
      )
    ) |>
    dplyr::summarize(
      .by = c("year", "fleet", "sex_group"),
      n_lengths = sum(n_length),
      n_ages = sum(n_age)
    ) |>
    dplyr::arrange(
      fleet,
      sex_group,
      year
    ) |>
    dplyr::rename(
      Year = year,
      Fleet = fleet,
      Sex = sex_group,
      `N lengths` = n_lengths,
      `N ages` = n_ages
    ) |>
    data.frame()
  return(samples_by_year)
}
