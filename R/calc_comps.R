#' Calculate expanded discard composition
#'
#'
#' @param dir Directory location to save files.
#' @param data A data frame of WCGOP biological data
#' @param comp_bins Length or age bins to apply to the composition data.
#' @param comp_column_name Column name for the type of composition data to process (e.g., "length" or "age")
#'
#' @author Chantel Wetzel
#' @export
#'
#'
calc_comps <- function(
  dir = NULL,
  data,
  comp_bins,
  comp_column_name = "length"
) {
  # Perhaps add a check for lengths being available
  if (comp_column_name == "length") {
    format_data <- data |>
      dplyr::rename(
        comp_column = length,
        n_fish = n_length_sampled_year,
        n_hauls = n_length_haul_year,
        n_trips = n_length_trip_year,
        weight = final_weight_length_capped
      )
  } else {
    format_data <- data |>
      dplyr::rename(
        comp_column = age,
        n_fish = n_age_sampled_year,
        n_hauls = n_age_haul_year,
        n_trips = n_age_trip_year,
        weight = final_weight_age_capped
      )
  }
  comp_data <- format_data |>
    dplyr::mutate(
      sex_group = dplyr::case_when(
        sex == "U" ~ "u",
        .default = "fm"
      )
    ) |>
    dplyr::group_by(year, fleet, sex_group) |>
    dplyr::mutate(
      ratio = sum(unique(n_fish)) / n_trips,
      input_n = dplyr::case_when(
        ratio < 44 ~ n_trips + 0.138 * sum(unique(n_fish)),
        .default = 7.06 * n_trips
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-ratio, -sex_group)

  bins <- c(comp_bins, Inf)
  comp_data <- comp_data |>
    dplyr::filter(!is.na(comp_column)) |>
    dplyr::mutate(
      bins = bins[findInterval(comp_column, bins, all.inside = TRUE)]
    )
  # Modify inComps to include all bins in comp_bins
  check_bin_width <- diff(comp_bins)
  if (any(check_bin_width != check_bin_width[1])) {
    cli::cli_inform(
      "The output should be careful checked to ensure correctness when unequal
      bin intervals are used."
    )
  }
  bin_width <- check_bin_width[1]
  grid <- comp_data |>
    tibble::tibble() |>
    tidyr::expand(
      year,
      fleet,
      sex,
      tidyr:::full_seq(comp_bins, bin_width)
    )
  colnames(grid)[ncol(grid)] <- "bins"
  filled_comps <- comp_data |>
    dplyr::right_join(grid) |>
    tidyr::complete(
      year,
      fleet,
      bins,
      fill = list(
        n_hauls = 0,
        n_trips = 0,
        n_fish = 0,
        input_n = 0,
        weight = 0
      )
    )
  target <- "bins"
  key_names <- c("year", "fleet")
  sex_label_left_side <- dplyr::case_when(
    all(c("M", "F", "U") %in% comp_data[["sex"]]) ~ "f",
    "F" %in% comp_data[["sex"]] ~ "f",
    "U" %in% comp_data[["sex"]] ~ "u"
  )
  vars <- c(key_names, "input_n", "sex", target)
  wider_cols <- c(key_names, "input_n", "sex")
  wide_composition_data <- filled_comps |>
    dplyr::summarize(
      .by = tidyr::all_of(vars),
      comp = round(sum(weight), digits = 4)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      # Create the f1 f2 ... m1 m2 ... or u1 u2 ... labels to move to wide
      # columns later
      sex_length = dplyr::case_when(
        sex == "U" ~ sprintf(fmt = "%s%05d", sex_label_left_side, bins),
        .default = sprintf(fmt = "%s%05d", tolower(sex), bins)
      ),
      # Relabel males as females in sex so they get cast to the right when
      # making a wide data frame
      sex = ifelse(sex == "M", "F", sex)
    ) |>
    dplyr::arrange(fleet, sex_length) |>
    tidyr::pivot_wider(
      id_cols = tidyr::all_of(wider_cols),
      names_from = "sex_length",
      values_from = "comp",
      names_sort = TRUE,
      values_fill = 0
    ) |>
    dplyr::arrange(sex) |>
    dplyr::mutate(
      month = factor(7),
      sex = ifelse(sex == "F", 3, 0),
      partition = 0
    ) |>
    dplyr::filter(input_n > 0) |>
    dplyr::relocate(month, fleet, sex, partition, .after = year) |>
    dplyr::arrange(fleet, sex, year) |>
    dplyr::rename_with(.fn = \(x) gsub("([a-z])0+([1-9])", "\\1\\2", x))

  #normalize <- wide_composition_data
  #normalize[, 7:dim(wide_composition_data)[2]] <- round(100 * wide_composition_data[, 7:dim(wide_composition_data)[2]] /
  #  apply(wide_composition_data[,7:dim(wide_composition_data)[2]], 1, sum), 4)

  if (comp_column_name == "age") {
    returned_composition_data <- wide_composition_data |>
      dplyr::mutate(
        ageerr = NA,
        Lbin_lo = -1,
        Lbin_hi = -1,
        .after = partition
      )
  } else {
    returned_composition_data <- wide_composition_data
  }

  sample_size <- comp_data |>
    dplyr::mutate(
      sex_group = dplyr::case_when(
        sex == "U" ~ "unsexed",
        .default = "sexed"
      )
    ) |>
    dplyr::summarise(
      .by = c(year, fleet, sex_group),
      hauls = unique(n_hauls),
      trips = unique(n_trips),
      fish = unique(n_fish),
      input_n = unique(input_n)
    ) |>
    dplyr::arrange(fleet, year) |>
    data.frame()

  if (!is.null(dir)) {
    if (dim(sample_size)[1] > 0) {
      write.csv(
        sample_size,
        file = file.path(
          dir,
          paste0("biological_sample_sizes_", comp_column_name, ".csv")
        ),
        row.names = FALSE
      )
    }

    if (dim(wide_composition_data)[1] > 0) {
      write.csv(
        wide_composition_data,
        file = file.path(
          dir,
          paste0("biological_discard_", comp_column_name, "s.csv")
        ),
        row.names = FALSE
      )
    }
  }
  return(wide_composition_data)
}
