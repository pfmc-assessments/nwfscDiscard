#' Visualize biological discard data
#'
#' @param dir Directory location to save files.
#' @param data A data frame of WCGOP biological data
#' @param species_name Species that you want composition data for.
#' @param comp_column Column in the data to plot (e.g. "LENGTH").
#' @param gear_names Vector of gear group names (example: c("trawl", "fixed gear")).
#' @param fleet_colname Column to use to determine areas for fleets (example: "r_state.x")
#' @param fleet_groups List of fleet groups to use (example: list(c("WA", "OR", "CA"))).
#' @param fleet_names Vector of fleet names (example: c("coastwide")).
#'
#' @author Chantel Wetzel
#' @export
#' @return Boxplot of discard length/age by gear group and year.
#'
#'
plot_wcgop_bio <- function(
  data,
  species_name,
  gear_groups,
  gear_names,
  fleet_groups,
  fleet_names,
  fleet_colname = "r_state",
  dir = NULL,
  comp_column = "length"
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
    data_grouped$fleet_groups <- "all-areas"
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

  data_expand <- data_grouped |>
    tidyr::uncount(frequency) |>
    dplyr::mutate(
      frequency = 1
    )

  data_expand[, "bio_plot"] <- data_expand[, comp_column]
  if (comp_column == "length") {
    y_lab <- "Length (cm)"
  } else {
    y_lab <- "Age"
  }

  data_plot <- data_expand |>
    dplyr::filter(!is.na(bio_plot))

  p1 <- ggplot2::ggplot(
    data_plot,
    ggplot2::aes(y = bio_plot, x = year, group = year)
  ) +
    ggplot2::geom_boxplot() +
    ggplot2::xlab("Year") +
    ggplot2::ylab(y_lab) +
    ggplot2::facet_wrap(
      ~ fleet_groups +
        factor(
          gear_to_use,
          levels = c(
            "Bottom Trawl",
            "Midwater Trawl",
            "Shrimp Trawl",
            "Fixed Gears",
            "Hook & Line",
            "Pot"
          )
        )
    )

  if (!is.null(dir)) {
    ggplot2::ggsave(
      filename = file.path(dir, paste0(comp_column, "_by_year_gear_area.png")),
      plot = p1,
      width = 14,
      height = 7
    )
  }

  if (is.null(dir)) {
    return(p1)
  }
}
