#' Visualize biological discard data
#'
#' @param dir Directory location to save files.
#' @param data A data frame of WCGOP biological data
#' @param species Species that you want composition data for.
#' @param plot A vector of integers to specify which plots you would like. The
#'   default is to print or save both figures, i.e., `plot = 1:2`. Integers
#'   correspond to the following figures:
#'   1. Boxplot of discard length/age by gear group and year.
#'   2. Boxplot of discard length/age by catch share and non-catch share by year.
#' @param comp_column Column in the data to plot (e.g. "LENGTH").
#'
#' @author Chantel Wetzel
#' @export
#'
#'
plot_wcgop_bio <- function(
    data,
    species,
    dir = NULL,
    plot = 1:2,
    comp_column = "length") {
  data <- data[, which(colnames(data) != "SCIENTIFIC_NAME")]
  colnames(data)[which(colnames(data) == "gear")] <- "gear_to_use"
  colnames(data) <- tolower(colnames(data))
  data$year <- data$ryear
  data$r_state <- data$r_state.x
  data <- data[which(data$common_name == species & data$catch_disposition == "D"), ]

  if (grepl("/", species)) {
    species_name_mod <- gsub("/", " ", species)
    replace <- which(data[, "species"] == species)
    data[replace, "species"] <- species_name_mod
    species <- species_name_mod
  }

  get_name <- unique(data$species)

  data$bio_plot <- data[, comp_column]
  if (comp_column == "length") {
    y_lab <- "Length (cm)"
  } else {
    y_lab <- "Age"
  }

  data$catch_shares <- "non_catch_shares"
  data$catch_shares[
    data$sector %in% c("Catch Shares", "Catch Shares EM", "Midwater Hake", "LE CA Halibut") &
      data$year > 2011
  ] <- "catch_shares"

  igroup <- 1
  if (igroup %in% plot) {
    p1 <- ggplot2::ggplot(data, ggplot2::aes(y = bio_plot, x = year, group = year)) +
      ggplot2::geom_boxplot() +
      ggplot2::xlab("Year") +
      ggplot2::ylab(y_lab) +
      ggplot2::facet_wrap(facets = c("gear_to_use")) +
      ggplot2::scale_fill_viridis_d()

    if (!is.null(dir)) {
      ggplot2::ggsave(
        filename = file.path(dir, paste0(species, "_length_by_year_gear.png")),
        plot = p1,
        width = 14,
        height = 7
      )
    } else {
      p1
    }
  }

  igroup <- 2
  if (igroup %in% plot) {
    p2 <- ggplot2::ggplot(data, ggplot2::aes(y = bio_plot, x = year, group = year)) +
      ggplot2::geom_boxplot() +
      ggplot2::xlab("Year") +
      ggplot2::ylab(y_lab) +
      ggplot2::facet_wrap(facets = c("catch_shares")) +
      ggplot2::scale_fill_viridis_d()

    if (!is.null(dir)) {
      ggplot2::ggsave(
        filename = file.path(dir, paste0(species, "_length_by_year_catch_share.png")),
        plot = p2,
        width = 14,
        height = 7
      )
    } else {
      p2
    }
  }

  samples_by_year <- data |>
    dplyr::group_by(gear_to_use, catch_shares, year) |>
    dplyr::reframe(
      n_lengths = length(!is.na(length))
    )
  samples_by_year <- as.data.frame(samples_by_year)

  return(samples_by_year)
}
