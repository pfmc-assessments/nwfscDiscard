#' Plot GEMM data
#'
#' Calculate the totals and proportions by gear and catch shares.  These data
#' are not grouped in order to see the amounts for each gear and catch share
#' sector prior to combining for WCGOP data processing.
#'
#' @param data Data frame of gemm data pulled using [nwfscSurvey::pull_gemm()]
#' @param plot A vector of integers to specify which plots you would like. The
#'   default is to print or save both figures, i.e., `plot = 1:2`. Integers
#'   correspond to the following figures:
#'   1. Catch, landings, and discard by catch share and gear
#'   2. Proportion of catch, landings, and discards by catch share and gear
#' @param dir Directory location to save files.
#'
#' @author Chantel Wetzel
#' @export
#'
#
plot_gemm <- function(
  data,
  plot = 1:2,
  dir = NULL
) {
  nwfscSurvey::check_dir(dir = dir)
  species <- unique(data[, "species"])

  # Remove research catch because it only appears in the
  # total_discard_with_mort_rates_applied_and_landings_mt which
  # results in the the catch != dead discards + landings for
  # the non-catch share trawl group.
  data_formatted <- data |>
    dplyr::filter(
      !sector %in%
        c(
          "Research",
          "California Recreational",
          "Oregon Recreational",
          "Washington Recreational",
          "Incidental"
        )
    ) |>
    dplyr::mutate(
      gear = dplyr::case_when(
        sector %in%
          c(
            "CS - Hook & Line",
            "Directed P Halibut",
            "LE Fixed Gear DTL - Hook & Line",
            "LE Sablefish - Hook & Line",
            "OA Fixed Gear - Hook & Line"
          ) ~
          "Hook & Line",
        sector == "Nearshore" ~ "Fixed Gears",
        sector %in%
          c(
            "CS - Pot",
            "CS EM - Pot",
            "LE Fixed Gear DTL - Pot",
            "OA Fixed Gear - Pot",
            "LE Sablefish - Pot"
          ) ~
          "Pot",
        sector %in%
          c(
            "At-Sea Hake CP",
            "At-Sea Hake MSCV",
            "Midwater Hake",
            "Midwater Hake EM",
            "Midwater Rockfish",
            "Midwater Rockfish EM",
            "Tribal At-Sea Hake"
          ) ~
          "Midwater Trawl",
        .default = "Bottom Trawl"
      ),
      catch_shares = dplyr::case_when(
        sector %in%
          c(
            "CS - Bottom and Midwater Trawl",
            "CS - Bottom Trawl",
            "CS - Hook & Line",
            "CS - Pot",
            "CS EM - Bottom Trawl",
            "CS EM - Pot",
            "Midwater Rockfish",
            "Midwater Hake EM",
            "Midwater Rockfish EM"
          ) ~
          "Catch Shares",
        sector %in%
          c("LE CA Halibut", "At-Sea Hake CP", "At-Sea Hake MSCV") &
          year >= 2011 ~
          "Catch Shares",
        .default = "Non-Catch Shares"
      )
    )

  data_formatted[, "Group"] <- apply(
    data_formatted[, c("catch_shares", "gear")],
    1,
    paste,
    collapse = "-"
  )

  catch_totals <- data_formatted |>
    dplyr::mutate(
      all_total = sum(total_discard_with_mort_rates_applied_and_landings_mt)
    ) |>
    dplyr::group_by(year) |>
    dplyr::mutate(
      landed_mt_by_year = sum(total_landings_mt),
      discard_mt_by_year = sum(total_discard_mt),
      dead_discard_mt_by_year = sum(total_discard_with_mort_rates_applied_mt),
      catch_by_year = sum(
        total_discard_with_mort_rates_applied_and_landings_mt
      ),
    ) |>
    dplyr::ungroup() |>
    dplyr::group_by(sector) |>
    dplyr::mutate(
      catch_all_years_total = sum(
        total_discard_with_mort_rates_applied_and_landings_mt
      )
    ) |>
    dplyr::mutate(
      prop = catch_all_years_total / all_total
    ) |>
    dplyr::filter(prop >= 0.005) |>
    dplyr::ungroup()

  catch_by_catch_share_gear <- catch_totals |>
    dplyr::summarise(
      .by = c("year", "Group"),
      discard_mt = sum(total_discard_mt),
      dead_discard_mt = sum(total_discard_with_mort_rates_applied_mt),
      landed_mt = sum(total_landings_mt),
      catch = sum(total_discard_with_mort_rates_applied_and_landings_mt),
      gemm_discard_rate = discard_mt / (landed_mt + discard_mt),
      gemm_dead_discard_rate = dead_discard_mt / (landed_mt + dead_discard_mt),
      prop_discard = discard_mt / unique(discard_mt_by_year),
      prop_landed = landed_mt / unique(landed_mt_by_year),
      prop_catch = catch / unique(catch_by_year)
    ) |>
    dplyr::ungroup()

  ylim <- c(0, max(catch_totals[, "catch_by_year"]) * 1.05)
  p1 <- ggplot2::ggplot(
    catch_by_catch_share_gear,
    ggplot2::aes(x = year, y = prop_catch, fill = Group)
  ) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::xlab("Year") +
    ggplot2::ylab("Proportion of Catch") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 10),
      axis.title = ggplot2::element_text(size = 10),
      strip.text.x = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 10)
    ) +
    ggplot2::scale_fill_viridis_d()
  p2 <- ggplot2::ggplot(
    catch_by_catch_share_gear,
    ggplot2::aes(x = year, y = prop_landed, fill = Group)
  ) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::xlab("Year") +
    ggplot2::ylab("Proportion of Landings") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 10),
      axis.title = ggplot2::element_text(size = 10),
      strip.text.x = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 10)
    ) +
    ggplot2::scale_fill_viridis_d()
  p3 <- ggplot2::ggplot(
    catch_by_catch_share_gear,
    ggplot2::aes(x = year, y = prop_discard, fill = Group)
  ) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::xlab("Year") +
    ggplot2::ylab("Proportion of Discards") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 10),
      axis.title = ggplot2::element_text(size = 10),
      strip.text.x = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 10)
    ) +
    ggplot2::scale_fill_viridis_d()
  c1 <- ggplot2::ggplot(
    catch_by_catch_share_gear,
    ggplot2::aes(x = year, y = catch, fill = Group)
  ) +
    ggplot2::geom_bar(stat = "identity") +
    #ggplot2::ylim(as.numeric(ylim[1]), as.numeric(ylim[2])) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 10),
      axis.title = ggplot2::element_text(size = 10),
      strip.text.x = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 10)
    ) +
    ggplot2::xlab("Year") +
    ggplot2::ylab("Catch (mt)") +
    ggplot2::scale_fill_viridis_d()
  c2 <- ggplot2::ggplot(
    catch_by_catch_share_gear,
    ggplot2::aes(x = year, y = landed_mt, fill = Group)
  ) +
    ggplot2::geom_bar(stat = "identity") +
    #ggplot2::ylim(as.numeric(ylim[1]), as.numeric(ylim[2])) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 10),
      axis.title = ggplot2::element_text(size = 10),
      strip.text.x = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 10)
    ) +
    ggplot2::xlab("Year") +
    ggplot2::ylab("Landings (mt)") +
    ggplot2::scale_fill_viridis_d()
  c3 <- ggplot2::ggplot(
    catch_by_catch_share_gear,
    ggplot2::aes(x = year, y = discard_mt, fill = Group)
  ) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::xlab("Year") +
    ggplot2::ylab("Discards (mt)") +
    #ggplot2::ylim(as.numeric(ylim[1]), as.numeric(ylim[2])) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 10),
      axis.title = ggplot2::element_text(size = 10),
      strip.text.x = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 10)
    ) +
    ggplot2::scale_fill_viridis_d()

  #plot_2 <- cowplot::plot_grid(p2, p3, ncol = 1, nrow = 3)
  #plot_1 <- cowplot::plot_grid(c2, c3, ncol = 1, nrow = 3)
  plot_1 <- c2 / c3
  plot_2 <- p2 / p3

  if (!is.null(dir)) {
    if (1 %in% plot) {
      ggplot2::ggsave(
        plot = plot_1,
        filename = file.path(dir, "gemm_catch.png"),
        height = 18,
        width = 12
      )
    }
    if (2 %in% plot) {
      ggplot2::ggsave(
        plot = plot_2,
        filename = file.path(dir, "gemm_proportion.png"),
        height = 18,
        width = 12
      )
    }
  }
  if (is.null(dir)) {
    if (length(plot) == 1) {
      if (plot == 1) {
        return(plot_1)
      }
      if (plot == 2) {
        return(plot_2)
      }
    } else {
      return(list(plot_1, plot_2))
    }
  }
}
