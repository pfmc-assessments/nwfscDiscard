#' Calculate the discards, landings, and catch by gear type and catch share fleet
#' and the proportions by year using GEMM data.
#'
#' @param dir Directory location to save files.
#' @param species species name to be used to pull catch from [nwfscSurvey::pull_gemm()]
#'
#'
#' @author Chantel Wetzel
#' @export
#'
#
calc_prop_gemm_catch <- function(
    dir = NULL,
    species = species) {
  catch <- nwfscSurvey::pull_gemm(common_name = species)

  # Remove research catch because it only appears in the
  # total_discard_with_mort_rates_applied_and_landings_mt which
  # results in the the catch != dead discards + landings for
  # the non-catch share trawl group.
  catch <- catch[which(catch$sector != "Research"), ]

  catch$catch_shares <- "Non-Catch Shares"
  catch_shares <- c(
    "At-Sea Hake CP",
    "At-Sea Hake MSCV",
    "CS - Bottom and Midwater Trawl",
    "CS - Bottom Trawl",
    "CS - Hook & Line",
    "CS - Pot",
    "CS EM - Bottom Trawl",
    "CS EM - Pot",
    "Midwater Hake",
    "Midwater Hake EM",
    "Midwater Rockfish",
    "Midwater Rockfish EM"
  )
  find <- which(catch$sector %in% catch_shares & catch$year >= 2011)
  if (length(find) > 0) {
    catch$catch_shares[find] <- "Catch Shares"
  }

  catch$gear <- NA
  fixed_gear <- c(
    "Combined LE & OA CA Halibut",
    "CS - Hook & Line",
    "CS - Pot",
    "CS EM - Pot",
    "Directed P Halibut",
    "Incidental",
    "LE CA Halibut",
    "LE Fixed Gear DTL - Hook & Line",
    "LE Fixed Gear DTL - Pot",
    "LE Sablefish - Hook & Line",
    "LE Sablefish - Pot",
    "Nearshore",
    "OA CA Halibut",
    "OA Fixed Gear - Hook & Line",
    "OA Fixed Gear - Pot"
  )
  hake <- c(
    "At-Sea Hake CP",
    "At-Sea Hake MSCV",
    "Midwater Hake",
    "Midwater Hake EM",
    "Shoreside Hake",
    "Tribal At-Sea Hake"
  )
  trawl <- c(
    "CS - Bottom and Midwater Trawl",
    "CS - Bottom Trawl",
    "CS EM - Bottom Trawl",
    "Limited Entry Trawl",
    "Midwater Rockfish",
    "Midwater Rockfish EM",
    "Pink Shrimp",
    "Research",
    "Tribal Shoreside"
  )
  rec <- c(
    "Washington Recreational",
    "Oregon Recreational",
    "California Recreational"
  )

  catch$gear[which(catch$sector %in% fixed_gear)] <- "Fixed Gear"
  catch$gear[which(catch$sector %in% c(hake, trawl))] <- "Trawl"
  catch$gear[which(catch$sector %in% rec)] <- "Recreational"

  catch$Group <- apply(catch[, c("catch_shares", "gear")], 1, paste, collapse = "-")

  catch_totals <- catch |>
    dplyr::group_by(year) |>
    dplyr::mutate(
      landed_mt_by_year = sum(total_landings_mt),
      discard_mt_by_year = sum(total_discard_mt),
      dead_discard_mt_by_year = sum(total_discard_with_mort_rates_applied_mt),
      catch_by_year = sum(total_discard_with_mort_rates_applied_and_landings_mt),
    )

  catch_by_catch_share_gear <- catch_totals |>
    dplyr::group_by(year, Group) |>
    dplyr::summarise(
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
  p1 <- ggplot2::ggplot(catch_by_catch_share_gear, ggplot2::aes(x = year, y = prop_catch, fill = Group)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::xlab("Year") +
    ggplot2::ylab("Proportion of Catch (GEMM)") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 14),
      axis.title = ggplot2::element_text(size = 14),
      strip.text.x = ggplot2::element_text(size = 14),
      legend.text = ggplot2::element_text(size = 14)
    ) +
    ggplot2::scale_fill_viridis_d()
  p2 <- ggplot2::ggplot(catch_by_catch_share_gear, ggplot2::aes(x = year, y = prop_landed, fill = Group)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::xlab("Year") +
    ggplot2::ylab("Proportion of Landings (GEMM)") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 14),
      axis.title = ggplot2::element_text(size = 14),
      strip.text.x = ggplot2::element_text(size = 14),
      legend.text = ggplot2::element_text(size = 14)
    ) +
    ggplot2::scale_fill_viridis_d()
  p3 <- ggplot2::ggplot(catch_by_catch_share_gear, ggplot2::aes(x = year, y = prop_discard, fill = Group)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::xlab("Year") +
    ggplot2::ylab("Proportion of Discards (GEMM)") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 14),
      axis.title = ggplot2::element_text(size = 14),
      strip.text.x = ggplot2::element_text(size = 14),
      legend.text = ggplot2::element_text(size = 14)
    ) +
    ggplot2::scale_fill_viridis_d()
  c1 <- ggplot2::ggplot(catch_by_catch_share_gear, ggplot2::aes(x = year, y = catch, fill = Group)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::ylim(as.numeric(ylim[1]), as.numeric(ylim[2])) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 14),
      axis.title = ggplot2::element_text(size = 14),
      strip.text.x = ggplot2::element_text(size = 14),
      legend.text = ggplot2::element_text(size = 14)
    ) +
    ggplot2::xlab("Year") +
    ggplot2::ylab("Catch (mt, GEMM)") +
    ggplot2::scale_fill_viridis_d()
  c2 <- ggplot2::ggplot(catch_by_catch_share_gear, ggplot2::aes(x = year, y = landed_mt, fill = Group)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::ylim(as.numeric(ylim[1]), as.numeric(ylim[2])) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 14),
      axis.title = ggplot2::element_text(size = 14),
      strip.text.x = ggplot2::element_text(size = 14),
      legend.text = ggplot2::element_text(size = 14)
    ) +
    ggplot2::xlab("Year") +
    ggplot2::ylab("Landings (mt, GEMM)") +
    ggplot2::scale_fill_viridis_d()
  c3 <- ggplot2::ggplot(catch_by_catch_share_gear, ggplot2::aes(x = year, y = discard_mt, fill = Group)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::xlab("Year") +
    ggplot2::ylab("Discards (mt, GEMM)") +
    ggplot2::ylim(as.numeric(ylim[1]), as.numeric(ylim[2])) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 14),
      axis.title = ggplot2::element_text(size = 14),
      strip.text.x = ggplot2::element_text(size = 14),
      legend.text = ggplot2::element_text(size = 14)
    ) +
    ggplot2::scale_fill_viridis_d()


  if (!is.null(dir)) {
    species_to_save <- tolower(species)
    if (grepl("/", species)) {
      species_to_save <- tolower(gsub("/", " ", species))
    }
    write.csv(catch_by_catch_share_gear,
      file = file.path(dir, paste0(species_to_save, "_gemm_catch_by_cs_gear.csv")),
      row.names = FALSE
    )
    cowplot::plot_grid(c1, c2, c3, ncol = 1, nrow = 3)
    ggplot2::ggsave(file.path(dir, paste0(species_to_save, "_gemm_catch.png")),
      height = 12, width = 12
    )
    cowplot::plot_grid(p1, p2, p3, ncol = 1, nrow = 3)
    ggplot2::ggsave(file.path(dir, paste0(species_to_save, "_gemm_proportions.png")),
      height = 12, width = 12
    )
  }

  return(catch_by_catch_share_gear)
}
