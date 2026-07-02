#' Format GEMM alternative data to match the WCGOP
#'
#' @param dir Directory where output will be saved. The directory where the file
#'   should be saved. If dir = NULL no output will be saved.
#' @param species_name A string to specify the species name that data should be
#'   summarized for. The string should match the name in the alternative
#'   gemm data file and needs to be a single species.
#' @param areas_to_keep A vector of area names in the alternative GEMM to filter
#'   the data for.
#' @param gears_to_keep A vector of gear names in the alternative GEMM to filter
#'    the data for.
#' @param data_file A string of the CSV file name of the alternative GEMM data
#'   provided by the FOS team for calculating data weights using alternative areas.
#'   The gear type (minor) and the gear group (major) are used to calculate totals
#'   by area and weighting proportions.
#'
#'
#' @author Chantel Wetzel
#' @export
#' @return dataframe
#'
#
format_gemm_alt <- function(
  dir,
  species_name,
  areas_to_keep,
  gears_to_keep,
  data_file = "SATAN_GEMM_2026-04-12.csv"
) {
  data <- read.csv(fs::path(dir, data_file))
  catch_share_sectors <- c(
    "CS - Bottom and Midwater Trawl",
    "CS - Bottom Trawl",
    "CS - Hook & Line",
    "CS - Pot",
    "CS EM - Bottom Trawl",
    "CS EM - Pot",
    "LE CA Halibut",
    "Midwater Hake",
    "Midwater Rockfish",
    "Midwater Hake EM",
    "Midwater Rockfish EM"
  )
  format_data <- data |>
    dplyr::filter(
      species == species_name,
      area %in% areas_to_keep
    ) |>
    dplyr::rename(
      gear = lilboy_fleet,
      gear_type = bigboy_fleet
    ) |>
    dplyr::mutate(
      gear = dplyr::case_when(
        gear == "bottom_trawl" ~ "Bottom Trawl",
        gear == "fixed_gear" ~ "Fixed Gears",
        gear == "hkl" ~ "Hook & Line",
        gear == "midwater_trawl" ~ "Midwater Trawl",
        gear == "pot" ~ "Pot",
        .default = "Shrimp Trawl"
      ),
      gear_type = gsub("_", "-", gear_type),
      catch_shares = dplyr::case_when(
        sector %in% catch_share_sectors ~ TRUE,
        sector %in% c(
          "CS EM - Bottom Trawl",
          "CS EM - Pot",
          "Midwater Hake EM",
          "Midwater Rockfish EM"
        ) &
          year >= 2024 ~ FALSE,
        .default = FALSE
      ),
      catch_shares = dplyr::case_when(
        year < 2011 ~ FALSE,
        .default = catch_shares
      ),
      gemm_dis_est = dplyr::coalesce(gemm_dis_est, 0),
      gemm_dis_est_area = dplyr::coalesce(gemm_dis_est_area, 0),
      landings_area = dplyr::coalesce(landings_area, 0),
      gemm_lnd_est = dplyr::coalesce(gemm_lnd_est, 0)
    ) |>
    dplyr::relocate(catch_shares, .after = sector) |>
    dplyr::filter(gear %in% gears_to_keep)

  return(format_data)
}
