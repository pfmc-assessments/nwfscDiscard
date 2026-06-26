#' Format GEMM alternative data to match the WCGOP
#'
#' @param dir Directory location to save files.
#' @param species_name species name that should match the name in the alternative
#'   gemm data file. Needs to be a single species.
#' @param areas_to_keep Area names in the alternative GEMM to filter the data for.
#' @param gears_to_keep Gear names in the alternative GEMM to filter the data for.
#' @param data_file CSV file name of the alternative GEMM data provided by the FOS
#'   team for calculating data weights using alternative areas. The gear type
#'   (minor) and the gear group (major) are used to calculate totals by area
#'   and weighting proportions.
#'
#'
#'
#' @author Chantel Wetzel
#' @export
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
      sector = dplyr::case_when(
        sector == "Pink Shrimp" ~ "Shrimp Trawl",
        .default = sector
      ),
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
        sector %in% c("CS EM - Bottom Trawl",
                      "CS EM - Pot", "Midwater Hake EM",
                      "Midwater Rockfish EM") &
        year >= 2024 ~ FALSE,
        .default = FALSE
      ),
      landings_area = dplyr::coalesce(landings_area, 0),
      gemm_lnd_est = dplyr::coalesce(gemm_lnd_est, 0)
    ) |>
    dplyr::relocate(catch_shares, .after = sector) |>
    dplyr::filter(gear %in% gears_to_keep)

  format_data$catch_shares[format_data$year < 2011] <- FALSE
  return(format_data)
}
