#' Format GEMM data to match the WCGOP
#'
#' @param dir Directory where output will be saved. The directory where the file
#'   should be saved. If dir = NULL no output will be saved.
#' @param data Dataframe of GEMM data from [nwfscSurvey::pull_gemm()]
#' @param species_name A string to specify the species name that data should be
#'   summarized for. The string should match the name in the gemm data file and
#'   needs to be a single species.
#' @param areas_to_keep A vector of area names in the GEMM grouping column to filter
#'   the data for.
#' @param gears_to_keep A vector of gear names to filter the data for. Gear is
#'   defined based on the sector column in the GEMM dataframe with the gear
#'   definitions based on WCGOP gear-sector specifications.
#'
#'
#' @author Chantel Wetzel
#' @export
#' @return dataframe
#'
#
format_gemm <- function(
  data,
  species_name,
  areas_to_keep,
  gears_to_keep,
  dir = NULL
) {
  format_data <- data |>
    dplyr::filter(species == species_name, grouping %in% areas_to_keep) |>
    dplyr::filter(!sector %in% c(
      "California Recreational",
      "Oregon Recreational",
      "Washington Recreational",
      "Incidental",
      "Research"
    )) |>
    dplyr::mutate(
      gear = dplyr::case_when(
        sector %in% c(
          "CS - Hook & Line", "Directed P Halibut", "LE Fixed Gear DTL - Hook & Line",
          "LE Sablefish - Hook & Line", "OA Fixed Gear - Hook & Line"
        ) ~ "Hook & Line",
        sector == "Nearshore" ~ "Fixed Gear",
        sector %in% c(
          "CS - Pot", "CS EM - Pot", "LE Fixed Gear DTL - Pot", "OA Fixed Gear - Pot",
          "LE Sablefish - Pot"
        ) ~
          "Pot",
        .default = "Bottom Trawl"
      ),
      gear_type = dplyr::case_when(
        gear == "Hook & Line" ~ "hook-and-line",
        gear == "Fixed Gear" ~ "hook-and-line",
        gear == "Pot" ~ "pot",
        .default = "trawl"
      ),
      catch_shares = dplyr::case_when(
        sector %in% c(
          "CS - Bottom and Midwater Trawl",
          "CS - Bottom Trawl", "CS - Hook & Line", "CS - Pot", "CS EM - Bottom Trawl",
          "CS EM - Pot",
          "Midwater Rockfish", "Midwater Hake EM", "Midwater Rockfish EM"
        ) ~ TRUE,
        sector %in% c("LE CA Halibut", "At-Sea Hake CP", "At-Sea Hake MSCV") & year >= 2011 ~ TRUE,
        .default = FALSE
      )
    ) |>
    dplyr::rename(
      gemm_dis_est_area = total_discard_mt,
      landings_area = total_landings_mt
    ) |>
    dplyr::relocate(catch_shares, .after = sector) |>
    dplyr::filter(gear %in% gears_to_keep)

  return(format_data)
}
