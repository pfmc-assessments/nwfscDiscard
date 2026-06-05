#' Format GEMM alternative data to match the WCGOP
#'
#'
#' @param data Data frame of the alternative GEMM data provided by the FOS
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
  data
) {
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
      catch_shares = dplyr::case_when(
        sector %in% catch_share_sectors ~ TRUE,
        .default = FALSE
      )
    ) |>
    dplyr::relocate(catch_shares, .after = sector)

  format_data$catch_shares[format_data$year < 2011] <- FALSE
  return(format_data)
}
