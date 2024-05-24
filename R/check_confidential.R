#' Check confidentiality using the catch data
#'
#'
#' @param dir Directory location to save files.
#' @param data A data frame of WCGOP catch data
#' @param gear_groups List of gear types to group together
#' (example: list(c("Bottom Trawl", "Midwater Trawl"), c("Hook & Line", "Pot", "Shrimp Trawl"))).
#' @param gear_name Vector of gear group names (example: c("trawl", "fixed gear")).
#' @param fleet_colname Column to use to determine areas for fleets (example: "r_state.x")
#' @param fleet_groups List of fleet groups to use (example: list(c("WA", "OR", "CA"))).
#' @param fleet_names Vector of fleet names (example: c("coastwide")).
#'
#' @author Chantel Wetzel
#' @export
#'
#'
check_confidential <- function(
  dir,
  data,
  gear_groups,
  gear_names,
  fleet_colname,
  fleet_groups,
  fleet_names){

  # Remove duplicate columns
  data <- data[, which(!colnames(data) %in% c("MT", "SPGRFTOB1", "SCIENTIFIC_NAME"))]
  colnames(data)[which(colnames(data) == "gear")] <- "gear_to_use"
  colnames(data) <- tolower(colnames(data))
  data$year <- data$ryear

  data$catch_shares <- "FALSE"
  catch_shares <- c("Catch Shares", "Catch Shares EM", "Midwater Hake", "LE CA Halibut")
  find <- which(data$sector %in% catch_shares & data$year >= 2011)
  data$catch_shares[find] <- "TRUE"

  if (fleet_colname == "r_state.x") {
    fleet_colname <- "r_state"
  }
  data <- create_groups(
    data = data,
    gear_groups = gear_groups,
    gear_names = gear_names,
    fleet_colname = fleet_colname,
    fleet_groups = fleet_groups,
    fleet_names = fleet_names)

  vessels_by_year_cs <- data |>
  	dplyr::group_by(year, gear_groups, fleet_groups, catch_shares) |>
  	dplyr::reframe(
  	  n_vessels = length(unique(drvid))
    ) |>
    dplyr::ungroup()

  vessels_by_year_cs <- as.data.frame(vessels_by_year_cs)
  write.csv(vessels_by_year_cs,
            file = file.path(dir, paste0(tolower(species), "_cs_confidentiality.csv")),
            row.names = FALSE)

  vessels_by_year <- vessels_by_year_cs |>
    dplyr::group_by(year, gear_groups, fleet_groups) |>
    dplyr::reframe(
      n_vessels = sum(n_vessels)
    ) |>
    dplyr::ungroup()
  vessels_by_year <- as.data.frame(vessels_by_year)
  write.csv(vessels_by_year,
            file = file.path(dir, paste0(tolower(species), "_confidentiality.csv")),
            row.names = FALSE)

  return(vessels_by_year_cs)
}