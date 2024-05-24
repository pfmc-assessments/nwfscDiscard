#' Create gear and fleet groups
#'
#'
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
create_groups <- function(
  data,
  gear_groups,
  gear_names,
  fleet_colname,
  fleet_groups,
  fleet_names){

  # Assign gear and fleet groups
  data$gear_groups <- NA
  for (g in 1:length(gear_groups)){
    find <- which(data$gear_to_use %in% unlist(gear_groups[g]))
    data[find, "gear_groups"] <- gear_names[g]
  }
  if (sum(is.na(data[, "gear_groups"])) > 0) {
    ind <- which(is.na(data[, "gear_groups"]))
    gear <- unique(data[ind, "gear_to_use"])
    glue::glue("The following gears are not included in the gear groupings and will be omitted: {gear}.")
    data <- data[!is.na(data$gear_groups), ]
  }
  data$fleet_groups <- NA
  for (f in 1:length(fleet_groups)){
    find <- which(data[, fleet_colname] %in% unlist(fleet_groups[f]))
    data[find, "fleet_groups"] <- fleet_names[f]
  }
  if (sum(is.na(data$fleet_groups)) > 0) {
    ind <- which(is.na(data$fleet_groups))
    fleet <- unique(data[ind, fleet_colname])
    glue::glue("The following state/areas are not included in the fleet groupings and will be omitted: {fleet}.")
    data <- data[!is.na(data$fleet_groups), ]
  }
  return(data)
}