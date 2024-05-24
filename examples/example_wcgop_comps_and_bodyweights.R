

# Example Script
check_confidential(
    dir = here::here(),
    data = OBCatch,
    gear_groups = list(c("Bottom Trawl", "Midwater Trawl", "Shrimp Trawl"), c("Hook & Line", "Pot")),
    gear_names = c("trawl", "fixed gear"),
    fleet_colname = "r_state.x",
    fleet_groups = list(c("WA", "OR", "CA")),
    fleet_names = c("coastwide"))

get_biological_data(
    dir = here::here(),
    data = OBBio2,
    species = "Sablefish",
    len_bins = seq(20, 90, 2),
    age_bins = 1:50,
    gear_groups = list(c("Bottom Trawl", "Midwater Trawl", "Shrimp Trawl"), c("Hook & Line", "Pot")),
    gear_names = c("trawl", "fixed gear"),
    fleet_colname = "r_state.x",
    fleet_groups = list(c("WA", "OR", "CA")),
    fleet_names = c("coastwide"))

get_mean_weights(
    dir = here::here(),
    data = OBCatch,
    species = "Sablefish",
    gear_groups = list(c("Bottom Trawl", "Midwater Trawl", "Shrimp Trawl"), c("Hook & Line", "Pot")),
    gear_names = c("trawl", "fixed gear"),
    fleet_colname = "r_state.x",
    fleet_groups = list(c("WA", "OR", "CA")),
    fleet_names = c("coastwide"))
