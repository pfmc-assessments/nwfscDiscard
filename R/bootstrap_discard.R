#' Bootstrap non-catch share data
#'
#' @details Bootstrap samples across year, fleet, and return port group.
#'
#' @param dir Directory location to save files.
#' @param data A data frame of WCGOP catch data filtered down to non-catch share data.
#' @param boot_number The number of bootstraps to conduct
#' @param boot_variable The column name to do the inner sampling across.
#' @param seed The seed number.
#' @param redact Switch to remove any confidential data records from output.
#'
#'
#' @author Chantel Wetzel, Allan Hicks, and Jason Jannot
#' @export
#'
#
boostrap_discard <- function(
  dir = NULL,
  data,
  boot_number = boot_number,
  boot_variable = "r_port_group",
  seed_number = NULL) {

  if (!is.null(seed_number)) {
    set.seed(seed_number)
  }
  years <- sort(unique(data[, "year"]))
  pe <- vector(mode = "list", length = length(years))
  out_df <- NULL

  for (yr in 1:length(years)) {

    pe[[yr]] <- list()

    data_yr <- data[which(data[,"year"] == years[yr]), ]

    # work within each strata separately, to get statistics by strata
    data_strat <- split(
      data_yr,
      data_yr[, "fleet"])

    numVessels <- unlist(
      lapply(
        data_strat, function(x) { length(unique(x[, "drvid"]))} ))

    # I think the below snippit of code could be deleted:
    if(any(numVessels < 1)) {
      message("WARNING: fewer than 1 vessels in at least one of the strata for year ", years[yr], "\n")
      message("Discard ratios will not be bootstrapped for these strata\n")
    }

    for(s in 1:length(data_strat)) {

        pe[[yr]][[s]] <- c(sum(data_strat[[s]][,"dis_mt"]), sum(data_strat[[s]][,"ret_mt"]))
        pe[[yr]][[s]] <- c(pe[[yr]][[s]], pe[[yr]][[s]][1] / sum(pe[[yr]][[s]]))
        names(pe[[yr]][[s]]) <- c("discard", "retained", "ratio")

        boot_data <- split(
          data_strat[[s]],
          as.character(data_strat[[s]][, boot_variable]))

        boot_samples <- do_bootstrap(
          data = boot_data,
          boot_number = boot_number)

        n <- length(boot_samples$boot_discard)

        df <- data.frame(
          year = rep(years[yr], n),
          fleet = rep(names(data_strat)[s], n),
          obs_discard = rep(pe[[yr]][[s]][1], n),
          obs_retained = rep(pe[[yr]][[s]][2], n),
          obs_ratio = rep(pe[[yr]][[s]][3], n),
          discard = boot_samples$boot_discard,
          retained = boot_samples$boot_retain,
          ratio = boot_samples$boot_discard / (boot_samples$boot_discard + boot_samples$boot_retain)
        )

        out_df <- rbind(out_df, df)
    }
  }

  boot_out <- out_df |>
    dplyr::group_by(year, fleet) |>
    dplyr::summarise(
      n_boot = boot_number,
      obs_discard = mean(obs_discard),
      obs_retained = mean(obs_retained),
      obs_ratio = mean(obs_ratio),
      median_discard = median(discard),
      sd_discard = sd(discard),
      median_ratio = median(ratio),
      sd_ratio = sd(ratio)
    ) |>
    dplyr::ungroup()

  cf_data <- data |>
    dplyr::group_by(year, fleet) |>
    dplyr::summarise(
      n_obs = dplyr::n(),
      n_hauls = length(unique(haul_id)),
      n_trips = length(unique(trip_id)),
      n_vessels = length(unique(drvid))
    ) |>
    dplyr::ungroup()

  all_boot_data <- dplyr::left_join(cf_data, boot_out)
  all_boot_data <- as.data.frame(all_boot_data)

  if (!is.null(dir)) {
    species <- tolower(unique(data[, "species"]))
    remove <- ifelse(all_boot_data$n_vessels >= 3, FALSE, TRUE)
    if (any(remove)) {
      all_boot_data_redacted <- all_boot_data
      all_boot_data_redacted[remove, c("observed_discard_mt", "observed_retained_mt", "discard_rate")] <- "confidential"
      write.csv(all_boot_data_redacted,
                file = file.path(dir, paste0(tolower(species), "_noncatch_share_discards_redacted.csv")),
                row.names = FALSE)
    }
    write.csv(all_boot_data,
              file = file.path(dir, paste0(tolower(species), "_noncatch_share_discards.csv")),
              row.names = FALSE)
  } else {
    warning("No directory provided. Catch share discard rates not saved.")
  }

  return(all_boot_data)
}
