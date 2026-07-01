#' Bootstrap non-catch share data
#'
#' @details Bootstrap samples across year, fleet, and return port group.
#'
#' @param dir Directory location to save files.
#' @param data A data frame of WCGOP catch data filtered down to non-catch share data.
#' @param boot_number The number of bootstraps to conduct
#' @param boot_variable The column name to do the inner sampling across.
#' @param seed_number The seed number.
#' @param conf_data_check Dataframe with the number of observations, trips, and vessels by fleet.
#'
#' @author Chantel Wetzel, Allan Hicks, and Jason Jannot
#' @export
#'
#
boostrap_discard <- function(
  data,
  conf_data_check,
  dir = NULL,
  boot_number = boot_number,
  boot_variable = "r_port_group",
  seed_number = NULL
) {
  if (!is.null(seed_number)) {
    set.seed(seed_number)
  }
  years <- sort(unique(data[, "year"]))
  pe <- vector(mode = "list", length = length(years))
  out_df <- NULL

  for (yr in 1:length(years)) {
    pe[[yr]] <- list()

    data_yr <- data[which(data[, "year"] == years[yr]), ]

    # work within each strata separately, to get statistics by strata
    data_strat <- split(
      data_yr,
      data_yr[, "fleet"]
    )

    numVessels <- unlist(
      lapply(
        data_strat,
        function(x) {
          length(unique(x[, "drvid"]))
        }
      )
    )

    # I think the below snippit of code could be deleted:
    if (any(numVessels < 1)) {
      message <- years[yr]
      cli::cli_alert_warning(
        "There are 1 or 0 vessels in at least one of the strata for year {message}.
        Discard ratios will not be bootstrapped for these strata"
      )
    }

    for (s in 1:length(data_strat)) {
      pe[[yr]][[s]] <- c(
        sum(data_strat[[s]][, "dis_mt"]),
        sum(data_strat[[s]][, "ret_mt"])
      )
      pe[[yr]][[s]] <- c(pe[[yr]][[s]], pe[[yr]][[s]][1] / sum(pe[[yr]][[s]]))
      names(pe[[yr]][[s]]) <- c("discard", "retained", "ratio")

      boot_data <- split(
        data_strat[[s]],
        as.character(data_strat[[s]][, boot_variable])
      )

      boot_samples <- bootstrap(
        data = boot_data,
        boot_number = boot_number
      )

      n <- length(boot_samples$boot_discard)

      df <- data.frame(
        year = rep(years[yr], n),
        fleet = rep(names(data_strat)[s], n),
        obs_discard = rep(pe[[yr]][[s]][1], n),
        obs_retained = rep(pe[[yr]][[s]][2], n),
        obs_ratio = rep(pe[[yr]][[s]][3], n),
        discard = boot_samples$boot_discard,
        retained = boot_samples$boot_retain,
        ratio = boot_samples$boot_discard /
          (boot_samples$boot_discard + boot_samples$boot_retain)
      )

      out_df <- rbind(out_df, df)
    }
  }

  actual_obs <- data |>
    dplyr::summarise(
      .by = c("year", "fleet"),
      n_ret = sum(ret_mt > 0),
      n_dis = sum(dis_mt > 0)
    )

  boot_out <- out_df |>
    dplyr::summarise(
      .by = c("year", "fleet"),
      n_boot = boot_number,
      obs_discard = mean(obs_discard),
      obs_retained = mean(obs_retained),
      obs_ratio = mean(obs_ratio),
      median_discard = median(discard),
      variance_discard = var(discard),
      sd_discard = sd(discard),
      median_ratio = median(ratio),
      var_ratio = var(ratio),
      sd_ratio = sd(ratio)
    ) |>
    dplyr::ungroup()

  boot_and_obs <- dplyr::left_join(
    x = boot_out,
    y = actual_obs,
    by = c("fleet", "year")
  )

  if ("catch_shares" %in% colnames(conf_data_check)) {
    conf_data_check <- conf_data_check |>
      dplyr::filter(catch_shares == FALSE) |>
      dplyr::select(-catch_shares)
  }

  all_boot_data <- dplyr::left_join(
    y = boot_and_obs,
    x = conf_data_check,
    by = c("fleet", "year")
  ) |>
    dplyr::filter(n_vessels >= 3) |>
    data.frame()

  if (!is.null(dir)) {
    write.csv(
      all_boot_data,
      file = file.path(dir, "wcgop_discard_rates_noncatch_share.csv"),
      row.names = FALSE
    )
  } else {
    cli::cli_alert_info(
      "No directory provided. Catch share discard rates not saved."
    )
  }
  return(all_boot_data)
}
