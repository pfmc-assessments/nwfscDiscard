#' Plot discard rates
#'
#' @param dir Directory where output will be saved. The directory where the file
#'   should be saved. If dir = NULL no output will be saved.
#' @param data A data frame produced by [weight_discard_rates()]
#'
#' @author Chantel Wetzel
#' @export
#' @return Boxplot of discard mean weight by gear group and year.
#'
#'
plot_discard_rates <- function(
    data,
    dir = NULL
) {

  data_modified <- data |>
    dplyr::mutate(
      lower_bound = qnorm(0.025, discard_rate, sd),
      upper_bound = qnorm(0.975, discard_rate, sd),
      lower_bound = dplyr::case_when(
        lower_bound < 0 ~ 0, .default = lower_bound
      )
    )

  g1 <- ggplot2::ggplot(data_modified, ggplot2::aes(x = year, y = discard_rate)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lower_bound, ymax = upper_bound)) +
    ggplot2::geom_point() +
    ggplot2::theme_bw() +
    ggplot2::ylab("Discard rates") +
    ggplot2::xlab("Year") +
    ggplot2::facet_wrap("fleet")

  if(!is.null(dir)){
    ggplot2::ggsave(
      plot = g1,
      filename = file.path(dir, "discard_rates.png"),
      height = 12,
      width = 12
    )
    invisible(g1)
  } else {
    return(g1)
  }
}