#' Plot mean weights with the CV
#'
#' @param dir Directory location to save files.
#' @param data A data frame produced by [get_mean_weights()]
#'
#' @author Chantel Wetzel
#' @export
#' @return Boxplot of discard mean weight by gear group and year.
#'
#'
plot_mean_weights <- function(
  data,
  dir = NULL
) {
  data_modified <- data |>
    dplyr::mutate(
      add = -obs * cv * qt(0.025, 30),
      lower_bound = obs - add,
      upper_bound = obs + add,
      lower_bound = dplyr::case_when(
        lower_bound < 0 ~ 0, .default = lower_bound
      )
    )

  g1 <- ggplot2::ggplot(data_modified, ggplot2::aes(x = year, y = obs)) +
    ggplot2::geom_errorbar(ggplot2::aes(
      ymin = lower_bound,
      ymax = upper_bound
    )) +
    ggplot2::geom_point() +
    ggplot2::theme_bw() +
    ggplot2::ylab("Mean Body Weight (kg)") +
    ggplot2::xlab("Year") +
    ggplot2::facet_wrap("fleet")

  if (!is.null(dir)) {
    ggplot2::ggsave(
      plot = g1,
      filename = file.path(dir, "discard_mean_body_weights.png"),
      height = 12,
      width = 12
    )
  } else {
    return(g1)
  }
}
