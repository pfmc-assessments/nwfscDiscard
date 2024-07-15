#' Bootstrap non-catch share data
#'
#' @details Bootstrap samples across the number of unique vessels in
#' each year, fleet, and return port group.
#'
#' @param data A list of data separated by year, fleet, and return port group.
#' @param boot_number The number of bootstraps to conduct.
#'
#'
#' @author Chantel Wetzel, Allan Hicks, and Jason Jannot
#' @export
#'
#
do_bootstrap <- function(
    data,
    boot_number) {
  boot_retain <- boot_discard <- rep(0, boot_number)
  for (i in 1:length(data)) {
    n_vessels <- unique(data[[i]][, "drvid"])
    for (b in 1:boot_number) {
      sampled_vessels <- sample(n_vessels, size = length(n_vessels), replace = TRUE)
      ind <- NULL
      for (j in 1:length(sampled_vessels)) {
        ind <- c(ind, which(data[[i]][, "drvid"] == sampled_vessels[j]))
      }
      boot_retain[b] <- boot_retain[b] + sum(data[[i]][ind, "ret_mt"])
      boot_discard[b] <- boot_discard[b] + sum(data[[i]][ind, "dis_mt"])
    }
  }

  return(list(
    boot_retain = boot_retain,
    boot_discard = boot_discard
  ))
}
