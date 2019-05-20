#'
#'
#' @param z list of strata names
#'
#' @export
#'
createStrataNames <- function(z) {
  strNms <- rep(NA,length(z))
  for(i in 1:length(z)) {
      strNms[i] <- paste(z[[i]],collapse="|")
  }
  return(strNms)
}