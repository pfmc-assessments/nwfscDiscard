#' clssifies values that may contain multiple levels per factor  (characters, factors, etc)
#' e.g., WA & OR into WaOR
#' different from year in that year converts the year to integer, whereas this function does not
#'
#' @template dat 
#' @template colnm 
#' @template z 
#' @template nmcol 
#' @template subset 
#'
#' @author Allan Hicks and Chantel Wetzel
#' @export
#'
classifyMult.fn<-function(dat, colnm, z, nmcol = paste(colnm,"new",sep=""), strNms = NULL, subset = F) {
  if(!(colnm %in% colnames(dat))) {
    stop("colnm ",colnm," is not a name in the dataframe\n")
  }

  dat[,nmcol] <- NA
  if(is.null(strNms)) {
    strNms <- createStrataNames(z)
  }

  if(length(z) != length(strNms)) {stop("Stratum names is not equal to number of levels",z,"\n")}
  for(i in 1:length(z)) {
    dat[dat[,colnm] %in% z[[i]],nmcol] <- strNms[i]
  }
  
  if(subset) {
    dat <- dat[!is.na(dat[,nmcol]),]
  }
  return(invisible(dat))
}