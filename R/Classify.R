#' Clssifies unique values (characters, factors, etc)
#' Different from year in that year converts the year to integer, whereas this function does not.
#'
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
classify.fn<-function(dat, colnm, z, nmcol = paste(colnm,"new",sep=""), subset = F) {
  if(!(colnm %in% colnames(dat))) {
    stop("colnm ",colnm," is not a name in the dataframe\n")
  }
  if(subset) {
     out <- dat[dat[,colnm] %in% z,]
     out[,nmcol] <- out[,colnm]
  }
  if(!subset) {
    dat[,nmcol] <- NULL
     out <- dat
     out[dat[,colnm] %in% z, nmcol] <- out[dat[, colnm] %in% z,colnm]  #put in strata to new column and keep not selected as NA
  }
  return(invisible(out))
}