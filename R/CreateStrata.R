#' 
#' This function will classify and subset the data frame based on a single variable and a set of values or cut points.
#' It produces an additional column with the strata levels.
#' If the variable is numeric, it creates a strata name that indicates the range of values in that strata
#' Note that numeric cuts are always closed on the left and open on the right
#' For non-numeric variables, the specific quantities are subset.

#' @template dat
#' @template colnm
#' @param vars the variables to subset by.
#'         If numeric & the variable is numeric (dat[,colnm]):
#'                     this must contain the lowest boundary and the upper boundary, and all divisions in between.
#'                     Anything less than the lowest boundary or greater than or equal to the largest boundary are given NA.
#'                     Note that anything EQUAL TO the largest boundary is given an NA
#' 
#'         if the variable to be stratified (dat[,colnm]) is a character or factor and the vars is numeric, character, or factor:
#'                    it simply subsets based on the specific values using the classifyV3.fxn
#' 
#'         if the variable to be stratified (dat[,colnm]) is numeric and the vars is not numeric:
#'                    A warnin gis issued that this is probably not the desired behavior (but could be, for example year, although I use yr.fxn)
#' @param strataNames
#' @template nmcol
#' @template subset
#'
#' @export
#'
createStrata.fn<-function(dat, colnm, vars, strataNames = NULL, nmcol = paste0(colnm, "new"), subset = F) 
{
  flag <- F
  dat[,nmcol] <- NA
  if(is.numeric(dat[,colnm]) & is.numeric(vars)) {   #both are numeric, so use FindInterval (left-hand side is closed, thus it categorizes by [low,high)
      tmp <- findInterval(dat[,colnm], sort(vars))
      tmp[tmp == 0] <- NA   #zero indicates it is less than lowest index value
      tmp[tmp == (length(vars))] <- NA   #the maximum number indicates that it is greater than the maximum strata value entered
      
      if(is.null(strataNames)) {
        strataNames <- createStrataNames(vars)
      }

      dat[ ,nmcol] <- strataNames[tmp]
      flag <- T

      if(!is.null(strataNames)) {
        cat("\nPlease check that strata names for continuous variables correctly match up\n\n")
      }
  }

  if(!is.list(vars)) {
      #change vars to character
      if((is.character(dat[ ,colnm]) | is.factor(dat[ ,colnm]))) {   
        if(is.numeric(vars)) { vars <- as.character(vars) }
        dat <- classify.fn(dat, colnm, vars, nmcol, subset)
        if(!is.null(strataNames)) {
            dat[,nmcol] <- strataNames[match(dat[ ,nmcol], vars)]
        }
      }
      subset <- F
      flag <- T

      if(is.numeric(dat[,colnm]) & !is.numeric(vars)) {
        cat("The data column",colnm,"is numeric, but the vars",vars,"is not numeric.\n")
        cat("This may not be the desired behavior, but it is subsetting the numeric variable by those specific values\n")
        cat("Please check your results\n")
        dat <- classify.fn(dat, colnm, vars, nmcol, subset)
        subset <- F
        flag <- T
        if(!is.null(strataNames)) {
            dat[ ,nmcol] <- strataNames[match(dat[ ,nmcol], vars)]
        }
      }
  }

  if(is.list(vars)) {
      if((is.character(dat[ ,colnm]) | is.factor(dat[ ,colnm]))) {   #change vars to character
        if(is.numeric(vars)) { vars <- as.character(vars) }
        dat <- classifyMult.fn(dat, colnm, vars, nmcol, strataNames, subset)
      }
      subset <- F
      flag <- T

      if(is.numeric(dat[ ,colnm]) & !any(unlist(lapply(vars, is.numeric)))) {
        cat("The data column", colnm, "is numeric, but the vars is not numeric.\n")
        cat("This may not be the desired behavior, but it is subsetting the numeric variable by those specific values\n")
        cat("Please check your results\n")
        dat <- classifyMult.fn(dat, colnm, vars, nmcol, strataNames, subset)
        subset <- F
        flag <- T
      }
  }

  if(!flag) {stop("Could not determine types of colnm (numeric, character, or factor)\n")}

  if(subset) {
     out <- dat[!is.na(dat[,nmcol]),]
  }
  if(!subset) {
     out <- dat
  }
  return(invisible(out))
}