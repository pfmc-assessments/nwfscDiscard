#' Function to return a dataframe with only specified values, categorized
#' takes in the full data.frame
#' returns a list with:
#'      dat: the subsetted dataframe with columns and levels for each variable. All observations not meeting criteria are removed if subset=T
#'      numVessels: The number of unique vessels in each strata.
#'      stratObs: the number of observations in each stratum
#'      summaryUnique: unique observations over the subsetted dataframe for the specified columns
#' 
#' @template dat
#' @template  colnms
#' @template  colLevs
#' @template  stratNms
#' @param yrColNm the name of the column for year (if there is one). If numeric, it is the column number
#' @param yrs the years. NOT entered as in version 2 functions. MUST use the concatenate function. For example c(2003:2005,2007,2008:2010,2012)
#' the reason I did not use the '...' is because it gets confused when not including multiple arguments.
#' For example, if using ... and not entering colnms because you want to use default, it will think that one of the years is colnms
#' @param summaryNames names of column to provide a summary over
#' @param vesselColName the column that defines vessels
#' @param subset whether or not to return a subset of the data
#' @param verbose whether to output messages to screen. These are also returned in the data.frame
#'
#'
#'
#' @author Allan Hicks and Chantel Wetzel
#' @export
#'
strata.fn <- function(dat, colnms = NULL, colnms.new = paste0(colnms, "new"), colLevs = NULL, stratNms = NULL,
                      yrColNm = NULL, yrColNm.new = paste0(yrColNm,"new"), yrs = NULL,
                      subset = T, verbose = T) {
    if(is.null(colnms) & is.null(yrColNm)) {
        cat("No columns or years defined for selection\n")
        return(invisible(NULL))
    }
    if(!is.null(stratNms) && length(stratNms)!=length(colLevs)) {stop("When stratNms is supplied for non-numeric levels, is must have same number of list elements as colLevs\n")}
    messages <- NULL
    columnNames <- NULL

    out <- dat  #just in case it does not do year
    #first do year, if necessary
    if(!is.null(yrColNm) & !is.null(yrs)) {
        columnNames <- yrColNm.new
        if(is.numeric(yrColNm)) { yrColNm <- names(dat)[yrColNm] }
        cat("Subsetting by", yrColNm, "\n")

        out <- year.fn(dat = dat, 
                       colnm = yrColNm, 
                       yrs = yrs, 
                       nmcol = yrColNm.new, 
                       subset = F)

        removed <- sum(is.na(out[,yrColNm.new]))
        messages <- c(messages,paste(removed,"observations removed because year did not match"),paste(sum(!is.na(out[,yrColNm.new])),"observations kept because of a matching year"))
        if(verbose) {
            cat(messages[length(messages)-1],"\n")
            cat(messages[length(messages)],"\n")
            if(removed>0) print(table(out[is.na(out[,yrColNm.new]),yrColNm]))
            flush.console()
        }
        if(subset) {
            out <- out[!is.na(out[,yrColNm.new]),]
        }
    }

    #now subset other columns
    if(!is.null(colnms)) {
        cat("subsetting by",colnms,"\n")
        if(length(colnms) > 1 & (!is.list(colLevs) | length(colnms) != length(colLevs))) {
            stop("colLevs must be a list and have an equal number of elements to the length of colnms\n")
        }
        if(length(colnms) == 1 & !is.list(colLevs)) {
            #make colLevs a list of 1 element so that it works in generalization below
            colLevs <- list(colLevs)
        }

        for(i in 1:length(colnms)) {
            if(!is.null(stratNms[[i]]) && !(is.numeric(dat[,colnms[i]]) & is.numeric(colLevs[[i]])) && length(stratNms[[i]])!=length(colLevs[[i]])) {
                stop("Strata names in level ",i," does not match the number of column levels.\n")}
            if(!is.null(stratNms[[i]]) && (is.numeric(dat[,colnms[i]]) & is.numeric(colLevs[[i]])) && length(stratNms[[i]])!=(length(colLevs[[i]])-1)) {
                stop("Strata names in numeric level ",i," should have one less name than column levels.\n")}
            columnNames <- c(columnNames, colnms.new[i])
            out <- createStrata.fn(
                dat = out,
                colnm = colnms[i],
                vars = colLevs[[i]], 
                strataNames = stratNms[[i]],
                nmcol = colnms.new[i],
                subset = FALSE)

            removed <- sum(is.na(out[ ,colnms.new[i] ]))
            messages <- c(messages,paste(removed,"observations removed because",colnms[i],"did not match"),paste(sum(!is.na(out[,colnms.new[i]])),"observations kept because of a matching",colnms[i]))
            if(verbose) {
                cat(messages[length(messages)-1],"\n")
                cat(messages[length(messages)],"\n")
                if(removed>0) print(table(out[is.na(out[,colnms.new[i]]),colnms[i]]))
                flush.console()
            }
            if(subset) {
                out <- out[!is.na(out[,colnms.new[i]]),]
            }
            #Remove missing factor levels from new columns
            if(is.factor(out[,colnms.new[i]])) {
                out[,colnms.new[i]] <- factor(out[,colnms.new[i]])
            }
        }
    }
    attributes(out)$messages <- messages
    invisible(out)
}