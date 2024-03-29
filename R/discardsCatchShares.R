#' Function that calculates discards for the catch share vessels
#' 
#' @template dat
#' @param strat
#' @param conf.df
#' @param conf.df.cols
#' @param dat.cols
#' @param ratio
#' @param logFile
#'
#'
#'
#' @author Allan Hicks and Chantel WEtzel
#' @export
#'
discardsCatchShares <- function(dat, strata, conf.df = NULL, conf.df.cols = NULL, dat.cols = NULL, 
	ratio = c("proportion","expansion"), logFile = "") {
	#calculate catch shares discard quantities

	#check for CatchShares, create if necessary and add to strata
	if(!"CatchShares" %in% colnames(dat)) {
		cat("Keeping only Catch Shares observations.\n")
		dat <- determineCatchShares(dat)
		cat("Removing", sum(!dat$CatchShares), "rows from your dataframe.\n")
		dat <- dat[dat$CatchShares, ]
	}
	if(!"CatchShares" %in% strata) {
		strata <- c(strata, "CatchShares")
	}
	if(!"CatchShares" %in% conf.df.cols) {
		conf.df.cols <- c(conf.df.cols, "CatchShares")
	}
	if(!"CatchShares" %in% dat.cols) {
		dat.cols <- c(dat.cols, "CatchShares")
	}

	outB <- bootDiscardRatio.fn(dat, 
								yrColNm = "ryear",
								strat = strata,
								B = 0,
								vesselColNm = "drvid",
								discard = "dis",
								retained = "ret",
								ratio = ratio,
								minVessels = 1,
								writeLog = logFile)

	dis <- bootSummary.fn(outB, B = 0, strtNms = strata)

	if(!is.null(conf.df)) {
		# Combine each summary dataframe with confidentiality output
		# Note: Some strata will have vessels but no data because those strata did not catch the species.
		out <- merge(conf.df[ ,!colnames(conf.df) %in% c("unique.drvid", "sum.dis", "sum.ret")],
					 dis,
					 by.x=conf.df.cols,
					 by.y=dat.cols, all.y=T)
		if(any(is.na(out$numVessels) | (out$numVessels < 3&out$numVessels > 0))) {
			cat("\n-------------- WARNING ------------\n")
			cat("WARNING: There are confidential strata in your catch shares data.\n\n")
		} 
	} else {
	  	out <- dis
	}
	out$ratio_type <- switch(ratio[1],
							 proportion = 'DIS/(DIS + RET)',
							 expansion  = 'DIS/RET',
							 stop("ratio type must be 'proportion' or 'expansion'\n"))

 	colnames(out) <- c(
 		'year', 'area', 'gear', 'catch_shares', 'n_obs', 'n_trips', 'n_hauls', 'n_vessels', 'nonconfidential', 'strata',
 		'ob_discard_mt','ob_retained_mt','ob_ratio', 'ratio_type')
 	out <- as.data.frame(out)
 	out <- out[as.logical(out$catch_shares), ]
 	ind <- which(out[, "nonconfidential"] == "FALSE")
 	out[ind, 11:dim(out)[2]] <- "redacted"
	return(out)
}

