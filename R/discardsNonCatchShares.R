#' Function that calculates discards for the non-catch share vessels
#' 
#' @template dat
#' @param strat
#' @param B
#' @param conf.df
#' @param conf.df.cols
#' @param dat.cols
#' @param ratio
#' @param saveBootFile
#' @param logFile
#'
#'
#'
#' @author Allan Hicks and Chantel WEtzel
#' @export
#'
discardsNonCatchShares <- function(dat, strata, B, conf.df=NULL, conf.df.cols=NULL, dat.cols=NULL, ratio=c("proportion","expansion"), saveBootFile="outB.Rdat", logFile="") {
	#calculate catch shares discard quantities
	#I think I can set dat.cols=c("Years",strata)
	#check for CatchShares, create if necessary and add to strata
	if(!"CatchShares"%in%colnames(dat)) {
		cat("Keeping only Non-Catch Shares observations.\n")
		dat <- determineCatchShares(dat)
		cat("Removing",sum(dat$CatchShares),"rows from your dataframe.\n")
		dat <- dat[!dat$CatchShares,]
	}
	if(!"CatchShares"%in%strata) {
		strata <- c(strata,"CatchShares")
	}
	if(!"CatchShares"%in%conf.df.cols) {
		conf.df.cols <- c(conf.df.cols,"CatchShares")
	}
	if(!"CatchShares"%in%dat.cols) {
		dat.cols <- c(dat.cols,"CatchShares")
	}

	outB <- bootDiscardRatio.fn(dat, 
		yrColNm="ryear",
		strat=strata,
		B=B,
		vesselColNm="drvid",
		discard="dis",
		retained="ret",
		ratio=ratio,
		minVessels=1,
		writeLog=logFile)

	if(!is.null(saveBootFile)) {
		save(outB, file = saveBootFile)
	}
	dis <- bootSummary.fn(outB, B = B, strtNms = strata)

	if(!is.null(conf.df)) {
		# Combine each summary dataframe with confidentiality output
		# Note: Some strata will have vessels but no data because those strata did not catch the species.
		out <- merge(conf.df[,!colnames(conf.df)%in%c("unique.drvid","sum.dis","sum.ret")],
					 dis,
					 by.x = conf.df.cols,
					 by.y = dat.cols, 
					 all.y = T)

		if(any(is.na(out$numVessels) | (out$numVessels < 3&out$numVessels > 0))) {
			cat("\n-------------- WARNING ------------\n")
			cat("WARNING: There are confidential strata in your non-catch shares data.\n\n")
		}

	} else {
	  	out <- dis
	}

	out$ratio_type <- switch(
		ratio[1],
		proportion = 'DIS/(DIS + RET)',
		expansion  = 'DIS/RET',
		stop("ratio type must be 'proportion' or 'expansion'\n"))

 	colnames(out) <- c(
		'year', 'area', 'gear', 'catch_shares', 'n_obs', 'n_trips', 'n_hauls', 'n_vessels', 'nonconfidential', 'strata',
		'ob_discard_mt','ob_retained_mt','ob_ratio',
		'mean_boot_discard_mt','median_boot_discard_mt','sd_boot_discard_mt','cv_boot_discard_mt',
		'mean_boot_retain_mt','median_boot_retain_mt','sd_boot_retain_mt','cv_boot_retain_mt',
		'mean_boot_ratio','median_boot_ratio','sd_boot_ratio','cv_boot_ratio',
		'n_bootstrap','est_bias','ratio_type')
	
	out <- out[!as.logical(out$catch_shares),]
	
	keep <- c('year', 'area', 'gear', 'catch_shares', 'n_obs', 
		'n_trips', 'n_hauls', 'n_vessels', 'nonconfidential',
		'ob_discard_mt','ob_retained_mt','ob_ratio',
		'median_boot_discard_mt','sd_boot_discard_mt',
		'median_boot_ratio','sd_boot_ratio', 'n_bootstrap', 'ratio_type')
	
	sub_out <- out[ , colnames(out) %in% keep]
	
	# If not doing a bootstrap remove those columns from the output
	if(B == 1){
		out = out[ , c(1:13, dim(out)[2])]
	}
	
	sub_out <- as.data.frame(sub_out)
	ind <- which(sub_out[, "nonconfidential"] == "FALSE")
	sub_out[ind, 11:dim(sub_out)[2]] <- "redacted"

 	return_list <- list(out, sub_out)
	return(return_list)
}

