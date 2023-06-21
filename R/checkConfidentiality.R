#' Bootstrap uncertainty and summarize WCGOP discard data
#'
#' @param dat passed ob data file filtered down to the species specific data (sp)
#' @param sp species name that should match the name in the ob data file
#' @template colnms 
#' @template colLevs
#' @param newColNm 
#' @param strNms 
#'
#'
#' @author Allan Hicks and Chantel Wetzel
#' @export
checkConfidentiality <- function(dat, colnms, colLevs, newColNm = paste(colnms,"new",sep=""), 
								 sectorLevs, strNms = NULL) {

	dat2 <-strata.fn(dat = dat, 
					 colnms = colnms, 
					 colLevs = colLevs, 
					 colnms.new = newColNm, 
					 stratNms = strNms)

	dat2$CatchShares <- F

	#separate catch shares and non-catch shares
	if(max(dat2$ryear) >= 2011) {
		dat2 <- determineCatchShares(dat = dat2, 
									 sectorLevs = sectorLevs,
									 yearLevs = as.character(2011:max(dat2$ryear)) )
	}
	cat("Summarizing strata across years to check for confidentiality. Please wait.\n")
	flush.console()

	out <- summarizeStrata.fn(dat = dat2, 
							  colnms = c(newColNm, "CatchShares"), 
							  yrColNm = "ryear")

	if(nrow(out[!out$OKnumVessels,]) > 0) {
		cat("THERE ARE SOME CONFIDENTIAL STRATA!\n")
		print(out[!out$OKnumVessels,])
	}
	return(out)
}





