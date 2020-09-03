#' Function that checks if observation was made by a Catch Share vessel
#' 
#' @template dat
#' @param  yearLevs years to split the data according to catch shares, default = as.character(2011:max(dat$ryear)
#' @param sectorLevs  sectors that are appart of catch shares, default = list(c('Catch Shares','LE CA Halibut','Shoreside Hake')
#' @template  colnms
#' @template colnms.new
#'
#'
#'
#' @author Allan Hicks and Chantel WEtzel
#' @export
#'
determineCatchShares <- function(dat,
								 yearLevs = as.character(2011:max(dat$ryear)),
								 sectorLevs,
								 colnms = c("ryear", "sector"),
								 colnms.new = c("yearCS", "sectorCS")) {

	yearLevs <- as.character(yearLevs[yearLevs >= 2011])
	#determine and classify catch shares and non-catch shares
    dat$CatchShares <- F
	dat2 <-strata.fn(dat,
					 colnms = colnms,
		             colLevs = list(yearLevs, sectorLevs),
		             colnms.new = colnms.new,
		             subset = F,
		             verbose = F)   #with subset=F, year has to be in main strata to not remove observations
	
	dat2$CatchShares[!is.na(dat2$yearCS) & !is.na(dat2$sectorCS)] <- T

	return(dat2)
}