determineCatchShares <- function(dat,
								 yearLevs=as.character(2011:max(dat$ryear)),
								 sectorLevs=list(c('Catch Shares','LE CA Halibut','Shoreside Hake')),
								 colnms=c("ryear","sector"),
								 colnms.new=c("yearCS","sectorCS")) {

		yearLevs <- as.character(yearLevs[yearLevs >= 2011])
		#determine and classify catch shares and non-catch shares
    	dat$CatchShares <- F
		dat2 <-strata.fn(dat,colnms=colnms,
			             colLevs=list(yearLevs,sectorLevs),
			             colnms.new=colnms.new,
			             subset=F,verbose=F)   #with subset=F, year has to be in main strata to not remove observations
		dat2$CatchShares[!is.na(dat2$yearCS) & !is.na(dat2$sectorCS)] <- T

		return(dat2)
}