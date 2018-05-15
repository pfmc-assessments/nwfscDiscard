checkConfidentiality <- function(dat,colnms,colLevs,newColNm=paste(colnms,"new",sep=""),strNms=NULL) {

	dat2 <-strata.fn(dat,colnms=colnms,colLevs=colLevs,colnms.new=newColNm,stratNms=strNms)
	dat2$CatchShares <- F
	if(max(dat2$ryear)>=2011) {
		#separate catch shares and non-catch shares
		dat2 <- determineCatchShares(dat2,
    						 yearLevs=as.character(2011:max(dat2$ryear)))
	}
	cat("Summarizing strata across years to check for confidentiality. Please wait.\n")
	flush.console()

	out <- summarizeStrata.fn(dat2,colnms=c(newColNm,"CatchShares"),yrColNm="ryear")
	#return(out)
	if(nrow(out[!out$OKnumVessels,])>0) {
		cat("THERE ARE SOME CONFIDENTIAL STRATA!\n")
		print(out[!out$OKnumVessels,])
	}
	return(out)
}


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


if(F) {
	gearLevels <- list(c("Bottom Trawl","Midwater Trawl"),"Hook & Line","Pot","Shrimp Trawl")
	gearNames <- c("TRAWLS","H&L","POT","SHRIMP")
	depthLevels <- c(0,55,366,2000)
	depthNames <- c("Near","Shelf","Slope")

	out <- checkConfidentiality(ob,colnms=c("gear2","set_depth"),colLevs=list(gearLevels,depthLevels),strNms=list(gearNames,depthNames))


  		sectorLevs <- list(c('Catch Shares','LE CA Halibut','Shoreside Hake'))
		out2 <-strata.fn(out,colnms=c("ryear","sector"),
			             colLevs=list(as.character(2011:max(out$ryear)),sectorLevs),
			             colnms.new=c("yearCS","sectorCS"),
			             subset=F,verbose=T)   #with subset=F, it doesn't owrk because it classifies year separate from gears, so I had to put year in with the main strata

		out2 <-strata.fn(out,colnms=c("ryear"),
			             colLevs=list("2011"),
			             colnms.new=c("yearCS"),
			             subset=F,verbose=T)   #with subset=F, it doesn't owrk because it classifies year separate from gears, so I had to put year in with the main strata
}