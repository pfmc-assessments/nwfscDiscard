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