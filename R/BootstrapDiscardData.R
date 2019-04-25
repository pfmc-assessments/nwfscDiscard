bootstrapDiscardData <- function(ob,sp,B,colnms,colnms.new,colLevs,stratNms,ratioType=c("proportion","expansion"),bootFile,resultsFile) {

	dte=Sys.Date()

	#I could put checkConf here
	conf.df <- checkConfidentiality(ob,colnms=colnms,
				colLevs=colLevs,
				newColNm=colnms.new,
				strNms=stratNms)

	dat <- ob[ob$species %in% sp,]
	dat2 <-strata.fn(dat,
		            colnms=colnms,
		            colnms.new=colnms.new,
					colLevs=colLevs,
					stratNms=stratNms)

	#split by catch shares
	#first add on CatchShares column with T or F
	dat2 <- determineCatchShares(dat2, yearLevs=sort(unique(dat2$ryear)))
	dat2.cs <- dat2[dat2$CatchShares,]
	dat2.ncs <- dat2[!dat2$CatchShares,]

	#bootstrap ratio, also reports discard lbs and cv

	if(nrow(dat2.cs)>0) { #calculate catch shares discard quantities
		dat.cs.out <- discardsCatchShares(dat2.cs, strata=colnms.new,
							conf.df=conf.df, conf.df.cols=c('ryear',colnms.new),
							dat.cols=c('Years',colnms.new),
							ratio=ratioType,logFile="")
	} else {
		dat.cs.out <- NULL
	}

	if(nrow(dat2.ncs)>0) { #calculate catch shares discard quantities
		dat.ncs.out <- discardsNonCatchShares(dat2.ncs, strata=colnms.new, B=B,
						conf.df=conf.df, conf.df.cols=c('ryear',colnms.new),
						dat.cols=c('Years',colnms.new),
						ratio=ratioType,
						saveBootFile=paste0(bootFile,"_dte_ncs.Rdat"),
						logFile="")
	}
	#save results
	save(dat.ncs.out,file=paste0(resultsFile,"_dte_ncs.Rdat"))

	return(list(cs=dat.cs.out,ncs=dat.ncs.out))
}
