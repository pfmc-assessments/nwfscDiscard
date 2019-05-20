#' Bootstrap uncertainty and summarize WCGOP discard data
#'
#' @template ob
#' @param sp species name that should match the name in the ob data file, can be single species or multiple names (e.g. c("Gopher Rockfish", "Black and Yellow Rockfish"))
#' @template colnms 
#' @template colnms.new 
#' @template colLevs
#' @template stratNms 
#' @param ratioType 
#' @param bootFile file with the bootstap output
#' @param resultsFile file with the summarized and final bootstap values
#'
#'
#' @author Allan Hicks and Chantel Wetzel
#' @export
#'
#' @examples
#'
#'   \dontrun{
#'      # define what data  you want to bootstrap
#'      gearLevels <- c("Bottom Trawl")
#'		gearNames <- c("BottomTrawl")
#'`		stateLevels <- list("CA","OR","WA")  #r_state
#'		stateNames <- c("CA","OR","WA")
#'
#'     # Call function
#'     out <- bootstrapDiscardData(ob, sp="Petrale Sole", B=10000,
#'	            colnms=c("gear2","r_state"),
#'	            colnms.new=c("gear3","State"),
#'				colLevs=list(gearLevels,stateLevels),
#'				stratNms=list(gearNames,stateNames),
#'				bootFile='Petrale_bootstap.out',
#'				resultsFile='Petrale_bootstap.out')
#'
#'	   write.csv(out$ncs, 'Petrale_OB_DisRatios_ncs.csv', row.names=F)
#'	   write.csv(out$cs,  'Petrale_OB_DisRatios_cs.csv', row.names=F)
#'
#'   }
#'
bootstrapDiscardData <- function(ob, sp, B, colnms, colnms.new, colLevs, stratNms, ratioType = c("proportion","expansion"), bootFile, resultsFile) {

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
