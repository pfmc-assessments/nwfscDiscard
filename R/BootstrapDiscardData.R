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
bootstrapDiscardData <- function(ob, sp, B, colnms, colnms.new, colLevs, stratNms, 
	ratioType = c("proportion", "expansion"), switch_names = TRUE,
	sectorLevs = list(c('Catch Shares', 'Catch Shares EM', 'LE CA Halibut','Shoreside Hake')),
	bootFile, resultsFile) {

	dte = Sys.Date()

	# The defualt in the code is to use the "ret" and "dis" columns which are in pounds
	# Providing these value is terms of metric tons allow for use without conversion.
	# Overwrite the "ret" and "dis" columns with the "RET_MT" and "DIS_RET" to avoid having to 
	# make changes in all the functions.
	# Additionally, columns names have been revised in WCGOP.  The below changes allow the code to
	# run without making changes thoughout.
	if(sum(colnames(ob) == "TRIP_ID") == 1){
		ob$ret     <- ob$RET_MT
		ob$dis     <- ob$DIS_MT
		ob$area    <- ob$AREA
		ob$ryear   <- ob$RYEAR
		ob$year    <- ob$YEAR
		ob$drvid   <- ob$DRVID
		ob$trip_id <- ob$TRIP_ID
		ob$haul_id <- ob$HAUL_ID
		ob$r_state <- ob$R_STATE
		ob$set_lat <- ob$SET_LAT
		ob$rmonth  <- ob$RMONTH
		ob$d_port_group <- ob$D_PORT_GROUP
		ob$r_port_group <- ob$R_PORT_GROUP
	}

	if(sum(colnames(ob) == "EMTRIP_ID") == 1){
		ob$ret     <- ob$RET_MT
		ob$dis     <- ob$DIS_MT
		ob$area    <- ob$AREA
		ob$ryear   <- ob$YEAR
		ob$year    <- ob$YEAR
		ob$drvid   <- ob$DRVID
		ob$trip_id <- ob$EMTRIP_ID
		ob$haul_id <- ob$HAUL_ID
		ob$r_state <- ob$R_STATE
		ob$set_lat <- ob$SET_LAT
		ob$d_port_group <- ob$D_PORT
		ob$r_port_group <- ob$R_PORT
	} 

	# This function uses the strat.fn (which calls the createStrata.fn, classify.fn, classifyMult.fn)
	# and the determineCatchShares (if year >= 2011)
	# Creates a data frame showing gear by year, area, catch shares, number of observations, vessels,
	# discards, and retained fish weights
	conf.df <- checkConfidentiality(
		dat = ob,
		colnms = colnms,
		colLevs = colLevs,
		newColNm = colnms.new,
		sectorLevs = sectorLevs,
		strNms = stratNms)

	dat <- ob[ob$species %in% sp, ]
	rm(ob)

	dat2 <- strata.fn(
		dat = dat,
		colnms = colnms,
		colnms.new = colnms.new,
		colLevs = colLevs,
		stratNms = stratNms)

	#split by catch shares
	#first add on CatchShares column with T or F
	dat2 <- determineCatchShares(
		dat2, 
		sectorLevs = sectorLevs, 
		yearLevs=sort(unique(dat2$ryear)))

	dat2.cs  <- dat2[dat2$CatchShares, ]
	dat2.ncs <- dat2[!dat2$CatchShares, ]
	rm(dat2)

	#bootstrap ratio, also reports discard mts and cv

	if(nrow(dat2.cs) > 0) { #calculate catch shares discard quantities
		dat.cs.out <- discardsCatchShares(
			dat2.cs, 
			strata = colnms.new,
			conf.df = conf.df, 
			conf.df.cols = c('ryear', colnms.new),
			dat.cols = c('Years', colnms.new),
			ratio = ratioType,
			logFile = "")
	} else {
		dat.cs.out <- NULL
	}

	if(nrow(dat2.ncs) > 0) { #calculate catch shares discard quantities
		dat.ncs.out <- discardsNonCatchShares(
			dat = dat2.ncs, 
			strata = colnms.new, 
			B = B,
			conf.df = conf.df, 
			conf.df.cols = c('ryear', colnms.new),
			dat.cols = c('Years', colnms.new),
			ratio = ratioType,
			saveBootFile = paste0(bootFile, "_dte_ncs.Rdat"),
			logFile = "")
	} else {
		dat.ncs.out <- NULL
	}
	#save results
	save(dat.ncs.out, file = paste0(resultsFile, "_dte_ncs.Rdat"))

	return(list(cs = dat.cs.out, ncs = dat.ncs.out))
}
