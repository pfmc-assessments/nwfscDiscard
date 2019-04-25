summarizeBoots <- function(outB,strata, B, conf.df=NULL, conf.df.cols=NULL, dat.cols=NULL) {
	dis <- Boot.sumry.fxn(outB,B=B,strtNms=strata)

	if(!all(dat.cols %in% names(dis))) {
		cat("Some columns are not available in dat.cols\nThe available columns are:\n",names(dis),"\n")
	}

	if(!is.null(conf.df)) {
		#Combine each summary dataframe with confidentiality output
		#   Note: Some strata will have vessels but no data because those strata did not catch the species.

		out <- merge(conf.df[,!colnames(conf.df)%in%c("unique.drvid","sum.dis","sum.ret")],
					 dis,
					 by.x=conf.df.cols,
					 by.y=dat.cols, all.y=T)
		if(any(is.na(out$numVessels) | (out$numVessels<3&out$numVessels>0))) {
			cat("\n-------------- WARNING ------------\n")
			cat("WARNING: There are confidential strata in your non-catch shares data.\n\n")
			#cat ("Press [Enter] to continue and acknowledge this.")
			#line <- readline()
		} #MAKE SURE IT IS FALSE
	} else {
	  	out <- dis
	}
	out$Ratio_Type='Standard = DIS/(DIS + RET)'
 	colnames(out)[(ncol(out)-17):ncol(out)] <- c('Observed_DISCARD.LBS','Observed_RETAINED.LBS','Observed_Ratio',
 		    'Mean.Boot_DISCARD.LBS','Median.Boot_DISCARD.LBS','StdDev.Boot_DISCARD.LBS','CV.Boot_DISCARD.LBS',
		    'Mean.Boot_RETAINED.LBS','Median.Boot_RETAINED.LBS','StdDev.Boot_RETAINED.LBS','CV.Boot_RETAINED.LBS',
		    'Mean.Boot_Ratio','Median.Boot_Ratio','StdDev.Boot_Ratio','CV.Boot_Ratio',
		    'no.Boot.samples','Est.Bias.Ratio','Ratio_Type')

	return(out)
}