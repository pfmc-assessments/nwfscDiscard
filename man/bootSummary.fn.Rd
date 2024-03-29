% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BootSummary.R
\name{bootSummary.fn}
\alias{bootSummary.fn}
\title{A function to summarize the output from bootstrappping using the
 'bootstrapDiscardRatio_ach.r' functionby Allan Hicks.
-------------------------------------------------------------------------------
 Arguments to the function
____________________________
     dat = the data, in list format, from the bootstrapDiscardRatio_ach.r function
     B = the number of bootstrap iterations used in the function.  This argument
         is used to give the appropriate output.  If set B=0, then the output is simply
         the observed ratios by strata in dataframe format.  If B = anything else
         (including NULL = default), then the output is a dataframe & a postscript file of
         plots of the actual distribution of the bootstrapped values.
    fname = the file name where the postscript file is to be saved
    strtNms = names you want to assign to strata (e.g., "gears", "states","seasons"), EXCEPT not years.  This argument
             does 2 jobs of labeling the output (a) it labels the strata columns in the summary
             output data frame and (b) it labels each bootstrap figure, along the y-axis
             with the combined strata names separated by '.' e.g., 'Bottom Trawl.CA.Winter'}
\usage{
bootSummary.fn(dat, B = NULL, fname = NULL, strtNms = NULL)
}
\arguments{
\item{dat}{the dataframe of observations}

\item{fname}{the file name where the postscript file is to be saved}

\item{strtNms}{the strata names}

\item{the}{number of bootstrap iterations used in the function.  This argument
is used to give the appropriate output.  If set B=0, then the output is simply
the observed ratios by strata in dataframe format.  If B = anything else
(including NULL = default), then the output is a dataframe & a postscript file of
plots of the actual distribution of the bootstrapped values.}
}
\description{
Output from the function
_________________________
   Two pieces are output
   1. a dataframe that summarizes the discard, retained, and ratio statistics
         a) column names with the prefix 'obs.' are observed values
         b) column names with the prefix 'mn.' are means of the bootstrapped values
         c) column names with the prefix 'md.' are medians of the bootstrapped values
         d) column names with the prefix 'sd.' are standard deviations of the bootstrapped values
         e) column names with the prefix 'cv.' are coefficients of variation (mean/sd) of the bootstrapped values
         f) boot.sample = B = the number of bootstrap iterations
   2. a postscript of the bootstrapped values by year and strata, which can be converted to pdf
         red vert. line = observed ratio
         green vert. line = mean ratio
         blue vert. line = median ratio
}
\details{
Example Syntax
______________
 This would just give you the observed discard, retained, and ratio without any summary of the bootstraps.
 Boot.sumry.fxn(outB.pet,B=0,fname=paste(drive2,"Petrale_Boot_History_19Mar2013",sep=''),strtNms=list("Gears","States","Seasons"))

 This would give you a summary dataframe and a ps file of plots (any value other than B=0, including B=NULL=default)
 Boot.sumry.fxn(outB.pet,B=10000,fname=paste(drive2,"Petrale_Boot_History_19Mar2013",sep=''),strtNms=list("Gears","States","Seasons"))

IMPORTANT NOTES
_______________
 1. Be sure to turn the postscript device off after running the function (i.e., 'dev.off()' )
 2. The x-axis label is currently hard-coded to 'standard discard ratio (dis/dis+ret)'. If you
     bootstrap a different discard ratio (e.g., dis/ret), this label is mis-leading.  It will still plot correctly,
     however, the x-axis label will be incorrect.
-------------------------------------------------------------------------------
Date         Author                    Modifications
-------------------------------------------------------------------------------
19 Mar 2013  J.Jannot                Original code
-------------------------------------------------------------------------------
27 Mar 2013  J.Jannot                Fixed the CV equation
-------------------------------------------------------------------------------
01 Apr 2013  A.Hicks                 Changed the CV equation for ratio so that the observed mean is in the denominator
-------------------------------------------------------------------------------
26 Feb 2015  A.Hicks                 Changed the CV equations so that the observed mean is in the denominator
_______________________________________________________________________________
}
\author{
Jason Jannot and Alan Hicks
}
