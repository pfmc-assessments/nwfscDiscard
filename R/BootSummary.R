#'  A function to summarize the output from bootstrappping using the
#'  'bootstrapDiscardRatio_ach.r' functionby Allan Hicks.
#' -------------------------------------------------------------------------------
#'  Arguments to the function
#' ____________________________
#'      dat = the data, in list format, from the bootstrapDiscardRatio_ach.r function
#'      B = the number of bootstrap iterations used in the function.  This argument
#'          is used to give the appropriate output.  If set B=0, then the output is simply
#'          the observed ratios by strata in dataframe format.  If B = anything else
#'          (including NULL = default), then the output is a dataframe & a postscript file of
#'          plots of the actual distribution of the bootstrapped values.
#'     fname = the file name where the postscript file is to be saved
#'     strtNms = names you want to assign to strata (e.g., "gears", "states","seasons"), EXCEPT not years.  This argument
#'              does 2 jobs of labeling the output (a) it labels the strata columns in the summary
#'              output data frame and (b) it labels each bootstrap figure, along the y-axis
#'              with the combined strata names separated by '.' e.g., 'Bottom Trawl.CA.Winter'
#' 
#' Output from the function
#' _________________________
#'    Two pieces are output
#'    1. a dataframe that summarizes the discard, retained, and ratio statistics
#'          a) column names with the prefix 'obs.' are observed values
#'          b) column names with the prefix 'mn.' are means of the bootstrapped values
#'          c) column names with the prefix 'md.' are medians of the bootstrapped values
#'          d) column names with the prefix 'sd.' are standard deviations of the bootstrapped values
#'          e) column names with the prefix 'cv.' are coefficients of variation (mean/sd) of the bootstrapped values
#'          f) boot.sample = B = the number of bootstrap iterations
#'    2. a postscript of the bootstrapped values by year and strata, which can be converted to pdf
#'          red vert. line = observed ratio
#'          green vert. line = mean ratio
#'          blue vert. line = median ratio
#' 
#' Example Syntax
#' ______________
#'  This would just give you the observed discard, retained, and ratio without any summary of the bootstraps.
#'  Boot.sumry.fxn(outB.pet,B=0,fname=paste(drive2,"Petrale_Boot_History_19Mar2013",sep=''),strtNms=list("Gears","States","Seasons"))
#' 
#'  This would give you a summary dataframe and a ps file of plots (any value other than B=0, including B=NULL=default)
#'  Boot.sumry.fxn(outB.pet,B=10000,fname=paste(drive2,"Petrale_Boot_History_19Mar2013",sep=''),strtNms=list("Gears","States","Seasons"))
#' 
#' IMPORTANT NOTES
#' _______________
#'  1. Be sure to turn the postscript device off after running the function (i.e., 'dev.off()' )
#'  2. The x-axis label is currently hard-coded to 'standard discard ratio (dis/dis+ret)'. If you
#'      bootstrap a different discard ratio (e.g., dis/ret), this label is mis-leading.  It will still plot correctly,
#'      however, the x-axis label will be incorrect.
#' -------------------------------------------------------------------------------
#' Date         Author                    Modifications
#' -------------------------------------------------------------------------------
#' 19 Mar 2013  J.Jannot                Original code
#' -------------------------------------------------------------------------------
#' 27 Mar 2013  J.Jannot                Fixed the CV equation
#' -------------------------------------------------------------------------------
#' 01 Apr 2013  A.Hicks                 Changed the CV equation for ratio so that the observed mean is in the denominator
#' -------------------------------------------------------------------------------
#' 26 Feb 2015  A.Hicks                 Changed the CV equations so that the observed mean is in the denominator
#' _______________________________________________________________________________
#'
#' @template dat
#' @param the number of bootstrap iterations used in the function.  This argument
#'          is used to give the appropriate output.  If set B=0, then the output is simply
#'          the observed ratios by strata in dataframe format.  If B = anything else
#'          (including NULL = default), then the output is a dataframe & a postscript file of
#'          plots of the actual distribution of the bootstrapped values.
#' @param fname the file name where the postscript file is to be saved
#' @param strtNms the strata names
#'
#' @export
#'
#' @author Jason Jannot and Alan Hicks
#'
  bootSummary.fn<-function(dat,B=NULL,fname=NULL,strtNms=NULL){
    if(B!=0){
    postscript(paste(fname,'_',Sys.Date(),'.ps',sep=''))
    par(mfcol=c(3,3))
           }
    tmp<-NULL
    tmp2<-NULL
    boot.ratio<-NULL
    out.df<-NULL
      for(y in 1:length(dat)){
        for(a in 1:length(names(dat[[y]]))){
          if(!is.na(dat[[y]][a])){
             cat("y:",y,"a:",a,"\n")
             tmp<-data.frame(Years=names(dat)[y],
                  Strata=names(dat[[y]])[a],
                  matrix(unlist(strsplit(as.character(names(dat[[y]])[a]),".",fixed=T)),nrow=1,byrow=T,dimnames=list(NULL,strtNms)),
                  matrix(unlist(dat[[y]][[a]][1]),nrow=1,byrow=T,dimnames=list(NULL,c("obs.discard.lbs","obs.retained.lbs","obs.stnd.dis.ratio"))),
                  matrix(mean(unlist(dat[[y]][[a]][2])),nrow=1,byrow=T,dimnames=list(NULL,c("mn.discard.lbs"))),
                  matrix(median(unlist(dat[[y]][[a]][2])),nrow=1,byrow=T,dimnames=list(NULL,c("md.discard.lbs"))),
                  matrix(sd(unlist(dat[[y]][[a]][2])),nrow=1,byrow=T,dimnames=list(NULL,c("sd.discard.lbs"))),
                  matrix(sd(unlist(dat[[y]][[a]][2]))/mean(unlist(dat[[y]][[a]][2])),nrow=1,byrow=T,dimnames=list(NULL,c("cv.discard.lbs"))),
                  matrix(mean(unlist(dat[[y]][[a]][3])),nrow=1,byrow=T,dimnames=list(NULL,c("mn.retained.lbs"))),
                  matrix(median(unlist(dat[[y]][[a]][3])),nrow=1,byrow=T,dimnames=list(NULL,c("md.retained.lbs"))),
                  matrix(sd(unlist(dat[[y]][[a]][3])),nrow=1,byrow=T,dimnames=list(NULL,c("sd.retained.lbs"))),
                  matrix(sd(unlist(dat[[y]][[a]][3]))/mean(unlist(dat[[y]][[a]][3])),nrow=1,byrow=T,dimnames=list(NULL,c("cv.retained.lbs"))),
                  matrix(mean(unlist(dat[[y]][[a]][4])),nrow=1,byrow=T,dimnames=list(NULL,c("mn.stnd.dis.ratio"))),
                  matrix(median(unlist(dat[[y]][[a]][4])),nrow=1,byrow=T,dimnames=list(NULL,c("md.stnd.dis.ratio"))),
                  matrix(sd(unlist(dat[[y]][[a]][4])),nrow=1,byrow=T,dimnames=list(NULL,c("sd.stnd.dis.ratio"))),
                  matrix((sd(unlist(dat[[y]][[a]][4]))/unlist(dat[[y]][[a]][1])[3]),nrow=1,byrow=T,dimnames=list(NULL,c("cv.stnd.dis.ratio"))),
                  boot.sample=length(unlist(dat[[y]][[a]][4])),
                  bias.est=(matrix(mean(unlist(dat[[y]][[a]][4])),nrow=1,byrow=T))-(matrix(unlist(dat[[y]][[a]][[1]][3]),nrow=1,byrow=T))
                  )
          #builds the summary data frame
          if((y==1)&(a==1)){
            out.df<-tmp
            }else{
            out.df<-rbind(out.df,tmp)
            }
          if(B==0){
            next
          #}else if(all(unlist(dat[[y]][[a]][2])==0)){
          #next
          }else{
          #plots the bootstrapped values as a histograms in a postscript file
           tmp2=data.frame(Years=names(dat)[y],Strata=names(dat[[y]])[a], matrix(unlist(strsplit(as.character(names(dat[[y]])[a]),".",fixed=T)),nrow=1,byrow=T,dimnames=list(NULL,strtNms)),
                       obs.ratio=matrix(unlist(dat[[y]][[a]][[1]][3]),nrow=1,byrow=T),
                       mn.bt.ratio=matrix(mean(unlist(dat[[y]][[a]][4])),nrow=1,byrow=T),
                       md.bt.ratio=matrix(median(unlist(dat[[y]][[a]][4])),nrow=1,byrow=T))
            boot.ratio=matrix(unlist(dat[[y]][[a]][4]),ncol=1,byrow=F)
            #hist(boot.ratio, main=tmp2$Years,xlab = 'standard discard ratio (dis/dis+ret)',ylab=tmp2$Strata)
            #abline(v=c(tmp2$obs.ratio,tmp2$mn.bt.ratio,tmp2$md.bt.ratio),col=c(2:4))
            #red= observed, green=mean, blue=median

           }
          }
        }
      }

      #modifies the output depending on if the user wants only the observed values or the summary of the bootstrap
        if(B==0){
        out.df=out.df[,c(1:(5+length(strtNms)))]
        }else{
        dev.off()
        }
        return(out.df)
  }
