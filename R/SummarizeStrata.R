#' Summarize discards within a strata
#' 
#' @template dat
#' @template colnms
#' @param yrColNm
#' @param summaryNames identifiers for the vessel, trip number, and haul id
#' @param totalNames Discard or retention
#' @param vesselColName Vessel id within the WCGOP database
#' @param vesselCrit the minimum number of vessels within a strata for confidentiality. Stratas with less this number will be flagged as having confidential information.
#'
#'
#'
#' @author Allan Hicks and Chantel WEtzel
#' @export
#'
summarizeStrata.fn <- function(dat,colnms=NULL,yrColNm=NULL,summaryNames=c("drvid","trip_id","haul_id"),totalNames=c("dis","ret"),vesselColName="drvid",vesselCrit=3) {

    numVessels <- NULL

    if(is.null(yrColNm) & is.null(colnms)) {
        numObs <- nrow(dat)
        dat.split <- list(allData=dat)
    }else {
        numObs <- as.data.frame(table(dat[,c(yrColNm,colnms)]))
        if(is.null(yrColNm) & length(colnms)==1) {names(numObs)[1] <- colnms}
        if(length(yrColNm)==1 & is.null(colnms)) {names(numObs)[1] <- yrColNm}
        dat.split <- split(dat,dat[,c(yrColNm,colnms)])
    }
    names(numObs)[ncol(numObs)] <- "numObs"
     summList.fn <- function(x) {
        if(length(summaryNames)>0) {
            uniqueCols <- apply(x[,summaryNames],2,function(x){length(unique(x))})
            nombres <- paste("unique",summaryNames,sep=".")
        } else {
            uniqueCols <- summaryNames <- NULL
        }

        if(length(totalNames)>0) {
            totCols <- apply(x[,totalNames],2,sum,na.rm=T)
            nombres <- c(nombres,paste("sum",totalNames,sep="."))
        } else {
            totCols <- totalNames <- NULL
        }
        numVessels <- length(unique(x[,vesselColName]))

        nombres <- c(nombres,"numVessels")
        xx <- c(uniqueCols,totCols,numVessels)
        names(xx) <- nombres
        return(xx)
    }


    out <- lapply(dat.split,summList.fn)
    out <- t(as.data.frame(out))
    out <- cbind(numObs,out)
    out$OKnumVessels<-F
    out$OKnumVessels[which((out$numVessels==0)|(out$numVessels>=vesselCrit))]<-T

    if(sum(!out[,"OKnumVessels"])>0) {cat("Some strata do not meet the vessel criteria for confidentiality\n")}


    return(out)
}