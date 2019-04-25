



#bootstraps each element of the list
#dat is a list where each element is a bott stratum within the stratum
#the boot strata are combined and reatined and discards are summed
#add in a smaller sample size (proportion and absolute number)
boot.fn <- function(dat,nBoots,bootUnit,ret,dis,propSize=propSize,out2screen=0.2*nBoots) {
    retain <- discard <- rep(0,nBoots)
    for(i in 1:length(dat)) {
        sampUnits <- unique(dat[[i]][,bootUnit])
        if(nBoots > 0) {
            for(b in 1:nBoots) {
                if(b%%out2screen == 0) {
                    cat(" Finished",b,"of",nBoots,"bootstraps","\r")
                    flush.console()
                }
                theSample <- sample(sampUnits,size=length(sampUnits)*propSize,replace=T)
                ind <- NULL
                for(j in 1:length(theSample)) {
                    ind <- c(ind,which(dat[[i]][,bootUnit]==theSample[j]))     #make repeated vessel repeat the observations
                }
                retain[b] <- retain[b] + sum(dat[[i]][ind,ret])
                discard[b] <- discard[b] + sum(dat[[i]][ind,dis])
            }
        } else {
            cat("No bootstrapping done because number of boots set to",nBoots,"\n")
            retain <- discard <- NA
        }
        cat("\r","\n")
    }
        #it returns the sum of retained and discarded in the entire stratum
    return(list(retain=retain,discard=discard))  #doesn't need to be a list
}

#combines retained and discarded across strata and calculates discard ratio and CVs for each year
#uses specific output from bootstrapping function
#keepSep is a list of strata variables to keep separate (i.e., c("TRAWL","FIXED GEAR"))
combineBootResults.fn <- function(bootDat,keepSep=NULL) {
    out <- matrix(NA,nrow=length(bootDat),ncol=12)
    dimnames(out) <- list(names(bootDat),c("ptEstRet","ptEstDis","ptEstRatio","meanRet","medRet","cvRet","meanDis","medDis","cvDis","meanRatio","medRatio","cvRatio"))

    if(is.list(keepSep)) {
        #This pastes all possible combinations of strata together with a . inbetween. Make sure they are in the same order as your strata names
        keepSep <- apply(expand.grid(keepSep,stringsAsFactors=F),1,paste,collapse=".")
    }

    tmp <- which(unlist(lapply(bootDat[[1]],length))>1)[1]   #select the first stratum that actually has bootstraps
    nBoots <- length(bootDat[[1]][[tmp]]$dis)
    ret<-dis<-rat<-rep(0,nBoots)

    peRet <- peDis <- 0

    if(!is.null(keepSep)) {
         outS <- vector("list",length(keepSep))
        for(k in 1:length(keepSep)) {
            outS[[k]] <- out
        }
       names(outS) <- keepSep
    }

    for(yr in 1:length(bootDat)) {
        tmp <- which(unlist(lapply(bootDat[[yr]],length))==1)   #select the strata that do not have bootstraps
        if(length(tmp) > 0) {
            cat("Warning: there are some strata in year",names(bootDat)[yr],"that have no data\n")
            cat(names(bootDat[[yr]])[tmp],"\n\n")
        }
        tmp <- which(unlist(lapply(bootDat[[yr]],length))>1)   #select the first stratum that actually has bootstraps
        if(!is.null(keepSep)) {
            for(k in 1:length(keepSep)) {
                ret<-dis<-rat<-rep(0,nBoots)
                peRet <- peDis <- 0
                #ind <- grep(paste(keepSep[k],"\\.",sep=""),names(bootDat[[yr]]))
                ind <- grep(keepSep[k],names(bootDat[[yr]]))
                if(length(ind)==0) {cat("No strata with name",keepSep[k],"\n\n")}
                for(j in ind[ind%in%tmp]) {
                    ret <- ret + bootDat[[yr]][[j]][["ret"]]
                    dis <- dis + bootDat[[yr]][[j]][["dis"]]
                    peRet <- peRet + bootDat[[yr]][[j]][["PtEst"]]["retained"]
                    peDis <- peDis + bootDat[[yr]][[j]][["PtEst"]]["discard"]
                }
                ratio <- dis/(ret+dis)  #give option for different denominator?
                peRatio <- peDis/(peDis+peRet)
                outS[[k]][yr,] <- c(peRet,peDis,peRatio,mean(ret),median(ret),sd(ret)/mean(ret),mean(dis),median(dis),sd(dis)/mean(dis),mean(ratio),median(ratio),sd(ratio)/peRatio)
            }
        }
        if(is.null(keepSep)) {
            for(i in tmp) {
                ret <- ret + bootDat[[yr]][[i]][["ret"]]
                dis <- dis + bootDat[[yr]][[i]][["dis"]]
                peRet <- peRet + bootDat[[yr]][[i]][["PtEst"]]["retained"]
                peDis <- peDis + bootDat[[yr]][[i]][["PtEst"]]["discard"]
            }
            ratio <- dis/(ret+dis)  #give option for different denominator?
            peRatio <- peDis/(peDis+peRet)
            out[yr,] <- c(peRet,peDis,peRatio,mean(ret),median(ret),sd(ret)/mean(ret),mean(dis),median(dis),sd(dis)/mean(dis),mean(ratio),median(ratio),sd(ratio)/mean(ratio))
        }
    }
    if(!is.null(keepSep)) {
        out <- outS
    }
    return(out)
}


if(F) {
    dd <- expand.grid(data.frame(a=1:5,b=as.character((1:5)+10),c=c("one","two","three","four","five"),d=c("uno","dos","tres","quatro","cinco"),yr=2001:2005))
    #dd <- expand.grid(data.frame(a=1:3,b=as.character((1:3)+10),c=c("one","two","three"),d=c("uno","dos","tres"),yr=2001:2003))
    dd$e <- seq(0,5,length=nrow(dd))
    dd$DRVID <- c("x","y","y","y","y","y","z","x","y","z","z","y","y","z","x","y","z","x","y","z","z","y","y","z","x")
    dd$SPC_RETlbs <- 1:25
    dd$SPC_DISlbs <- (1:25)*0.2

    bootDiscardRatio.fn(dd,years=2001,strat="c",B=10,bootVars=c("c"),yrColNm="yr",vesselColNm="DRVID",retained="SPC_RETlbs",discard="SPC_DISlbs",minVessels=2,verbose=0.1*B)

}

