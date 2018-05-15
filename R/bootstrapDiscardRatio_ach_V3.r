#dat is a dataframe of all data.

#strat is a vector indicating strata of each row in dat. An NA value removes that observation.
#OR strat is a name of the column in dat (or the column number)

#B is the number of bootstraps to do

#mod indicates how often reporting will be done

#ratio type of ratio to calculate. proportion=d/(d+r), exapnsion=d/r

bootDiscardRatio.fn <- function(dat,strat,B,bootVars="d_port_group",years=NULL,yrColNm="ryear",vesselColNm="drvid",
                                retained="ret",discard="dis",ratio=c("proportion","expansion"),minVessels=1,
                                toScreen=0.2,writeLog="",seed=NULL,propSize=1) {
    if(!is.null(seed)) {set.seed(seed)}

    if(toScreen<1) { toScreen <- toScreen*B }
    if(writeLog!="") {cat(as.character(Sys.time()),"\n\n",file=writeLog)}
    if(is.null(years)) {
        years <- sort(unique(dat[,yrColNm]))
        if(writeLog!="") cat("Bootstrapping over all years since years was not specified\n",years,"\n",file=writeLog,append=T)
    }
    pe <- dis <- ret <- rat <- out <- vector(mode="list",length=length(years))

    #subset the dataframe to only include the years specified
    for(yr in 1:length(years)) {
        if(writeLog!="") {cat("\nYear",years[yr],"\n",file=writeLog,append=T)}
        cat("\nYear",years[yr],"\n")
        pe[[yr]] <- dis[[yr]] <- ret[[yr]] <- rat[[yr]] <- out[[yr]] <- list()
        dat.yr <- yrV3.fxn(dat,colnm=yrColNm,yrs=years[yr],subset=T)

        #work within each strata separately, to get statistics by strata
        dat.strat <- split(dat.yr,dat.yr[,strat])

        #check that minVessels is met
        numVessels <- unlist(lapply(dat.strat,function(x){length(unique(x[,vesselColNm]))}))
        if(any(numVessels<minVessels)) {
            if(writeLog!="") cat("WARNING: fewer than",minVessels,"vessels in at least one of the strata for year",years[yr],"\n",file=writeLog,append=T)
            if(writeLog!="") cat("Discard ratios will not be bootstrapped for these strata\n",file=writeLog,append=T)
        }

        for(s in 1:length(dat.strat)) {
            if(writeLog!="") cat("\nStratum",names(dat.strat)[s],"\n",file=writeLog,append=T)
            if(numVessels[s] >= minVessels) {
                #determine point estimates

                pe[[yr]][[s]] <- c(sum(dat.strat[[s]][,discard]),sum(dat.strat[[s]][,retained]))
                pe[[yr]][[s]] <- switch(ratio[1],
                    proportion = c(pe[[yr]][[s]],pe[[yr]][[s]][1]/sum(pe[[yr]][[s]])),
                    expansion  = c(pe[[yr]][[s]],sum(dat.strat[[s]][,discard])/sum(dat.strat[[s]][,retained])),
                    stop("ratio must be proportion or expansion\n"))
                names(pe[[yr]][[s]]) <- c("discard","retained","ratio")

                if(length(bootVars)>1) {
                    dat.strat[[s]][,"combineBootVars"] <- apply(dat.strat[[s]][,bootVars],1,function(x){paste(x[1],x[2],sep=".")})
                    inds <- 1:length(bootVars)
                    bootDat <- split(dat.strat[[s]],dat.strat[[s]][,"combineBootVars"])  #creates a list with each element specific to the boot variables
                }
                if(length(bootVars)==1) {
                    bootDat <- split(dat.strat[[s]],dat.strat[[s]][,bootVars])  #creates a list with each element specific to the boot variables
                    inds <- 1
                }
                if(length(bootVars)<1) {
                    inds <- 0
                    bootDat <- list(dat.strat[[s]])  #a one element list
                }
                #
                #bootDat <- split(dat.strat[[s]],dat.strat[[s]][,bootVars[inds]])  #creates a list with each element specific to the boot variables, without the last variable
#print(unlist(lapply(bootDat,length)))
#return(bootDat)
                #bootstrap the strata specific, bootVars specific vessels. Combine if necessary from rightmost bootVars
                #Warn if combined (note that it will combine over D_PORT_GROUP if necessary (default))
                numVboot <- unlist(lapply(bootDat,function(x){length(unique(x[,vesselColNm]))}))
                while(any(numVboot<minVessels) & length(inds)>=1) {   #####CHECK if 1 is right
                    #cat("Removing boot level",bootVars[inds[length(inds)]],"\n")
                    inds <- inds[-length(inds)]
                    if(length(inds)>0) {  #only if there is anything left to split on
                        bootDat <- split(dat.strat[[s]],as.character(dat.strat[[s]][,bootVars[inds]]))  #creates a list with each element specific to the boot variables, without the last variable
                        numVboot <- unlist(lapply(bootDat,function(x){length(unique(x[,vesselColNm]))}))
                    }
                }
                if(length(inds)==0) {
                    if(writeLog!="") cat("Cannot meet minimum vessel requirement within bootVars. Bootstrapping over all data in the strata for year",years[yr],".\n",file=writeLog,append=T)
                    bootDat <- list(dat.strat[[s]])  #a one element list
                }
                if(writeLog!="") cat("Final Bootstrapping strata variables:",bootVars[inds],"\n",file=writeLog,append=T)
                #DO THE BOOTSTRAP using bootDat
                #bootstrap within a strata and over all bootVars
                #by bootstrapping a specific combination to the boot strata (i.e, portGr/period)
                #combine those boot strata to the strata
                tmp <- boot.fn(dat=bootDat,nBoots=B,bootUnit=vesselColNm,ret=retained,dis=discard,propSize=propSize,out2screen=toScreen)
                cat("\r")  #just to clear the screen and put cursor back to home position

                dis[[yr]][[s]] <- tmp$discard
                ret[[yr]][[s]] <- tmp$retain
                rat[[yr]][[s]] <- switch(ratio[1],
                    proportion = tmp$discard/(tmp$discard+tmp$retain),
                    expansion  = tmp$discard/tmp$retain)
                out[[yr]][[s]] <- list(PtEst=pe[[yr]][[s]],dis=tmp$discard,ret=tmp$retain,ratio=rat[[yr]][[s]])
            } else {   #the strata didn't have enough vessels
                pe[[yr]][[s]] <- NA
                out[[yr]][[s]] <- NA
                dis[[yr]][[s]] <- NA
                ret[[yr]][[s]] <- NA
                rat[[yr]][[s]] <- NA
            }

        }
        names(pe[[yr]]) <- names(dis[[yr]]) <- names(ret[[yr]]) <- names(rat[[yr]]) <- names(out[[yr]]) <- names(dat.strat)
    }
    names(pe) <- names(ret) <- names(dis) <- names(rat) <- names(out) <- as.character(years)
    #return(list(retain=ret,discard=dis,ratio=rat))
    return(out)
}



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
















#1) sample vessels by port_group (D_PORT_GROUP) within a year and period
#--- must do it by period to match sampling design
#---  don't want to pick one vessel over two periods
#--- i.e., A,B,C in period 1, D,E in period 2. Don't want to pick D,D,D,D,D for period 1 and 2, thus keep it separate
#2) may need to combine periods if only 1 vessel in a period
#--- may want a higher criteria to make sure there is variability in bootstrap. i.e., combine periods if less than 3 in a period, or less than 5 overall? need to have 3 per stratum?
#2) check the number of vessels (DO WE NEED TO HAVE AT LEAST THREE VESSEL PER STRATUM?)

#define your own subgroup
