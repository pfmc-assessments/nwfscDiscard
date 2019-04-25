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