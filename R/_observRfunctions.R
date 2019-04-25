

























##I should give the summarizeStrata.fxn a default if no yr or colnms are given
##Name the output strata for continuous variables something different (cannot do numeric right now because it is complicated and I don't want to risk an error

##column in nyumVessles that tells if did not meet confidential criteria
##summaryUnique by strata with numVessels
##summarize into one data frame with numVessels, summaryNames, and numberOfObs
##Which strata are breached
##maybe a summary after calling function of how many strata were breached



combineBoots <- function(dat,catch,strataNames,combineVar,newStrata,maxExp=100) {
  #dat is the output from the bootstrapping for a specific year
  #catch is a dataframe of catches for specific year with columns matching the strata names (e.g., state, gear, etc)
  #    and one named catch
  #strataNames is a vector of strata names match catch columns in the order separated by periods in names(dat), without catch shares
  ##  e.g., BottomTrawl.CA.Winter would be c("gear","state","season")
  #combineVar is the name of the strata that is to have some strata combined NOT NEEDED
  #newStrata is a list with each element indicating how strata are combined (e.g., to combine WA/OR and keep CA separate list(c("WA","OR"),"CA") )


  if(!("PtEst" %in% unlist(lapply(dat,names)))) stop("outB must be a single year with results. The first list element should be names 'PrEst'\n")
  if(length(combineVar) > 1) stop("Only able to work with one combineVar at a time\n")
  if(!"catch" %in% names(catch)) {stop("There must be a column named 'catch' in the catch dataframe\n")}

  cat("\n")

  check <- unlist(lapply(dat,function(x)is.na(x)[1]))
  if(any(check)) { #check if there is an observed strata that has no observations. i'm not sure why these sneak in
    nombres <- names(dat)[!check]
    tmp <- vector(length=length(nombres),mode="list")
    names(tmp) <- nombres
    for(i in 1:length(nombres)) {
      tmp[[i]] <- dat[[nombres[i] ]]
    }
    dat <- tmp
  }


  #figure out which stratum we are dealing with
  strataCats <- t(as.data.frame(strsplit(names(dat),"[\\.]")))
  for(i in 1:ncol(strataCats)) {
    if(all(strataCats[,i] %in% unlist(newStrata))) {
      ind <- i
    }
  }

  #Check that strataCats and catch categories match
  if(nrow(catch) < length(names(dat))) {
    stop("catch dataframe has less rows that number or strata in names(dat)\n")
  }

 if((ncol(strataCats)-1) != length(strataNames)) { #-1 because catchSahres is last column
    cat("strata names from names(dat):",strataCats[1,-ncol(strataCats)],
       "\nis not the same length as strataNames:",strataNames,"\n")
    stop()
  }

  #work with catches and compare strata. Find out which strata were not observed, but have catch
  catch <- catch[catch$catch>0 & !is.na(catch$catch),]
  catchObs <- rep(TRUE,nrow(catch)) #start assuming all catch strata observed and flag those strata that are not
  #check that they both have the same strata
  for(i in 1:length(strataNames)) {
    if(!all(catch[,strataNames][,i] %in% strataCats[,i])) {
      #there are catches that are not observed
      catchObs <- catchObs & (catch[,strataNames][,i] %in% strataCats[,i])
      cat("catch column associated with strataNames",i,"is not the same as strata",i,"in names(dat)\n")
      cat("In other words, there are strata with catch that was not observed. These will be incorporated using the combined discard rate from observed strata.\n")
      #stop()
    }
    if(!all(strataCats[,i] %in% catch[,strataNames][,i])) {
      cat(i,"strata in names(dat) is not the same as catch column associated with strataNames",i,"\n")
      cat("In other words, there is no catch associated with at least one strata that was observed.\n")
      stop()
    }
  }
  strataNotObs <- apply(catch[!catchObs,strataNames],1,paste,collapse=".")
  catchStrata <- paste(apply(catch[catchObs,strataNames],1,paste,collapse="."),strataCats[1,ncol(strataCats)],sep=".")
  if(!all(names(dat) %in% catchStrata)) {
    stop(cat("Some strata in names(dat) not contained in catch strata:\n",catchStrata,"\n"))
  }

  catchObsMat <- catch[catchObs,]

  out <- NULL
  allNewStrataNames <- matrix(NA,ncol=length(strataNames)+1,nrow=0,dimnames=list(NULL,c(strataNames,"CatchShares")))
  #strataCats and catch[,strataNames] is in order of how it is labeled, so I simply just have to paste it together
  #since I am only working with one stratum variable, I can loop over the newStrata
  for(i in 1:length(newStrata)) {   #loop over multiple strata types (i.e., states, sectors, etc.). newStrata is a list
cat("i:",i,"ind:",ind,"\n")
    combStrata <- strataCats[strataCats[,ind] %in% newStrata[[i]],]
    if(is.null(nrow(combStrata))) {
      combStrata <- matrix(combStrata,nrow=1)
      sepStrata <- combStrata
    }else {
      sepStrata <- combStrata[!duplicated(combStrata[,-ind]),]
    }
    if(is.null(nrow(sepStrata))) {
      sepStrata <- matrix(sepStrata,nrow=1)
    }

    #now loop over this separate strata and replace'ind' column with the strata definition
    for(j in 1:nrow(sepStrata)) {
      tmpStrata <- sepStrata[j,]
      tmpStrata[ind] <- paste(newStrata[[i]],collapse=":")    #replace strata name to collapse over with collapsed strata
      allNewStrataNames <- rbind(allNewStrataNames,tmpStrata)
      tmp <- matrix(sepStrata[j,],nrow=length(newStrata[[i]]),ncol=ncol(sepStrata),byrow=T)
      tmp[,ind] <- newStrata[[i]]
      nam <- apply(tmp,1,paste,collapse=".")  #strata names to extract from bootstrapping and collapse over
      if(nrow(tmp==1)) {
        nam2 <- paste(tmp[,-ncol(tmp)],collapse=".")  #this just doesn't have the catch shares tacked on
      }else {
        nam2 <- apply(tmp[,-ncol(tmp)],1,paste,collapse=".")  #this just doesn't have the catch shares tacked on
      }
      #now loop over these to combine them (dis and ret)
      #rownumber of tmp matches with position of nam

      if(is.null(dat[[nam[1]]]) || is.na(dat[[nam[1]]])[1]) {
        cat("No observations for strata ",nam[1],".\n",sep="")
        obsRate <- bootRate <- tmpCat <- tmpRet <- tmpDis <- obsDis <- 0
      } else {
        obsRate <- min(dat[[nam[1]]]$PtEst["discard"]/dat[[nam[1]]]$PtEst["retained"],maxExp)
        bootRate <- dat[[nam[1]]]$dis/dat[[nam[1]]]$ret
        bootRate[bootRate>maxExp] <- maxExp #I put in an arbitrary max expansion. This could easily be explored and determined when is a discard rate not a useful way to model discards?
        tmpCat <- catchObsMat[catchStrata==nam[1],"catch"]
        tmpRet <- tmpCat
        tmpDis <- bootRate * tmpCat
        obsDis <- obsRate * tmpCat
      }
      if(length(nam)>1) {
        for(k in 2:length(nam)) {

          if(is.null(dat[[nam[k]]]) || is.na(dat[[nam[k]]])[1]) {
            tmpRet <- tmpRet + 0
            tmpDis <- tmpDis + 0
            obsDis <- obsDis + 0
          } else {
            obsRate <- min(dat[[nam[k]]]$PtEst["discard"]/dat[[nam[k]]]$PtEst["retained"],maxExp)
            bootRate <- dat[[nam[k]]]$dis/dat[[nam[k]]]$ret
            bootRate[bootRate>maxExp] <- maxExp #arbitrary max expansion
            tmpCat <- catchObsMat[catchStrata==nam[k],"catch"]
            tmpRet <- tmpRet + tmpCat
            tmpDis <- tmpDis + bootRate * tmpCat
            obsDis <- obsDis + obsRate * tmpCat
          }
        }
      }

      #now add in estimates for strata that were not observed using the rate from combined strata that were observed
      #there are many other ways to do this, but this is easiest for now (i.e., use previous year instead)
      noObsRet <- catch[!catchObs,][strataNotObs %in% nam2,"catch"]
      if(length(noObsRet>0)) {
        cat("Adding in unobserved strata with catches:\n")
        print(catch[!catchObs,][strataNotObs %in% nam2,])
        bootRatio <- tmpDis/tmpRet
        obsRatio <- obsDis/tmpRet
        for(k in 1:length(noObsRet)) {
            tmpCat <- noObsRet[k]
            tmpRet <- tmpRet + tmpCat
            tmpDis <- tmpDis + bootRatio * tmpCat
            obsDis <- obsDis + obsRatio * tmpCat
        }
      }

      #calculate the discard rate for this combined strata (dis/(dis+ret))
      newStrataName <- paste(tmpStrata,collapse=".")
      ptEst <- c(obsDis,tmpRet,obsDis/(obsDis+tmpRet))  #tmpRet is simply the sum of landings
      names(ptEst) <- c("discard","retained","ratio")
      out[[newStrataName]] <- list(PtEst = ptEst,
                     dis = tmpDis,
                     ret = tmpRet,
                     ratio = tmpDis/(tmpDis+tmpRet))


    }

  }
  out
}


combineBootsCS <- function(dat,catch,strataNames,combineVar,newStrata,maxExp=100) {
  #combines the strata you specify, plus a CatchShares stratum (no need to specify this)
  #outB is the output from the bootstrapping for a specific year
  #catch is a dataframe of catches for specific year with columns matching the strata names (e.g., state, gear, etc)
  #    and one named catch
  #strataNames is a vector of strata names match catch columns in the order separated by periods in names(dat), without catch shares
  ##  e.g., BottomTrawl.CA.Winter would be c("gear","state","season")
  #combineVar is the name of the strata that is to have some strata combined
  #newStrata is a list with each element indicating how strata are combined (e.g., to combine WA/OR and keep CA separate list(c("WA","OR"),"CA") )

}