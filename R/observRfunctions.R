


createStrataNames <- function(z) {
  strNms <- rep(NA,length(z))
  for(i in 1:length(z)) {
      strNms[i] <- paste(z[[i]],collapse="|")
  }
  return(strNms)
}

#dat = name of data set
#colnm = name of the column where year is located, put in quotes, e.g., 'YEAR'
#yrs = the years as a vector (must have c(yr,yr,etc)). Can be any integer values (i.e., c(2001, 2003:2007,2009,1999))
#nmcol = name of additional column to add that indicates the years specified
#subset = whether or not to return a dataframe with only rows that meet criteria
yrV3.fxn<-function(dat,colnm,yrs,nmcol=paste(colnm,"new",sep=""),subset=F) {
  if(!(colnm %in% colnames(dat))) {
    stop("colnm ",colnm," is not a name in the dataframe\n")
  }
  z <- as.integer(yrs)
  dat[,nmcol] <- NA
  if(subset) {
     out <- dat[dat[,colnm] %in% z,]
     out[,nmcol] <- out[,colnm]
  }
  if(!subset) {
     out <- dat
     out[dat[,colnm] %in% z,nmcol] <- out[dat[,colnm] %in% z,colnm]  #put in years to new column and keep not selected as NA
  }
  return(invisible(out))
}



#clssifies unique values (characters, factors, etc)
#different from year in that year converts the year to integer, whereas this function does not
#dat = name of data set
#colnm = name of the column where the variable to be classified is located, put in quotes, e.g., 'GEAR'
#z = the values to classify that variable, as a vector (must have c(,,etc)). Should be unique values
#nmcol = name of additional column to add that indicates the values specified.  An NA will be put for values that are not in z
#subset = whether or not to return a dataframe with only rows that meet criteria
classify.fn<-function(dat,colnm,z,nmcol=paste(colnm,"new",sep=""),subset=F) {
  if(!(colnm %in% colnames(dat))) {
    stop("colnm ",colnm," is not a name in the dataframe\n")
  }
  if(subset) {
     out <- dat[dat[,colnm] %in% z,]
     out[,nmcol] <- out[,colnm]
  }
  if(!subset) {
    dat[,nmcol] <- NULL
     out <- dat
     out[dat[,colnm] %in% z,nmcol] <- out[dat[,colnm] %in% z,colnm]  #put in strata to new column and keep not selected as NA
  }
  return(invisible(out))
}



#clssifies values that may contain multiple levels per factor  (characters, factors, etc)
#e.g., WA & OR into WaOR
#different from year in that year converts the year to integer, whereas this function does not
#dat = name of data set
#colnm = name of the column where the variable to be classified is located, put in quotes, e.g., 'GEAR'
#z = the values to classify that variable, as vectors within list elements. Should be unique values
#nmcol = name of additional column to add that indicates the values specified.  An NA will be put for values that are not in z
#subset = whether or not to return a dataframe with only rows that meet criteria
classifyMult.fn<-function(dat,colnm,z,nmcol=paste(colnm,"new",sep=""),strNms=NULL,subset=F) {
  if(!(colnm %in% colnames(dat))) {
    stop("colnm ",colnm," is not a name in the dataframe\n")
  }
  dat[,nmcol] <- NA
  if(is.null(strNms)) {
    strNms <- createStrataNames(z)
    # strNms <- rep(NA,length(z))
    # for(i in 1:length(z)) {
    #     strNms[i] <- paste(z[[i]],collapse="|")
    # }
  }
  if(length(z) != length(strNms)) {stop("Stratum names is not equal to number of levels",z,"\n")}
  for(i in 1:length(z)) {
    dat[dat[,colnm] %in% z[[i]],nmcol] <- strNms[i]
  }
  if(subset) {
    dat <- dat[!is.na(dat[,nmcol]),]
  }
  return(invisible(dat))
}


###############################################################################################################################
#this function will classify and subset the data frame based on a single variable and a set of values or cut points.
#it produces an additional column with the strata levels.
#If the variable is numeric, it creates a strata name that indicates the range of values in that strata
#Note that numeric cuts are always closed on the left and open on the right
#For non-numeric variables, the specific quantities are subset.

#dat: the data frame
#vars: the variables to subset by.
#        if numeric & the variable is numeric (dat[,colnm]):
#                    this must contain the lowest boundary and the upper boundary, and all divisions in between.
#                    Anything less than the lowest boundary or greater than or equal to the largest boundary are given NA.
#                    Note that anything EQUAL TO the largest boundary is given an NA
#
#        if the variable to be stratified (dat[,colnm]) is a character or factor and the vars is numeric, character, or factor:
#                   it simply subsets based on the specific values using the classifyV3.fxn
#
#        if the variable to be stratified (dat[,colnm]) is numeric and the vars is not numeric:
#                   A warnin gis issued that this is probably not the desired behavior (but could be, for example year, although I use yr.fxn)
#colnm: name of the column that is stratified
#nmcol: name of the new output column showing distinct strata
#subset: logical indicating if the returned dataframe should contain only the variables in the subset or all variables including those outside of the specified range

createStrata.fn<-function(dat,colnm,vars,strataNames=NULL,nmcol=paste(colnm,"new",sep=""),subset=F) {
  flag <- F
  dat[,nmcol] <- NA
  if(is.numeric(dat[,colnm]) & is.numeric(vars)) {   #both are numeric, so use FindInterval (left-hand side is closed, thus it categorizes by [low,high)
      tmp <- findInterval(dat[,colnm],sort(vars))
      tmp[tmp==0] <- NA   #zero indicates it is less than lowest index value
      tmp[tmp==(length(vars))] <- NA   #the maximum number indicates that it is greater than the maximum strata value entered
      #levNames <- paste("[",vars[-length(vars)],"-",vars[-1],")",sep="")
      if(is.null(strataNames)) {
        strataNames <- createStrataNames(vars)
      }
      dat[,nmcol] <- strataNames[tmp]
      flag <- T
      #tmp <- unlist(lapply(strsplit(c("[0-55)","[366-2000)"),"-"),function(x){which(substring(x[1],2)==depthLevels)}))

      if(!is.null(strataNames)) {
        cat("\nPlease check that strata names for continuous variables correctly match up\n\n")
      }
  }

  if(!is.list(vars)) {
      if((is.character(dat[,colnm])|is.factor(dat[,colnm]))) {   #change vars to character
        if(is.numeric(vars)) {vars <- as.character(vars)}
        dat <- classify.fn(dat,colnm,vars,nmcol,subset)
        if(!is.null(strataNames)) {
            dat[,nmcol] <- strataNames[match(dat[,nmcol],vars)]
        }
      }
      subset <- F
      flag <- T

      if(is.numeric(dat[,colnm]) & !is.numeric(vars)) {
        cat("The data column",colnm,"is numeric, but the vars",vars,"is not numeric.\n")
        cat("This may not be the desired behavior, but it is subsetting the numeric variable by those specific values\n")
        cat("Please check your results\n")
        dat <- classify.fn(dat,colnm,vars,nmcol,subset)
        subset <- F
        flag <- T
        if(!is.null(strataNames)) {
            dat[,nmcol] <- strataNames[match(dat[,nmcol],vars)]
        }
      }
  }

  if(is.list(vars)) {
      if((is.character(dat[,colnm])|is.factor(dat[,colnm]))) {   #change vars to character
        if(is.numeric(vars)) {vars <- as.character(vars)}
        dat <- classifyMult.fn(dat,colnm,vars,nmcol,strataNames,subset)
      }
      subset <- F
      flag <- T

      if(is.numeric(dat[,colnm]) & !any(unlist(lapply(vars,is.numeric)))) {
        cat("The data column",colnm,"is numeric, but the vars is not numeric.\n")
        cat("This may not be the desired behavior, but it is subsetting the numeric variable by those specific values\n")
        cat("Please check your results\n")
        dat <- classifyMult.fn(dat,colnm,vars,nmcol,strataNames,subset)
        subset <- F
        flag <- T
      }
  }

  if(!flag) {stop("Could not determine types of colnm (numeric, character, or factor)\n")}

  if(subset) {
     out <- dat[!is.na(dat[,nmcol]),]
  }
  if(!subset) {
     out <- dat
  }
  return(invisible(out))
}



###############################
# Allan Hicks: function to return a dataframe with only specified values, categorized
# takes in the full data.frame
# returns a list with:
#      dat: the subsetted dataframe with columns and levels for each variable. All observations not meeting criteria are removed if subset=T
#      numVessels: The number of unique vessels in each strata.
#      stratObs: the number of observations in each stratum
#      summaryUnique: unique observations over the subsetted dataframe for the specified columns
# Arguments:
#   dat: the dataframe of observations
#   colnms: the names of the columns to subset over (i.e., c("GEAR_TYPE","COMMON_NAME") ).
#           Do not include year columns here, that is kept separate.
#           If numeric, it refers to the column number
#   colLevs: A list of the levels to cut at, or select from each column. The list elements must be in the same order as colnms
#            If numeric, then it will cut. If character, it will select that specific value.
#            Year is always selected for a particular year
#   stratNms: Optional names for levels in each strata. This must be a list with an element for each column.
#             Within each element is a vector of names for each stratum in the same order as colLevs
#   yrColNm: the name of the column for year (if there is one). If numeric, it is the column number
#   yrs: the years. NOT entered as in version 2 functions. MUST use the concatenate function. For example c(2003:2005,2007,2008:2010,2012)
#          the reason I did not use the '...' is because it gets confused when not including multiple arguments.
#          For example, if using ... and not entering colnms because you want to use default, it will think that one of the years is colnms
#   summaryNames: names of column to provide a summary over
#   vesselColName: the column that defines vessels
#   subset: whether or not to return a subset of the data
#   verbose: whether to output messages to screen. These are also returned in the data.frame

strata.fn <- function(dat,colnms=NULL,colnms.new=paste(colnms,"new",sep=""),colLevs=NULL,stratNms=NULL,
                            yrColNm=NULL,yrColNm.new=paste(yrColNm,"new",sep=""),yrs=NULL,
                            subset=T,verbose=T) {
    if(is.null(colnms) & is.null(yrColNm)) {
        cat("No columns or years defined for selection\n")
        return(invisible(NULL))
    }
    if(!is.null(stratNms) && length(stratNms)!=length(colLevs)) {stop("When stratNms is supplied for non-numeric levels, is must have same number of list elements as colLevs\n")}
    messages <- NULL
    columnNames <- NULL

    out <- dat  #just in case it does not do year
    #first do year, if necessary
    if(!is.null(yrColNm) & !is.null(yrs)) {
        columnNames <- yrColNm.new
        if(is.numeric(yrColNm)) {yrColNm <- names(dat)[yrColNm]}
        cat("Subsetting by",yrColNm,"\n")
        out <- yrV3.fxn(dat=dat,colnm=yrColNm,yrs=yrs,nmcol=yrColNm.new,subset=F)
        removed <- sum(is.na(out[,yrColNm.new]))
        messages <- c(messages,paste(removed,"observations removed because year did not match"),paste(sum(!is.na(out[,yrColNm.new])),"observations kept because of a matching year"))
        if(verbose) {
            cat(messages[length(messages)-1],"\n")
            cat(messages[length(messages)],"\n")
            if(removed>0) print(table(out[is.na(out[,yrColNm.new]),yrColNm]))
            flush.console()
        }
        if(subset) {
            out <- out[!is.na(out[,yrColNm.new]),]
        }
    }

    #now subset other columns
    if(!is.null(colnms)) {
        cat("subsetting by",colnms,"\n")
        if(length(colnms)>1 & (!is.list(colLevs) | length(colnms)!=length(colLevs))) {
            stop("colLevs must be a list and have an equal number of elements to the length of colnms\n")
        }
        if(length(colnms)==1 & !is.list(colLevs)) {
            #make colLevs a list of 1 element so that it works in generalization below
            colLevs <- list(colLevs)
        }

        for(i in 1:length(colnms)) {
            if(!is.null(stratNms[[i]]) && !(is.numeric(dat[,colnms[i]]) & is.numeric(colLevs[[i]])) && length(stratNms[[i]])!=length(colLevs[[i]])) {stop("Strata names in level ",i," does not match the number of column levels.\n")}
            if(!is.null(stratNms[[i]]) && (is.numeric(dat[,colnms[i]]) & is.numeric(colLevs[[i]])) && length(stratNms[[i]])!=(length(colLevs[[i]])-1)) {stop("Strata names in numeric level ",i," should have one less name than column levels.\n")}
            columnNames <- c(columnNames,colnms.new[i])
            out <- createStrata.fn(dat=out,colnm=colnms[i],vars=colLevs[[i]],stratNms[[i]],nmcol=colnms.new[i],subset=F)
            removed <- sum(is.na(out[,colnms.new[i]]))
            messages <- c(messages,paste(removed,"observations removed because",colnms[i],"did not match"),paste(sum(!is.na(out[,colnms.new[i]])),"observations kept because of a matching",colnms[i]))
            if(verbose) {
                cat(messages[length(messages)-1],"\n")
                cat(messages[length(messages)],"\n")
                if(removed>0) print(table(out[is.na(out[,colnms.new[i]]),colnms[i]]))
                flush.console()
            }
            if(subset) {
                out <- out[!is.na(out[,colnms.new[i]]),]
            }
            #Remove missing factor levels from new columns
            if(is.factor(out[,colnms.new[i]])) {
                out[,colnms.new[i]] <- factor(out[,colnms.new[i]])
            }
        }
    }

    #some extra stats
    #uniqueCols <- apply(out[,summaryNames],2,function(x){length(unique(x))})

    #stratObs <- as.data.frame(table(out[,columnNames]))  #number of observations per stratum
    #stratObs <- stratObs[stratObs$Freq>0,]

    #numVessels <- NULL
    #if(!is.null(vesselColName)) {
    #    strat <- apply(out[,c(columnNames)],1,paste,collapse=",")
    #    numVessels <- as.data.frame(apply(table(out[,c(columnNames,vesselColName)])>0,1:length(columnNames),sum))
    #    tmp <- apply(numVessels,2,sum)
    #    numVessels <- numVessels[,tmp>0]
    #}
    #if(subset) {
    #    if(length(columnNames)==1) {
    #        strat <- out[,columnNames]
    #    }
    #    if(length(columnNames)>1) {
    #        strat <- apply(out[,columnNames],1,paste,collapse=",")
    #    }
    #    out$strat <- strat
    #    stratObs <- sort(table(strat))
    #}
    #if(!subset) {
    #    stratObs <- "Only output when subset=T"
    #}

    #return(invisible(list(dat=out,numVessels=numVessels,stratObs=stratObs,summaryUnique=uniqueCols,messages=messages)))
    attributes(out)$messages <- messages
    invisible(out)
}



#some stats by year and strata
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
        #numObs <- nrow(x)
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
        #if(numVessels==0 | numVessels>=vesselCrit) {
        #    OKnumVessels <- T
        #} else {
        #    OKnumVessels <- F
        #}

        #nombres <- c(nombres,"numVessels","OKnumVessels")
        #xx <- c(uniqueCols,totCols,numVessels,OKnumVessels)
        nombres <- c(nombres,"numVessels")
        xx <- c(uniqueCols,totCols,numVessels)
        names(xx) <- nombres
        return(xx)
    }


    out <- lapply(dat.split,summList.fn)
    out <- t(as.data.frame(out))
    out <- cbind(numObs,out)
    #tmp <- rep(F,nrow(out))
    #tmp[out[,"OKnumVessels"]==1] <- T
    #out[,"OKnumVessels"] <- T
    out$OKnumVessels<-F
    out$OKnumVessels[which((out$numVessels==0)|(out$numVessels>=vesselCrit))]<-T

    if(sum(!out[,"OKnumVessels"])>0) {cat("Some strata do not meet the vessel criteria for confidentiality\n")}


    return(out)
}

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