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
