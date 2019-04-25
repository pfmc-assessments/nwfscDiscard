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