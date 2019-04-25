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