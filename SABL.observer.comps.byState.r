################################################################################
# CODE for SABLEFISH STOCK ASSESSMENT LENGTH FREQ., AGE, and AVE WGHT PROCESSING
# SA Author, 2009, 2011: Ian Stewart, NWFSC
#
# DATE          ANALYST                  MODIFICATIONS
#-------------------------------------------------------------------------------
# 2010 Sept.    E. Heery                 Original Code for 2009 assessment
# 2011 May 31   J. Jannot                Updated Code for 2011 assessment
# 2012 Nov-Dec   J. Jannot                Updated Code for 2013 assessment.  Used
#                                        as example code for Andi Stephens to feed length
#                                        frequency data to stock assessors.
# Feb. 2013	Andi		Customized for SABL stratification
# Mar. 2013	Andi		Customized for PTRL stratification
################################################################################

# Notes:  No Sexes for SABL. 
# Petrale at 1cm, 2cm bins, divided into summer(March-October) and winter (Nov1
# through Feb of the following year.

# Clean up

#  rm(list = ls())
#  library(foreign)
  library(plyr)
#  library(RODBC)

# "find.matching.rows" is also known as "match.f"

  source( "../Utilities.R")
  
# I've changed my strategy, I just read in the files manually, but here's how you would.
# get the BIOLOGICAL data set
# No longer a function
# getBio = function() {

  print("Reading Bio data")

#  Y = read.csv("../Data/Preprocessed.OBSVR.BIO.2015.csv", as.is=T)
  
  Y = OBBio2

  print("Got Bio data")

  Y = Y[Y$CATCH_DISPOSITION == "D",]
 
  Y = Y[Y$COMMON_NAME == "Sablefish", ]

  return(Y)
  
#} # End function getBio


#  Fix UNSEXED

   Y$SEX[is.na(Y$SEX)] = "U"
   Y$SEX[Y$SEX == "" ] = "U"
   Y$SEX = factor(Y$SEX)

 
# LENGTH FREQUENCIES  
###############################################################################
  
#-------------------------------------------------------
  nrow(Y)

# SPECIFY how data should be aggregated
#-------------------------------------------------------
 
  # Old examples:
  # grouping = expand.grid( FISHERY=unique(Y$fishery2), GEAR=unique(Y$gear2), STATE = unique(Y$D_STATE))
  # grouping = expand.grid( STATE = unique(Y$comp.area), GEAR_TYPE = c('TRAWL', 'FIXED GEAR', 'OTHER'))
  # grouping = expand.grid( YEAR = unique(Y$RYEAR),GEAR = unique(Y$vgear), FISHERY = unique(Y$fishery2), STATE = unique(Y$STATE))

  Y$vgear = Y$gear

  Y$vgear[Y$gear %in% c("Bottom Trawl","Midwater Trawl","Shrimp Trawl") ] = "TRAWL" 

  # Y$STATE = "Coastwide"
  
  Y$STATE = Y$R_STATE

  grouping = expand.grid(YEAR = unique(Y$RYEAR), GEAR = unique(Y$vgear), STATE = unique(Y$STATE))

  cat("Got grouping\n")

  # This is a matrix showing all the different strata
  # Alternatively, an assessment author might ask for stratification by latitude,
  # depth, season, etc.  These variables are in the data file already.

  # ASSIGN length bins

  #L.inf = max( Y$LENGTH+10 , na.rm = T)
  L.inf = 90
  L.maxbin = 90
  L.min = 20
  bin.width = 2 

  Y$use_length = Y$LENGTH
  Y$use_length[Y$use_length > L.inf] = L.inf
  Y$use_length[Y$use_length < L.min] = L.min

  length.bins <- cbind( bin = rep(seq(L.min, L.inf, by=bin.width), 
                        each=bin.width)[1:length(L.min:L.inf)], len = L.min:L.inf )


# find.matching.rows is also known as "match.f"
# I rewrote it for myself because I couldn't understand what it did!

  Y$Lbin = find.matching.rows( file = Y, table = length.bins,
                               findex = 'use_length', tindex = 'len',
                               tcol = 'bin')

  # This is just a check - Here we have 0 line items without a bin specified (no length record)

  dim(Y[ is.na(Y$Lbin),])  
  
# COMPUTE weighting
#-------------------------------------------------------  

  Y$exp1 <- Y$SPECIES_NUMBER / Y$BIO_SPECIMEN_COUNT

  Y$EXP_SP_WT[is.na(Y$EXP_SP_WT)]=
                              (Y$SPECIES_WEIGHT[is.na(Y$EXP_SP_WT)]/
                               Y$HOOKS_SAMPLED[is.na(Y$EXP_SP_WT)])*
                               Y$TOTAL_HOOKS[is.na(Y$EXP_SP_WT)]

  Y$exp2 <- Y$EXP_SP_WT / Y$SPECIES_WEIGHT   

  Y$wghtd_freq <- Y$FREQUENCY * Y$exp1 * Y$exp2
      
  # Andi added 2015 to stop NAs propagating throughout the analysis.

  NA_wgts = sum(is.na(Y$wghtd_freq))
  cat("Converting", NA_wgts, "of", nrow(Y), "to zero for arithmetic\n")
  Y$wghtd_freq[is.na(Y$wghtd_freq)] = 0
  
  # Andi added 2015


# No longer a function
# doLengths = function(Y) {

# AGGREGATE original & weighted LFs for each stratum
#-------------------------------------------------------


# Note that a 0 lenbin is generated if the LENGTH is na!

#  SaveY = Y
  
  Y = Y[! is.na(Y$LENGTH),]

  # Create empty container

  L.final = NULL
  
  for (i in 1:nrow(grouping)) {

    cat("\nGroup", i, "\n")
    print(grouping[i,])
        
    Y.i = Y[ Y$RYEAR == as.character(grouping$YEAR[i]) &  
             Y$STATE == as.character(grouping$STATE[i]) &
             Y$vgear == as.character(grouping$GEAR[i]),]


    cat("\n N records", nrow(Y.i), "\n\n")
  
    if ( nrow(Y.i) > 1 ) {

      lencomp <- ddply(Y.i,.(vgear,RYEAR,STATE,Lbin),summarize,
                       FREQ=sum(FREQUENCY), Weighted=sum(wghtd_freq, na.rm=T))

      colnames(lencomp) = c("Gear", "Year","State","Lenbin", "N_Fish","Weighted")
      
      L = lencomp 
      L = merge( L, expand.grid( Year = unique(L$Year), Gear=unique(L$Gear), 
                                 State = unique(L$State),
                                 Lenbin = unique(length.bins[,"bin"])), all.y=T)

      L[ is.na(L) ] <- 0

      tot.freq.wghtd=ddply(L,.(Year),summarize,Nfish.total.byYear=sum(N_Fish),
                           Wghtd.total.byYear=sum(Weighted, na.rm=T))

      L.out=merge(L,tot.freq.wghtd,all=T)
      
      L.out$Prop.numbers = L.out$N_Fish / L.out$Nfish.total.byYear
      L.out$Prop.wghtd = L.out$Weighted / L.out$Wghtd.total.byYear
                                                                            
      #L.out = merge(grouping[i, ], L.out)

      L.out = L.out[ order(L.out$Gear, L.out$State, L.out$Year, L.out$Lenbin), ]
  
      L.final = rbind(L.final, L.out)

    } # End if

  } # End for

  L.final$Prop.numbers[is.nan(L.final$Prop.numbers)]=0     
  L.final$Prop.wghtd[is.nan(L.final$Prop.wghtd)]=0     

  write.csv(L.final, "SABL_LF.csv", row.names=F)
      
      
# COMPUTE sample size for each stratum
#-------------------------------------------------------

  cat("Sample sizes\n"); flush.console();

  n.fish.samp=ddply(Y,.(vgear, RYEAR, STATE),summarise, FREQUENCY=sum(FREQUENCY), 
                       NUM_HAULS=length(unique(HAUL_ID)),
                       N_TRIPS=length(unique(TRIP_ID)),
                       N_VESSELS=length(unique(DRVID)))

  names(n.fish.samp) = c("Gear","Year","State","N_Fish","N_unique_Hauls","N_unique_Trips","N_unique_Vessels")

  write.csv(n.fish.samp, "SABL_LF_Nsamp.csv", row.names=F)

#} # End function doLengths


# AGE COMPS-  NO AGES for PETRALE
###############################################################################


# AGE COMPS- 
###############################################################################
# Use the same stratification as above
 
# ASSIGN age bins
#-------------------------------------------------------

# No longer a function  
#doAges = function(Y) {

  cat("Age comps\n"); flush.console();

  unique(Y$AGE) #if NA, then there are no ages for the species
  
  A.max = 35

  #age.bins = 1:A.max
  age.bins = 0:A.max
  
  Y$Age = Y$AGE
  Y$Age[Y$Age > A.max] = A.max
  

# AGGREGATE original & weighted ages for each stratum
#-------------------------------------------------------

  # Create empty container

  Z = Y[!is.na(Y$AGE),]

  A.final = NULL

  for (i in 1:nrow(grouping)){

    cat("\nGroup", i, "\n")
    print(grouping[i,])
        

    Y.i = Z[ Z$RYEAR == as.character(grouping$YEAR[i]) &  
             Z$STATE == as.character(grouping$STATE[i]) &
             Z$vgear == as.character(grouping$GEAR[i]),]

    if ( nrow(Y.i) > 1 ) {

      if ( !all(is.na(Y.i$AGE)) ) {

        agecomp <- ddply(Y.i,.(vgear,RYEAR,STATE,Age), summarise,
                         N_Fish=sum(FREQUENCY),Weighted=sum(wghtd_freq))

        colnames(agecomp)=c("Gear","Year","State","Age","N_Fish","Weighted")

        A = agecomp
        A = merge( A, expand.grid( Year = unique(A$Year), Gear = unique(A$Gear), 
                                   State = unique(A$STATE),
                                   Age = age.bins), all.y=T)

        A[ is.na(A) ] <- 0

        tot.freq.wghtd=ddply(A,.(Year), summarise, Nfish.total.byYear=sum(N_Fish),Wghtd.total.byYear=sum(Weighted))
        A.out = merge(A,tot.freq.wghtd,all=T)
        
        A.out$Prop.numbers = A.out$N_Fish/ A.out$Nfish.total.byYear
        A.out$Prop.wghtd = A.out$Weighted / A.out$Wghtd.total.byYear
        
        # A.out = merge(grouping[i, ], A.out)
  
        A.out = A.out[ order(A.out$Gear, A.out$Year, A.out$State, A.out$Age), ]

        A.final = rbind(A.final, A.out)
    
      } # End if not all 

    } # End if nrow

  } # End for

  A.final$Prop.numbers[is.nan(A.final$Prop.numbers)]=0     
  A.final$Prop.wghtd[is.nan(A.final$Prop.wghtd)]=0     

  nrow(A.final)     
  write.csv(A.final, "SABL_AF.csv", row.names=F)

  # COMPUTE sample size for each stratum

  ########################################################################

  cat("Age Sample sizes\n"); flush.console();

  n.fish.age.samp=ddply(Z,.(vgear, RYEAR, STATE),summarise, FREQUENCY=sum(FREQUENCY), 
                       NUM_HAULS=length(unique(HAUL_ID)),
                       N_TRIPS=length(unique(TRIP_ID)))

  names(n.fish.age.samp) = c("Gear","Year","State","N_Fish","N_unique_Hauls","N_unique_Trips")

  write.csv(n.fish.age.samp, "SABL_AGE_Nsamp.csv", row.names=F)

  ###########################################################################
  #
  # Now Age-at-lengths
  #
  ###########################################################################
  
  # Create empty container

  aal.final = NULL

  print("AAL Comps")

  for (i in 1:nrow(grouping)){

    cat("\nGroup", i, "\n")
    print(grouping[i,])
        
    Y.i = Z[ Z$RYEAR == as.character(grouping$YEAR[i]) &  
             Z$vgear == as.character(grouping$GEAR[i]) &
             Z$STATE == as.character(grouping$STATE[i]),]

    if ( nrow(Y.i) > 1 ) {

      if ( !all(is.na(Y.i$AGE)) ) {

        aalcomp <- ddply(Y.i,.(vgear,RYEAR,STATE,Age,Lbin), summarise,
                         N_Fish=sum(FREQUENCY),Weighted=sum(wghtd_freq))

        colnames(aalcomp)=c("Gear","Year","State","Age","Lbin", "N_Fish","Weighted")

        aal = aalcomp
        aal = merge( aal, expand.grid( Year = unique(aal$Year), Gear = unique(aal$Gear), 
                                       State = unique(aal$STATE),
                                       Age = age.bins), all.y=T)

        aal[ is.na(aal) ] <- 0

        tot.freq.wghtd=ddply(aal,.(Year), summarise, Nfish.total.byYear=sum(N_Fish),Wghtd.total.byYear=sum(Weighted))
        aal.out = merge(aal,tot.freq.wghtd,all=T)
        
        aal.out$Prop.numbers = aal.out$N_Fish/ aal.out$Nfish.total.byYear
        aal.out$Prop.wghtd = aal.out$Weighted / aal.out$Wghtd.total.byYear
        
        aal.out = aal.out[ order(aal.out$Gear, aal.out$Year, aal.out$State, aal.out$Age, aal$Lbin), ]

        aal.final = rbind(aal.final, aal.out)
    
      } # End if not all 

    } # End if nrow

  } # End for

  aal.final$Prop.numbers[is.nan(aal.final$Prop.numbers)]=0     
  aal.final$Prop.wghtd[is.nan(aal.final$Prop.wghtd)]=0     

  nrow(aal.final)     
  write.csv(aal.final, "SABL.aal.csv", row.names=F)


#} # End function doAges

# doWeights is a function because everything else runs pretty quickly, but 
# doWeights goes through the entire bio dataset.  

# Note that you need to manually set the gear to match what you used for
# the lengths and ages above, and fix the output filenames.
       
doWeights = function() {

# AVERAGE WEIGHTS
################################################################################

  #Get the Observer data set

  cat("Reading OB\n"); flush.console();

  # OB = read.csv( "../Data/Extracted_OB.csv", as.is=T )
  
  OB = OBCatch

  OB = OB[!is.na(OB$RYEAR),]

##############################################################################
# Specific per-species setup
##############################################################################

  OB = OB[OB$CATCH_DISPOSITION == "D",]

  # OB$D_STATE = "Coastwide"

  OB$vgear = OB$gear

  OB$vgear[OB$gear2 %in% c("Bottom Trawl","Midwater Trawl","Shrimp Trawl") ] = "TRAWL" 

##############################################################################
# Vessels per stratum for confidentiality (DRVID >= 3)
##############################################################################

  vps = ddply(OB,.(RYEAR,vgear,D_STATE), summarise, N_Vessels=length(unique(DRVID)))

  vps = vps[!is.na(vps$RYEAR),]

  colnames(vps) = c("Year", "Gear", "State", "N_Vessels")

  write.csv(vps, "SABL_VPS.csv", row.names=F)

##############################################################################
# Average Weight calculations
##############################################################################

  OB = OB[grepl("Sablefish", OB$COMMON_NAME), ]

  OB$EXP_SP_WT[is.na(OB$EXP_SP_WT)] = 0

  # Trying this to fix SD calculation

  OB$EXP_SP_CT[is.na(OB$EXP_SP_WT)] = 1

  OB = OB[!is.na(OB$SPECIES_NUMBER),]

  OB$AVG_WEIGHT = OB$SPECIES_WEIGHT / OB$SPECIES_NUMBER
  OB$AVG_WEIGHT_wghtd = OB$AVG_WEIGHT * OB$EXP_SP_CT

  OB$WGTD_MEAN = weighted.mean(OB$AVG_WEIGHT, OB$EXP_SP_CT)

  # Check for species in gear types by year 

  # spp.count=ddply(OB,.(RYEAR,vgear,D_STATE),summarise, NUM=sum(EXP_SP_WT))

  stats.cnts.cvg=ddply(OB,.(RYEAR,vgear,D_STATE), summarize,
                       AVG_WEIGHT.Mean=mean(AVG_WEIGHT, na.rm=T),
                       AVG_WEIGHT.SD=sd(AVG_WEIGHT, na.rm=T),
                       AVG_WEIGHT.Median=median(AVG_WEIGHT,na.rm=T),
                       AVG_WEIGHT.Min=min(AVG_WEIGHT,na.rm=T),
                       AVG_WEIGHT.Max=max(AVG_WEIGHT,na.rm=T),
                       Wghtd.AVG_W=(sum(AVG_WEIGHT_wghtd,na.rm=T))/(sum(EXP_SP_CT,na.rm=T)),
                       V = sum(EXP_SP_CT * (AVG_WEIGHT - WGTD_MEAN)^2)/sum(EXP_SP_CT),
                       MAX.CT = max(EXP_SP_CT),
                       Total.CT.Sum=sum(EXP_SP_CT, na.rm=T),
                       N_unique_Trips=length(unique(TRIP_ID)),
                       N_unique_Hauls=length(unique(HAUL_ID)),
                       N_Vessels=length(unique(DRVID))) 

  X=stats.cnts.cvg

  X$Wghtd.AVG_W.SD = sqrt(X$V / ((X$Total.CT.Sum/X$MAX.CT) -1))

  colnames(X)[1:3] = c("Year","Gear","State")

  X = X[!is.na(X$Year),]
  
  write.csv(X, "SABL_AvgWt.csv", row.names=F)

} # End function doWeights

#
# That's All, Folks!
#
#############################################################################
