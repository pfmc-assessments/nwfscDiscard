dont source

rm(list=ls(all=TRUE))

load(nwfscDiscard)

species = "Yellowtail Rockfish"
species.save = "Yellowtail"

# Load all the date to evaluate how many observations for 

load(" load the data ") 
ob <- OBCatch


# Do som examinations of the data for the species by year
table(ob$year, ob$COMMON_NAME == species)


# How much data is there by state?
table(ob$r_state, ob$COMMON_NAME == species)

# Subset the data to only work with the single species.
ind = which(ob$COMMON_NAME == species)
sub.ob = ob[ind,]
table(sub.ob$year)

table(sub.ob$sector, sub.ob$gear2)


aggregate(DIS ~ year + gear2 + sector, FUN = sum, sub.ob)
ind = which(sub.ob$gear2 == "Bottom Trawl" | sub.ob$gear2 == "Midwater Trawl" | sub.ob$gear2 == "Shrimp Trawl") 
trawl = sub.ob[ind,]
aggregate(DIS ~ year, FUN = sum, trawl[trawl$sector == "Catch Shares", ])
aggregate(DIS ~ year + gear2, FUN = sum, trawl[trawl$sector == "Catch Shares", ])



###############################################################################################################

gearLevels  <- list(c("Bottom Trawl", "Midwater Trawl", "Shrimp Trawl"), "Hook & Line")
gearNames   <- c("BottomTrawl","Hook&Line")
#stateLevels <- c("CA","OR","WA")  #r_state
#stateNames  <- c("CA","OR","WA")
#latLevels <- c(0, 40.1667, 100)  #r_state
#latNames <- c("South4010", "North4010")

areaLevels <- c("SOUTH", "NORTH")  #r_state
areaNames <- c("South4010", "North4010")

run.name = "GearArea"


#By state for all gears combined
#######################################################################################################################
### check for confidentiality using all observations
out <- NULL

ob$area = ob$AREA
out$'pre2011' <- checkConfidentiality(dat = ob, 
				 colnms = c("area", 'gear2'),
				 colLevs= list(areaLevels, gearLevels), 
				 strNms = list(areaNames, gearNames))

save(out, file = file.name (species.save, "RdatFiles/allForConfidentiality_", run.name, ".Rdat"))


### Start here if already done above
load(file.name(species.save, "RdatFiles/allForConfidentiality_", run.name, ".Rdat")



lapply(out,function(x){
	x[!x$OKnumVessels,]
})


# There are some confidentiality issues here but they are in the catch share sector for hook & line.
# The majority of catch is by non-catch share sector of hook & line and it is unlikely the assessment will use CS h&l data.  
# Run the bootstrap to check the estimates across gears and sectors.

# $pre2011
#  [1] ryear          areanew        gear2new       CatchShares    numObs         unique.drvid   unique.trip_id unique.haul_id sum.dis        sum.ret        numVessels    
# [12] OKnumVessels  
# <0 rows> (or 0-length row.names)
# 
# $`2011`
#  [1] ryear          areanew        gear2new       CatchShares    numObs         unique.drvid   unique.trip_id unique.haul_id sum.dis        sum.ret        numVessels    
# [12] OKnumVessels  
# <0 rows> (or 0-length row.names)
# 
# $`2012`
#                                ryear   areanew  gear2new CatchShares numObs unique.drvid unique.trip_id unique.haul_id sum.dis  sum.ret numVessels OKnumVessels
# X2012.South4010.Hook.Line.TRUE  2012 South4010 Hook&Line        TRUE    136            2             10             15 1897.32 7131.758          2        FALSE
# 
# $`2013`
#                                ryear   areanew  gear2new CatchShares numObs unique.drvid unique.trip_id unique.haul_id  sum.dis  sum.ret numVessels OKnumVessels
# X2013.North4010.Hook.Line.TRUE  2013 North4010 Hook&Line        TRUE   1328            2              5            129 75895.71 144096.5          2        FALSE
# 
# $`2014`
#  [1] ryear          areanew        gear2new       CatchShares    numObs         unique.drvid   unique.trip_id unique.haul_id sum.dis        sum.ret        numVessels    
# [12] OKnumVessels  
# <0 rows> (or 0-length row.names)
# 
# $`2015`
#                                ryear   areanew  gear2new CatchShares numObs unique.drvid unique.trip_id unique.haul_id sum.dis sum.ret numVessels OKnumVessels
# X2015.South4010.Hook.Line.TRUE  2015 South4010 Hook&Line        TRUE    323            2              4             37   16459 21784.2          2        FALSE
# 