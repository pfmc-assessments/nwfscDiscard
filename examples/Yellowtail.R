dont source

rm(list=ls(all=TRUE))
setwd("C:/Assessments/ObserverDatabase/2017/")
#source("../ExtractR/Rcode/functions/Functions.R")
files <- dir("Rcode/functions")
for(i in 1:length(files)) {
	source(file.path("Rcode/functions", files[i]))
}

species = "Yellowtail Rockfish"
species.save = "Yellowtail"

# Load all the date to evaluate how many observations for 
# California Scorpionfish

load("extractedData/ob.pre2011.Rdat") #object named ob
ob.all <- ob
load("extractedData/ob.2011.Rdat") #object named ob
ob.all <- rbind(ob.all,ob)
load("extractedData/ob.2012.Rdat") #object named ob
ob.all <- rbind(ob.all,ob)
load("extractedData/ob.2013.Rdat") #object named ob
ob.all <- rbind(ob.all,ob)
load("extractedData/ob.2014.Rdat") #object named ob
ob.all <- rbind(ob.all,ob)
load("extractedData/ob.2015.Rdat") #object named ob
ob.all <- rbind(ob.all,ob)

ob <- ob.all
rm(ob.all)

table(ob$year, ob$COMMON_NAME == species)

#        FALSE   TRUE
#   2002  68346    323
#   2003  59584    166
#   2004  96026    394
#   2005  95960    454
#   2006  72066    275
#   2007  77948    207
#   2008  90160    190
#   2009 103317    184
#   2010  85494    248
#   2011 227928    720
#   2012 205634    874
#   2013 210524    694
#   2014 195173    781
#   2015 196965    886


table(ob$r_state, ob$COMMON_NAME == species)
#      FALSE    TRUE
#  CA  518918     898
#  OR 1047238    3941
#  WA  218969    1557


ind = which(ob$COMMON_NAME == species)
sub.ob = ob[ind,]
table(sub.ob$year)

table(sub.ob$sector, sub.ob$gear2)
#                          Bottom Trawl Hook & Line Midwater Trawl Shrimp Trawl
#  Catch Shares                    2534          65            166            0
#  LE CA Halibut                      4           0              0            0
#  LE Fixed Gear DTL                  0           4              0            0
#  Limited Entry Sablefish            0         348              0            0
#  Limited Entry Trawl             1403           0             14            0
#  Midwater Hake                      0           0             17            0
#  Midwater Rockfish                  0           0            117            0
#  Nearshore                          0        1384              0            0
#  OA Fixed Gear                      0          15              0            0
#  Pink Shrimp                        0           0              0          190
#  Shoreside Hake                     0           0            135            0


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
load("extractedData/ob.pre2011.Rdat") #object named ob
ob$area = ob$AREA
out$'pre2011' <- checkConfidentiality(dat = ob, 
				 colnms = c("area", 'gear2'),
				 colLevs= list(areaLevels, gearLevels), 
				 strNms = list(areaNames, gearNames))

load("extractedData/ob.2011.Rdat") #object named ob
ob$area = ob$AREA
out$'2011' <- checkConfidentiality(dat = ob, 
				 colnms = c("area", 'gear2'),
				 colLevs= list(areaLevels, gearLevels), 
				 strNms = list(areaNames, gearNames))

load("extractedData/ob.2012.Rdat") #object named ob
ob$area = ob$AREA
out$'2012' <- checkConfidentiality(dat = ob, 
			  colnms = c("area", 'gear2'),
			  colLevs= list(areaLevels, gearLevels), 
			  strNms = list(areaNames, gearNames))

load("extractedData/ob.2013.Rdat") #object named ob
ob$area = ob$AREA
out$'2013' <- checkConfidentiality(dat = ob, 
				 colnms = c("area", 'gear2'),
				 colLevs= list(areaLevels, gearLevels), 
				 strNms = list(areaNames, gearNames))

load("extractedData/ob.2014.Rdat") #object named ob
ob$area = ob$AREA
out$'2014' <- checkConfidentiality(dat = ob, 
				 colnms = c("area", 'gear2'),
				 colLevs= list(areaLevels, gearLevels), 
				 strNms = list(areaNames, gearNames))

load("extractedData/ob.2015.Rdat") #object named ob
ob$area = ob$AREA
out$'2015' <- checkConfidentiality(dat = ob, 
				 colnms = c("area", 'gear2'),
				 colLevs= list(areaLevels, gearLevels), 
				 strNms = list(areaNames, gearNames))

save(out,file=paste0(species.save,"/RdatFiles/allForConfidentiality_", run.name, ".Rdat"))


### Start here if already done above
load(paste0(species.save,"/RdatFiles/allForConfidentiality_", run.name, ".Rdat")



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