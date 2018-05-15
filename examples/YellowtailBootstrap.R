dont source

rm(list=ls(all=TRUE))

species = "Yellowtail Rockfish"
save.name = "Yellowtail"

setwd("C:/Assessments/ObserverDatabase")
source("Rcode/functions/observRfunctions.R")
source("Rcode/functions/checkConfidentiality.R")
source("C:/Assessments/ObserverDatabase/2017/Rcode/functions/Boot_summary_fxn_jej.r")
source("C:/Assessments/ObserverDatabase/2017/Rcode/functions/bootstrapDiscardBiomass_ach_V3.r")
source("C:/Assessments/ObserverDatabase/2017/Rcode/functions/bootstrapDiscardRatio_ach_V3.r")
source("C:/Assessments/ObserverDatabase/Rcode/BootstrapDiscardData.R")

source("C:/Assessments/ObserverDatabase/2017/Rcode/discardsNonCatchShares.R")
source("C:/Assessments/ObserverDatabase/2017/Rcode/discardsCatchShares.R")

dte=Sys.Date()

setwd("C:/Assessments/ObserverDatabase/2017")
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

ob$area <- ob$AREA

############################################################################################################################
# Exploration based on latitude for CA and OR, there are confidentiality issues with this stratification
############################################################################################################################
#strata definitions

gearLevels  <- list(c("Bottom Trawl", "Midwater Trawl", "Shrimp Trawl"), "Hook & Line")
gearNames   <- c("BottomTrawl","Hook&Line")
areaLevels <- c("SOUTH", "NORTH")  #r_state
areaNames <- c("South4010", "North4010")

run = "GearArea"

outBoot <- bootstrapDiscardData(ob, sp = species, B=1,
	            colnms=c("area","gear2"),
	            colnms.new=c("Area","gear3"),
				colLevs=list(areaLevels,gearLevels),
				stratNms=list(areaNames,gearNames),
				bootFile= paste0(save.name, '/RdatFiles/noboot_',run,'.out'),
				resultsFile=paste0(save.name, '/RdatFiles/noboot_dat_',run,'.out') )

write.csv(outBoot$ncs,paste0(save.name,'/', save.name, '_OB_DisRatios_noboot_ncs_',run,'_',dte,'.csv'),row.names=F)
write.csv(outBoot$cs, paste0(save.name,'/', save.name, '_OB_DisRatios_noboot_cs_',run, '_',dte,'.csv'),row.names=F)


outBoot <- bootstrapDiscardData(ob,sp = species, B = 10000,
	            colnms=c("area","gear2"),
	            colnms.new=c("Area","gear3"),
				colLevs=list(areaLevels,gearLevels),
				stratNms=list(areaNames,gearNames),
				bootFile=paste0(save.name, '/RdatFiles/boot_',run,'.out'),
				resultsFile=paste0(save.name, '/RdatFiles/boot_dat',run,'.out') )

write.csv(outBoot$ncs,paste0(save.name, '/', save.name, '_OB_DisRatios_boot_ncs_',run,'_',dte,'.csv'),row.names=F)
write.csv(outBoot$cs, paste0(save.name, '/', save.name, '_OB_DisRatios_boot_cs_',run,'_', dte,'.csv'),row.names=F)

