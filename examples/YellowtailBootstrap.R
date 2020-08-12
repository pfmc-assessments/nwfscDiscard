dont source

rm(list=ls(all=TRUE))

species = "Yellowtail Rockfish"
save.name = "Yellowtail"

library(nwfscDiscard)

dte = Sys.Date()

setwd(" Set the working directory")
load(" load the data ") 

ob <- OBCatch
ob$area <- ob$AREA

############################################################################################################################
# Exploration based on latitude for CA and OR, there are confidentiality issues with this stratification
############################################################################################################################
#strata definitions

gearLevels  <- list(c("Bottom Trawl", "Midwater Trawl", "Shrimp Trawl"), "Hook & Line")
gearNames   <- c("BottomTrawl","Hook&Line")
areaLevels  <- c("SOUTH", "NORTH")  #r_state
areaNames   <- c("South4010", "North4010")

run = "GearArea"

outBoot <- bootstrapDiscardData(ob, sp = species, B=1,
	            colnms = c("area", "gear2"),
	            colnms.new = c("Area", "gear3"),
				colLevs = list(areaLevels, gearLevels),
				stratNms = list(areaNames, gearNames),
				bootFile = file.path(save.name, 'RdatFiles', 'noboot_', run, '.out'),
				resultsFile = paste0(save.name, 'RdatFiles', 'noboot_dat_', run, '.out') )

write.csv(outBoot$ncs, file.path(save.name, paste0(save.name, '_OB_DisRatios_noboot_ncs_',run, '_', dte, '.csv') ), row.names = FALSE)
write.csv(outBoot$cs,  file.path(save.name, paste0(save.name, '_OB_DisRatios_noboot_cs_', run, '_', dte, '.csv') ), row.names = FALSE)


outBoot <- bootstrapDiscardData(ob, sp = species, B = 10000,
	            colnms = c("area", "gear2"),
	            colnms.new = c("Area", "gear3"),
				colLevs = list(areaLevels, gearLevels),
				stratNms = list(areaNames, gearNames),
				bootFile = file.path(save.name, 'RdatFiles', 'boot_', run, '.out'),
				resultsFile = paste0(save.name, 'RdatFiles', 'boot_dat', run, '.out') )

write.csv(outBoot$ncs, file.path(save.name, paste0(save.name, '_OB_DisRatios_boot_ncs_',run, '_', dte,'.csv')), row.names = FALSE)
write.csv(outBoot$cs,  file.path(save.name, paste0(save.name, '_OB_DisRatios_boot_cs_', run, '_', dte,'.csv')), row.names = FALSE)

