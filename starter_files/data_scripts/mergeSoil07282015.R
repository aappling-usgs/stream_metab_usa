#Load required libraries

library(foreign)
library(plyr)

#Set directories
soilDir <- "C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Soil/"
workingDir <- "C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/"

#Create list of powstream COMIDs
comid640 <- read.csv(paste0(workingDir,"comid_640_sites_nodup2.csv"),
                     header=TRUE,colClasses=c("character","character","character","numeric","character"))
comid640 <- rename(comid640,c("bestcomid"="COMID"))
comid640 <- comid640[c("site_no","COMID","COMID_CONFIDENCE")]

#Soil Water Capacity (inches H2O / inches soil depth)
AC_AWC_zip <- "AC_AWC.zip"
unzip(paste0(soilDir,AC_AWC_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Soil")
AC_AWC <- read.dbf(paste0(soilDir,"AC_AWC.dbf"))
powAWC <- merge(x=comid640,y=AC_AWC,by="COMID",all.x=TRUE)
powAWC <- rename(powAWC,c("MEAN" = "AWC_RE"))
powAWC <- powAWC[c("site_no","COMID","COMID_CONFIDENCE","AWC_AC","AWC_RE")]
rm(AC_AWC)
file.remove(paste0(soilDir,"AC_AWC.dbf"))
summary(powAWC)

#Soil bulk density (g/cm3)
AC_BD_zip <- "AC_BD.zip"
unzip(paste0(soilDir,AC_BD_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Soil")
AC_BD <- read.dbf(paste0(soilDir,"AC_BD.dbf"))
powBD <- merge(x=comid640,y=AC_BD,by="COMID",all.x=TRUE)
powBD <- rename(powBD,c("MEAN" = "BD_RE"))
powBD <- powBD[c("site_no","BD_AC","BD_RE")]
rm(AC_BD)
file.remove(paste0(soilDir,"AC_BD.dbf"))
summary(powBD)
powSoil <- merge(x=powAWC, y=powBD, by="site_no")

#Soil clay content (%)
AC_CLAY_zip <- "AC_CLAY.zip"
unzip(paste0(soilDir,AC_CLAY_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Soil")
AC_CLAY <- read.dbf(paste0(soilDir,"AC_CLAY.dbf"))
powCLAY <- merge(x=comid640,y=AC_CLAY,by="COMID",all.x=TRUE)
powCLAY <- rename(powCLAY,c("MEAN" = "CLAY_RE"))
powCLAY <- powCLAY[c("site_no","CLAY_AC","CLAY_RE")]
rm(AC_CLAY)
file.remove(paste0(soilDir,"AC_CLAY.dbf"))
summary(powCLAY)
powSoil <- merge(x=powSoil, y=powCLAY, by="site_no")

#Soil erodibility
AC_KFACTUP_zip <- "AC_KFACTUP.zip"
unzip(paste0(soilDir,AC_KFACTUP_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Soil")
AC_KFACTUP <- read.dbf(paste0(soilDir,"AC_KFACTUP.dbf"))
powKFACTUP <- merge(x=comid640,y=AC_KFACTUP,by="COMID",all.x=TRUE)
powKFACTUP <- rename(powKFACTUP,c("MEAN" = "KFACTUP_RE"))
powKFACTUP <- powKFACTUP[c("site_no","KFACTUP_AC","KFACTUP_RE")]
rm(AC_KFACTUP)
file.remove(paste0(soilDir,"AC_KFACTUP.dbf"))
summary(powKFACTUP)
powSoil <- merge(x=powSoil, y=powKFACTUP, by="site_no")

#Soil organic material
AC_OM_zip <- "AC_OM.zip"
unzip(paste0(soilDir,AC_OM_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Soil")
AC_OM <- read.dbf(paste0(soilDir,"AC_OM.dbf"))
powOM <- merge(x=comid640,y=AC_OM,by="COMID",all.x=TRUE)
powOM <- rename(powOM,c("MEAN" = "OM_RE"))
powOM <- powOM[c("site_no","OM_AC","OM_RE")]
rm(AC_OM)
file.remove(paste0(soilDir,"AC_OM.dbf"))
summary(powOM)
powSoil <- merge(x=powSoil, y=powOM, by="site_no")

#Soil permeability (inches/hr)
AC_Perm_zip <- "AC_Perm.zip"
unzip(paste0(soilDir,AC_Perm_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Soil")
AC_Perm <- read.dbf(paste0(soilDir,"AC_Perm.dbf"))
powPerm <- merge(x=comid640,y=AC_Perm,by="COMID",all.x=TRUE)
powPerm <- rename(powPerm,c("MEAN" = "Perm_RE","PERM__AC" = "Perm_AC"))
powPerm <- powPerm[c("site_no","Perm_AC","Perm_RE")]
rm(AC_Perm)
file.remove(paste0(soilDir,"AC_Perm.dbf"))
summary(powPerm)
powSoil <- merge(x=powSoil, y=powPerm, by="site_no")

#Soil rock depth (inches)
AC_ROCKDEP_zip <- "AC_ROCKDEP.zip"
unzip(paste0(soilDir,AC_ROCKDEP_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Soil")
AC_ROCKDEP <- read.dbf(paste0(soilDir,"AC_ROCKDEP.dbf"))
powROCKDEP <- merge(x=comid640,y=AC_ROCKDEP,by="COMID",all.x=TRUE)
powROCKDEP <- rename(powROCKDEP,c("MEAN" = "ROCKDEP_RE"))
powROCKDEP <- powROCKDEP[c("site_no","ROCKDEP_AC","ROCKDEP_RE")]
rm(AC_ROCKDEP)
file.remove(paste0(soilDir,"AC_ROCKDEP.dbf"))
summary(powROCKDEP)
powSoil <- merge(x=powSoil, y=powROCKDEP, by="site_no")

#Soil average salinity
AC_SALINAVE_zip <- "AC_SALINAVE.zip"
unzip(paste0(soilDir,AC_SALINAVE_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Soil")
AC_SALINAVE <- read.dbf(paste0(soilDir,"AC_SALINAVE.dbf"))
powSALINAVE <- merge(x=comid640,y=AC_SALINAVE,by="COMID",all.x=TRUE)
powSALINAVE <- rename(powSALINAVE,c("MEAN" = "Saline_RE","SALINE_AC" = "Saline_AC"))
powSALINAVE <- powSALINAVE[c("site_no","Saline_AC","Saline_RE")]
rm(AC_SALINAVE)
file.remove(paste0(soilDir,"AC_SALINAVE.dbf"))
summary(powSALINAVE)
powSoil <- merge(x=powSoil, y=powSALINAVE, by="site_no")

#Soil percent sand
AC_SAND_zip <- "AC_SAND.zip"
unzip(paste0(soilDir,AC_SAND_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Soil")
AC_SAND <- read.dbf(paste0(soilDir,"AC_SAND.dbf"))
powSAND <- merge(x=comid640,y=AC_SAND,by="COMID",all.x=TRUE)
powSAND <- rename(powSAND,c("MEAN" = "SAND_RE"))
powSAND <- powSAND[c("site_no","SAND_AC","SAND_RE")]
rm(AC_SAND)
file.remove(paste0(soilDir,"AC_SAND.dbf"))
summary(powSAND)
powSoil <- merge(x=powSoil, y=powSAND, by="site_no")

#Soil percent silt
AC_SILT_zip <- "AC_SILT.zip"
unzip(paste0(soilDir,AC_SILT_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Soil")
AC_SILT <- read.dbf(paste0(soilDir,"AC_SILT.dbf"))
powSILT <- merge(x=comid640,y=AC_SILT,by="COMID",all.x=TRUE)
powSILT <- rename(powSILT,c("MEAN" = "SILT_RE"))
powSILT <- powSILT[c("site_no","SILT_AC","SILT_RE")]
rm(AC_SILT)
file.remove(paste0(soilDir,"AC_SILT.dbf"))
summary(powSILT)
powSoil <- merge(x=powSoil, y=powSILT, by="site_no")

#Percent soil restrictive layer at 25 cm depth
AC_SRL25_zip <- "AC_SRL25.zip"
unzip(paste0(soilDir,AC_SRL25_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Soil")
AC_SRL25 <- read.dbf(paste0(soilDir,"AC_SRL25.dbf"))
powSRL25 <- merge(x=comid640,y=AC_SRL25,by="COMID",all.x=TRUE)
powSRL25 <- rename(powSRL25,c("MEAN" = "SRL25_RE"))
powSRL25 <- powSRL25[c("site_no","SRL25_AC","SRL25_RE")]
rm(AC_SRL25)
file.remove(paste0(soilDir,"AC_SRL25.dbf"))
summary(powSRL25)
powSoil <- merge(x=powSoil, y=powSRL25, by="site_no")

#Percent soil restrictive layer at 35 cm depth
AC_SRL35_zip <- "AC_SRL35.zip"
unzip(paste0(soilDir,AC_SRL35_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Soil")
AC_SRL35 <- read.dbf(paste0(soilDir,"AC_SRL35.dbf"))
powSRL35 <- merge(x=comid640,y=AC_SRL35,by="COMID",all.x=TRUE)
powSRL35 <- rename(powSRL35,c("MEAN" = "SRL35_RE","SRL35__AC" = "SRL35_AC"))
powSRL35 <- powSRL35[c("site_no","SRL35_AC","SRL35_RE")]
rm(AC_SRL35)
file.remove(paste0(soilDir,"AC_SRL35.dbf"))
summary(powSRL35)
powSoil <- merge(x=powSoil, y=powSRL35, by="site_no")

#Percent soil restrictive layer at 45 cm depth
AC_SRL45_zip <- "AC_SRL45.zip"
unzip(paste0(soilDir,AC_SRL45_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Soil")
AC_SRL45 <- read.dbf(paste0(soilDir,"AC_SRL45.dbf"))
powSRL45 <- merge(x=comid640,y=AC_SRL45,by="COMID",all.x=TRUE)
powSRL45 <- rename(powSRL45,c("MEAN" = "SRL45_RE","SRL45__AC" = "SRL45_AC"))
powSRL45 <- powSRL45[c("site_no","SRL45_AC","SRL45_RE")]
rm(AC_SRL45)
file.remove(paste0(soilDir,"AC_SRL45.dbf"))
summary(powSRL45)
powSoil <- merge(x=powSoil, y=powSRL45, by="site_no")

#Percent soil restrictive layer at 55 cm depth
AC_SRL55_zip <- "AC_SRL55.zip"
unzip(paste0(soilDir,AC_SRL55_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Soil")
AC_SRL55 <- read.dbf(paste0(soilDir,"AC_SRL55.dbf"))
powSRL55 <- merge(x=comid640,y=AC_SRL55,by="COMID",all.x=TRUE)
powSRL55 <- rename(powSRL55,c("MEAN" = "SRL55_RE","SRL55__AC" = "SRL55_AC"))
powSRL55 <- powSRL55[c("site_no","SRL55_AC","SRL55_RE")]
rm(AC_SRL55)
file.remove(paste0(soilDir,"AC_SRL55.dbf"))
summary(powSRL55)
powSoil <- merge(x=powSoil, y=powSRL55, by="site_no")

#Soil water depth (feet)
AC_WATDEP_zip <- "AC_WTDEP.zip"
unzip(paste0(soilDir,AC_WATDEP_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Soil")
AC_WATDEP <- read.dbf(paste0(soilDir,"AC_WTDEP.dbf"))
powWATDEP <- merge(x=comid640,y=AC_WATDEP,by="COMID",all.x=TRUE)
powWATDEP <- rename(powWATDEP,c("MEAN" = "WtDep_RE","WTDEP_AC" = "WtDep_AC"))
powWATDEP <- powWATDEP[c("site_no","WtDep_AC","WtDep_RE")]
rm(AC_WATDEP)
file.remove(paste0(soilDir,"AC_WTDEP.dbf"))
summary(powWATDEP)
powSoil <- merge(x=powSoil, y=powWATDEP, by="site_no")

#Soil hydrologic group A (High infiltration rate)
AC_HGA_zip <- "HGA.zip"
unzip(paste0(soilDir,AC_HGA_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Soil")
AC_HGA <- read.dbf(paste0(soilDir,"HGA.dbf"))
powHGA <- merge(x=comid640,y=AC_HGA,by="COMID",all.x=TRUE)
powHGA <- rename(powHGA,c("MEAN" = "HGA_RE"))
powHGA <- powHGA[c("site_no","HGA_AC","HGA_RE")]
rm(AC_HGA)
file.remove(paste0(soilDir,"HGA.dbf"))
summary(powHGA)
powSoil <- merge(x=powSoil, y=powHGA, by="site_no")

#Soil hydrologic group AC (High infilitration rates when drained / Slow when not drained)
AC_HGAC_zip <- "AC_HGAC.zip"
unzip(paste0(soilDir,AC_HGAC_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Soil")
AC_HGAC <- read.dbf(paste0(soilDir,"AC_HGAC.dbf"))
powHGAC <- merge(x=comid640,y=AC_HGAC,by="COMID",all.x=TRUE)
powHGAC <- rename(powHGAC,c("MEAN" = "HGAC_RE"))
powHGAC <- powHGAC[c("site_no","HGAC_AC","HGAC_RE")]
rm(AC_HGAC)
file.remove(paste0(soilDir,"AC_HGAC.dbf"))
summary(powHGAC)
powSoil <- merge(x=powSoil, y=powHGAC, by="site_no")

#Soil hydrologic group AD (High infiltration when drained, very slow when not drained)
AC_HGAD_zip <- "AC_HGAD.zip"
unzip(paste0(soilDir,AC_HGAD_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Soil")
AC_HGAD <- read.dbf(paste0(soilDir,"AC_HGAD.dbf"))
powHGAD <- merge(x=comid640,y=AC_HGAD,by="COMID",all.x=TRUE)
powHGAD <- rename(powHGAD,c("MEAN" = "HGAD_RE"))
powHGAD <- powHGAD[c("site_no","HGAD_AC","HGAD_RE")]
rm(AC_HGAD)
file.remove(paste0(soilDir,"AC_HGAD.dbf"))
summary(powHGAD)
powSoil <- merge(x=powSoil, y=powHGAD, by="site_no")

#Soil hydrologic group B (Moderate infiltration rates)
AC_HGB_zip <- "AC_HGB.zip"
unzip(paste0(soilDir,AC_HGB_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Soil")
AC_HGB <- read.dbf(paste0(soilDir,"AC_HGB.dbf"))
powHGB <- merge(x=comid640,y=AC_HGB,by="COMID",all.x=TRUE)
powHGB <- rename(powHGB,c("MEAN" = "HGB_RE"))
powHGB <- powHGB[c("site_no","HGB_AC","HGB_RE")]
rm(AC_HGB)
file.remove(paste0(soilDir,"AC_HGB.dbf"))
summary(powHGB)
powSoil <- merge(x=powSoil, y=powHGB, by="site_no")

#Soil hydrologic group BC (Moderate infiltration when drained, slow infiltration when not drained)
AC_HGBC_zip <- "AC_HGBC.zip"
unzip(paste0(soilDir,AC_HGBC_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Soil")
AC_HGBC <- read.dbf(paste0(soilDir,"AC_HGBC.dbf"))
powHGBC <- merge(x=comid640,y=AC_HGBC,by="COMID",all.x=TRUE)
powHGBC <- rename(powHGBC,c("MEAN" = "HGBC_RE"))
powHGBC <- powHGBC[c("site_no","HGBC_AC","HGBC_RE")]
rm(AC_HGBC)
file.remove(paste0(soilDir,"AC_HGBC.dbf"))
summary(powHGBC)
powSoil <- merge(x=powSoil, y=powHGBC, by="site_no")

#Soil hydrologic group BD (Moderate infiltration rate when drained, very slow infiltration when not drained)
AC_HGBD_zip <- "AC_HGBD.zip"
unzip(paste0(soilDir,AC_HGBD_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Soil")
AC_HGBD <- read.dbf(paste0(soilDir,"AC_HGBD.dbf"))
powHGBD <- merge(x=comid640,y=AC_HGBD,by="COMID",all.x=TRUE)
powHGBD <- rename(powHGBD,c("MEAN" = "HGBD_RE"))
powHGBD <- powHGBD[c("site_no","HGBD_AC","HGBD_RE")]
rm(AC_HGBD)
file.remove(paste0(soilDir,"AC_HGBD.dbf"))
summary(powHGBD)
powSoil <- merge(x=powSoil, y=powHGBD, by="site_no")

#Soil hydrologic group C (Slow infiltration rate)
AC_HGC_zip <- "AC_HGC.zip"
unzip(paste0(soilDir,AC_HGC_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Soil")
AC_HGC <- read.dbf(paste0(soilDir,"AC_HGC.dbf"))
powHGC <- merge(x=comid640,y=AC_HGC,by="COMID",all.x=TRUE)
powHGC <- rename(powHGC,c("MEAN" = "HGC_RE"))
powHGC <- powHGC[c("site_no","HGC_AC","HGC_RE")]
rm(AC_HGC)
file.remove(paste0(soilDir,"AC_HGC.dbf"))
summary(powHGC)
powSoil <- merge(x=powSoil, y=powHGC, by="site_no")

#Soil hydrologic group CD (slow infiltration rate when drained, very slow infiltration rate when not drained)
AC_HGCD_zip <- "AC_HGCD.zip"
unzip(paste0(soilDir,AC_HGCD_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Soil")
AC_HGCD <- read.dbf(paste0(soilDir,"AC_HGCD.dbf"))
powHGCD <- merge(x=comid640,y=AC_HGCD,by="COMID",all.x=TRUE)
powHGCD <- rename(powHGCD,c("MEAN" = "HGCD_RE"))
powHGCD <- powHGCD[c("site_no","HGCD_AC","HGCD_RE")]
rm(AC_HGCD)
file.remove(paste0(soilDir,"AC_HGCD.dbf"))
summary(powHGCD)
powSoil <- merge(x=powSoil, y=powHGCD, by="site_no")

#Soil hydrologic group D (very slow infiltration rate)
AC_HGD_zip <- "AC_HGD.zip"
unzip(paste0(soilDir,AC_HGD_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Soil")
AC_HGD <- read.dbf(paste0(soilDir,"AC_HGD.dbf"))
powHGD <- merge(x=comid640,y=AC_HGD,by="COMID",all.x=TRUE)
powHGD <- rename(powHGD,c("MEAN" = "HGD_RE"))
powHGD <- powHGD[c("site_no","HGD_AC","HGD_RE")]
rm(AC_HGD)
file.remove(paste0(soilDir,"AC_HGD.dbf"))
summary(powHGD)
powSoil <- merge(x=powSoil, y=powHGD, by="site_no")

str(powSoil)

write.table(powSoil, paste0(soilDir,"powSoil.csv"), sep=",", row.names=FALSE, col.names=TRUE)
