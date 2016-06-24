#Load required libraries

library(foreign)
library(plyr)

#Set directories
hydrolDir <- "C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Hydrol/"
workingDir <- "C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/"

#Create list of powstream COMIDs
comid640 <- read.csv(paste0(workingDir,"comid_640_sites_nodup2.csv"),
            header=TRUE,colClasses=c("character","character","character","numeric","character"))
comid640 <- rename(comid640,c("bestcomid"="COMID"))

#Baseflow Index
AC_BFI_zip <- "AC_BFI.zip"
unzip(paste0(hydrolDir,AC_BFI_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Hydrol")
AC_BFI <- read.dbf(paste0(hydrolDir,"AC_BFI.dbf"))
powBFI <- merge(x=comid640,y=AC_BFI,by="COMID",all.x=TRUE)
powBFI <- rename(powBFI,c("MEAN" = "BFI_RE"))
powBFI <- powBFI[c("site_no","COMID","COMID_CONFIDENCE","BFI_AC","BFI_RE")]
rm(AC_BFI)
file.remove(paste0(hydrolDir,"AC_BFI.dbf"))
summary(powBFI)

#Contact time
AC_Contact_zip <- "AC_CONTACT.zip"
unzip(paste0(hydrolDir,AC_Contact_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Hydrol")
AC_Contact <- read.dbf(paste0(hydrolDir,"AC_CONTACT.dbf"))
powContact <- merge(x=comid640,y=AC_Contact,by="COMID",all.x=TRUE)
powContact <- rename(powContact,c("MEAN" = "Contact_RE", "CONTACT_AC" = "Contact_AC"))
powContact <- powContact[c("site_no","Contact_RE","Contact_AC")]
rm(AC_Contact)
file.remove(paste0(hydrolDir,"AC_CONTACT.dbf"))
summary(powContact)
powHydrol <- merge(x=powBFI, y=powContact, by="site_no")

#Infiltration excess overland flow
AC_IEOF_zip <- "AC_IEOF.zip"
unzip(paste0(hydrolDir,AC_IEOF_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Hydrol")
AC_IEOF <- read.dbf(paste0(hydrolDir,"AC_IEOF.dbf"))
powIEOF <- merge(x=comid640,y=AC_IEOF,by="COMID",all.x=TRUE)
powIEOF <- rename(powIEOF,c("MEAN" = "IEOF_RE"))
powIEOF <- powIEOF[c("site_no","IEOF_RE","IEOF_AC")]
rm(AC_IEOF)
file.remove(paste0(hydrolDir,"AC_IEOF.dbf"))
summary(powIEOF)
powHydrol <- merge(x=powHydrol, y=powIEOF, by="site_no")


#Groundwater recharge (mm / yr)
AC_RECHG_zip <- "AC_RECHG.zip"
unzip(paste0(hydrolDir,AC_RECHG_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Hydrol")
AC_RECHG <- read.dbf(paste0(hydrolDir,"AC_RECHG.dbf"))
powRECHG <- merge(x=comid640,y=AC_RECHG,by="COMID",all.x=TRUE)
powRECHG <- rename(powRECHG,c("MEAN" = "Rechg_RE","RECHG_AC" = "Rechg_AC"))
powRECHG <- powRECHG[c("site_no","Rechg_RE","Rechg_AC")]
rm(AC_RECHG)
file.remove(paste0(hydrolDir,"AC_RECHG.dbf"))
summary(powRECHG)
powHydrol <- merge(x=powHydrol, y=powRECHG, by="site_no")

#Rainfall runoff factor (100s ft-ton in h-1 ac-1 yr-1 x10)
AC_RF30yr_zip <- "AC_RF30yr.zip"
unzip(paste0(hydrolDir,AC_RF30yr_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Hydrol")
AC_RF30yr <- read.dbf(paste0(hydrolDir,"RF30yr_aggresults.dbf"))
powRF30yr <- merge(x=comid640,y=AC_RF30yr,by="COMID",all.x=TRUE)
powRF30yr <- rename(powRF30yr,c("MEAN" = "RF30yr_RE","RF30YR_AC" = "RF30yr_AC"))
powRF30yr <- powRF30yr[c("site_no","RF30yr_RE","RF30yr_AC")]
rm(AC_RF30yr)
file.remove(paste0(hydrolDir,"RF30yr_aggresults.dbf"))
summary(powRF30yr)
powHydrol <- merge(x=powHydrol, y=powRF30yr, by="site_no")

#Runoff (mm / yr)
AC_RUNOFF_zip <- "AC_RUNOFF.zip"
unzip(paste0(hydrolDir,AC_RUNOFF_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Hydrol")
AC_RUNOFF <- read.dbf(paste0(hydrolDir,"AC_RUNOFF.dbf"))
powRUNOFF <- merge(x=comid640,y=AC_RUNOFF,by="COMID",all.x=TRUE)
powRUNOFF <- rename(powRUNOFF,c("MEAN" = "RUNOFF_RE"))
powRUNOFF <- powRUNOFF[c("site_no","RUNOFF_RE","RUNOFF_AC")]
rm(AC_RUNOFF)
file.remove(paste0(hydrolDir,"AC_RUNOFF.dbf"))
summary(powRUNOFF)
powHydrol <- merge(x=powHydrol, y=powRUNOFF, by="site_no")

#Saturation overland flow (percent of total streamflow)
AC_SATOF_zip <- "AC_SATOF.zip"
unzip(paste0(hydrolDir,AC_SATOF_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Hydrol")
AC_SATOF <- read.dbf(paste0(hydrolDir,"AC_SATOF.dbf"))
powSATOF <- merge(x=comid640,y=AC_SATOF,by="COMID",all.x=TRUE)
powSATOF <- rename(powSATOF,c("MEAN" = "SATOF_RE"))
powSATOF <- powSATOF[c("site_no","SATOF_RE","SATOF_AC")]
rm(AC_SATOF)
file.remove(paste0(hydrolDir,"AC_SATOF.dbf"))
summary(powSATOF)
powHydrol <- merge(x=powHydrol, y=powSATOF, by="site_no")

str(powHydrol)
