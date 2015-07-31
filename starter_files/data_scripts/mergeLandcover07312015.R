#Load required libraries

library(foreign)
library(plyr)

#Set directories
landcoverDir <- "C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Landcover/"
workingDir <- "C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/"

#Create list of powstream COMIDs
comid640 <- read.csv(paste0(workingDir,"comid_640_sites_nodup2.csv"),
                     header=TRUE,colClasses=c("character","character","character","numeric","character"))
comid640 <- rename(comid640,c("bestcomid"="COMID"))
comid640 <- comid640[c("site_no","COMID","COMID_CONFIDENCE")]

#NLCD 2001 Reach-scale land Use;  keep reach areas for subsequent % land coverage calculations
NLCD01_zip <- "NLCD01.zip"
unzip(paste0(landcoverDir,NLCD01_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Landcover")
NLCD01 <- read.dbf(paste0(landcoverDir,"NLCD01.dbf"))
powNLCD01 <- merge(x=comid640,y=NLCD01,by="COMID",all.x=TRUE)
powNLCD01["RE_NLCD01pct11"] <- powNLCD01$NLCD01_11/powNLCD01$NLCD01_ARE
powNLCD01["RE_NLCD01pct12"] <- powNLCD01$NLCD01_12/powNLCD01$NLCD01_ARE
powNLCD01["RE_NLCD01pct21"] <- powNLCD01$NLCD01_21/powNLCD01$NLCD01_ARE
powNLCD01["RE_NLCD01pct22"] <- powNLCD01$NLCD01_22/powNLCD01$NLCD01_ARE
powNLCD01["RE_NLCD01pct23"] <- powNLCD01$NLCD01_23/powNLCD01$NLCD01_ARE
powNLCD01["RE_NLCD01pct24"] <- powNLCD01$NLCD01_24/powNLCD01$NLCD01_ARE
powNLCD01["RE_NLCD01pct31"] <- powNLCD01$NLCD01_31/powNLCD01$NLCD01_ARE
powNLCD01["RE_NLCD01pct41"] <- powNLCD01$NLCD01_41/powNLCD01$NLCD01_ARE
powNLCD01["RE_NLCD01pct42"] <- powNLCD01$NLCD01_42/powNLCD01$NLCD01_ARE
powNLCD01["RE_NLCD01pct43"] <- powNLCD01$NLCD01_43/powNLCD01$NLCD01_ARE
powNLCD01["RE_NLCD01pct52"] <- powNLCD01$NLCD01_52/powNLCD01$NLCD01_ARE
powNLCD01["RE_NLCD01pct71"] <- powNLCD01$NLCD01_71/powNLCD01$NLCD01_ARE
powNLCD01["RE_NLCD01pct81"] <- powNLCD01$NLCD01_81/powNLCD01$NLCD01_ARE
powNLCD01["RE_NLCD01pct82"] <- powNLCD01$NLCD01_82/powNLCD01$NLCD01_ARE
powNLCD01["RE_NLCD01pct90"] <- powNLCD01$NLCD01_90/powNLCD01$NLCD01_ARE
powNLCD01["RE_NLCD01pct95"] <- powNLCD01$NLCD01_95/powNLCD01$NLCD01_ARE
powNLCD01 <- rename(powNLCD01,c("NLCD01_ARE" = "REACH_AREA"))
powNLCD01 <- powNLCD01[c("site_no","COMID","COMID_CONFIDENCE","RE_NLCD01pct11","RE_NLCD01pct12","RE_NLCD01pct21","RE_NLCD01pct22",
                         "RE_NLCD01pct23","RE_NLCD01pct24","RE_NLCD01pct31","RE_NLCD01pct41","RE_NLCD01pct42","RE_NLCD01pct43",
                         "RE_NLCD01pct52","RE_NLCD01pct71","RE_NLCD01pct81","RE_NLCD01pct82","RE_NLCD01pct90","RE_NLCD01pct95","REACH_AREA")]
rm(NLCD01)
file.remove(paste0(landcoverDir,"NLCD01.dbf"))
summary(powNLCD01)

#NLCD 2006 Reach-scale land Use;  keep reach areas for subsequent % land coverage calculations
NLCD06_zip <- "NLCD06.zip"
unzip(paste0(landcoverDir,NLCD06_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Landcover")
NLCD06 <- read.dbf(paste0(landcoverDir,"NLCD06.dbf"))
powNLCD06 <- merge(x=comid640,y=NLCD06,by="COMID",all.x=TRUE)
powNLCD06["RE_NLCD06pct11"] <- powNLCD06$NLCD06_11/powNLCD06$NLCD06_ARE
powNLCD06["RE_NLCD06pct12"] <- powNLCD06$NLCD06_12/powNLCD06$NLCD06_ARE
powNLCD06["RE_NLCD06pct21"] <- powNLCD06$NLCD06_21/powNLCD06$NLCD06_ARE
powNLCD06["RE_NLCD06pct22"] <- powNLCD06$NLCD06_22/powNLCD06$NLCD06_ARE
powNLCD06["RE_NLCD06pct23"] <- powNLCD06$NLCD06_23/powNLCD06$NLCD06_ARE
powNLCD06["RE_NLCD06pct24"] <- powNLCD06$NLCD06_24/powNLCD06$NLCD06_ARE
powNLCD06["RE_NLCD06pct31"] <- powNLCD06$NLCD06_31/powNLCD06$NLCD06_ARE
powNLCD06["RE_NLCD06pct41"] <- powNLCD06$NLCD06_41/powNLCD06$NLCD06_ARE
powNLCD06["RE_NLCD06pct42"] <- powNLCD06$NLCD06_42/powNLCD06$NLCD06_ARE
powNLCD06["RE_NLCD06pct43"] <- powNLCD06$NLCD06_43/powNLCD06$NLCD06_ARE
powNLCD06["RE_NLCD06pct52"] <- powNLCD06$NLCD06_52/powNLCD06$NLCD06_ARE
powNLCD06["RE_NLCD06pct71"] <- powNLCD06$NLCD06_71/powNLCD06$NLCD06_ARE
powNLCD06["RE_NLCD06pct81"] <- powNLCD06$NLCD06_81/powNLCD06$NLCD06_ARE
powNLCD06["RE_NLCD06pct82"] <- powNLCD06$NLCD06_82/powNLCD06$NLCD06_ARE
powNLCD06["RE_NLCD06pct90"] <- powNLCD06$NLCD06_90/powNLCD06$NLCD06_ARE
powNLCD06["RE_NLCD06pct95"] <- powNLCD06$NLCD06_95/powNLCD06$NLCD06_ARE
powNLCD06 <- rename(powNLCD06,c("NLCD06_ARE" = "REACH_AREA"))
powNLCD06 <- powNLCD06[c("site_no","RE_NLCD06pct11","RE_NLCD06pct12","RE_NLCD06pct21","RE_NLCD06pct22",
            "RE_NLCD06pct23","RE_NLCD06pct24","RE_NLCD06pct31","RE_NLCD06pct41","RE_NLCD06pct42","RE_NLCD06pct43",
            "RE_NLCD06pct52","RE_NLCD06pct71","RE_NLCD06pct81","RE_NLCD06pct82","RE_NLCD06pct90","RE_NLCD06pct95")]
rm(NLCD06)
file.remove(paste0(landcoverDir,"NLCD06.dbf"))
summary(powNLCD06)
powLandcover <- merge(x=powAC_NLCD01, y=powNLCD06,by="site_no")

#NLCD 2011 Reach-scale land Use;  keep reach areas for subsequent % land coverage calculations
NLCD11_zip <- "NLCD11.zip"
unzip(paste0(landcoverDir,NLCD11_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Landcover")
NLCD11 <- read.dbf(paste0(landcoverDir,"NLCD11.dbf"))
powNLCD11 <- merge(x=comid640,y=NLCD11,by="COMID",all.x=TRUE)
powNLCD11["RE_NLCD11pct11"] <- powNLCD11$NLCD11_11/powNLCD11$NLCD11_ARE
powNLCD11["RE_NLCD11pct12"] <- powNLCD11$NLCD11_12/powNLCD11$NLCD11_ARE
powNLCD11["RE_NLCD11pct21"] <- powNLCD11$NLCD11_21/powNLCD11$NLCD11_ARE
powNLCD11["RE_NLCD11pct22"] <- powNLCD11$NLCD11_22/powNLCD11$NLCD11_ARE
powNLCD11["RE_NLCD11pct23"] <- powNLCD11$NLCD11_23/powNLCD11$NLCD11_ARE
powNLCD11["RE_NLCD11pct24"] <- powNLCD11$NLCD11_24/powNLCD11$NLCD11_ARE
powNLCD11["RE_NLCD11pct31"] <- powNLCD11$NLCD11_31/powNLCD11$NLCD11_ARE
powNLCD11["RE_NLCD11pct41"] <- powNLCD11$NLCD11_41/powNLCD11$NLCD11_ARE
powNLCD11["RE_NLCD11pct42"] <- powNLCD11$NLCD11_42/powNLCD11$NLCD11_ARE
powNLCD11["RE_NLCD11pct43"] <- powNLCD11$NLCD11_43/powNLCD11$NLCD11_ARE
powNLCD11["RE_NLCD11pct52"] <- powNLCD11$NLCD11_52/powNLCD11$NLCD11_ARE
powNLCD11["RE_NLCD11pct71"] <- powNLCD11$NLCD11_71/powNLCD11$NLCD11_ARE
powNLCD11["RE_NLCD11pct81"] <- powNLCD11$NLCD11_81/powNLCD11$NLCD11_ARE
powNLCD11["RE_NLCD11pct82"] <- powNLCD11$NLCD11_82/powNLCD11$NLCD11_ARE
powNLCD11["RE_NLCD11pct90"] <- powNLCD11$NLCD11_90/powNLCD11$NLCD11_ARE
powNLCD11["RE_NLCD11pct95"] <- powNLCD11$NLCD11_95/powNLCD11$NLCD11_ARE
powNLCD11 <- rename(powNLCD11,c("NLCD11_ARE" = "REACH_AREA"))
powNLCD11 <- powNLCD11[c("site_no","RE_NLCD11pct11","RE_NLCD11pct12","RE_NLCD11pct21","RE_NLCD11pct22",
            "RE_NLCD11pct23","RE_NLCD11pct24","RE_NLCD11pct31","RE_NLCD11pct41","RE_NLCD11pct42","RE_NLCD11pct43",
            "RE_NLCD11pct52","RE_NLCD11pct71","RE_NLCD11pct81","RE_NLCD11pct82","RE_NLCD11pct90","RE_NLCD11pct95")]
rm(NLCD11)
file.remove(paste0(landcoverDir,"NLCD11.dbf"))
summary(powNLCD11)
powLandcover <- merge(x=powLandcover, y=powAC_NLCD11,by="site_no")

#Accumulated NLCD 2001 Land Use;  keep accumulated areas for subsequent % land coverage calculations
#NLCD 2001 Land Use;  keep reach areas for subsequent % land coverage calculations
AC_NLCD01_zip <- "AC_NLCD01.zip"
unzip(paste0(landcoverDir,AC_NLCD01_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Landcover")
AC_NLCD01 <- read.dbf(paste0(landcoverDir,"AC_NLCD01.dbf"))
powAC_NLCD01 <- merge(x=comid640,y=AC_NLCD01,by="COMID",all.x=TRUE)
powAC_NLCD01["AC_NLCD01pct11"] <- powAC_NLCD01$ACCUM_11/powAC_NLCD01$ACCUM_AREA
powAC_NLCD01["AC_NLCD01pct12"] <- powAC_NLCD01$ACCUM_12/powAC_NLCD01$ACCUM_AREA
powAC_NLCD01["AC_NLCD01pct21"] <- powAC_NLCD01$ACCUM_21/powAC_NLCD01$ACCUM_AREA
powAC_NLCD01["AC_NLCD01pct22"] <- powAC_NLCD01$ACCUM_22/powAC_NLCD01$ACCUM_AREA
powAC_NLCD01["AC_NLCD01pct23"] <- powAC_NLCD01$ACCUM_23/powAC_NLCD01$ACCUM_AREA
powAC_NLCD01["AC_NLCD01pct24"] <- powAC_NLCD01$ACCUM_24/powAC_NLCD01$ACCUM_AREA
powAC_NLCD01["AC_NLCD01pct31"] <- powAC_NLCD01$ACCUM_31/powAC_NLCD01$ACCUM_AREA
powAC_NLCD01["AC_NLCD01pct41"] <- powAC_NLCD01$ACCUM_41/powAC_NLCD01$ACCUM_AREA
powAC_NLCD01["AC_NLCD01pct42"] <- powAC_NLCD01$ACCUM_42/powAC_NLCD01$ACCUM_AREA
powAC_NLCD01["AC_NLCD01pct43"] <- powAC_NLCD01$ACCUM_43/powAC_NLCD01$ACCUM_AREA
powAC_NLCD01["AC_NLCD01pct52"] <- powAC_NLCD01$ACCUM_52/powAC_NLCD01$ACCUM_AREA
powAC_NLCD01["AC_NLCD01pct71"] <- powAC_NLCD01$ACCUM_71/powAC_NLCD01$ACCUM_AREA
powAC_NLCD01["AC_NLCD01pct81"] <- powAC_NLCD01$ACCUM_81/powAC_NLCD01$ACCUM_AREA
powAC_NLCD01["AC_NLCD01pct82"] <- powAC_NLCD01$ACCUM_82/powAC_NLCD01$ACCUM_AREA
powAC_NLCD01["AC_NLCD01pct90"] <- powAC_NLCD01$ACCUM_90/powAC_NLCD01$ACCUM_AREA
powAC_NLCD01["AC_NLCD01pct95"] <- powAC_NLCD01$ACCUM_95/powAC_NLCD01$ACCUM_AREA
powAC_NLCD01 <- powAC_NLCD01[c("site_no","AC_NLCD01pct11","AC_NLCD01pct12","AC_NLCD01pct21","AC_NLCD01pct22",
               "AC_NLCD01pct23","AC_NLCD01pct24","AC_NLCD01pct31","AC_NLCD01pct41","AC_NLCD01pct42","AC_NLCD01pct43",
               "AC_NLCD01pct52","AC_NLCD01pct71","AC_NLCD01pct81","AC_NLCD01pct82","AC_NLCD01pct90","AC_NLCD01pct95","ACCUM_AREA")]
rm(AC_NLCD01)
file.remove(paste0(landcoverDir,"AC_NLCD01.dbf"))
summary(powAC_NLCD01)
powLandcover <- merge(x=powNLCD01, y=powAC_NLCD01,by="site_no")

#Accumulated NLCD 2006 Land Use  
AC_NLCD06_zip <- "AC_NLCD06.zip"
unzip(paste0(landcoverDir,AC_NLCD06_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Landcover")
AC_NLCD06 <- read.dbf(paste0(landcoverDir,"AC_NLCD06.dbf"))
powAC_NLCD06 <- merge(x=comid640,y=AC_NLCD06,by="COMID",all.x=TRUE)
powAC_NLCD06["AC_NLCD06pct11"] <- powAC_NLCD06$ACCUM_11/powAC_NLCD06$ACCUM_AREA
powAC_NLCD06["AC_NLCD06pct12"] <- powAC_NLCD06$ACCUM_12/powAC_NLCD06$ACCUM_AREA
powAC_NLCD06["AC_NLCD06pct21"] <- powAC_NLCD06$ACCUM_21/powAC_NLCD06$ACCUM_AREA
powAC_NLCD06["AC_NLCD06pct22"] <- powAC_NLCD06$ACCUM_22/powAC_NLCD06$ACCUM_AREA
powAC_NLCD06["AC_NLCD06pct23"] <- powAC_NLCD06$ACCUM_23/powAC_NLCD06$ACCUM_AREA
powAC_NLCD06["AC_NLCD06pct24"] <- powAC_NLCD06$ACCUM_24/powAC_NLCD06$ACCUM_AREA
powAC_NLCD06["AC_NLCD06pct31"] <- powAC_NLCD06$ACCUM_31/powAC_NLCD06$ACCUM_AREA
powAC_NLCD06["AC_NLCD06pct41"] <- powAC_NLCD06$ACCUM_41/powAC_NLCD06$ACCUM_AREA
powAC_NLCD06["AC_NLCD06pct42"] <- powAC_NLCD06$ACCUM_42/powAC_NLCD06$ACCUM_AREA
powAC_NLCD06["AC_NLCD06pct43"] <- powAC_NLCD06$ACCUM_43/powAC_NLCD06$ACCUM_AREA
powAC_NLCD06["AC_NLCD06pct52"] <- powAC_NLCD06$ACCUM_52/powAC_NLCD06$ACCUM_AREA
powAC_NLCD06["AC_NLCD06pct71"] <- powAC_NLCD06$ACCUM_71/powAC_NLCD06$ACCUM_AREA
powAC_NLCD06["AC_NLCD06pct81"] <- powAC_NLCD06$ACCUM_81/powAC_NLCD06$ACCUM_AREA
powAC_NLCD06["AC_NLCD06pct82"] <- powAC_NLCD06$ACCUM_82/powAC_NLCD06$ACCUM_AREA
powAC_NLCD06["AC_NLCD06pct90"] <- powAC_NLCD06$ACCUM_90/powAC_NLCD06$ACCUM_AREA
powAC_NLCD06["AC_NLCD06pct95"] <- powAC_NLCD06$ACCUM_95/powAC_NLCD06$ACCUM_AREA
powAC_NLCD06 <- powAC_NLCD06[c("site_no","AC_NLCD06pct11","AC_NLCD06pct12","AC_NLCD06pct21","AC_NLCD06pct22",
               "AC_NLCD06pct23","AC_NLCD06pct24","AC_NLCD06pct31","AC_NLCD06pct41","AC_NLCD06pct42","AC_NLCD06pct43",
               "AC_NLCD06pct52","AC_NLCD06pct71","AC_NLCD06pct81","AC_NLCD06pct82","AC_NLCD06pct90","AC_NLCD06pct95")]
rm(AC_NLCD06)
file.remove(paste0(landcoverDir,"AC_NLCD06.dbf"))
summary(powAC_NLCD06)
powLandcover <- merge(x=powLandcover, y=powAC_NLCD06,by="site_no")

#Accumulated NLCD 2011 Land Use  
AC_NLCD11_zip <- "AC_NLCD11.zip"
unzip(paste0(landcoverDir,AC_NLCD11_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Landcover")
AC_NLCD11 <- read.dbf(paste0(landcoverDir,"AC_NLCD11.dbf"))
powAC_NLCD11 <- merge(x=comid640,y=AC_NLCD11,by="COMID",all.x=TRUE)
powAC_NLCD11["AC_NLCD11pct11"] <- powAC_NLCD11$AC11_11/powAC_NLCD11$ACLU_AREA
powAC_NLCD11["AC_NLCD11pct12"] <- powAC_NLCD11$AC11_12/powAC_NLCD11$ACLU_AREA
powAC_NLCD11["AC_NLCD11pct21"] <- powAC_NLCD11$AC11_21/powAC_NLCD11$ACLU_AREA
powAC_NLCD11["AC_NLCD11pct22"] <- powAC_NLCD11$AC11_22/powAC_NLCD11$ACLU_AREA
powAC_NLCD11["AC_NLCD11pct23"] <- powAC_NLCD11$AC11_23/powAC_NLCD11$ACLU_AREA
powAC_NLCD11["AC_NLCD11pct24"] <- powAC_NLCD11$AC11_24/powAC_NLCD11$ACLU_AREA
powAC_NLCD11["AC_NLCD11pct31"] <- powAC_NLCD11$AC11_31/powAC_NLCD11$ACLU_AREA
powAC_NLCD11["AC_NLCD11pct41"] <- powAC_NLCD11$ACLU11_41/powAC_NLCD11$ACLU_AREA
powAC_NLCD11["AC_NLCD11pct42"] <- powAC_NLCD11$ACLU11_42/powAC_NLCD11$ACLU_AREA
powAC_NLCD11["AC_NLCD11pct43"] <- powAC_NLCD11$ACLU11_43/powAC_NLCD11$ACLU_AREA
powAC_NLCD11["AC_NLCD11pct52"] <- powAC_NLCD11$ACLU11_52/powAC_NLCD11$ACLU_AREA
powAC_NLCD11["AC_NLCD11pct71"] <- powAC_NLCD11$ACLU11_71/powAC_NLCD11$ACLU_AREA
powAC_NLCD11["AC_NLCD11pct81"] <- powAC_NLCD11$ACLU11_81/powAC_NLCD11$ACLU_AREA
powAC_NLCD11["AC_NLCD11pct82"] <- powAC_NLCD11$ACLU11_82/powAC_NLCD11$ACLU_AREA
powAC_NLCD11["AC_NLCD11pct90"] <- powAC_NLCD11$ACLU11_90/powAC_NLCD11$ACLU_AREA
powAC_NLCD11["AC_NLCD11pct95"] <- powAC_NLCD11$ACLU11_95/powAC_NLCD11$ACLU_AREA
powAC_NLCD11 <- powAC_NLCD11[c("site_no","AC_NLCD11pct11","AC_NLCD11pct12","AC_NLCD11pct21","AC_NLCD11pct22",
               "AC_NLCD11pct23","AC_NLCD11pct24","AC_NLCD11pct31","AC_NLCD11pct41","AC_NLCD11pct42","AC_NLCD11pct43",
               "AC_NLCD11pct52","AC_NLCD11pct71","AC_NLCD11pct81","AC_NLCD11pct82","AC_NLCD11pct90","AC_NLCD11pct95")]
rm(AC_NLCD11)
file.remove(paste0(landcoverDir,"AC_NLCD11.dbf"))
summary(powAC_NLCD11)
powLandcover <- merge(x=powLandcover, y=powAC_NLCD11,by="site_no")

#NLCD 2011 Land use in the 100m riparian buffer
NLCD11Buff_zip <- "NLCD11_Buf100.zip"
unzip(paste0(landcoverDir,NLCD11Buff_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Landcover")
NLCD11Buff <- read.dbf(paste0(landcoverDir,"NLCD11_Buf100.dbf"))
powNLCD11Buff <- merge(x=comid640,y=NLCD11Buff,by="COMID",all.x=TRUE)
powNLCD11Buff["RE_NLCD11Buffpct11"] <- powNLCD11Buff$NLCD11_11/powNLCD11Buff$NLCD11_ARE
powNLCD11Buff["RE_NLCD11Buffpct12"] <- powNLCD11Buff$NLCD11_12/powNLCD11Buff$NLCD11_ARE
powNLCD11Buff["RE_NLCD11Buffpct21"] <- powNLCD11Buff$NLCD11_21/powNLCD11Buff$NLCD11_ARE
powNLCD11Buff["RE_NLCD11Buffpct22"] <- powNLCD11Buff$NLCD11_22/powNLCD11Buff$NLCD11_ARE
powNLCD11Buff["RE_NLCD11Buffpct23"] <- powNLCD11Buff$NLCD11_23/powNLCD11Buff$NLCD11_ARE
powNLCD11Buff["RE_NLCD11Buffpct24"] <- powNLCD11Buff$NLCD11_24/powNLCD11Buff$NLCD11_ARE
powNLCD11Buff["RE_NLCD11Buffpct31"] <- powNLCD11Buff$NLCD11_31/powNLCD11Buff$NLCD11_ARE
powNLCD11Buff["RE_NLCD11Buffpct41"] <- powNLCD11Buff$NLCD11_41/powNLCD11Buff$NLCD11_ARE
powNLCD11Buff["RE_NLCD11Buffpct42"] <- powNLCD11Buff$NLCD11_42/powNLCD11Buff$NLCD11_ARE
powNLCD11Buff["RE_NLCD11Buffpct43"] <- powNLCD11Buff$NLCD11_43/powNLCD11Buff$NLCD11_ARE
powNLCD11Buff["RE_NLCD11Buffpct52"] <- powNLCD11Buff$NLCD11_52/powNLCD11Buff$NLCD11_ARE
powNLCD11Buff["RE_NLCD11Buffpct71"] <- powNLCD11Buff$NLCD11_71/powNLCD11Buff$NLCD11_ARE
powNLCD11Buff["RE_NLCD11Buffpct81"] <- powNLCD11Buff$NLCD11_81/powNLCD11Buff$NLCD11_ARE
powNLCD11Buff["RE_NLCD11Buffpct82"] <- powNLCD11Buff$NLCD11_82/powNLCD11Buff$NLCD11_ARE
powNLCD11Buff["RE_NLCD11Buffpct90"] <- powNLCD11Buff$NLCD11_90/powNLCD11Buff$NLCD11_ARE
powNLCD11Buff["RE_NLCD11Buffpct95"] <- powNLCD11Buff$NLCD11_95/powNLCD11Buff$NLCD11_ARE
powNLCD11Buff <- powNLCD11Buff[c("site_no","RE_NLCD11Buffpct11","RE_NLCD11Buffpct12","RE_NLCD11Buffpct21","RE_NLCD11Buffpct22",
                 "RE_NLCD11Buffpct23","RE_NLCD11Buffpct24","RE_NLCD11Buffpct31","RE_NLCD11Buffpct41","RE_NLCD11Buffpct42","RE_NLCD11Buffpct43",
                 "RE_NLCD11Buffpct52","RE_NLCD11Buffpct71","RE_NLCD11Buffpct81","RE_NLCD11Buffpct82","RE_NLCD11Buffpct90","RE_NLCD11Buffpct95")]
rm(NLCD11Buff)
file.remove(paste0(landcoverDir,"NLCD11_Buf100.dbf"))
summary(powNLCD11Buff)
powLandcover <- merge(x=powLandcover, y=powNLCD11Buff, by="site_no")

#Accumulated NLCD 2011 Land use in the 100m riparian buffer
ACNLCD11Buff_zip <- "AC_NLCD11_Buf100.zip"
unzip(paste0(landcoverDir,ACNLCD11Buff_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Landcover")
ACNLCD11Buff <- read.dbf(paste0(landcoverDir,"AC_NLCD11_Buf100.dbf"))
powACNLCD11Buff <- merge(x=comid640,y=ACNLCD11Buff,by="COMID",all.x=TRUE)
summary(powACNLCD11Buff)
powACNLCD11Buff["AC_NLCD11Buffpct11"] <- powACNLCD11Buff$ACLU11_11/powACNLCD11Buff$ACLU_AREA
powACNLCD11Buff["AC_NLCD11Buffpct12"] <- powACNLCD11Buff$ACLU11_12/powACNLCD11Buff$ACLU_AREA
powACNLCD11Buff["AC_NLCD11Buffpct21"] <- powACNLCD11Buff$ACLU11_21/powACNLCD11Buff$ACLU_AREA
powACNLCD11Buff["AC_NLCD11Buffpct22"] <- powACNLCD11Buff$ACLU11_22/powACNLCD11Buff$ACLU_AREA
powACNLCD11Buff["AC_NLCD11Buffpct23"] <- powACNLCD11Buff$ACLU11_23/powACNLCD11Buff$ACLU_AREA
powACNLCD11Buff["AC_NLCD11Buffpct24"] <- powACNLCD11Buff$ACLU11_24/powACNLCD11Buff$ACLU_AREA
powACNLCD11Buff["AC_NLCD11Buffpct31"] <- powACNLCD11Buff$ACLU11_31/powACNLCD11Buff$ACLU_AREA
powACNLCD11Buff["AC_NLCD11Buffpct41"] <- powACNLCD11Buff$ACLU11_41/powACNLCD11Buff$ACLU_AREA
powACNLCD11Buff["AC_NLCD11Buffpct42"] <- powACNLCD11Buff$ACLU11_42/powACNLCD11Buff$ACLU_AREA
powACNLCD11Buff["AC_NLCD11Buffpct43"] <- powACNLCD11Buff$ACLU11_43/powACNLCD11Buff$ACLU_AREA
powACNLCD11Buff["AC_NLCD11Buffpct52"] <- powACNLCD11Buff$ACLU11_52/powACNLCD11Buff$ACLU_AREA
powACNLCD11Buff["AC_NLCD11Buffpct71"] <- powACNLCD11Buff$ACLU11_71/powACNLCD11Buff$ACLU_AREA
powACNLCD11Buff["AC_NLCD11Buffpct81"] <- powACNLCD11Buff$ACLU11_81/powACNLCD11Buff$ACLU_AREA
powACNLCD11Buff["AC_NLCD11Buffpct82"] <- powACNLCD11Buff$ACLU11_82/powACNLCD11Buff$ACLU_AREA
powACNLCD11Buff["AC_NLCD11Buffpct90"] <- powACNLCD11Buff$ACLU11_90/powACNLCD11Buff$ACLU_AREA
powACNLCD11Buff["AC_NLCD11Buffpct95"] <- powACNLCD11Buff$ACLU11_95/powACNLCD11Buff$ACLU_AREA
powACNLCD11Buff <- powACNLCD11Buff[c("site_no","AC_NLCD11Buffpct11","AC_NLCD11Buffpct12","AC_NLCD11Buffpct21","AC_NLCD11Buffpct22",
                  "AC_NLCD11Buffpct23","AC_NLCD11Buffpct24","AC_NLCD11Buffpct31","AC_NLCD11Buffpct41","AC_NLCD11Buffpct42","AC_NLCD11Buffpct43",
                  "AC_NLCD11Buffpct52","AC_NLCD11Buffpct71","AC_NLCD11Buffpct81","AC_NLCD11Buffpct82","AC_NLCD11Buffpct90","AC_NLCD11Buffpct95")]
rm(ACNLCD11Buff)
file.remove(paste0(landcoverDir,"AC_NLCD11_Buf100.dbf"))
summary(powACNLCD11Buff)
powLandcover <- merge(x=powLandcover, y=powACNLCD11Buff, by="site_no")

#Create file with relevant areas for later merging.
#This file will be used to merge all NHDplus with powstreams for the rest of the program.
powCOMID_AREA <- powLandcover[c("site_no","COMID","ACCUM_AREA","REACH_AREA")]

#Corn from crop data layer 2011
AC_CDL11_corn_zip <- "AC_CDL11_corn.zip"
unzip(paste0(landcoverDir,AC_CDL11_corn_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Landcover")
AC_CDL11_corn <- read.dbf(paste0(landcoverDir,"AC_CDL11_Corn.dbf"))
powCDL11corn <- merge(x=powCOMID_AREA,y=AC_CDL11_corn,by="COMID",all.x=TRUE)
powCDL11corn["RE_CDL11pct1"] <- powCDL11corn$CDL11_1 / powCDL11corn$REACH_AREA
powCDL11corn["AC_CDL11pct1"] <- powCDL11corn$AC_11_1 / powCDL11corn$ACCUM_AREA
summary(powCDL11corn)
powCDL11corn <- powCDL11corn[c("site_no","RE_CDL11pct1","AC_CDL11pct1")]
rm(AC_CDL11_corn)
file.remove(paste0(landcoverDir,"AC_CDL11_Corn.dbf"))
summary(powCDL11corn)
powLandcover <- merge(x=powLandcover, y=powCDL11corn,by="site_no")

#Corn from crop data layer 2012
AC_CDL12_corn_zip <- "AC_CDL12_corn.zip"
unzip(paste0(landcoverDir,AC_CDL12_corn_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Landcover")
AC_CDL12_corn <- read.dbf(paste0(landcoverDir,"AC_CDL12_Corn.dbf"))
powCDL12corn <- merge(x=powCOMID_AREA,y=AC_CDL12_corn,by="COMID",all.x=TRUE)
summary(powCDL12corn)
powCDL12corn["RE_CDL12pct1"] <- powCDL12corn$CDL12_1 / powCDL12corn$REACH_AREA
powCDL12corn["AC_CDL12pct1"] <- powCDL12corn$AC_11_1 / powCDL12corn$ACCUM_AREA
powCDL12corn <- powCDL12corn[c("site_no","RE_CDL12pct1","AC_CDL12pct1")]
rm(AC_CDL12_corn)
file.remove(paste0(landcoverDir,"AC_CDL12_Corn.dbf"))
summary(powCDL12corn)
powLandcover <- merge(x=powLandcover, y=powCDL12corn,by="site_no")

#Corn from crop data layer 2013
AC_CDL13_corn_zip <- "AC_CDL13_corn.zip"
unzip(paste0(landcoverDir,AC_CDL13_corn_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Landcover")
AC_CDL13_corn <- read.dbf(paste0(landcoverDir,"AC_CDL13_Corn.dbf"))
powCDL13corn <- merge(x=powCOMID_AREA,y=AC_CDL13_corn,by="COMID",all.x=TRUE)
powCDL13corn["RE_CDL13pct1"] <- powCDL13corn$CDL13_1 / powCDL13corn$REACH_AREA
powCDL13corn["AC_CDL13pct1"] <- powCDL13corn$AC_11_1 / powCDL13corn$ACCUM_AREA
powCDL13corn <- powCDL13corn[c("site_no","RE_CDL13pct1","AC_CDL13pct1")]
rm(AC_CDL13_corn)
file.remove(paste0(landcoverDir,"AC_CDL13_Corn.dbf"))
summary(powCDL13corn)
powLandcover <- merge(x=powLandcover, y=powCDL13corn,by="site_no")

#Canopy cover 2011
AC_CNPY11_zip <- "AC_CNPY11.zip"
unzip(paste0(landcoverDir,AC_CNPY11_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Landcover")
AC_CNPY11 <- read.dbf(paste0(landcoverDir,"AC_CNPY11.dbf"))
powCNPY11 <- merge(x=powCOMID_AREA,y=AC_CNPY11,by="COMID",all.x=TRUE)
summary(powCNPY11)
powCNPY11 <- rename(powCNPY11,c("MEAN" = "RE_CNPY11"))
powCNPY11 <- powCNPY11[c("site_no","RE_CNPY11","AC_CNPY11")]
rm(AC_CNPY11)
file.remove(paste0(landcoverDir,"AC_CNPY11.dbf"))
summary(powCNPY11)
powLandcover <- merge(x=powLandcover, y=powCNPY11, by="site_no")

#Land subjected to ditches, NRI 1992.
AC_Ditches92_zip <- "AC_Ditches92.zip"
unzip(paste0(landcoverDir,AC_Ditches92_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Landcover")
AC_Ditches92 <- read.dbf(paste0(landcoverDir,"AC_Ditches92.dbf"))
powDitches92 <- merge(x=powCOMID_AREA,y=AC_Ditches92,by="COMID",all.x=TRUE)
powDitches92["RE_Ditch_pct"] <- powDitches92$SUM / powDitches92$REACH_AREA
powDitches92["AC_Ditch_pct"] <- powDitches92$AC_DITCHES/ (powDitches92$ACCUM_AREA/1000000)
powDitches92 <- powDitches92[c("site_no","RE_Ditch_pct","AC_Ditch_pct")]
rm(AC_Ditches92)
file.remove(paste0(landcoverDir,"AC_Ditches92.dbf"))
summary(powDitches92)
powLandcover <- merge(x=powLandcover, y=powDitches92, by="site_no")

#Land subjected to tile drainage, NRI 1992.
AC_Drain92_zip <- "AC_Drain92.zip"
unzip(paste0(landcoverDir,AC_Drain92_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Landcover")
AC_Drain92 <- read.dbf(paste0(landcoverDir,"AC_Drain92.dbf"))
powDrain92 <- merge(x=powCOMID_AREA,y=AC_Drain92,by="COMID",all.x=TRUE)
summary(powDrain92)
powDrain92["RE_Drain_pct"] <- powDrain92$SUM / powDrain92$REACH_AREA
powDrain92["AC_Drain_pct"] <- powDrain92$AC_DRAIN / (powDrain92$ACCUM_AREA/1000000)
powDrain92 <- powDrain92[c("site_no","RE_Drain_pct","AC_Drain_pct")]
rm(AC_Drain92)
file.remove(paste0(landcoverDir,"AC_Drain92.dbf"))
summary(powDrain92)
powLandcover <- merge(x=powLandcover, y=powDrain92, by="site_no")

#Accumulated network hydrologic landscape regions.
AC_HLR_zip <- "AC_HLR.zip"
unzip(paste0(landcoverDir,AC_HLR_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Landcover")
AC_HLR <- read.dbf(paste0(landcoverDir,"AC_HLR.dbf"))
powAC_HLR <- merge(x=powCOMID_AREA,y=AC_HLR,by="COMID",all.x=TRUE)
powAC_HLR["AC_HLRpct1"] <- powAC_HLR$AC_HLR1/powAC_HLR$ACCUM_AREA
powAC_HLR["AC_HLRpct2"] <- powAC_HLR$AC_HLR2/powAC_HLR$ACCUM_AREA
powAC_HLR["AC_HLRpct3"] <- powAC_HLR$AC_HLR3/powAC_HLR$ACCUM_AREA
powAC_HLR["AC_HLRpct4"] <- powAC_HLR$AC_HLR4/powAC_HLR$ACCUM_AREA
powAC_HLR["AC_HLRpct5"] <- powAC_HLR$AC_HLR5/powAC_HLR$ACCUM_AREA
powAC_HLR["AC_HLRpct6"] <- powAC_HLR$AC_HLR6/powAC_HLR$ACCUM_AREA
powAC_HLR["AC_HLRpct7"] <- powAC_HLR$AC_HLR7/powAC_HLR$ACCUM_AREA
powAC_HLR["AC_HLRpct8"] <- powAC_HLR$AC_HLR8/powAC_HLR$ACCUM_AREA
powAC_HLR["AC_HLRpct9"] <- powAC_HLR$AC_HLR9/powAC_HLR$ACCUM_AREA
powAC_HLR["AC_HLRpct10"] <- powAC_HLR$AC_HLR10/powAC_HLR$ACCUM_AREA
powAC_HLR["AC_HLRpct11"] <- powAC_HLR$AC_HLR11/powAC_HLR$ACCUM_AREA
powAC_HLR["AC_HLRpct12"] <- powAC_HLR$AC_HLR12/powAC_HLR$ACCUM_AREA
powAC_HLR["AC_HLRpct13"] <- powAC_HLR$AC_HLR13/powAC_HLR$ACCUM_AREA
powAC_HLR["AC_HLRpct14"] <- powAC_HLR$AC_HLR14/powAC_HLR$ACCUM_AREA
powAC_HLR["AC_HLRpct15"] <- powAC_HLR$AC_HLR15/powAC_HLR$ACCUM_AREA
powAC_HLR["AC_HLRpct16"] <- powAC_HLR$AC_HLR16/powAC_HLR$ACCUM_AREA
powAC_HLR["AC_HLRpct17"] <- powAC_HLR$AC_HLR17/powAC_HLR$ACCUM_AREA
powAC_HLR["AC_HLRpct18"] <- powAC_HLR$AC_HLR18/powAC_HLR$ACCUM_AREA
powAC_HLR["AC_HLRpct19"] <- powAC_HLR$AC_HLR19/powAC_HLR$ACCUM_AREA
powAC_HLR["AC_HLRpct20"] <- powAC_HLR$AC_HLR20/powAC_HLR$ACCUM_AREA
powAC_HLR <- powAC_HLR[c("site_no","AC_HLRpct1","AC_HLRpct2","AC_HLRpct3","AC_HLRpct4",
            "AC_HLRpct5","AC_HLRpct6","AC_HLRpct7","AC_HLRpct8","AC_HLRpct9","AC_HLRpct10",
            "AC_HLRpct11","AC_HLRpct12","AC_HLRpct13","AC_HLRpct14","AC_HLRpct15","AC_HLRpct16",
            "AC_HLRpct17","AC_HLRpct18","AC_HLRpct19","AC_HLRpct20")]
rm(AC_HLR)
file.remove(paste0(landcoverDir,"AC_HLR.dbf"))
summary(powAC_HLR)
powLandcover <- merge(x=powLandcover, y=powAC_HLR, by="site_no")

#Watershed percent impervious cover 2001
AC_Impv01_zip <- "AC_Impv01.zip"
unzip(paste0(landcoverDir,AC_Impv01_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Landcover")
AC_Impv01 <- read.dbf(paste0(landcoverDir,"AC_Impv01.dbf"))
powAC_Impv01 <- merge(x=powCOMID_AREA,y=AC_Impv01,by="COMID",all.x=TRUE)
powAC_Impv01 <- rename(powAC_Impv01,c("MEAN" = "RE_IMPV01"))
powAC_Impv01 <- powAC_Impv01[c("site_no","RE_IMPV01","AC_IMPV01")]
rm(AC_Impv01)
file.remove(paste0(landcoverDir,"AC_Impv01.dbf"))
summary(powAC_Impv01)
powLandcover <- merge(x=powLandcover, y=powAC_Impv01, by="site_no")

#Watershed percent impervious cover 2006
AC_Impv06_zip <- "AC_Impv06.zip"
unzip(paste0(landcoverDir,AC_Impv06_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Landcover")
AC_Impv06 <- read.dbf(paste0(landcoverDir,"AC_Impv06.dbf"))
powAC_Impv06 <- merge(x=powCOMID_AREA,y=AC_Impv06,by="COMID",all.x=TRUE)
powAC_Impv06 <- rename(powAC_Impv06,c("MEAN" = "RE_IMPV06"))
powAC_Impv06 <- powAC_Impv06[c("site_no","RE_IMPV06","AC_IMPV06")]
rm(AC_Impv06)
file.remove(paste0(landcoverDir,"AC_Impv06.dbf"))
summary(powAC_Impv06)
powLandcover <- merge(x=powLandcover, y=powAC_Impv06, by="site_no")

#Watershed percent impervious cover 2011
AC_Impv11_zip <- "AC_Impv11.zip"
unzip(paste0(landcoverDir,AC_Impv11_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Landcover")
AC_Impv11 <- read.dbf(paste0(landcoverDir,"AC_Impv11.dbf"))
powAC_Impv11 <- merge(x=powCOMID_AREA,y=AC_Impv11,by="COMID",all.x=TRUE)
powAC_Impv11 <- rename(powAC_Impv11,c("MEAN" = "RE_IMPV11"))
powAC_Impv11 <- powAC_Impv11[c("site_no","RE_IMPV11","AC_IMPV11")]
rm(AC_Impv11)
file.remove(paste0(landcoverDir,"AC_Impv11.dbf"))
summary(powAC_Impv11)
powLandcover <- merge(x=powLandcover, y=powAC_Impv11, by="site_no")

#Riparian buffer percent impervious cover, 2006
AC_Impv06Buff_zip <- "AC_Impv06_Buff100.zip"
unzip(paste0(landcoverDir,AC_Impv06Buff_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Landcover")
AC_Impv06Buff <- read.dbf(paste0(landcoverDir,"AC_Impv06_Buff100.dbf"))
powImpv06Buff <- merge(x=powCOMID_AREA,y=AC_Impv06Buff,by="COMID",all.x=TRUE)
powImpv06Buff <- rename(powImpv06Buff,c("MEAN" = "RE_Impv06Buff","IMPV06_AC" = "AC_Impv06Buff"))
powImpv06Buff <- powImpv06Buff[c("site_no","RE_Impv06Buff","AC_Impv06Buff")]
rm(AC_Impv06Buff)
file.remove(paste0(landcoverDir,"AC_Impv06_Buff100.dbf"))
summary(powImpv06Buff)
powLandcover <- merge(x=powLandcover, y=powImpv06Buff, by="site_no")

#Riparian buffer percent impervious cover, 2011
AC_Impv11Buff_zip <- "AC_IMPV11_Buf100.zip"
unzip(paste0(landcoverDir,AC_Impv11Buff_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Landcover")
AC_Impv11Buff <- read.dbf(paste0(landcoverDir,"AC_IMPV11_Buf100.dbf"))
powImpv11Buff <- merge(x=powCOMID_AREA,y=AC_Impv11Buff,by="COMID",all.x=TRUE)
powImpv11Buff <- rename(powImpv11Buff,c("MEAN" = "RE_Impv11Buff","AC_IMPV11" = "AC_Impv11Buff"))
powImpv11Buff <- powImpv11Buff[c("site_no","RE_Impv11Buff","AC_Impv11Buff")]
rm(AC_Impv11Buff)
file.remove(paste0(landcoverDir,"AC_IMPV11_Buf100.dbf"))
summary(powImpv11Buff)
powLandcover <- merge(x=powLandcover, y=powImpv11Buff, by="site_no")

#Percentage of watershed subjected to irrigation, MODIS 2002.
AC_Irrig02_zip <- "AC_IRRIG02.zip"
unzip(paste0(landcoverDir,AC_Irrig02_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Landcover")
AC_Irrig02 <- read.dbf(paste0(landcoverDir,"AC_IRRIG02.dbf"))
powIrrig02 <- merge(x=powCOMID_AREA,y=AC_Irrig02,by="COMID",all.x=TRUE)
powIrrig02["RE_Irrig02_pct"] <- powIrrig02$SUM / powIrrig02$REACH_AREA
powIrrig02["AC_Irrig02_pct"] <- powIrrig02$AC_IRRIG02 / (powIrrig02$ACCUM_AREA/1000000)
summary(powIrrig02)
powIrrig02 <- powIrrig02[c("site_no","RE_Irrig02_pct","AC_Irrig02_pct")]
rm(AC_Irrig02)
file.remove(paste0(landcoverDir,"AC_IRRIG02.dbf"))
summary(powIrrig02)
powLandcover <- merge(x=powLandcover, y=powIrrig02, by="site_no")

#Percentage of watershed subjected to irrigation, MODIS 2007.
AC_Irrig07_zip <- "AC_IRRIG07.zip"
unzip(paste0(landcoverDir,AC_Irrig07_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Landcover")
AC_Irrig07 <- read.dbf(paste0(landcoverDir,"AC_IRRIG07.dbf"))
powIrrig07 <- merge(x=powCOMID_AREA,y=AC_Irrig07,by="COMID",all.x=TRUE)
powIrrig07["RE_Irrig07_pct"] <- powIrrig07$SUM / powIrrig07$REACH_AREA
powIrrig07["AC_Irrig07_pct"] <- powIrrig07$AC_IRRIG07 / (powIrrig07$ACCUM_AREA)
powIrrig07 <- powIrrig07[c("site_no","RE_Irrig07_pct","AC_Irrig07_pct")]
rm(AC_Irrig07)
file.remove(paste0(landcoverDir,"AC_Irrig07.dbf"))
summary(powIrrig07)
powLandcover <- merge(x=powLandcover, y=powIrrig07, by="site_no")

#Population density in the watershed, 2010
AC_popd10_zip <- "AC_popd10.zip"
unzip(paste0(landcoverDir,AC_popd10_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Landcover")
AC_POPD10 <- read.dbf(paste0(landcoverDir,"AC_popd10.dbf"))
powPOPD10 <- merge(x=powCOMID_AREA,y=AC_POPD10,by="COMID",all.x=TRUE)
str(powPOPD10)
powPOPD10 <- rename(powPOPD10,c("MEAN" = "RE_POPD10","POPD10_AC" = "AC_POPD10"))
powPOPD10 <- powPOPD10[c("site_no","RE_POPD10","AC_POPD10")]
rm(AC_POPD10)
file.remove(paste0(landcoverDir,"AC_POPD10.dbf"))
summary(powPOPD10)
powLandcover <- merge(x=powLandcover, y=powPOPD10, by="site_no")

str(powLandcover,list.len=200)
write.table(powLandcover, paste0(landcoverDir,"powLandcover.csv"), sep=",", row.names=FALSE, col.names=TRUE)


