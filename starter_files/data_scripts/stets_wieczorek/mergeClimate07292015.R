#Load required libraries

library(foreign)
library(plyr)

#Set directories
climateDir <- "C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Climate/"
workingDir <- "C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/"

#Create list of powstream COMIDs
#comid640 <- read.csv(paste0(workingDir,"comid_640_sites_nodup2.csv"),
#            header=TRUE,colClasses=c("character","character","numeric","character"))
#Nodup file added 07/28/2015 to deal with duplicated site_no's in the powstreams file.
comid640 <- read.csv(paste0(workingDir,"comid_640_sites_nodup2.csv"),
                     header=TRUE,colClasses=c("character","character","character","numeric","character"))
comid640 <- rename(comid640,c("bestcomid"="COMID"))

#30 yr precip
ppt30yrzip <- "ppt30yr.zip"
unzip(paste0(climateDir,ppt30yrzip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Climate")
ppt30yr <- read.dbf(paste0(climateDir,"ppt30yr.dbf"))
powppt30yr <- merge(x=comid640,y=ppt30yr,by="COMID",all.x=TRUE)
#Seems to be a problem with reach-scale precip.  This code corrects it.
powppt30yr$PPT30YR_RE <- powppt30yr$MEAN / 1000
powppt30yr <- powppt30yr[c("site_no","COMID","COMID_CONFIDENCE","PPT30YR_AC","PPT30YR_RE")]
rm(ppt30yr)
file.remove(paste0(climateDir,"ppt30yr.dbf"))
summary(powppt30yr)

#30 yr temp
temp30yrzip <- "TMean.zip"
unzip(paste0(climateDir,temp30yrzip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Climate")
temp30yr <- read.dbf(paste0(climateDir,"TMean.dbf"))
powtemp30yr <- merge(x=comid640,y=temp30yr,by="COMID",all.x=TRUE)
powtemp30yr <- rename(powtemp30yr,c("MEAN" = "TMEAN_RE"))
powtemp30yr <- powtemp30yr[c("site_no","TMEAN_AC","TMEAN_RE")]
rm(temp30yr)
file.remove(paste0(climateDir,"TMEAN.dbf"))
summary(powtemp30yr)
powClimate <- merge(x=powppt30yr, y=powtemp30yr, by="site_no")

#First freeze
AC_FstFz_zip <- "AC_FstFz.zip"
unzip(paste0(climateDir,AC_FstFz_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Climate")
AC_FstFz <- read.dbf(paste0(climateDir,"AC_FstFz.dbf"))
powFstFz <- merge(x=comid640,y=AC_FstFz,by="COMID",all.x=TRUE)
powFstFz <- rename(powFstFz,c("MEAN" = "FstFz_RE","FSTFZ_AC" = "FstFz_AC"))
powFstFz <- powFstFz[c("site_no","FstFz_AC","FstFz_RE")]
rm(AC_FstFz)
file.remove(paste0(climateDir,"AC_FstFz.dbf"))
summary(powFstFz)
powClimate <- merge(x=powClimate, y=powFstFz, by="site_no")

#Last freeze
AC_LstFz_zip <- "AC_LstFz.zip"
unzip(paste0(climateDir,AC_LstFz_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Climate")
AC_LstFz <- read.dbf(paste0(climateDir,"AC_LstFz.dbf"))
powLstFz <- merge(x=comid640,y=AC_LstFz,by="COMID",all.x=TRUE)
powLstFz <- rename(powLstFz,c("MEAN" = "LstFz_RE", "LSTFZ_AC" = "LstFz_AC"))
powLstFz <- powLstFz[c("site_no","LstFz_AC","LstFz_RE")]
rm(AC_LstFz)
file.remove(paste0(climateDir,"AC_LstFz.dbf"))
summary(powLstFz)
powClimate <- merge(x=powClimate, y=powLstFz, by="site_no")

#MaxP
MaxP_zip <- "AC_MaxP.zip"
unzip(paste0(climateDir,MaxP_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Climate")
AC_MaxP <- read.dbf(paste0(climateDir,"AC_MaxP.dbf"))
powMaxP <- merge(x=comid640,y=AC_MaxP,by="COMID",all.x=TRUE)
powMaxP <- rename(powMaxP,c("MEAN" = "MaxP_RE", "MAXP_AC" = "MaxP_AC"))
powMaxP <- powMaxP[c("site_no","COMID","MaxP_AC","MaxP_RE")]
rm(AC_MaxP)
file.remove(paste0(climateDir,"AC_MaxP.dbf"))
summary(powMaxP)
powClimate <- merge(x=powClimate, y=powMaxP, by="site_no")

#MinP
MinP_zip <- "AC_MinP.zip"
unzip(paste0(climateDir,MinP_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Climate")
AC_MinP <- read.dbf(paste0(climateDir,"AC_MinP.dbf"))
powMinP <- merge(x=comid640,y=AC_MinP,by="COMID",all.x=TRUE)
powMinP <- rename(powMinP,c("MEAN" = "MinP_RE", "MINP_AC" = "MinP_AC"))
powMinP <- powMinP[c("site_no","MinP_AC","MinP_RE")]
rm(AC_MinP)
file.remove(paste0(climateDir,"AC_MinP.dbf"))
summary(powMinP)
powClimate <- merge(x=powClimate, y=powMinP, by="site_no")

#MaxWD
MaxWD_zip <- "AC_MaxWD.zip"
unzip(paste0(climateDir,MaxWD_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Climate")
AC_MaxWD <- read.dbf(paste0(climateDir,"AC_MaxWD.dbf"))
powMaxWD <- merge(x=comid640,y=AC_MaxWD,by="COMID",all.x=TRUE)
powMaxWD <- rename(powMaxWD,c("MEAN" = "MaxWD_RE","MAXWD_AC" = "MaxWD_AC"))
powMaxWD <- powMaxWD[c("site_no","MaxWD_AC","MaxWD_RE")]
rm(AC_MaxWD)
file.remove(paste0(climateDir,"AC_MaxWD.dbf"))
summary(powMaxWD)
powClimate <- merge(x=powClimate, y=powMaxWD, by="site_no")

#MinWD
MinWD_zip <- "AC_MinWD.zip"
unzip(paste0(climateDir,MinWD_zip),exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Climate")
AC_MinWD <- read.dbf(paste0(climateDir,"AC_MinWD.dbf"))
powMinWD <- merge(x=comid640,y=AC_MinWD,by="COMID",all.x=TRUE)
powMinWD <- rename(powMinWD,c("MEAN" = "MinWD_RE","MINWD_AC" = "MinWD_AC"))
powMinWD <- powMinWD[c("site_no","MinWD_AC","MinWD_RE")]
rm(AC_MinWD)
file.remove(paste0(climateDir,"AC_MinWD.dbf"))
summary(powMinWD)
powClimate <- merge(x=powClimate, y=powMinWD, by="site_no")

#Monthly precip in alphabetical order
#April
pptApr_zip <- "ppt7100apr.zip"
unzip(paste0(climateDir,"MonthPrecip/",pptApr_zip),
      exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Climate/MonthPrecip")
pptApr <- read.dbf(paste0(climateDir,"MonthPrecip/ppt7100apr.dbf"))
powpptApr <- merge(x=comid640,y=pptApr,by="COMID",all.x=TRUE)
powpptApr <- rename(powpptApr,c("MEAN" = "PPTApr_RE", "PPTAPR_AC" = "PPTApr_AC"))
powpptApr <- powpptApr[c("site_no","PPTApr_AC","PPTApr_RE")]
rm(pptApr)
file.remove(paste0(climateDir,"MonthPrecip/ppt7100apr.dbf"))
summary(powpptApr)
powClimate <- merge(x=powClimate, y=powpptApr, by="site_no")

#August
pptAug_zip <- "ppt7100aug.zip"
unzip(paste0(climateDir,"MonthPrecip/",pptAug_zip),
      exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Climate/MonthPrecip")
pptAug <- read.dbf(paste0(climateDir,"MonthPrecip/ppt7100aug.dbf"))
powpptAug <- merge(x=comid640,y=pptAug,by="COMID",all.x=TRUE)
powpptAug <- rename(powpptAug,c("MEAN" = "PPTAug_RE","PPTAUG_AC" = "PPTAug_AC"))
powpptAug <- powpptAug[c("site_no","PPTAug_AC","PPTAug_RE")]
rm(pptAug)
file.remove(paste0(climateDir,"MonthPrecip/ppt7100aug.dbf"))
summary(powpptAug)
powClimate <- merge(x=powClimate, y=powpptAug, by="site_no")

#December
pptDec_zip <- "ppt7100dec.zip"
unzip(paste0(climateDir,"MonthPrecip/",pptDec_zip),
      exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Climate/MonthPrecip")
pptDec <- read.dbf(paste0(climateDir,"MonthPrecip/ppt7100dec.dbf"))
powpptDec <- merge(x=comid640,y=pptDec,by="COMID",all.x=TRUE)
powpptDec <- rename(powpptDec,c("MEAN" = "PPTDec_RE","PPTDEC_AC" = "PPTDec_AC"))
powpptDec <- powpptDec[c("site_no","PPTDec_AC","PPTDec_RE")]
rm(pptDec)
file.remove(paste0(climateDir,"MonthPrecip/ppt7100dec.dbf"))
summary(powpptDec)
powClimate <- merge(x=powClimate, y=powpptDec, by="site_no")

#February
pptFeb_zip <- "ppt7100feb.zip"
unzip(paste0(climateDir,"MonthPrecip/",pptFeb_zip),
      exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Climate/MonthPrecip")
pptFeb <- read.dbf(paste0(climateDir,"MonthPrecip/ppt7100feb.dbf"))
powpptFeb <- merge(x=comid640,y=pptFeb,by="COMID",all.x=TRUE)
powpptFeb <- rename(powpptFeb,c("MEAN" = "PPTFeb_RE","PPTFEB_AC" = "PPTFeb_AC"))
powpptFeb <- powpptFeb[c("site_no","PPTFeb_AC","PPTFeb_RE")]
rm(pptFeb)
file.remove(paste0(climateDir,"MonthPrecip/ppt7100feb.dbf"))
summary(powpptFeb)
powClimate <- merge(x=powClimate, y=powpptFeb, by="site_no")

#January
pptJan_zip <- "ppt7100jan.zip"
unzip(paste0(climateDir,"MonthPrecip/",pptJan_zip),
      exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Climate/MonthPrecip")
pptJan <- read.dbf(paste0(climateDir,"MonthPrecip/ppt7100jan.dbf"))
powpptJan <- merge(x=comid640,y=pptJan,by="COMID",all.x=TRUE)
powpptJan <- rename(powpptJan,c("MEAN" = "PPTJan_RE","PPTJAN_AC" = "PPTJan_AC"))
powpptJan <- powpptJan[c("site_no","PPTJan_AC","PPTJan_RE")]
rm(pptJan)
file.remove(paste0(climateDir,"MonthPrecip/ppt7100jan.dbf"))
summary(powpptJan)
powClimate <- merge(x=powClimate, y=powpptJan, by="site_no")

#July
pptJul_zip <- "ppt7100jul.zip"
unzip(paste0(climateDir,"MonthPrecip/",pptJul_zip),
      exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Climate/MonthPrecip")
pptJul <- read.dbf(paste0(climateDir,"MonthPrecip/ppt7100jul.dbf"))
powpptJul <- merge(x=comid640,y=pptJul,by="COMID",all.x=TRUE)
powpptJul <- rename(powpptJul,c("MEAN" = "PPTJul_RE","PPTJUL_AC" = "PPTJul_AC"))
powpptJul <- powpptJul[c("site_no","PPTJul_AC","PPTJul_RE")]
rm(pptJul)
file.remove(paste0(climateDir,"MonthPrecip/ppt7100jul.dbf"))
summary(powpptJul)
powClimate <- merge(x=powClimate, y=powpptJul, by="site_no")

#June
pptJun_zip <- "ppt7100jun.zip"
unzip(paste0(climateDir,"MonthPrecip/",pptJun_zip),
      exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Climate/MonthPrecip")
pptJun <- read.dbf(paste0(climateDir,"MonthPrecip/ppt7100jun.dbf"))
powpptJun <- merge(x=comid640,y=pptJun,by="COMID",all.x=TRUE)
powpptJun <- rename(powpptJun,c("MEAN" = "PPTJun_RE","PPTMJUN_AC" = "PPTJun_AC"))
powpptJun <- powpptJun[c("site_no","PPTJun_AC","PPTJun_RE")]
rm(pptJun)
file.remove(paste0(climateDir,"MonthPrecip/ppt7100jun.dbf"))
summary(powpptJun)
powClimate <- merge(x=powClimate, y=powpptJun, by="site_no")

#March
pptMar_zip <- "ppt7100mar.zip"
unzip(paste0(climateDir,"MonthPrecip/",pptMar_zip),
      exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Climate/MonthPrecip")
pptMar <- read.dbf(paste0(climateDir,"MonthPrecip/ppt7100mar.dbf"))
powpptMar <- merge(x=comid640,y=pptMar,by="COMID",all.x=TRUE)
powpptMar <- rename(powpptMar,c("MEAN" = "PPTMar_RE","PPTMAR_AC" = "PPTMar_AC"))
powpptMar <- powpptMar[c("site_no","PPTMar_AC","PPTMar_RE")]
rm(pptMar)
file.remove(paste0(climateDir,"MonthPrecip/ppt7100mar.dbf"))
summary(powpptMar)
powClimate <- merge(x=powClimate, y=powpptMar, by="site_no")

#May
pptMay_zip <- "ppt7100may.zip"
unzip(paste0(climateDir,"MonthPrecip/",pptMay_zip),
      exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Climate/MonthPrecip")
pptMay <- read.dbf(paste0(climateDir,"MonthPrecip/ppt7100may.dbf"))
powpptMay <- merge(x=comid640,y=pptMay,by="COMID",all.x=TRUE)
powpptMay <- rename(powpptMay,c("MEAN" = "PPTMay_RE","PPTMAY_AC" = "PPTMay_AC"))
powpptMay <- powpptMay[c("site_no","PPTMay_AC","PPTMay_RE")]
rm(pptMay)
file.remove(paste0(climateDir,"MonthPrecip/ppt7100may.dbf"))
summary(powpptMay)
powClimate <- merge(x=powClimate, y=powpptMay, by="site_no")

#November
pptNov_zip <- "ppt7100nov.zip"
unzip(paste0(climateDir,"MonthPrecip/",pptNov_zip),
      exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Climate/MonthPrecip")
pptNov <- read.dbf(paste0(climateDir,"MonthPrecip/ppt7100nov.dbf"))
powpptNov <- merge(x=comid640,y=pptNov,by="COMID",all.x=TRUE)
powpptNov <- rename(powpptNov,c("MEAN" = "PPTNov_RE","PPTNOV_AC" = "PPTNov_AC"))
powpptNov <- powpptNov[c("site_no","PPTNov_AC","PPTNov_RE")]
rm(pptNov)
file.remove(paste0(climateDir,"MonthPrecip/ppt7100nov.dbf"))
summary(powpptNov)
powClimate <- merge(x=powClimate, y=powpptNov, by="site_no")

#October
pptOct_zip <- "ppt7100oct.zip"
unzip(paste0(climateDir,"MonthPrecip/",pptOct_zip),
      exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Climate/MonthPrecip")
pptOct <- read.dbf(paste0(climateDir,"MonthPrecip/ppt7100oct.dbf"))
powpptOct <- merge(x=comid640,y=pptOct,by="COMID",all.x=TRUE)
powpptOct <- rename(powpptOct,c("MEAN" = "PPTOct_RE","PPTOCT_AC" = "PPTOct_AC"))
powpptOct <- powpptOct[c("site_no","PPTOct_AC","PPTOct_RE")]
rm(pptOct)
file.remove(paste0(climateDir,"MonthPrecip/ppt7100oct.dbf"))
summary(powpptOct)
powClimate <- merge(x=powClimate, y=powpptOct, by="site_no")

#September
pptSep_zip <- "ppt7100sep.zip"
unzip(paste0(climateDir,"MonthPrecip/",pptSep_zip),
      exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Climate/MonthPrecip")
pptSep <- read.dbf(paste0(climateDir,"MonthPrecip/ppt7100sep.dbf"))
powpptSep <- merge(x=comid640,y=pptSep,by="COMID",all.x=TRUE)
powpptSep <- rename(powpptSep,c("MEAN" = "PPTSep_RE","PPTSEP_AC" = "PPTSep_AC"))
powpptSep <- powpptSep[c("site_no","PPTSep_AC","PPTSep_RE")]
rm(pptSep)
file.remove(paste0(climateDir,"MonthPrecip/ppt7100sep.dbf"))
summary(powpptSep)
powClimate <- merge(x=powClimate, y=powpptSep, by="site_no")

#Average number of wet days per month
#Months in alphabetical order
#April
WDApr_zip <- "AC_WetApr.zip"
unzip(paste0(climateDir,"WetMonth/",WDApr_zip),
      exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Climate/WetMonth")
WDApr <- read.dbf(paste0(climateDir,"WetMonth/AC_WetApr.dbf"))
powWDApr <- merge(x=comid640,y=WDApr,by="COMID",all.x=TRUE)
powWDApr <- rename(powWDApr,c("MEAN" = "WetApr_RE", "WETAPR_AC" = "WetApr_AC"))
powWDApr <- powWDApr[c("site_no","WetApr_AC","WetApr_RE")]
rm(WDApr)
file.remove(paste0(climateDir,"WetMonth/AC_WetApr.dbf"))
summary(powWDApr)
powClimate <- merge(x=powClimate, y=powWDApr, by="site_no")

#August
WDAug_zip <- "AC_WetAug.zip"
unzip(paste0(climateDir,"WetMonth/",WDAug_zip),
      exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Climate/WetMonth")
WDAug <- read.dbf(paste0(climateDir,"WetMonth/AC_WetAug.dbf"))
powWDAug <- merge(x=comid640,y=WDAug,by="COMID",all.x=TRUE)
powWDAug <- rename(powWDAug,c("MEAN" = "WetAug_RE", "WETAUG_AC" = "WetAug_AC"))
powWDAug <- powWDAug[c("site_no","WetAug_AC","WetAug_RE")]
rm(WDAug)
file.remove(paste0(climateDir,"WetMonth/AC_WetAug.dbf"))
summary(powWDAug)
powClimate <- merge(x=powClimate, y=powWDAug, by="site_no")

#December
WDDec_zip <- "AC_WetDec.zip"
unzip(paste0(climateDir,"WetMonth/",WDDec_zip),
      exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Climate/WetMonth")
WDDec <- read.dbf(paste0(climateDir,"WetMonth/AC_WetDec.dbf"))
powWDDec <- merge(x=comid640,y=WDDec,by="COMID",all.x=TRUE)
powWDDec <- rename(powWDDec,c("MEAN" = "WetDec_RE", "WETDEC_AC" = "WetDec_AC"))
powWDDec <- powWDDec[c("site_no","WetDec_AC","WetDec_RE")]
rm(WDDec)
file.remove(paste0(climateDir,"WetMonth/AC_WetDec.dbf"))
summary(powWDDec)
powClimate <- merge(x=powClimate, y=powWDDec, by="site_no")

#February
WDFeb_zip <- "AC_WetFeb.zip"
unzip(paste0(climateDir,"WetMonth/",WDFeb_zip),
      exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Climate/WetMonth")
WDFeb <- read.dbf(paste0(climateDir,"WetMonth/AC_WetFeb.dbf"))
powWDFeb <- merge(x=comid640,y=WDFeb,by="COMID",all.x=TRUE)
powWDFeb <- rename(powWDFeb,c("MEAN" = "WetFeb_RE", "WETFEB_AC" = "WetFeb_AC"))
powWDFeb <- powWDFeb[c("site_no","WetFeb_AC","WetFeb_RE")]
rm(WDFeb)
file.remove(paste0(climateDir,"WetMonth/AC_WetFeb.dbf"))
summary(powWDFeb)
powClimate <- merge(x=powClimate, y=powWDFeb, by="site_no")

#January
WDJan_zip <- "AC_WetJan.zip"
unzip(paste0(climateDir,"WetMonth/",WDJan_zip),
      exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Climate/WetMonth")
WDJan <- read.dbf(paste0(climateDir,"WetMonth/AC_WetJan.dbf"))
powWDJan <- merge(x=comid640,y=WDJan,by="COMID",all.x=TRUE)
powWDJan <- rename(powWDJan,c("MEAN" = "WetJan_RE", "WETJAN_AC" = "WetJan_AC"))
powWDJan <- powWDJan[c("site_no","WetJan_AC","WetJan_RE")]
rm(WDJan)
file.remove(paste0(climateDir,"WetMonth/AC_WetJan.dbf"))
summary(powWDJan)
powClimate <- merge(x=powClimate, y=powWDJan, by="site_no")

#July
WDJul_zip <- "AC_WetJul.zip"
unzip(paste0(climateDir,"WetMonth/",WDJul_zip),
      exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Climate/WetMonth")
WDJul <- read.dbf(paste0(climateDir,"WetMonth/AC_WetJul.dbf"))
powWDJul <- merge(x=comid640,y=WDJul,by="COMID",all.x=TRUE)
powWDJul <- rename(powWDJul,c("MEAN" = "WetJul_RE", "WETJUL_AC" = "WetJul_AC"))
powWDJul <- powWDJul[c("site_no","WetJul_AC","WetJul_RE")]
rm(WDJul)
file.remove(paste0(climateDir,"WetMonth/AC_WetJul.dbf"))
summary(powWDJul)
powClimate <- merge(x=powClimate, y=powWDJul, by="site_no")

#June
WDJun_zip <- "AC_WetJun.zip"
unzip(paste0(climateDir,"WetMonth/",WDJun_zip),
      exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Climate/WetMonth")
WDJun <- read.dbf(paste0(climateDir,"WetMonth/AC_WetJun.dbf"))
powWDJun <- merge(x=comid640,y=WDJun,by="COMID",all.x=TRUE)
powWDJun <- rename(powWDJun,c("MEAN" = "WetJun_RE", "WETJUN_AC" = "WetJun_AC"))
powWDJun <- powWDJun[c("site_no","WetJun_AC","WetJun_RE")]
rm(WDJun)
file.remove(paste0(climateDir,"WetMonth/AC_WetJun.dbf"))
summary(powWDJun)
powClimate <- merge(x=powClimate, y=powWDJun, by="site_no")

#March
WDMar_zip <- "AC_WetMar.zip"
unzip(paste0(climateDir,"WetMonth/",WDMar_zip),
      exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Climate/WetMonth")
WDMar <- read.dbf(paste0(climateDir,"WetMonth/AC_WetMar.dbf"))
powWDMar <- merge(x=comid640,y=WDMar,by="COMID",all.x=TRUE)
powWDMar <- rename(powWDMar,c("MEAN" = "WetMar_RE", "WETMAR_AC" = "WetMar_AC"))
powWDMar <- powWDMar[c("site_no","WetMar_AC","WetMar_RE")]
rm(WDMar)
file.remove(paste0(climateDir,"WetMonth/AC_WetMar.dbf"))
summary(powWDMar)
powClimate <- merge(x=powClimate, y=powWDMar, by="site_no")

#May
WDMay_zip <- "AC_WetMay.zip"
unzip(paste0(climateDir,"WetMonth/",WDMay_zip),
      exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Climate/WetMonth")
WDMay <- read.dbf(paste0(climateDir,"WetMonth/AC_WetMay.dbf"))
powWDMay <- merge(x=comid640,y=WDMay,by="COMID",all.x=TRUE)
powWDMay <- rename(powWDMay,c("MEAN" = "WetMay_RE", "WETMAY_AC" = "WetMay_AC"))
powWDMay <- powWDMay[c("site_no","WetMay_AC","WetMay_RE")]
rm(WDMay)
file.remove(paste0(climateDir,"WetMonth/AC_WetMay.dbf"))
summary(powWDMay)
powClimate <- merge(x=powClimate, y=powWDMay, by="site_no")

#November
WDNov_zip <- "AC_WetNov.zip"
unzip(paste0(climateDir,"WetMonth/",WDNov_zip),
      exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Climate/WetMonth")
WDNov <- read.dbf(paste0(climateDir,"WetMonth/AC_WetNov.dbf"))
powWDNov <- merge(x=comid640,y=WDNov,by="COMID",all.x=TRUE)
powWDNov <- rename(powWDNov,c("MEAN" = "WetNov_RE", "WETNOV_AC" = "WetNov_AC"))
powWDNov <- powWDNov[c("site_no","WetNov_AC","WetNov_RE")]
rm(WDNov)
file.remove(paste0(climateDir,"WetMonth/AC_WetNov.dbf"))
summary(powWDNov)
powClimate <- merge(x=powClimate, y=powWDNov, by="site_no")

#October
WDOct_zip <- "AC_WetOct.zip"
unzip(paste0(climateDir,"WetMonth/",WDOct_zip),
      exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Climate/WetMonth")
WDOct <- read.dbf(paste0(climateDir,"WetMonth/AC_WetOct.dbf"))
powWDOct <- merge(x=comid640,y=WDOct,by="COMID",all.x=TRUE)
powWDOct <- rename(powWDOct,c("MEAN" = "WetOct_RE", "WETOCT_AC" = "WetOct_AC"))
powWDOct <- powWDOct[c("site_no","WetOct_AC","WetOct_RE")]
rm(WDOct)
file.remove(paste0(climateDir,"WetMonth/AC_WetOct.dbf"))
summary(powWDOct)
powClimate <- merge(x=powClimate, y=powWDOct, by="site_no")

#September
WDSep_zip <- "AC_WetSep.zip"
unzip(paste0(climateDir,"WetMonth/",WDSep_zip),
      exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Climate/WetMonth")
WDSep <- read.dbf(paste0(climateDir,"WetMonth/AC_WetSep.dbf"))
powWDSep <- merge(x=comid640,y=WDSep,by="COMID",all.x=TRUE)
powWDSep <- rename(powWDSep,c("MEAN" = "WetSep_RE", "WETSEP_AC" = "WetSep_AC"))
powWDSep <- powWDSep[c("site_no","WetSep_AC","WetSep_RE")]
rm(WDSep)
file.remove(paste0(climateDir,"WetMonth/AC_WetSep.dbf"))
summary(powWDSep)
powClimate <- merge(x=powClimate, y=powWDSep, by="site_no")

#Year
WDYr_zip <- "AC_WetYr.zip"
unzip(paste0(climateDir,"WetMonth/",WDYr_zip),
      exdir="C:/Users/estets/Documents/R/workSpaces/POWELL_CENTER/wieczorek/Climate/WetMonth")
WDYr <- read.dbf(paste0(climateDir,"WetMonth/AC_WetYr.dbf"))
powWDYr <- merge(x=comid640,y=WDYr,by="COMID",all.x=TRUE)
powWDYr <- rename(powWDYr,c("MEAN" = "WetYr_RE", "WETYR_AC" = "WetYr_AC"))
powWDYr <- powWDYr[c("site_no","WetYr_AC","WetYr_RE")]
rm(WDYr)
file.remove(paste0(climateDir,"WetMonth/AC_WetYr.dbf"))
summary(powWDYr)
powClimate <- merge(x=powClimate, y=powWDYr, by="site_no")

#Drop mystery column
dropCols <- names(powClimate) %in% c("COMID.y")
powClimate <- powClimate[!dropCols]
powClimate <- rename(powClimate,c("COMID.x" = "COMID"))

str(powClimate)

write.table(powClimate, paste0(climateDir,"powClimate.csv"), sep=",", row.names=FALSE, col.names=TRUE)
