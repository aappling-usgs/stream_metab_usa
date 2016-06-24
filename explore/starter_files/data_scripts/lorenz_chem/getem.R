# Dave Lorenz's original script, reworked by Alison Appling to be runnable as a
# script

# define the list of sites for which to acquire data. AA doesn't know where the
# original list of 640 came from, so just use the current site list here.
library(mda.streams)
comid_640 <- data.frame(site_name=substring(grep("^nwis_", list_sites(), value=TRUE),6), stringsAsFactors = FALSE)

#### Pull data from NWIS ####
library(smwrQW)

# Note that the original method was discontinued. It was insprired by a very slow
# internet connection that timed out. (AA guesses that the new method is to break the
# comids into chunks of 100)

# Suspended sediment
PC <- "80154"
Tm1 <- importNWISqw(comid_640[1:100,1], PC, begin.date="2007-01-01")
Tm2 <- importNWISqw(comid_640[101:200,1], PC, begin.date="2007-01-01")
Tm3 <- importNWISqw(comid_640[201:300,1], PC, begin.date="2007-01-01")
Tm4 <- importNWISqw(comid_640[301:400,1], PC, begin.date="2007-01-01")
Tm5 <- importNWISqw(comid_640[401:500,1], PC, begin.date="2007-01-01")
Tm6 <- importNWISqw(comid_640[501:nrow(comid_640),1], PC, begin.date="2007-01-01")
SS <- rbindQW(Tm5, Tm1, Tm2, Tm3, Tm4, Tm6) # was getting 'Error in charToDate(x) : character string is not in a standard unambiguous format' until I put Tm5 at the start of the list
rm(Tm1, Tm2, Tm3, Tm4, Tm5, Tm6)
# For > 5% censoring
# with the exception of site 041482663, simple sub should be OK. For that site, the 
# values were censored at 2, but no other values at 2, but 3 and greater.
SS <- transform(SS, SS=as.numeric(SuspSed))
dumpdf(SS, "SS.tsv", list(SS="mg/L"))

# Need sand-fine split too
PC <- "70331"
Fines.Hmm <- whatNWISdata(comid_640[[1]], service="qw", parameterCd=PC)
Fines.Hmm <- subset(Fines.Hmm, end_date > "2007-01-01")
Fines <- importNWISqw(unique(Fines.Hmm$site_no), PC, begin.date="2007-01-01")
# No censoring, just convert
Fines <- transform(Fines, Fines=as.numeric(Particle.size))
dumpdf(Fines, "Fines.tsv", list(Fines="%"))

# Turbidity; very few records since 2007: look at Turb.hmm
Turb.hmm
PC <- c("00070", "00075", "00076", "61028", "82079", "99872")
#Tm1 <- importNWISqw(comid_640[1:100,1], PC, begin.date="2007-01-01")
#Tm2 <- importNWISqw(comid_640[101:200,1], PC, begin.date="2007-01-01")
#Tm3 <- importNWISqw(comid_640[201:300,1], PC, begin.date="2007-01-01")
#Tm4 <- importNWISqw(comid_640[301:400,1], PC, begin.date="2007-01-01")
#Tm5 <- importNWISqw(comid_640[401:500,1], PC, begin.date="2007-01-01")
#Tm6 <- importNWISqw(comid_640[501:636,1], PC, begin.date="2007-01-01")
#Turb <- rbindQW(Tm1, Tm2, Tm3, Tm4, Tm5, Tm6)
#rm(Tm1, Tm2, Tm3, Tm4, Tm5, Tm6)

# Sulfate
PC <- c("00945", "00946", "99113", "99127")
SO4.Hmm <- whatNWISdata(comid_640[[1]], service="qw", parameterCd=PC)
SO4.Hmm <- subset(SO4.Hmm, end_date > "2007-01-01")
# Only 00945 is generally available, recast PC
PC <- "00945"
SO4 <- importNWISqw(unique(SO4.Hmm$site_no), PC, begin.date="2007-01-01")
# For > 5% censoring
# all look good for conversion to 1/2 DL
SO4 <- transform(SO4, SO4=as.numeric(Sulfate))
dumpdf(SO4, "SO4.tsv", list(SO4="mg/L"))

# Calcium
PC <- c("00910", "00915")
Ca.Hmm <- whatNWISdata(comid_640[[1]], service="qw", parameterCd=PC)
Ca.Hmm <- subset(Ca.Hmm, end_date > "2007-01-01")
# Only 00915 were found, get em
Ca <- importNWISqw(unique(Ca.Hmm$site_no), PC, begin.date="2007-01-01")
# Only 1 site hace censored values (1.45%)
Ca <- transform(Ca, Ca=as.numeric(Calcium))
dumpdf(Ca, "Ca.tsv", list(Ca="mg/L"))

# pH
PC <- c("00400", "00403")
pH.Hmm <- whatNWISdata(comid_640[[1]], service="qw", parameterCd=PC)
pH.Hmm <- subset(pH.Hmm, end_date > "2007-01-01")
pH <- importNWISqw(unique(pH.Hmm$site_no), PC, begin.date="2007-01-01")
# removed site 01467200, only 3 days and one set were all right censored
pH <- subset(pH, site_no != "01467200")
# No censoring, try coalescing, need to convert Fld to numeric--
# Lab was numeric, the default for pH, unless the data are censored.
pH <- transform(pH, pH=coalesce(as.numeric(pH_WW.Fld), pH$pH_WW.Lab))
dumpdf(pH, "pH.tsv", list(pH="standard units"))

# Organic carbon
PC <- c("00680", "00681")
OC.Hmm <- whatNWISdata(comid_640[[1]], service="qw", parameterCd=PC)
OC.Hmm <- subset(OC.Hmm, end_date > "2007-01-01")
OC <- importNWISqw(unique(OC.Hmm$site_no), PC, begin.date="2007-01-01")
# 4 are very suspicious in magnitude:
which(as.numeric(OC$CarbonOrg) - as.numeric(OC$CarbonOrg_WW) > 2)
# remove them
OC <- OC[-which(as.numeric(OC$CarbonOrg) - as.numeric(OC$CarbonOrg_WW) > 2), ]
# Two sites have > 5 % censoring 07257500 and 08155500, setting the censored values
# to hdl seems to not introduce any patterns
OC <- transform(OC, DOC=as.numeric(CarbonOrg), TOC=as.numeric(CarbonOrg_WW))
dumpdf(OC, "OC.tsv", list(DOC="mg/L", TOC="mg/L"))

#Alkalinity
# This is the preferred order according to NWIS
PC <- c("39086", "29802", "39036", "00418", "39087", "29803", "29801", "00421")
Alk.Hmm <- whatNWISdata(comid_640[[1]], service="qw", parameterCd=PC)
Alk.Hmm <- subset(Alk.Hmm, end_date > "2007-01-01")
Alk <- importNWISqw(unique(Alk.Hmm$site_no), unique(Alk.Hmm$parm_cd),
                    begin.date="2007-01-01", use.pnames=TRUE)
# There are only censored values in 29801 and only 11 times.
# But 29801 will be used when it is censored--it is last in the order to be used
# and it just so happens that no other alkalinity samples were taken when it
# was censored. Appears not to be a problem with inducing patterns.
Alk <- transform(Alk, Alk=as.numeric(qwCoalesce(P39086, P29802, P39036, P00418, P39087, P29801)))
dumpdf(Alk, "Alk.tsv", list(Alk="mg/L as CaCO3"))

# The nutrients
# Set PC to a few selected pcodes
PC <- c("00600", "00618", "00665", "00660")
Nuts.Hmm <- whatNWISdata(comid_640[[1]], service="qw", parameterCd=PC)
Nuts.Hmm <- subset(Nuts.Hmm, end_date > "2007-01-01")
PC <- "NUT"
Nuts <- importNWISqw(unique(Nuts.Hmm$site_no), PC, begin.date="2007-01-01")
# Nitrate: 2 methods, unknown ("") and ALGOR-- NO2+NO3 - NO2 fix ALGOR to correct interval
Nuts$Nitrate.N[which(Nuts$Nitrate.N@analyte.method == "ALGOR"),] <- NA
Nuts <- transform(Nuts, Nitrate.N2 = add(NO2PlusNO3.N, -Nitrite.N))
Nuts <- transform(Nuts, NO3.qw = qwCoalesce(Nitrate.N, Nitrate.N2))
# Convert--see also NO3.Cen whihc reports the percentage censored, 
# some of which are very larger percentages, but probably also have small sizes
# Some 0s in NO3.Cen may have only missing values too.
Nuts <- transform(Nuts, NO3 = as.numeric(NO3.qw)) # Note as N
#
# Total N: 2 methods, unknown ("") and ALGOR-- NO2+NO3 + Kjeldahl fix ALGOR to correct interval
Nuts$NitrogenTotal_WW[which(Nuts$NitrogenTotal_WW@analyte.method == "ALGOR"),] <- NA
Nuts <- transform(Nuts, NitrogenTotal_WW2 = add(NO2PlusNO3.N, Kjeldahl_WW.N.00625))
Nuts <- transform(Nuts, TN.qw = qwCoalesce(NitrogenTotal_WW, NitrogenTotal_WW2))
# Note sites 01544500, 301527088521500, and 07061270 have percent left-censored > than 10%
# use with caution
Nuts <- transform(Nuts, TN = as.numeric(TN.qw))
#
# Organic N: 2 methods, unknown ("") and ALGOR-- Kjeldahl - NH3 fix ALGOR to correct interval
# Note only 28 values that were not algorithmic and all of those were right-censored
# And I believe that they will all be replaced by the computation
Nuts$NitrogenOrg_WW[which(Nuts$NitrogenOrg_WW@analyte.method == "ALGOR"),] <- NA
Nuts <- transform(Nuts, OrgN.qw = add(Kjeldahl_WW.N.00625, -Ammonia.N))
# There is one that looks suspiscous
which(as.numeric(Nuts$Ammonia.N) - as.numeric(Nuts$Kjeldahl_WW.N.00625) > .5)
Nuts$OrgN.qw[which(as.numeric(Nuts$Ammonia.N) - as.numeric(Nuts$Kjeldahl_WW.N.00625) > .5),] <- NA
# The same kind of issue with censoring as with NO3, there are many that are highly left-censored,
# see OrgN.Cen
Nuts <- transform(Nuts, OrgN = as.numeric(OrgN.qw))
#
# Ammonia
# more than 28% of the entire dataset is left-censored NH3.Cen summarizes for each station. I'd
# be unwilling to place any confidence in the analysis of such highly censored data and they 
# were not processed. Note that this is a separate issue from arithmetic operations done earlier.
#
# Total P
# Some sites have pretty extensive censoring, see TP.Cen
# Just convert the data
Nuts <- transform(Nuts, TP = as.numeric(Phosphorus_WW.P))
#
# Orthophosphate
# Some sites have pretty extensive censoring, see PO4.Cen
# Just convert the data
Nuts <- transform(Nuts, PO4 = as.numeric(OrthoPhosphate.P)) # as P
# Dump the data
dumpdf(Nuts, "Nuts.tsv", list(NO3="mg/L as N", TN="mg/L", OrgN="mg/L", TP="mg/L", PO4="mg/L as P"))

# here's how the .Cen datasets were made:
NO3.Cen <- data.frame(PctCen=
  sort(with(Nuts, tapply(NO3.qw, site_no, pctCens, type="left"))))
TN.Cen <- data.frame(PctCen=
  sort(with(Nuts, tapply(TN.qw, site_no, pctCens, type="left"))))
OrgN.Cen <- data.frame(PctCen=
  sort(with(Nuts, tapply(OrgN.qw, site_no, pctCens, type="left"))))
TP.Cen <- data.frame(PctCen=
  sort(with(Nuts, tapply(Phosphorus_WW.P, site_no, pctCens, type="left"))))
PO4.Cen <- data.frame(PctCen=
  sort(with(Nuts, tapply(OrthoPhosphate.P, site_no, pctCens, type="left"))))
# And dumped as tsv files
dC(NO3.Cen, "NO3_Cen.tsv")
dC(TN.Cen, "TN_Cen.tsv")
dC(OrgN.Cen, "OrgN_Cen.tsv")
dC(TP.Cen, "TP_Cen.tsv")
dC(PO4.Cen, "PO4_Cen.tsv")

