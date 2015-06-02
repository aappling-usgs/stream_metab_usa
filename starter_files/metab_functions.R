#A Generic function for calculating metabolism one day at a time and seeing the model fit. Plus the ancillary functions needed for metabolism.  These estimate light, oxygen saturation, Schmidt number scaling, nightttime regression etc.


##estimate oxygen saturation.  This is from Garcia and Gordon 1992.  It is oh so slightly incorrect, but that is within the range of sonde error (i.e., < 0.02 mg/L).
osat<-function (temp, bp) 
{
  sato <- (exp(2.00907 + 3.22014 * (log((298.15 - temp)/(273.15 + 
                                                           temp))) + 4.0501 * (log((298.15 - temp)/(273.15 + temp)))^2 + 
                 4.94457 * (log((298.15 - temp)/(273.15 + temp)))^3 - 
                 0.256847 * (log((298.15 - temp)/(273.15 + temp)))^4 + 
                 3.88767 * (log((298.15 - temp)/(273.15 + temp)))^5)) * 
    1.4276 * bp/760
  sato
}



##Schmidt number scaling from K600 to KO2 at some temperature
##K600 to KO2  Temp is deg C.  K can be any units you want, per time rate or even a gas exchange velocity.
Kcor<-function (temp,K600) {
  K600/(600/(1800.6-(temp*120.1)+(3.7818*temp^2)-(0.047608*temp^3)))^-0.5
}
##


##Schmidt number scaling from KO2 to K600.  This is needed for going from nighttime regression K to K600

K600fromO2 <- function (temp,KO2) {
  ((600/(1800.6-(120.1*temp)+(3.7818*temp^2)-(0.047608*temp^3)))^-0.5)*KO2
  
}




###Estimate barometric pressure as a function of altitude and standard BP
# since we will not correct for dail change in BP use 29.92 (=760 mm)
##this is based on the barometric formula
###temp is degC, alt is m, and bpst is in mm of Hg.  Temp is usually relative to a standard, 15 degC.  This is from Colt's book
bpcalc<- function(bpst, alt, temp) {
  
  bpst*exp((-9.80665*0.0289644*alt)/(8.31447*(273.15+temp)))
  
}

##call as
bpcalc(bpst=760,alt=2400,temp=15)




### A light model. From Yard et al. 1995, Ecological modelling.  Remember your trig? But the model is standard.
#convert degrees to radians
radi<-function(degrees){(degrees*pi/180)}

##make light function that takes as input a date and time.  year is entered as
##"2013-01-01"Longstd is the standard longitude for your time zone (e.g.,
##Mountain is 105).  Note that this changes with daylight savings time.  Our
##data will be in UTC, I suggest that we use standard time for all time zones. 
##This function only makes light for one year, so we will need to change it,
##have at it.

lightest<- function (time, lat, longobs, longstd, year ) {
  
  #jday<-as.numeric(trunc(time)-as.numeric(as.Date(year)))
  jday<-as.numeric(trunc(time))-as.numeric(as.Date(year)) # parens were in funny place, though it doesn't affect output
  
  # E = the equation of time, "accounts for the earth-sun geometric 
  # relationship"; describes deviation of apparent (true?) solar time from mean 
  # (clock) solar time. convert w/ radi because R's sin & cos require radians.
  # result is in minutes.
  E<- 9.87*sin(radi((720*(jday-81))/365)) - 7.53*cos(radi((360*(jday-81))/365)) - 1.5*sin(radi((360*(jday-81))/365))
  
  # LST = local standard time, in days
  LST<-as.numeric (time-trunc(time))
  
  # ST = solar time. doing the conversion here from longstd, longobs, & E in minutes to ST in days
  ST<-LST+(3.989/1440)*(longstd-longobs)+E/1440
  
  # solardel = solar declination, "the earth's angular tilt to the sun, which shifts seasonally"
  solardel<- 24.439*sin(radi(360*((283+jday)/365))) # the first number doesn't match Yard et al. - should be 23.439
  
  # hourangle = hour angle omega, "the angle of departure from solar noon (0 deg), which varies +/- 15 h^-1"
  hourangle<-(0.5-ST)*360 # cos(hourangle) is unaffected, but technically hour angles should be negative before noon, positive after (http://en.wikipedia.org/wiki/Hour_angle) (ST-0.5)
  
  # theta = zenith.angle
  theta<- acos(  sin(radi(solardel)) * sin(radi(lat)) +  cos(radi(solardel)) * cos(radi(lat)) *  cos(radi(hourangle)) )
  
  # take cosine and keep it positive
  suncos<-ifelse(cos(theta)<0, 0, cos(theta))
  
  # GI = ground incidence. 2326 umol m^-2 s^-1 is estimated GI[N] from Yard et 
  # al. 2005 for "clear-sunny daytime conditions characteristic of this large 
  # geographical region" (Glen Canyon, Marble Canyon, Central Grand Canyon, and
  # Western Grand Canyon)
  GI<- suncos*2326
  GI	
  
}
#end of function

#Call as
library(chron)
lightest(time=chron(15500.1), lat=41.33,  longobs=105.7, longstd= 105, year="2012-01-01") # changed year from '13 to '12


#####nighttime regression
#lots of room to play with this code to addapt to Powell data, but here is the core
#o2 file is your oxygen data, bp is baro press in mm Hg for your site, ts is the time step in MINUTES
#o2 file is a dataframe where the temp and oxy coumns ave those names.  Note that you must define the time perion of the nreg before calling it.  See example below where I subset the data based on time.
#K is in units of per day

nightreg<-function(o2file, bp, ts){
  
  temp<-o2file$temp
  oxy<-o2file$oxy
  
  ##moving average on oxy data
  oxyf1<-filter(o2file$oxy,rep(1/3,3), sides=2)
  
  #trim the ends of the oxy data
  oxyf2<- oxyf1[c(-1,-length(oxyf1))]
  
  ##calculate delO/delt
  deltaO2<-((oxyf2[-1]-oxyf2[-length(oxyf2)])/ts)*1440
  
  #Trim the first two and last one from the temp data to match the filter oxy data
  temptrim<-temp[c(-2:-1,-length(temp))]
  #calc the dodef
  satdef<-osat(temptrim,bp)-oxyf2[-1]
  
  #calculate regression and plot
  nreg<-lm(deltaO2~satdef)
  plot(satdef,deltaO2)
  abline(nreg)
  
  coeff<-coef(nreg)
  
  out<-list(coeff, K600fromO2(mean(temp), coeff[2]))
  out
}
#####End of function

call as	
nightreg(spring[ spring $dtime>=as.numeric(chron(dates="10/27/14", times="18:05:00")) & spring $dtime<=as.numeric(chron(dates="10/27/14", times="23:00:00")), ], bp=595, ts=10)



###a generic one station model
#Model to calculate GPP, ER and K simultaneously.
#This model is advantageous in the sense that one can estimate K from the data, but beware that it may be an overparameterized model.  Note that you have the opportunity below to use your light data (coded data$light) or modeled light ($modlight).  You decide and modify the function with a #
#z is depth (m), bp is baro press (mm Hg), ts is time step (d), oxy is DO (g m-3)

rivermetabK<-function(o2file, z, bp, ts, mlefunction, plotfunction){
  
  ##pull data out of loaded file and give it a name. 
  
  temp<-o2file$temp
  oxy<-o2file$oxy
  #light<-o2file$light
  light<-o2file$modlight
  
  
  ##calculate metabolism by non linear minimization of MLE function (below)
  river.mle<-nlm(mlefunction, p=c(3,-5, 10), oxy=oxy, z=z,temp=temp,light=light, bp=bp, ts=ts)
  
  ##plot data
  plotfunction(GPP=river.mle$estimate[1], ER=river.mle$estimate[2],oxy=oxy,z=z,temp=temp,light=light, K=river.mle$estimate[3], bp=bp, ts=ts)
  
  ##return GPP and MLE value
  b<-c(river.mle$estimate[1], river.mle$estimate[2],river.mle$estimate[3],  river.mle$minimum[1])
  b
  
}
# end of function



#function returns the likelihood value for given GPP ER and K (which is vector MET)
onestationmleK<-function(MET,temp, oxy, light, z, bp, ts) {
  
  
  metab<-numeric(length(data))
  
  metab[1]<-oxy[1]
  ##equation is same as Van de Bogert 2007 L&O methods
  for (i in 2:length(oxy)) {metab[i]<-metab[i-1]+((MET[1]/z)*(light[i]/sum(light)))+ MET[2]*ts/z+(Kcor(temp[i],MET[3]))*ts*(osat(temp[i],bp)-metab[i-1]) }
  ##below is MLE calculation
  sqdiff<-(oxy-metab)^2 
  length(oxy)*(log(((sum(sqdiff)/length(oxy))^0.5)) +0.5*log(6.28))   + ((2*sum(sqdiff)/length(oxy))^-1)*sum(sqdiff)
}
#end of function

##function plots model and data from fixed GPP and ER
onestationplot<-function(GPP, ER, oxy, z, temp, K, light, bp, ts) {
  
  
  metab<-numeric(length(oxy))
  metab[1]<-oxy[1]
  for (i in 2:length(oxy)) { metab[i]<-metab[i-1]+((GPP/z)*(light[i]/sum(light)))+ ER*ts/z+(Kcor(temp[i],K))*ts*(osat(temp[i],bp)-metab[i-1]) }
  
  
  plot(seq(1:length(oxy)),metab, type="l",xlab="Time", ylab="Dissolved oxygen  (mg/L)", cex.lab=1.5, cex.axis=1.5, lwd=2 )
  points(seq(1:length(oxy)),oxy)
  
}
#end of function



###Call as
rivermetabK(o2file=spring[ spring $dtime>=as.numeric(chron(dates="10/27/14", times="22:00:00")) & spring $dtime<=as.numeric(chron(dates="10/29/14", times="06:00:00")), ], z=0.18, bp=595, ts=0.006944, mlefunction= onestationmleK, plotfunction=onestationplot)


rivermetabK(o2file=french[french$dtime>=as.numeric(chron(dates="09/15/12", times="22:00:00")) & french $dtime<=as.numeric(chron(dates="09/17/12", times="06:00:00")), ], z=0.18, bp=595, ts=0.003422, mlefunction= onestationmleK, plotfunction=onestationplot)


####Here is same function but for fixed and known K600.  Say you estimated K600 from nightreg or gas tracer.  Or you made it up
rivermetab<-function(o2file, z, bp, ts, K, mlefunction, plotfunction){
  
  ##pull data out of loaded file and give it a name. 
  
  temp<-o2file$temp
  oxy<-o2file$oxy
  #light<-o2file$light
  light<-o2file$modlight
  
  
  ##calculate metabolism by non linear minimization of MLE function (below)
  river.mle<-nlm(mlefunction, p=c(3,-5), oxy=oxy, z=z,temp=temp,light=light, bp=bp, ts=ts, K=K)
  
  ##plot data
  plotfunction(GPP=river.mle$estimate[1], ER=river.mle$estimate[2],oxy=oxy,z=z,temp=temp,light=light, K=K, bp=bp, ts=ts)
  
  ##return GPP and MLE value
  b<-c(river.mle$estimate[1], river.mle$estimate[2],  river.mle$minimum[1])
  b
  
}
# end of function


#function returns the likelihood value for given GPP ER and K (which is vector MET)
onestationmle<-function(MET,temp, oxy, light, z, bp, ts, K) {
  
  
  metab<-numeric(length(data))
  
  metab[1]<-oxy[1]
  
  for (i in 2:length(oxy)) {metab[i]<-metab[i-1]+((MET[1]/z)*(light[i]/sum(light)))+ MET[2]*ts/z+(Kcor(temp[i],K))*ts*(osat(temp[i],bp)-metab[i-1]) }
  ##below is MLE calculation
  sqdiff<-(oxy-metab)^2 
  length(oxy)*(log(((sum(sqdiff)/length(oxy))^0.5)) +0.5*log(6.28))   + ((2*sum(sqdiff)/length(oxy))^-1)*sum(sqdiff)
}
#end of function


#Call as:
rivermetab(o2file=spring[ spring $dtime>=as.numeric(chron(dates="10/27/14", times="22:00:00")) & spring $dtime<=as.numeric(chron(dates="10/29/14", times="06:00:00")), ], z=0.18, bp=595, K=29, ts=0.006944, mlefunction= onestationmle, plotfunction=onestationplot)

rivermetab(o2file=french[french$dtime>=as.numeric(chron(dates="09/15/12", times="22:00:00")) & french $dtime<=as.numeric(chron(dates="09/17/12", times="06:00:00")), ], z=0.18, bp=595, ts=0.003422, K=35, mlefunction= onestationmle, plotfunction=onestationplot)



