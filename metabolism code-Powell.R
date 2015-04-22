###here is some code adapted to the USGS data.  It is written in such a way as to allow looping.  There is tons of room for improvemnet here, and this may not at all be the way we analyze the data.  This code is rough draft at best.


##pick a river
##Canadian river, calumet OK 1380 feet, 723 mm

cana<- load_timeseries(site = "nwis_07239450", variable = c('wtr','doobs','disch'))
site_location("nwis_07239450")
head(cana)

cana$dtime<-chron((as.numeric(cana$DateTime)/86400)-(6/24))
bpcalc(bpst=760,alt=1400/3.28,temp=20)
head(cana)
tail(cana)

##cut out one year and invent some light data

cana2014<-cana[cana$dtime>as.numeric(chron(dates="01/01/14", times="00:00:00")) & cana$dtime < as.numeric(chron(dates="12/15/14", times="00:00:00")), ]
#light data
cana2014$light<-lightest(cana2014$dtime, 38.6, 95.0, 90, "2014-01-01")


###format code for looping MLE metabolism estimates for each day
###this adds a tag at 10 pm each daya telling the looping function where to start each new day
##only works on one yer, so improvement needed here

powformat<-function(data, date){

data$jday<-as.numeric(trunc(data $dtime)-as.numeric(as.Date(date)))
data$timeofday<-(as.numeric (data$dtime-trunc(data$dtime)))
data$mday<-ifelse(data$timeofday>=0.9132 & data$timeofday<= 0.9201,data$jday+1, 0 )
data
}

cana2014<-powformat(cana2014, "2014-01-01")


head(cana2014) #10
tail(cana2014)  #342

## look to see if it worked
cana2014[cana2014$mday > 0 , ] 


#function returns the likelihood value for given GPP and ER (which is vector MET) #day is julian day of the year
powmle<-function(MET,o2file, z, bp, ts, day) {

tenpm <- match(day, o2file$mday)
oxyday <- o2file[tenpm:(tenpm+(round(1.333/ts))), ]


	temp<-oxyday $ts_wtr
	oxy<-oxyday $ts_doobs
	light<-oxyday$light
	
	metab<-numeric(length(oxy))
	
metab[1]<-oxy[1]
## below eqn is exactly Van de Bogert 2007
for (i in 2:length(oxy)) {metab[i]<-metab[i-1]+((MET[1]/z)*((light[i]/sum(light)))+ MET[2]*ts/z+(Kcor(temp[i],MET[3]))*ts*(osat(temp[i],bp)-metab[i-1]) }
	##below is MLE calculation
	 sqdiff<-(oxy-metab)^2 
length(oxy)*(log(((sum(sqdiff)/length(oxy))^0.5)) +0.5*log(6.28))   + ((2*sum(sqdiff)/length(oxy))^-1)*sum(sqdiff)
}


###run one day and get parameters
powout<-nlm(powmle, p=c(2,20,20), o2file=poque, bp=760, z=0.2, day=258, ts=0.5/24)

###now look at the fit
powplot<-function(GPP, ER, K, o2file, z, bp, ts, day) {

 tenpm <- match(day, o2file$mday)
oxyday <- o2file[tenpm:(tenpm+(round(1.333/ts))), ]


	temp<-oxyday $ts_wtr
	oxy<-oxyday $ts_doobs
	light<-oxyday$light


metab<-numeric(length(oxy))
metab[1]<-oxy[1]
for (i in 2:length(oxy)) { metab[i]<-metab[i-1]+((GPP/z)*(light[i]/sum(light)))+ ER*ts/z+(Kcor(temp[i],K))*ts*(osat(temp[i],bp)-metab[i-1]) }


plot(oxyday$dtime,metab, type="l",xlab="Time", ylab="Dissolved oxygen  (mg/L)", cex.lab=1.5, cex.axis=1.5, lwd=2 )
points(oxyday$dtime,oxy)

}

###Doe it work?  try for one day
powout<-nlm(powmle, p=c(2,5,20), o2file=cana2014, bp=723, z=0.25, day=130, ts=0.5/24)

##and then plot the results

powplot(GPP=5.56, ER=-5.64, K=5.84, o2file=cana2014, bp=723, z=0.25, day=130, ts=0.021)

#now loop through all days
###THIS DOES NOT WORK with the data for  canadian river
# the loop quits wehn there is a break in the data.  I simply ran for all hunks of data.  Fine with one site.  The days I have in there now will work, but that is not all of the year.
  
GPPERK<-matrix(0, 365, 4)

for (i in 281:342) {

GPERK <- nlm(powmle, p=c(0,0,5), o2file=cana2014, bp=760, z=0.2, day=i, ts=0.5/24)

GPPERK[i,] <-c(GPERK$estimate, GPERK$minimum)
}

day <- c(1:365)
GPPERK

cana2014date<-as.Date(seq(from=10, to=342, by=1) + as.numeric(as.Date("2014-01-01")), origin="1970-01-01")
plot(seq(258:337), GPPERK[258:337,2], type="l")
plot(cana2014date, GPPERK[10:342,1],  ylab="GPP")
plot(cana2014date, GPPERK[10:342,3],  ylab="K")


plot(GPPERK[258:337,1], GPPERK[258:337,2], ylim=c(-5,1))

plot(cana2014$dtime, cana2014$ts_disch, pch=16, cex=0.5)
plot(cana2014$dtime, cana2014$ts_disch, pch=16, cex=0.5, log='y')

canamet<-GPPERK

save(canamet, file="/Users/bobhall/Powell metabolism synthesis/canamet.RData")
load("/Users/bobhall/Powell metabolism synthesis/canamet.RData")


