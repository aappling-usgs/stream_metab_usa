
#One station metabolism code.  Tius code provides all of the function necessary to estimate metaboism.  It give you light data if you need at.  It also allows calculating nighttime regression.  The methods to estimate metabolism are via non-linear minimization of the -log likelhood of the model vs the data.  There are two appproaches, one with unknown gas exchange (K), and one with known.  Most will use the unknown K, with the caveat that solving for 3 parameters may give an over parameterized model, and thus uncertain estimates of ER (GPP is fairly robust to uncertainty in K).

#I also include 2 data files

#Bob Hall 13 November 2014.


#Call the data.  Use you own path.
spring<-read.csv("/Users/bobhall/Rivers/spring14.csv")  ##data from Spring Creek WY, across from my house.  Used PME
french<-read.csv("/Users/bobhall/O2 modeling erin/french1.csv") ##data from French Creek, Hotchkiss and Hall, In press, Ecology
french<-french[french$station=="low",]##Getting data from only one station

##look at the files
head(spring)
head(french)

###First make make date and time a chron object. Load package chron from the R website and look online for how it works  How you do make a chron object depends on your data.  PME gives data in seconds since 1970 whichks makes for a really easy translation into a chron object.  Hydrolabs are a bit tougher.  Below are two ways of doing it.

spring$dtime<-chron(spring$Time/86400)-(6/24)

french$dtime<-chron(dates=as.character(french $date), times=as.character(french $time))

plot(french$dtime, french$oxy)

##Function to calculate oxygen saturation given temperature and BP (mmHg). From Garcia and Gordon 1992 L&O

osat  <- function(temp,bp){
	sato<-(exp(2.00907 + 3.22014 * (log((298.15-temp) / (273.15 + temp))) + 4.0501 * (log((298.15 - temp) / (273.15 + temp))) ^ 2 + 4.94457 * (log((298.15 - temp) / (273.15 + temp))) ^ 3 - 0.256847 * (log((298.15 - temp) / (273.15 + temp))) ^ 4 + 3.88767 * (log((298.15 - temp) / (273.15 + temp))) ^ 5)) * 1.4276 * bp / 760
	
	sato
}

##correct for altitude.  From Colt's book
###temp is degC, alt is m, and bpst is in inches of Hg.  Temp is usually relative to a standard, 15 degC.  
bpcalc<- function(bpst, alt, temp) {

bpst*25.4*exp((-9.80665*0.0289644*alt)/(8.31447*(273.15+temp)))

}
bpcalc(29.92,2400,15)


bpcalc(30,53,20)
# [1] 757.3081


## convert K600 to value at specific temp by Schmidt number scaling
Kcor<-function (temp,K600) {
	K600/(600/(1800.6-(temp*120.1)+(3.7818*temp^2)-(0.047608*temp^3)))^-0.5
	}
	
##estimate K600 from KO2  needed for nightime regression code:
K600fromO2<-function (temp, KO2) 
{
    ((600/(1800.6 - (120.1 * temp) + (3.7818 * temp^2) - (0.047608 * 
        temp^3)))^-0.5) * KO2
}

	
	
	
	
	###nighttime regression code
	#o2 file is your oxygen data, bp is baro press in mm Hg for your site, ts is the time step in MINUTES
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

	
	
	nightreg(spring[ spring $dtime>=as.numeric(chron(dates="10/27/14", times="18:05:00")) & spring $dtime<=as.numeric(chron(dates="10/27/14", times="23:00:00")), ], bp=595, ts=10)
	
	nightreg(french[french$dtime>=as.numeric(chron(dates="09/15/12", times="19:40:00")) & french$dtime<=as.numeric(chron(dates="09/15/12", times="23:00:00")), ], bp=523, ts=5)
	


###now make up light data if you don't have it


##From Yard et al. 1995, Ecological modelling.  Remember your trig?  
##calculate light as umol photon m-2 s-1. Arguments are:  a date and time input (i.e. a chron object) lat=latitude of field site, longobs = longitude of field site.  longstd = standard longitude of the field site (note, watch daylight savings time!!!).  For PST, the standard longitude would be 120 degrees.  But during PDT it is 105 degrees. MST is 105 deg. MDT is 90 
#  year is the year for which you collected data and is entered as "2013-01-01"
#convert degrees to radians
radi<-function(degrees){(degrees*pi/180)}

lightest<- function (time, lat, longobs, longstd, year ) {
	
	jday<-as.numeric(trunc(time)-as.numeric(as.Date(year)))
	
	
	E<- 9.87*sin(radi((720*(jday-81))/365)) - 7.53*cos(radi((360*(jday-81))/365)) - 1.5*sin(radi((360*(jday-81))/365))

LST<-as.numeric (time-trunc(time))

ST<-LST+(3.989/1440)*(longstd-longobs)+E/1440

solardel<- 23.439*sin(radi(360*((283+jday)/365)))

hourangle<-(0.5-ST)*360

theta<- acos(  sin(radi(solardel)) * sin(radi(lat)) +  cos(radi(solardel)) * cos(radi(lat)) *  cos(radi(hourangle)) )

suncos<-ifelse(cos(theta)<0, 0, cos(theta))

GI<- suncos*2326
GI	
	
}
#end of function

##onespecific time just to see if function works
lightest(time=chron(15500.5), lat=41.33,  longobs=105.7, longstd= 105, year="2013-01-01")

spring$modlight<- lightest(time=spring$dtime, lat=41.33,  longobs=105.6, longstd= 90, year="2014-01-01")
french$modlight<- lightest(time=french$dtime, lat=41.33,  longobs=106.3, longstd= 90, year="2012-01-01")


#####now estimate metabolism


#Model to calculate GPP, ER and K simultaneously.
#This model is advantageous in the sense that one can estimate K from the data, but beware that it may be an overparameterized model.  Note that you have the opportunity below to use your light data (coded data$light) or modeled light ($modlight).  You decide and modify the function with a #

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

for (i in 2:length(oxy)) {metab[i]<-metab[i-1]+((MET[1]/z)*(light[i]/sum(light)))+ MET[2]*ts/z+(Kcor(temp[i],MET[3]))*ts*(osat(temp[i],bp)-metab[i-1]) }
	##below is MLE calculation
	 sqdiff<-(oxy-metab)^2 
length(oxy)*(log(((sum(sqdiff)/length(oxy))^0.5)) +0.5*log(6.28))   + ((2*sum(sqdiff)/length(oxy))^-1)*sum(sqdiff)
}


##function plots model and data from fixed GPP and ER
onestationplot<-function(GPP, ER, oxy, z, temp, K, light, bp, ts) {


metab<-numeric(length(oxy))
metab[1]<-oxy[1]
for (i in 2:length(oxy)) { metab[i]<-metab[i-1]+((GPP/z)*(light[i]/sum(light)))+ ER*ts/z+(Kcor(temp[i],K))*ts*(osat(temp[i],bp)-metab[i-1]) }


plot(seq(1:length(oxy)),metab, type="l",xlab="Time", ylab="Dissolved oxygen  (mg/L)", cex.lab=1.5, cex.axis=1.5, lwd=2 )
points(seq(1:length(oxy)),oxy)


}




###Call as
rivermetabK(o2file=spring[ spring $dtime>=as.numeric(chron(dates="10/27/14", times="22:00:00")) & spring $dtime<=as.numeric(chron(dates="10/29/14", times="06:00:00")), ], z=0.18, bp=595, ts=0.006944, mlefunction= onestationmleK, plotfunction=onestationplot)


rivermetabK(o2file=french[french$dtime>=as.numeric(chron(dates="09/15/12", times="22:00:00")) & french $dtime<=as.numeric(chron(dates="09/17/12", times="06:00:00")), ], z=0.18, bp=595, ts=0.003422, mlefunction= onestationmleK, plotfunction=onestationplot)



####Here is same function but for fixed and known K600
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









#### 1 station Bayesian models. Much tougher to do, but has benefit of estimating uncertainty.  You will need to know a bunch about MCMC to use this because you need to uses "scale" to set the proposal distribution correctly.  Rule of thumb is 20% acceptance rate.
rivermetabpost1<-function(MET,data,z, bp, ts, Kmean, Ksd) {
	oxy<-data$oxy
	temp<-data$temp
	#light<-data$light
	light<-data$modlight
	
	GPP<-MET[1]
	ER<- MET[2]
	K<- MET[3]
	sigma<-exp(MET[4])
	metab<-numeric(length(data))
	
metab[1]<-oxy[1]
## below eqn is exactly Van de Bogert 2007
for (i in 2:length(oxy)) {metab[i]<-metab[i-1]+((GPP/z)*(light[i]/sum(light)))+ ER*ts/z+(Kcor(temp[i],K))*ts*(osat(temp[i],bp)-metab[i-1]) }


	##below is MLE calculation
	 loglik<- sum(dnorm(oxy,metab,sigma, log=TRUE)) 
	 #and prior
	 prior<-log(dnorm(GPP,mean=10,sd=10))+log(dnorm(ER,mean=-3,sd=10))+log(dnorm(K,mean=Kmean,sd=Ksd))	 
	 
	 #posterior is product of L and prior
	 loglik+prior
	 
}
#end of function

##call as:  See documention on this metrop function--it requires some babying. Starting values matter a lot.
met.post<-metrop(rivermetabpost1, initial=c(1.9, -2.7, 25, -2.2), nbatch=20000, scale=0.03, data=spring[ spring $dtime>=as.numeric(chron(dates="10/27/14", times="22:00:00")) & spring $dtime<=as.numeric(chron(dates="10/29/14", times="06:00:00")), ], z=0.18, bp=595, ts=0.006944, Kmean=30, Ksd=10)

#Here how to look at output of the MCMC
met.post$accept

met.post$time
plot(ts(met.post$batch))
quantile(met.post$batch[,1], c(0.025, 0.5, 0.975))
quantile(met.post$batch[,2], c(0.025, 0.5, 0.975))
quantile(met.post$batch[,3], c(0.025, 0.5, 0.975))
quantile(met.post$batch[,4], c(0.025, 0.5, 0.975))






	
	
	