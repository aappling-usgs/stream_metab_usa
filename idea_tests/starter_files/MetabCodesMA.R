

####PACKAGES
library(chron)
library(zoo)
library(xts)
library(plyr)


##1. Create a data frame with dtime, day, time of day, julian day, temperature, oxygen, discharge and light
#In our case, data every 10 minutes. Example for 1997
dtime<-seq(chron("01/01/1997","00:00:00"),chron("12/31/1997","23:55:00"),by=10/(24*60))
day<-as.Date(trunc(dtime))
timeofday<-as.numeric(dtime-trunc(dtime))
jday<-as.numeric(trunc(dtime)-as.numeric(as.Date("1997-01-01")))
#aizarna = name of one of our stations
aizarna<-data.frame(dtime,day,timeofday,jday,temp,oxy,Q,light)


##2. Add columns indicating 10 pm (jday+1) and 6 am (jday-1) --> metab from 10 pm previous day to 6 am next day
mday<-ifelse(aizarna$timeofday>=0.9132 & aizarna$timeofday<= 0.9201,aizarna$jday+1,0)
sday<-ifelse(aizarna$timeofday>=0.2465 & aizarna$timeofday<= 0.2535,aizarna$jday-1,0)


##3. Create a data frame indicating sunrise and sunset times each day. Need to round times to make them match with sampling times.
#srss is a data frame with date, sunrise and sunset times, but format is not correct --> correct
srss$srdtime<-chron(dates=as.character(srss$date),times=paste(as.character(srss$SunRise),":00"))
srss$ssdtime<-chron(dates=as.character(srss$date),times=paste(as.character(srss$SunSet),":00"))
#Round times. "Problem" with align.time: when it's :10,:20,:30,:40,:50 goes to the next ten minutes
srss$roundedsrdtime<-as.chron(align.time(as.POSIXlt(srss$srdtime,tz="GMT"),n=10*60))
srss$roundedssdtime<-as.chron(align.time(as.POSIXlt(srss$ssdtime,tz="GMT"),n=10*60))


##4. Create a vector that will serve as a reference for subseting the data frame and take only night data to calculate regressions
#This indicates sunset position in the data frame
positions<-as.numeric(na.omit(match(srss$roundedssdtime,aizarna$dtime)))       
#Vector of 1s. Length: 6 data/hour (in our case) * hours to be taken. So this should be modified depending on hou many data you want to include in the regression, starting from sunset
vec1<-rep(1,6*8)   
#Create next vector and bind columns
nextvec<-vec1+1
vec2<-cbind(vec1,nextvec)
#Create a list with vec1, whose name will be the first row in vector "positions", which indicates sunset time on the first day
list1<- setNames(list(vec1), positions[1])
#Create another list with the vector in the next column of the matrix and the number in the next row of the vector "positions" as the name. Bind lists
nextlist<-setNames(list(vec2[,ncol(vec2)]), positions[ncol(vec2)])
list2<-c(list1,nextlist)
#Loop. We obtain a big list. Names indicate in which row of the main data frame the first data after sunset is on conscutive days, and they contain vectors with consecutive numbers (1,2,3...) with the same length, that depends on the number of data we want to include in the regression
for (i in 3:length(positions)) {                      
	nextvec<-vec2[,ncol(vec2)]+1
	vec2<-cbind(vec2,nextvec)
	nextlist<-setNames(list(vec2[,ncol(vec2)]),positions[ncol(vec2)])
	list2<-c(list2,nextlist) 
	}


##5. Using the list we just created, insert a column to our data frame
#Create the object to be inserted, a vector of NAs
nreg<-vector(length=dim(aizarna)[1])    
#Insert the vectors in the list in the new vector of NAs (nreg), in the positions determined by the names in the list 
for (i in seq_along(list2)) {
	nreg<- append(nreg, list2[[i]], after = as.numeric(names(list2)[[i]])-1) 
	}
#Cut the NA tail to add it to the main data frame
aizarna$nreg<-nreg[1:dim(aizarna)[1]]	


##6. Subset the data frame and only take data from sunset to sunset time+x hours (8 in the example given)
aizarnanreg<-aizarna[aizarna$nreg>0,]


##7. NIGHT TIME REGRESSION:

#F1: Function to calculate oxygen saturation given temperature and BP (mmHg). From Garcia and Gordon 1992 L&O
osat  <- function(temp,bp){
	sato<-(exp(2.00907 + 3.22014 * (log((298.15-temp) / (273.15 + temp))) + 4.0501 * (log((298.15 - temp) / (273.15 + temp))) ^ 2 + 4.94457 * (log((298.15 - temp) / (273.15 + temp))) ^ 3 - 0.256847 * (log((298.15 - temp) / (273.15 + temp))) ^ 4 + 3.88767 * (log((298.15 - temp) / (273.15 + temp))) ^ 5)) * 1.4276 * bp / 760
	sato
	}

#F2: Returns the intercept and the slope of the night time regression
nightreg<-function(o2file, bp){  

	temp<-o2file$temp
	oxy<-o2file$oxy
	
	#Smooth O2 data and calculate dC/dt
	oxyf1<-filter(o2file$oxy,rep(1/3,3), sides=2)
	oxyf2<- oxyf1[c(-1,-length(oxyf1))]
	deltaO2<-((oxyf2[-1]-oxyf2[-length(oxyf2)])/10)*1440

	#Calculate saturation deficit (Cs-C)
	temptrim<-temp[c(-2:-1,-length(temp))]
	satdef<-osat(temptrim,bp)-oxyf2[-1]

	#This is a bit arbitrary. We have data every ten minutes and decided to include 8 hours for night time regressions, so this would be 48 points for each regression. We decided to exclude regressions with less than 40 points, but this depends on how many hours you consider and on the number of data during each hour.
	#Returns a list inside a list, easier to use with the functions we need afterwards.
	out<-ifelse(length(na.omit(deltaO2))<40,list(c(NA,NA)),ifelse(length(na.omit(satdef))<40,list(c(NA,NA)),lm(deltaO2~satdef)))  
	out
	}

##F3: Returns the p value of the night time regression
nightregP<-function(o2file, bp){  

	temp<-o2file$temp
	oxy<-o2file$oxy

	#Smooth O2 data and calculate dC/dt
	oxyf1<-filter(oxy,rep(1/3,3), sides=2)
	oxyf2<- oxyf1[c(-1,-length(oxyf1))]
	deltaO2<-((oxyf2[-1]-oxyf2[-length(oxyf2)])/10)*1440

	#Calculate saturation deficit (Cs-C)
	temptrim<-temp[c(-2:-1,-length(temp))]
	satdef<-osat(temptrim,bp)-oxyf2[-1]

	#Again, the same decision: at least 40 points. I observed that sometimes instead of NAs we had exactly the same O2 concentration during the whole night, so included another condition, saying the sum of dC/dt should be >0
	pvalue<-ifelse(length(na.omit(deltaO2))<40,list(NA),ifelse(length(na.omit(satdef))<40,list(NA),ifelse(sum(deltaO2,na.rm=T)==0,list(NA),list(summary.lm(lm(deltaO2~satdef))$coefficients["satdef","Pr(>|t|)"]))))  
	pvalue
	}

##F4: Converts K to K600
K600fromO2<- function (temp,KO2) {
	((600/(1800.6-(120.1*temp)+(3.7818*temp^2)-(0.047608*temp^3)))^-0.5)*KO2
	}

#Calculate the regression independently for each night. We obtain the intercept, slope and p value
nregout<-dlply(aizarnanreg, .(aizarnanreg$nreg), function(x) nightreg(o2file = x, bp=756)) 
p<-dlply(aizarnanreg, .(aizarnanreg$nreg), function(x) nightregP(o2file = x, bp=756))
#Take these values out of the list (for me it's easier to work with data frames)
nregslope<-matrix(unlist(nregout),ncol=2,byrow=TRUE)
p<-matrix(unlist(p),ncol=1,byrow=TRUE)
nregslope<-data.frame(nregslope[,1],nregslope[,2],p)
colnames(nregslope)<-c("intercept","slope","p")
nregslope$day<-as.Date(seq.dates("01/01/1997","12/31/1997","days"))
#These filters are also arbitrary. We decided to regressions with p > 0.05 and negative values
filterK<-ifelse(nregslope$p>0.05,NA,nregslope$slope)
nregslope$filterK<-ifelse(filterK<0,NA,nregslope$slope)
#In the new data frame include the mean temperature and discharge measured during the time frame used for regressions
nregslope$Qn<-tapply(aizarnanreg$Q,aizarnanreg$nreg,mean,na.rm=T)
nregslope$tempn<-tapply(aizarnanreg$temp,aizarnanreg$nreg,mean,na.rm=T)
#Convert K to K 600 
nregslope$K600<-K600fromO2(nregslope$tempn,nregslope$filterK)


##8. Analyze the relationship between discharge and K600
#This was done with the whole time series, not year by year
NAOUTnregslope<-na.omit(nregslope)
smspl<-smooth.spline(NAOUTnregslope$Qn,NAOUTnregslope$K600,df=10)


##9. METABOLISM

#F5: Converts K600 to value at specific temp by Schmidt number scaling
Kcor<-function (temp,K600) {
	K600/(600/(1800.6-(temp*120.1)+(3.7818*temp^2)-(0.047608*temp^3)))^-0.5
	}

#F6: function returns the likelihood value for given GPP and ER (which is vector MET). 
aizarnamle<-function(MET,o2file, bp, Kq, ts, day) {

	#Subset the data frame to calculate metab from 10pm on the previous day to 6am on the next day
	tenpm<-match(day,o2file$mday)
	sixam<-match(day,o2file$sday)
	oxyday<-o2file[tenpm:sixam, ]

	temp<-oxyday$temp
	oxy<-oxyday$oxy
	#We had light data in a nearby station, but used modelled data in case we had NA. If you only have modelled data: light<-oxyday$light
	light<-ifelse(rep(NA %in% oxyday$light == T,dim(oxyday)[1]), oxyday$mdllight, oxyday$light)
	#We had very exceptional NAs in discharge, so used the mean discharge of the day to replace them
	oxyday$Q[is.na(oxyday$Q)]<-mean(oxyday$Q,na.rm=T)
	#We had functions to calculate depth from discharge in all the stations
	z<-zcalc(disch=oxyday$Q)
	Qday<-mean(oxyday$Q,na.rm=T)
	#Kq will be defined as the smooth.spline function built with all the data that determines K as a function of discharge. Therefore, this predicts K basing on the relationship between K and Q and the mean Q during the day.
	K<-predict(Kq,Qday)$y  

	metab<-numeric(length(oxy))
	metab[1]<-oxy[1]

	## below eqn is exactly Van de Bogert 2007
	for (i in 2:length(oxy)){
	metab[i]<-metab[i-1]+((MET[1]/z[i])*(light[i]/sum(light)))+ MET[2]*ts/z[i]+(Kcor(temp[i],K))*ts*(osat(temp[i],bp)-metab[i-1])}
	##below is MLE calculation
	sqdiff<-(oxy-metab)^2 
	length(oxy)*(log(((sum(sqdiff)/length(oxy))^0.5))+0.5*log(6.28))   + ((2*sum(sqdiff)/length(oxy))^-1)*sum(sqdiff)
	}

#Create a matrix where GPP, ER and MIN will be inserted. Length: number of days = number of rows/number of data each day. Columns: 3
GPPERmaQ<-matrix(0,dim(aizarna)[1]/144,3)
#Loop
for (i in 1:363){
	#Subset our data frame from 10 pm previous day to 6 am next day, so that we can exclude days with NAs
	tenpm<-match(i,aizarna$mday)
	sixam<-match(i,aizarna$sday)
	oxyday<-aizarna[tenpm:sixam, ]

	#Data taken every ten minutes from 10 pm to 6 am means 193 rows. So we excluded days even with a single NA. This could be improved by filling short gaps (few NAs?) in the main data frame. Don't know what to do with bigger gaps.
	GPP<-ifelse(length(na.omit(oxyday$oxy))<193,NA,ifelse(length(na.omit(oxyday$temp))<193,NA,nlm(aizarnamle, p=c(2,20), o2file=aizarna, bp=756, Kq=smspl, ts=10/1440, day=i)$estimate[1]))
	ER<-ifelse(length(na.omit(oxyday$oxy))<193,NA,ifelse(length(na.omit(oxyday$temp))<193,NA,nlm(aizarnamle, p=c(2,20), o2file=aizarna, bp=756, Kq=smspl, ts=10/1440, day=i)$estimate[2]))
	MIN<-ifelse(length(na.omit(oxyday$oxy))<193,NA,ifelse(length(na.omit(oxyday$temp))<193,NA,nlm(aizarnamle, p=c(2,20), o2file=aizarna, bp=756, Kq=smspl, ts=10/1440, day=i)$minimum))

	GPPERmaQ[i,] <-c(GPP, ER, MIN)
	}



