#title: CaseStudy
#author: Vishi Cline
#date: November 22, 2016

#install.packages("tseries")
library(tseries)
library(ggplot2)
library(xts)

#Create matrix with R
X <- matrix( 
  c(4,5,1,2,1,0,3,5,2,1,8,2), # the data elements 
  nrow=3,              # number of rows 
  ncol=4,              # number of columns 
  byrow = TRUE)
print(X)

#For XOM, do the following:
# Download the data.
#	Calculate log returns.
#	Calculate volatility measure.
#	Calculate volatility over entire length of series for the different decay factors.
#	Plot the results, overlaying the volatility curves on the data, just as was done in the S&P example.</b>
  
#Download the data for stocks relating to TC Pilpelines.
SNPdata<-get.hist.quote(instrument = 'XOM',quote="Close")
length(SNPdata)

#We used log return as opposed to percent return because log returns are symmetric with respect to gains and losses. They are also additive.
SNPret<-log(lag(SNPdata))-log(SNPdata)
length(SNPret)

#Get the running estimate of how much change has occured using sample standard deviation.
SNPvol<-sd(SNPret) * sqrt(250)*100
SNPvol

#The following function exponentially downweights the older data. It calculates the variance as the weighted sum of squares of the previous returns and then takes the square root to estimate the volatility.
Vol<-function(d,logrets){
  var=0
  lam=0
  varlist<-c()
  for (r in logrets){
    lam=lam*(1-1/d)+1
    var=(1-1/lam)*var+(1/lam)*r^2
    varlist<-c(varlist,var)
  }
  sqrt(varlist)
}

#volatility with different weights
volest<-Vol(10, SNPret)
volest2<-Vol(30, SNPret)
volest3<-Vol(100, SNPret)


#Plot the results
plot(volest, type="l")
lines(volest2, type="l", col="red")
lines(volest3, type="l", col="blue")

#Calculate the mean and the median of the trunk circumferences for different size of the trees. (Tree)
tapply(Orange$circumference, Orange$Tree, mean)
tapply(Orange$circumference, Orange$Tree, median)

#Make a scatter plot of the trunk circumferences against the age of the tree. Use different plotting symbols for different size of trees.
ggplot(Orange, aes(x=factor(age), y=circumference, shape=factor(Tree))) + geom_point()

#Display the trunk circumferences on a comparative boxplot against tree. Be sure you order the boxplots in the increasing order of maximum diameter.
NewOrange<-Orange
NewOrange$Diameter<-(NewOrange$circumference/pi)
NewOrange<-NewOrange[order(NewOrange$Diameter), ]
ggplot(NewOrange, aes(x=factor(Tree), y=circumference))+geom_boxplot(fill="gray")+
  labs(title="Plot of circumference by tree",x="Tree", y = "Circumference")+
  theme_classic()

#Using the "Temp" data set:  
#Find the difference between the maximum and the minimum monthly average temperatures for each country and report/visualize top 20 countries with the maximum differences for the period since 1900.

#Download data
temps<-read.csv("C:/Homework/R/caseStudy2/Temp.csv", stringsAsFactors = FALSE, header = TRUE)

#Replace all blanks with NA's
temps[temps==""] <- NA

#Remove NA's from Date and AverageTemp columns
temps <- temps[!is.na(temps$Date),]
temps <- temps[!is.na(temps$Monthly.AverageTemp),]

#Convert the dates to a consistent format
dmy <- as.Date(temps$Date,format="%d/%m/%Y")
ydm <- as.Date(temps$Date,format="%Y-%d-%m") 
dmy[is.na(dmy)] <- ydm[!is.na(ydm)] 
temps$Date <- dmy

#Filter data for period>=1900
ds1900<-subset(temps, (temps$Date>="1900-01-01"))

#Get the min and max average temps by Country
minTemps<-aggregate(Monthly.AverageTemp~Country,ds1900,min)
maxTemps<-aggregate(Monthly.AverageTemp~Country,ds1900,max)

#Merge the data by Country
MergedData<-merge(minTemps,maxTemps, by=c("Country"))

#Calculate the diff between the maximum and minimum monthly average temperatures.
MergedData$Diff<-MergedData$Monthly.AverageTemp.y-MergedData$Monthly.AverageTemp.x

#Report top 20 countries with maximum average differences
top20<-head(MergedData[order(-MergedData[,4]),],20)
print(top20[,c("Country","Diff")])

#visualize top 20 countries with maximum average differences
ggplot(top20, aes(x = factor(Country), y = reorder(Diff,Diff,function(y)-length(y)), fill=factor(Country))) + geom_bar(stat = "identity")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.05))+
  xlab("Country") +
  ylab("Temperature Difference")+
  labs(fill="Country")+
  ggtitle("Top 20 countries with maximum differences")


#Select a subset of data and call it "UStemp" for US land temperatures starting from 01/01/1990 in Temp data. Use UStemp dataset to answer the following:
UStemp1990<-subset(temps, (temps$Date>="1990-01-01"))
UStemp<-subset(UStemp1990, (UStemp1990$Country=="United States"))

#Create a new column to display the monthly average land temperatures in Fahrenheit (°F).
UStemp$Fahrenheit<-((UStemp$Monthly.AverageTemp*1.8000)+32.00)

#Calculate average land temperature by year and plot it. The original file has the average land temperature by month. 
ts <- xts(UStemp$Monthly.AverageTemp, UStemp$Date)
ts_yearly = apply.yearly(ts,mean)
ts_yearly
plot(ts_yearly)

#Calculate the one year difference of average land temperature by year and provide the maximum difference (value) with corresponding two years.
OneYearDiff<-apply(ts_yearly,2,diff) 
print(OneYearDiff)
period.apply(OneYearDiff,INDEX=endpoints(OneYearDiff), FUN=max)

#Using the "CityTemp" data set:  
#Find the difference between the maximum and the minimum temperatures for each major city and report/visualize top 20 cities with maximum differences for the period since 1900. 
tempsCity<-read.csv("C:/Homework/R/caseStudy2/CityTemp.csv", stringsAsFactors = FALSE, header = TRUE)

#Replace all blanks with NA's
tempsCity[tempsCity==""] <- NA

#Remove NA's from Date and AverageTemp columns
tempsCity <- tempsCity[!is.na(tempsCity$Date),]
tempsCity <- tempsCity[!is.na(tempsCity$Monthly.AverageTemp),]

#Convert the dates to a consistent format
dmyCity <- as.Date(tempsCity$Date,format="%d/%m/%Y")
ydmCity <- as.Date(tempsCity$Date,format="%Y-%d-%m") 
dmyCity[is.na(dmyCity)] <- ydmCity[!is.na(ydmCity)] 
tempsCity$Date <- dmyCity 

#Filter data for period>=1900
ds1900tempsCity<-subset(tempsCity, (tempsCity$Date>="1900-01-01"))

#Get the min and max average temps by City
mintempsCity<-aggregate(Monthly.AverageTemp~City,ds1900tempsCity,min)
maxtempsCity<-aggregate(Monthly.AverageTemp~City,ds1900tempsCity,max)

#Merge the data by City
MergedDatatempsCity<-merge(mintempsCity,maxtempsCity, by=c("City"))

#Calculate the diff between the maximum and minimum monthly average temperatures.
MergedDatatempsCity$Diff<-MergedDatatempsCity$Monthly.AverageTemp.y-MergedDatatempsCity$Monthly.AverageTemp.x

#Report top 20 cities with maximum average differences
top20tempsCity<-head(MergedDatatempsCity[order(-MergedDatatempsCity[,4]),],20)
print(top20tempsCity[,c("City","Diff")])

#visualize top 20 cities with maximum average differences
ggplot(top20tempsCity, aes(x = factor(City), y = reorder(Diff,Diff,function(y)-length(y)), fill=factor(City))) + geom_bar(stat = "identity")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.05))+
  xlab("City") +
  ylab("Temperature Difference")+
  labs(fill="City")+
  ggtitle("Top 20 Cities with maximum differences")

#Compare the two graphs in (i) and (iii) and comment on your comparison

#Get a unique list of the cities and their corresponding countries
CC<-unique(tempsCity[,c("Country","City")])

#Merge the Countries with the City Data
ByCountry<-merge(top20tempsCity,CC,by=c("City"))

#Get the data for each country for only the top 20 cities
GetCountryData<-ds1900tempsCity[ds1900tempsCity$Country %in% ByCountry$Country,]

#Calculate min and max temps for countries in question
mintempsCountry<-aggregate(Monthly.AverageTemp~Country,GetCountryData,min)
maxtempsCountry<-aggregate(Monthly.AverageTemp~Country,GetCountryData,max)

#Merge the min and max data for countries
MergedDatatempsCountry<-merge(mintempsCountry,maxtempsCountry, by=c("Country"))

#Calculate Diff
MergedDatatempsCountry$CountryDiff<-MergedDatatempsCountry$Monthly.AverageTemp.y-MergedDatatempsCountry$Monthly.AverageTemp.x

#Merge with the City Data
ByCountry<-merge(ByCountry,MergedDatatempsCountry,by=c("Country"))

#Calculate the ratio of each city temperature difference with country temperature difference
ByCountry$Percentage<-(ByCountry$Diff/ByCountry$CountryDiff)

#Plot the results
ComparisonPlot<-ByCountry[order(-ByCountry[,5]),c("City","Country","Diff","CountryDiff","Percentage")]

print(ComparisonPlot)

ggplot(ComparisonPlot,aes(Country,Percentage,fill=City))+
  geom_bar(stat="identity",position="dodge")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.05))
