rm(list=ls())


install.packages('gdata')
install.packages('ggplot2')
install.packages('reshape')
install.packages('plyr')
library(gdata)
library(ggplot2)
library(reshape)
library(plyr)

runningData=read.csv('../data/cardioActivities.csv', as.is=T)
runningData=subset(runningData,Distance..mi.>0) #weeds out strength training workouts

#runningData$AvgPace=as.numeric(runningData$Average.Pace)
#runningData$Date2=as.Date(runningData$Date)
#if(grep(runningData$Date, pattern="2016", value=F)){print("butts")}
#runningData$Year=substr(runningData$Date,1,4)
runningData$Date = substr(runningData$Date,1,10)
runningData$Date = as.Date(runningData$Date)
runningData$Month = as.Date(cut(runningData$Date, breaks="month"))


#runningData$Distance..mi.

#converting Average Pace to a numerical value
#runningData$AvgPaceNumeric = strsplit(runningData$Average.Pace,':')
avgpace = as.data.frame(strsplit(runningData$Average.Pace,':'), stringsAsFactors = F, row.names = c("minutes","seconds"))

avgpace = sapply(avgpace, 
                      function(x){
                        x = as.numeric(x)
                        x = x[1]+(x[2]/60)
                        })
runningData$AvgPaceNumeric = as.numeric(avgpace)

#runningData$AvgPaceNumericTotal = cumsum(runningData$AvgPaceNumeric)
#runningData$AvgPaceTotalNorm = runningData$AvgPaceNumericTotal/sum(runningData$AvgPaceNumeric)

#runningData$Distance..mi.Total = cumsum(runningData$Distance..mi.)
#runningData$Distance..mi.TotalNorm = runningData$Distance..mi.Total/sum(runningData$Distance..mi.)

runningData$DistanceQuantiles = cut(runningData$Distance..mi., breaks = quantile(runningData$Distance..mi., include.lowest = T))
runningMonthly=ddply(runningData, .(DistanceQuantiles, Month), function(x) data.frame(distance=sum(x$Distance..mi.), pace=sum(x$AvgPaceNumeric)))

ggplot(data=runningData, aes(x=Date, y=AvgPaceNumeric))+
  geom_point()+
  geom_smooth(method='lm')

ggplot(data=subset(runningData,Distance..mi.<10 & Distance..mi.>2.5), aes(x=Date, y=Distance..mi.))+
  geom_point()

ggplot(data=runningData, aes(x=Average.Speed..mph., y=AvgPaceNumeric))+
  geom_point()+
  geom_smooth(method='lm')


