# R Duggan 3/27/18
# Data validation for Phil - importing a single HR text file, extracting warm up, workout, and cool-down, and splitting each
# into its own column

rm(list=ls())

#install.packages("RHRV") #currently these aren't being used for analysis (as of 3/29)
#library(RHRV)

library(lubridate)

logFile1 = read.csv('../data/LogFiles/MT13984DN_logfile_20171110-143813_hrm_w.txt', as.is=T)
subjectID = colnames(logFile1[1])
subjectID = substring(subjectID, regexpr('..name.', subjectID) + 7)

logFile1 = logFile1[,1] #remove unused empty variables


#need to differentiate between warm-up, workout, cooldown

logFile2 = logFile1[grepl('workout|bpm/r-r',logFile1)] #keep rows that contain 'bpm/r-r' or "workout" - preserves timestamps


logFile2 = gsub('|workout|Start', '|workout|Start|NA|NA|NA|NA', logFile2, fixed = T)
logFile2 = gsub('|workout|Init', '|workout|Init|NA|NA|NA|NA', logFile2, fixed = T)
logFile2 = gsub('|workout|Stop', '|workout|Stop|NA|NA|NA|NA', logFile2, fixed = T)

logFile2 = gsub('|workout|', '...workout...', logFile2, fixed = T) #replace |workout| w/ "..." so strsplit won't pick it up

logFile2 = strsplit(logFile2, split = '[/|]', fixed = F)   #split time, HR, and HRV (splits 'bpm' and 'r-r' labels too)




logFile2Length = length(logFile2)
logFile2 = data.frame(matrix(unlist(logFile2), nrow=logFile2Length, byrow=T))
logFile2[,2] = NULL
logFile2[,2] = NULL #removes bpm + r-r columns
logFile2[logFile2 == "NA"] = NA
logFile2$X4 = as.numeric(as.character(logFile2$X4))   # convert to numeric values
logFile2$X5 = as.numeric(as.character(logFile2$X5))   

colnames(logFile2) = c("Time", "HR (bpm)","HRV")

SDNN = function(x){
  sqrt(var(x, na.rm = T))
}

SDNNtest = NULL
SDNNtest$workout = SDNN(logFile2$HRV[242:455])
SDNNtest$warmup = SDNN(logFile2$HRV[1:241])
SDNNtest$cooldown = SDNN(logFile2$HRV[458:494])
SDNNtest$overall = SDNN(logFile2$HRV)
SDNNtest = as.data.frame(SDNNtest)

#write.csv(logFile2, file = "../output/dataOutputTest_3_28_18.csv")
#write.csv(SDNNtest, file = "../output/SDNNtestvalues.csv")

logFile2SDNN = sqrt(var(logFile2$HRV, na.rm = T))

######
# 4/10/18 - R Duggan
#####
workoutStartRow = which(grepl("workout...Start", logFile2$Time, ignore.case = T))
workoutEndRow = which(grepl("workout...Stop", logFile2$Time, ignore.case = T))



# using lubridate to convert time variable to class period, coming up with way to classify warm-up, workout, cooldown
# this should be done after splitting up logfile into $warmup, $workout, $cooldown...

#for (x in 1:length(logFile2$Time)) {
#  logFile2$Time = as.character(logFile2$Time[!grepl("...workout...", logFile2$Time)])   #this removes workout start/stop/etc lines
#  logFile2$Time = hms(logFile2$Time[!grepl("...workout...", logFile2$Time)])            #and converts times to char
#  }                                                                                   



warmupStartRow = (as.numeric(hms(as.character(logFile2$Time[241]))))-300

#THIS WORKS in returning all rows with time greater than the seconds specified!!!
#logFile2[as.numeric(hms(as.character(logFile2$Time)))>55950,]
warmup = logFile2[as.numeric(hms(as.character(logFile2$Time))) > warmupStartRow,]
warmupEndRow = which(grepl("workout...Start", warmup$Time, ignore.case = T))
warmup = warmup[0:warmupEndRow,]