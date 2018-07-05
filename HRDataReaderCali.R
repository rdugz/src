#####
# HR Data Extractor / Combiner v2
# R Duggan 4/2/18 - using code from HRDataValidation.R
#####

rm(list=ls())
install.packages('ggplot2')
#install.packages('Rmisc')  #loaded for multiplot function
install.packages("lubridate")
library(ggplot2)
#library(Rmisc)   #loaded for multiplot function
library(lubridate)

file.names = dir("../data/HRforCali/", pattern = ".xlsx", full.names = T)
#df.names = substring(file.names, regexpr('/LogFiles/', file.names) + 10, regexpr('_logfile', file.names) - 1) #keep only subject IDs

df.names = substring(file.names, regexpr('/data/HRforCali/', file.names) + 16, regexpr('_', file.names) -1 ) #keep only subject IDs - specifically for files for Cali


for(i in 1:length(file.names)){
  logfile = read.table(file.names[i], header = T, stringsAsFactors = F)
  
  logfile = logfile[,1] #remove unused empty variables
  
  #need to differentiate between warm-up, workout, cooldown
  logfile = logfile[grepl('workout|bpm/r-r',logfile)] #keep rows that contain 'bpm/r-r' or "workout" - preserves timestamps
  logfile = gsub('|workout|Start', '|workout|Start|NA|NA|NA|NA', logfile, fixed = T)
  logfile = gsub('|workout|Init', '|workout|Init|NA|NA|NA|NA', logfile, fixed = T)
  logfile = gsub('|workout|Stop', '|workout|Stop|NA|NA|NA|NA', logfile, fixed = T)
  logfile = gsub('|workout|', '...workout...', logfile, fixed = T) #replace |workout| w/ "..." so strsplit won't pick it up
  
  logfile = strsplit(logfile, split = '[/|]', fixed = F)   #split time, HR, and HRV (splits 'bpm' and 'r-r' labels too)
  logfile.length=length(logfile)
  logfile = data.frame(matrix(unlist(logfile), nrow=logfile.length, byrow=T))
  logfile[,2] = NULL
  logfile[,2] = NULL #removes bpm + r-r columns
  logfile[logfile == "NA"] = NA
  
  logfile$X1 = as.character(logfile$X1)   #convert time to character vector
  logfile$X4 = as.numeric(as.character(logfile$X4))   # convert to numeric values
  logfile$X5 = as.numeric(as.character(logfile$X5))   # "
  
  colnames(logfile) = c("Time", "HR","HRV") #following four lines add the subject ID to each column name 
  subjectID = df.names[i]
  subjectID = paste(subjectID, "_", sep = "")
  colnames(logfile) = paste(subjectID, colnames(logfile), sep = "")
  subjectID = NULL
  

  #  output = rbind(output, logfile) #adding logs to output file
  assign(paste(df.names[i],i,sep=''),logfile)    #to remove numbers at end of subject ID, keep first 9 chars of df name
   
  
}



ggplot(data = MT13984DN4, aes(x = MT13984DN_Time, y = MT13984DN_HR))+
  geom_path(aes(group = 1))