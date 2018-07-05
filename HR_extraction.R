
rm(list=ls())


log1 = read.csv('../data/LogFiles/logfile1.txt', as.is=T)
subjectID = colnames(log1[1])
subjectID = substring(subjectID, regexpr('..name.', subjectID) + 7)


log1 = log1[,1]


log1 = log1[grepl('bpm/r-r',log1)]

log2 = substring(log1, regexpr('bpm/r-r', log1) + 8)

log2 = strsplit(log2,'/')
log2.length=length(log2)

log3 = data.frame(matrix(unlist(log2), nrow=log2.length, byrow=T),stringsAsFactors=FALSE)
colnames(log3) = c("HR (bpm)","HRV")


#11/15/17 - next step - turn into dataframe / separate vectors / columns - add to spreadsheet - pull out subject ID from first line
#compile into master spreadsheet organized by subject?

#2/2/18 - trying out a for loop! a lot of the code taken from https://www.r-bloggers.com/looping-through-files/

file.names = dir("../data/LogFiles/", pattern = ".txt", full.names = T)
df.names = substring(file.names, regexpr('/LogFiles/', file.names) + 10, regexpr('_logfile', file.names) - 1) #keep only subject IDs
#output = "" #this is uncessary if not using rbind to add all looped info to same data frame

for(i in 1:length(file.names)){
  logfile = read.table(file.names[i], header = T, stringsAsFactors = F)
  
  logfile = logfile[,1]
  
  logfile = logfile[grepl('bpm/r-r',logfile)]
  
  
  logfile = strsplit(logfile,'/')

  logfile.length=length(logfile)
  logfile = data.frame(matrix(unlist(logfile), nrow=logfile.length, byrow=T),stringsAsFactors=FALSE)
  
  logfile[,1] = unlist(strsplit(logfile[,1], split = '|bpm', fixed=TRUE)) # this works because the stuff we want to keep is on the left side of the split  
  logfile[,2] = substring(logfile[,2], regexpr('r-r|', logfile[,2]) + 4)  # this works bc the stuff to keep is on the right side of the regexpr function? (don't really understand this)
  
  colnames(logfile) = c("Time", "BPM","HRV")
  logfile$BPM = as.numeric(logfile$BPM)
  logfile$HRV = as.numeric(logfile$HRV)
  
#  output = rbind(output, logfile) #adding logs to output file
  assign(paste(df.names[i],i,sep=''),logfile)    #to remove numbers at end of subject ID, keep first 9 chars of df name
#  print(df.names[i]) testing for loop

  }


# 3.26 next step is going to be to add in 5 minutes before / after workout start, and then combining everything into one output
# dataframe. So SUBJECT$BMP.warmup, SUBJECT$HRV.warmup, SUBJECT$BMP.workout, etc.