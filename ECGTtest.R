
#Test using edfReader, 6.28.18

install.packages("edfReader")
library("edfReader")


ECGtest = paste (libDir, '/edfPlusC.edf' , sep='')


ECGtest = readEdfHeader('../data/RSCH013P25S1EyesOpenECG.edf')

ECGsignalTest = readEdfSignals(ECGtest)

plotEdfSignals <- function (signals,labels, from=0, till=Inf) {
  nLabels <- length (labels)
  sRate   <- numeric (length = nLabels)
  fromS   <- integer (length = nLabels)
  tillS   <- integer (length = nLabels)
  sLength <- integer (length = nLabels)
  for (i in 1:nLabels) {
    sRate[i]    <- signals[[labels[i]]]$sRate
    fromS[i]    <- ceiling (sRate[i] * max (from, 0)) +1
    tillS[i]    <- ceiling (sRate[i] * till)
    tillS[i]    <- min (tillS[i], length(signals[[labels[i]]]$signal))
    sLength[i]  <- tillS[i] - fromS[i] + 1 
  }
  totLength  <- sum (sLength)
  cat (" totLength=",  totLength)
  time    <- numeric   (length = totLength)
  signal  <- numeric   (length = totLength)
  label   <- character (length = totLength)
  from <- 1
  for (i in 1:nLabels) {
    till <- from + sLength[i] - 1
    time  [from:till]   <- seq (from=fromS[i]-1, to=(tillS[i]-1)) / sRate[i]
    signal[from:till]   <- signals[[labels[i]]]$signal[fromS[i]:tillS[i]]
    label [from:till]   <- rep(labels[i], sLength[i])
    from <- till + 1
  }
  cat (" | from-1=", from-1,'\n')
  
  ggplotDF <- data.frame (time=time, signal=signal, label=label)
  ggplot (ggplotDF, aes(x=time, y=signal, colour=label)) + geom_line()
}

if (require(ggplot2)) {
  CSignals <- readEdfSignals (CHdr)
  plotEdfSignals (CSignals, labels=c('sine 8.5 Hz', 'sine 50 Hz'), from=.2, till=0.5)
}

chart = ggplot(data=ECGsignalTest$`ECG ECG`, aes(x=))