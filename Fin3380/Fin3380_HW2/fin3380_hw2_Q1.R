setwd("/Users/guangyaohe/Desktop/FIN3380/Fin3380_HW2")
library(zoo)
library(dplyr)
msft = read.csv("msft.csv")

BBands = function(dataName, window = 20 ){
  prc = dataName
  dataName.bb = data.frame(prc = prc, stringsAsFactors = T)
  dataName.bb = dataName.bb %>% 
    mutate(avg = rollmean(prc, window,
                          fill=NA,align='right'),
           sd = rollapply(prc, window,
                          FUN=sd,na.rm=T,fill=NA,align='right')) %>% 
    mutate(sd2up=avg+2*sd,
           sd2down=avg-2*sd)
  dataName.bb$sd = NULL
  return(dataName.bb)
}

msft = msft[,c("permno","date","prc")]
msft$date = as.Date(msft$date)
msft = msft %>% arrange(date)

head(BBands(msft$prc, window = 15))


#Here, I call the Function BBands()
msft.bb = BBands(msft$prc, window = 15)

head(msft.bb)

#Plot MSFT from the beginning to the end
yrange = range(msft.bb[,c("prc","avg","sd2up","sd2down")], na.rm = T)
plot(msft.bb$prc~msft$date,
     ylim=yrange,
     xlab='date',
     ylab='Price ($)',
     type='l',
     main='MSFT: Bollinger Bands')
lines(msft.bb$avg~msft$date,
      col='blue')
lines(msft.bb$sd2up~msft$date,
      col='red')
lines(msft.bb$sd2down~msft$date,
      col='red')