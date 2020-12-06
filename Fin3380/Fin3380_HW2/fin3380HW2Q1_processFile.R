setwd("/Users/guangyaohe/Desktop/FIN3380/Fin3380_HW2")
library(zoo)
library(dplyr)
msft = read.csv("msft.csv")

msft.bb = msft[,c("permno","date","prc")]
msft.bb$date = as.Date(msft.bb$date)


head(msft.bb)
msft.bb = msft.bb %>% 
  arrange(date) %>%
  mutate(avg = rollmean(prc,k=20,fill=NA,align='right'),
         sd = rollapply(prc,width=20,FUN=sd,na.rm=T,fill=NA,align='right'))
tail(msft.bb)
head(msft.bb)

msft.bb =msft.bb %>% 
  mutate(sd2up=avg+2*sd,
         sd2down=avg-2*sd)
head(msft.bb,3)

msft.bb = na.omit(msft.bb)
head(msft.bb)

BBands = function(dataName, window = 20 ){
  prc = dataName
  dataName.bb = data.frame(prc = prc, stringsAsFactors = T)
  dataName.bb = dataName.bb %>% 
    mutate(avg = rollmean(prc, window,
                          fill=NA,align='right'),
           sd = rollapply(prc, window,
                          FUN=sd,na.rm=T,fill=NA,align='right')) %>% 
    mutate(sd2up=avg+2*sd,
           sd2down=avg-2*sd) %>%
    na.omit()
  return(dataName.bb)
}

head(BBands(msft.bb$prc, window = 20))




