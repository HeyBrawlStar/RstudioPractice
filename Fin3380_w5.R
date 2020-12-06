setwd("/Users/guangyaohe/Desktop/FIN3380/Fin3380_HW1")
library(zoo)
library(dplyr)
 aapl = read.csv("aapl.csv")
 head(aapl)
 
aapl.sma = aapl[,c("permno","date","prc")]
str(aapl.sma)

aapl.sma$date = as.Date(aapl.sma$date)
print(aapl.sma[1:3,])

# check missing data
any(is.na(aapl.sma$prc))

tmp = c(NA,1,2,NA,4,5,6,7,NA)
tmp
rollmean(tmp, k = 2)

aapl.sma$sma50 = rollmean(aapl.sma$prc, k = 50, fill = NA, align = "right")
aapl.sma$sma200 = rollmean(aapl.sma$prc, k = 200, fill = NA, align = "right")

print(aapl.sma[198:203,])

aapl.sma = aapl.sma %>%
  mutate(sma50 = rollmean(aapl.sma$prc, k = 50, fill = NA, align = "right"),
         sma50 = rollmean(aapl.sma$prc, k = 50, fill = NA, align = "right"))

print(aapl.sma[48:52,])

aapl.sma = aapl.sma[aapl.sma$date>= "2016-01-01",]
#dplyr method 
aapl.sma = aapl.sma %>% filter(date >= "2016-01-01")
head(aapl.sma)

aapl.sma = aapl.sma[order(aapl.sma$date),]
#dplyr method
aapl.sma = aapl.sma %>% arrange(date)


yrange=range(aapl.sma[,c('prc','sma50','sma200')])

plot(aapl.sma$prc~aapl.sma$date,
     type = 'l', # be careful, it is l, not one(1)
     xlab ="date",
     ylab = 'Price ($)',
     ylim = yrange,
     main = "AAPL:SMA")
lines(aapl.sma$sma50~aapl.sma$date, col="red")
lines(aapl.sma$sma200~aapl.sma$date, col="blue")
legend("topleft",
       c("Price", "50-Day Moving Average","200-Day Moving Average"),
       lty=c(1,1,1),
       col=c("black","red","blue"))













