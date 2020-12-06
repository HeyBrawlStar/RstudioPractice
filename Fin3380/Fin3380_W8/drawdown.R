#Drawdown
setwd("/Users/guangyaohe/Desktop/FIN3380/RstudioPractice/Fin3380/Fin3380_W8")
library(zoo)
library(tidyr)
library(dplyr)
library(ggplot2)

sh1 = read.csv("SH000001.csv")
head(sh1)
tail(sh1)
summary(sh1)
#check the potential error???
sh1[(which(sh1$Idxrtn==max(sh1$Idxrtn))-2): (which(sh1$Idxrtn==max(sh1$Idxrtn))+2),]

#modify the data for convenience
sh1$Month = as.yearmon(sh1$Month)
colnames(sh1) = c("date","idx","ret")
head(sh1)

plot(sh1$idx~sh1$date,type='l',
     lwd=2,xlab='month',ylab='idx',main='SSE Composite Index 1990-12 to 2019-09')

#just remember par(new = T) allows you put two graph into one coloum
par(new = T)
plot(sh1$ret~sh1$date,type='l',col='red',
     xlab='',ylab='',yaxt='n')
#axis(4) means at the right side; yaxt = "n" above means not y axis name; 
axis(4)

#Drawdown(t) = (Price_t-Cummax_t)/Cummax_t
# cummax is cummulative maximun, 就是新高
# dd goes back to zero means it return back to the cummax; if it bacomes larger than that
# it will be 0; the value of cummax will be flashed
sh1$dd = (sh1$idx - cummax(sh1$idx))/cummax(sh1$idx)
head(sh1)
plot(sh1$dd ~ sh1$date,
     type = 'l',
     xlab = "date",
     ylab = "dd",
     main = "SSE Composite Drawdowns")

#summarize the drawdown; like what is the maxmium drawdown; what is the longest drawdown period
#First, max
dd.max = max(-sh1$dd)
dd.max

#the longest period of drawdown; the time unit should be year, so there would be "/12"
sh1$dd = round(sh1$dd,4)
sh1 = sh1 %>% mutate(flag = ifelse(sh1$dd<0, 1,0))
rles = rle(sh1$flag)
rles
#rle() returns the length and values of runs of equal values in a vector
#what we need is the length when dd<0, then flag = 1 (value = 1)
# "/12" is for yearly time unit
summary(rles$lengths[rles$values == 1])/12

#calmar ration
mean(sh1$ret) * 12/dd.max

#Co-drawdown of Bond and Equity
with()




