setwd("/Users/guangyaohe/Desktop/FIN3380/Fin3380_Quiz4")

dj.rsi= read.csv("dj.csv")
dj.rsi = dj.rsi[-which(dj.rsi$dji==0),]
dj.rsi = na.omit(dj.rsi)

library(zoo)
library(dplyr)

dj.rsi = dj.rsi %>% 
  select(date,dji) %>% 
  arrange(date) %>%
  mutate(date=as.Date(date),
         delta=dji-lag(dji,1))
head(dj.rsi,3)

dj.rsi = dj.rsi %>% 
  mutate(up=ifelse(delta>0,1,0),
         down=ifelse(delta<0,1,0))
head(dj.rsi)

dj.rsi = dj.rsi %>% 
  mutate(up.val=delta*up,
         down.val=-delta*down)
head(dj.rsi)

dj.rsi = dj.rsi[2:nrow(dj.rsi),] %>%
  mutate(up.first.avg=rollapply(up.val,
                                width=14,
                                align='right',
                                fill=NA,FUN=mean,na.rm=T),
         down.first.avg=rollapply(down.val,
                                  width=14,
                                  align='right',
                                  fill=NA,FUN=mean,na.rm=T))
head(dj.rsi,20)

# initialise the up.avg and down.avg with up.first.avg and down.first.avg values
dj.rsi$up.avg =dj.rsi$up.first.avg
dj.rsi$down.avg = dj.rsi$down.first.avg

# fill up.avg and down.avg values with updated value for t>=15
for (t in 15:length(dj.rsi$up.avg)){
  dj.rsi$up.avg[t] = dj.rsi$up.avg[t-1]*13/14 + dj.rsi$up.val[t]*1/14
  dj.rsi$down.avg[t] = dj.rsi$down.avg[t-1]*13/14 + dj.rsi$down.val[t]*1/14
}

head(dj.rsi,20)

# RS = upside wema/downside wema
# RSI = 100-100/(1+RS)
dj.rsi$rs=dj.rsi$up.avg/dj.rsi$down.avg
dj.rsi$rsi=100-100/(1+dj.rsi$rs)
head(dj.rsi,20)

dj.rsi = dj.rsi %>% filter(substr(date,1,4)=='2006')

plot(dj.rsi$rsi~dj.rsi$date,
     ylab='RSI (14-Day Moving Avg)',
     xlab='Date',
     ylim=c(0,100),
     lwd=2,
     type='l',
     main='DJ: RSI (14-Day Moving Avg)')
abline(h=c(30,70),col='red')
par(new=T)
plot(dj.rsi$dji~dj.rsi$date,
     xlab='',
     ylab='',
     yaxt='n',
     type='l',col='lightblue')

legend("topleft",
       c('14-Day Moving Avg', 'Moving Avg Abline','dji'),
       lty=c(1,1,1),
       col=c("black","red",'lightblue'))
axis(4)

