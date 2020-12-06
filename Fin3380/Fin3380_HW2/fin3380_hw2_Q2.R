setwd("/Users/guangyaohe/Desktop/FIN3380/Fin3380_HW2")
library(zoo)
library(dplyr)

dj = read.csv("dj.csv")

#Q2.a
dj$date = as.Date(dj$date)

dj = subset(dj, dj$date<="2007-12-31" & dj$date>="1985-01-01")
summary(dj)

#Q2,b
dj.rsi = dj %>% 
  select(date,dji) %>% 
  arrange(date) %>%
  mutate(date = date,
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
                                width=7,
                                align='right',
                                fill=NA,FUN=mean,na.rm=T),
         down.first.avg=rollapply(down.val,
                                  width=7,
                                  align='right',
                                  fill=NA,FUN=mean,na.rm=T))
head(dj.rsi,20)

# initialise the up.avg and down.avg with up.first.avg and down.first.avg values
dj.rsi$up.avg = dj.rsi$up.first.avg
dj.rsi$down.avg = dj.rsi$down.first.avg

# fill up.avg and down.avg values with updated value for t>=15
for (t in 8:length(dj.rsi$up.avg)){
  dj.rsi$up.avg[t] = dj.rsi$up.avg[t-1]*6/7 + dj.rsi$up.val[t]*1/7
  dj.rsi$down.avg[t] = dj.rsi$down.avg[t-1]*6/7 + dj.rsi$down.val[t]*1/7
}

head(dj.rsi,20)

# RS = upside wema/downside wema
# RSI = 100-100/(1+RS)
dj.rsi$rs=dj.rsi$up.avg/dj.rsi$down.avg
dj.rsi$rsi=100-100/(1+dj.rsi$rs)
head(dj.rsi,20)

#Q2.c
Above_70 = dj.rsi %>% filter(dj.rsi$rsi>=70)
count(Above_70)

Below_30 = dj.rsi %>% filter(dj.rsi$rsi<=30)
count(Below_30)
summary(Below_30)

#Q2.d

#Calculate dj logarithmic return

dj_dji <- dj.rsi %>% select(date,dji,rsi) %>%
  arrange(date) %>%
  mutate(below.flag = ifelse(rsi<30,1,0),
         above.flag = ifelse(rsi>=30,1,0),
         cross.flag = ifelse(lag(below.flag)*above.flag == 1, 1,0)) %>%
  mutate(ret = ifelse(lag(cross.flag == 1), dji/lag(dji)-1,0))  %>%
  filter(!is.na(ret)) %>%
  mutate(cumret=cumprod(ret+1))





head(dj_dji)
tail(dj_dji)
summary(dj_dji)

plot(dj_dji$cumret ~ as.Date(dj_dji$date), type='l',
     ylab='cumulative returns', xlab='date',
     main='cumulative returns ')







