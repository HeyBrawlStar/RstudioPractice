setwd("/Users/guangyaohe/Desktop/FIN3380/Fin3380 W7")
library(RPostgres)
library(tidyr)
library(dplyr)

#permnos.RData contains 10 stock permonos
#permno is id of security and permco is id of company
load('permnos.RData')
permnos

#crsp.sample contains 10 stock in permnos.RData and date>='2010-01-01'
load("/Users/guangyaohe/Desktop/FIN3380/Fin3380 W7/crsp.sample.RData")
head(crsp.sample)

#shrout is share outstanding
#In CRSP, the price is negative to indicate it is a bid/ask average 
#and not an actual closing price. Adjust the price using the abs() function to change it to positive.
crsp.sample = crsp.sample %>% 
  mutate(me=abs(prc)*shrout) %>% 
  select(permno,date,ret,me) %>%
  mutate(permno=as.character(permno))

# handling missing returns 
crsp.ret = spread(crsp.sample[,c("permno","date","ret")],permno,ret)
crsp.ret = gather(crsp.ret,key=permno,value=ret,-date)
crsp.ret = crsp.ret %>% 
  mutate(ret=ifelse(is.na(ret),0,ret))

# seperate market cap into another dataframe and merge back onto returns
crsp.me = crsp.sample %>% select(permno,date,me)
crsp.sample = crsp.ret %>% left_join(crsp.me,by=c("permno","date"))
head(crsp.sample)

# lag me 
crsp.vw = crsp.sample %>% 
  group_by(permno) %>%
  arrange(date) %>%
  mutate(lag.me=lag(me,1)) %>% ungroup()

# keep the first value of lag.me for each month which is the last date me from last month
crsp.vw = crsp.vw %>% 
  mutate(yymm=substr(date,1,7)) %>%
  group_by(permno,yymm) %>%
  arrange(date) %>% 
  mutate(lag.me=first(lag.me)) %>% ungroup()
head(crsp.vw)

# remove the first month of entire data as all the lag.me are NA
crsp.vw = crsp.vw %>% filter(yymm!=first(yymm))
head(crsp.vw)

# Check the quality of the lag.me column to see if any missing values still exist. 
summary(crsp.vw$lag.me)

#na.rm = T is not necessary cause in previour code, we have clear NA
crsp.vw = crsp.vw %>% 
  group_by(date) %>%
  mutate(wgt=lag.me/sum(lag.me,na.rm=T)) %>%
  ungroup()
head(crsp.vw)


crsp.vw = crsp.vw %>% 
  group_by(yymm,permno) %>% 
  arrange(date) %>%
  mutate(cumret=cumprod(ret+1)) %>% ungroup()
crsp.vw = crsp.vw %>% arrange(date,permno)
head(crsp.vw)

crsp.vw = crsp.vw %>% 
  mutate(vw=wgt*cumret)
head(crsp.vw)

crsp.vw = crsp.vw %>%
  group_by(date) %>% 
  summarise(vw=sum(vw),
            yymm=first(yymm)) %>% 
  ungroup()

head(crsp.vw)

load('crsp.ew.RData')

crsp.vw.2010.02 = crsp.vw %>% filter(yymm=='2010-02')
crsp.ew.2010.02=crsp.ew %>% filter(yymm=='2010-02')
yrange=range(c(crsp.ew.2010.02$ew,crsp.vw.2010.02$vw))
plot(crsp.vw.2010.02$vw~as.Date(crsp.vw.2010.02$date),
     type='l',xlab='date',ylab='ew.id',
     main='Value Weighted Index 2010-02',
     ylim=yrange)
lines(crsp.ew.2010.02$ew~as.Date(crsp.ew.2010.02$date),
      col='red')
legend("bottomright",c("EW","VW"),col=c("red","black"),lty=c(1,1))

# calculate return on index on each day
crsp.vw = crsp.vw %>% 
  group_by(yymm) %>% 
  arrange(date) %>%
  mutate(ret=vw/lag(vw,1)-1) %>% ungroup()
head(crsp.vw)

# fill the first date of each month with the return on that day
crsp.vw = crsp.vw %>% 
  mutate(ret=ifelse(is.na(ret),vw-1,ret))

# cumulative returns over all periods
crsp.vw = crsp.vw %>% 
  arrange(date) %>% 
  mutate(vw.all = cumprod(ret+1))

# visualise equal weight index cumulative returns 
plot(crsp.ew$ew.all~as.Date(crsp.ew$date),
     type='l',xlab='date',ylab='ew.id',
     main='EW Index & VW Index', col = "red")

lines(crsp.vw$vw.all~as.Date(crsp.vw$date), col = "black")
legend("bottomright",c("EW","VW"),col=c("red","black"),lty=c(1,1))

save(crsp.vw,file='crsp.vw.RData')
