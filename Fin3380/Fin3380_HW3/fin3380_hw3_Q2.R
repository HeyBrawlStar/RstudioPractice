setwd("F:/RstudioPractice/RstudioPractice/Fin3380/Fin3380_HW3")
library(RPostgres)
library(tidyr)
library(dplyr)
load("mystock.RData")
load("stockid.RData")
tmp.summary = mystock %>% group_by(permno) %>% summarise(n = n(),min(date)) %>%
  ungroup()
tmp.summary

#Since BABA has not come into the market yet, our porfolio will contain only 5 stocks

#Q2.a
#First, we choose data on 2012-02
mystock = mystock %>%
  filter(date >= "2012-01-01")

mystock.Rp = mystock %>%
  mutate(prc=ifelse(prc<0,-prc,prc)) %>%
  group_by(permno) %>%
  mutate(date=as.character(date)) %>%
  arrange(date) %>%
  mutate(ret=prc/lag(prc,1)-1) %>%
  mutate(sd.all = sd(ret,na.rm = T))%>%
  ungroup()%>%
  mutate(yymm=substr(date,1,7))

head(mystock.Rp)

risk = mystock.Rp %>%
  group_by(permno,yymm)%>%
  arrange(date) %>%
  mutate(sd.yymm=sd(ret,na.rm = T),
            sd.all=first(sd.all))%>%ungroup()

head(risk,20)

risk = risk %>%
  group_by(yymm,date)%>%
  mutate(inverse.sum = sum(1/sd.yymm)) %>%
  ungroup()

head(risk)

wight = risk %>%
  arrange(date)%>%
  group_by(permno,yymm)%>%
  mutate(wgt.Rp = 1/sd.yymm/inverse.sum)%>%
  ungroup()%>%
  group_by(permno)%>%
  mutate(wgt.Rp = lag(wgt.Rp,1))%>%
  ungroup()
    
  
head(wight)

wgt.2012.03 = wight %>%
  select(permno,yymm,wgt.Rp) %>%
  arrange(yymm) %>%
  filter(wight$yymm == '2012-03')
#below is weights of each asset in "2012-03" using Risk-parity Investment
head(wgt.2012.03,5)

#Q2.b
head(mystock.Rp)

wight = wight %>%
  arrange(date,permno)
mystock.Rp = mystock.Rp %>%
  mutate(ret=ifelse(is.na(ret),0,ret)) %>%
  group_by(yymm,permno) %>% 
  arrange(date,permno) %>%
  mutate(cumret=cumprod(ret+1)) %>% ungroup() %>%
  mutate(wgt.Rp = wight$wgt.Rp)

mystock.Rp = mystock.Rp %>% filter(yymm!=first(yymm))
head(mystock.Rp)
summary(mystock.Rp)

mystock.Rp = mystock.Rp %>%
  mutate(Rp = wgt.Rp*cumret)
head(mystock.Rp)

mystock.Rp = mystock.Rp %>%
  group_by(date) %>%
  summarise(Rp=sum(Rp),
            yymm=first(yymm))%>%
  ungroup()

head(mystock.Rp)
mystock.Rp.2013 = mystock.Rp %>% 
  mutate(yy = substr(yymm,1,4)) %>%
  filter(yy == "2013") %>%
  group_by(yymm)%>%
  arrange(date)%>%
  mutate(ret=Rp/lag(Rp,1)-1)%>%
  ungroup()
head(mystock.Rp.2013)
mystock.Rp.2013 = mystock.Rp.2013%>%
  mutate(ret = ifelse(is.na(ret),Rp-1,ret))%>%
  arrange(date)%>%
  mutate(Rp.2013 = cumprod(ret+1))
head(mystock.Rp.2013)


#Now we calculate Ew

mystock.sample = mystock%>%
  mutate(prc=ifelse(prc<0,-prc,prc)) %>%
  group_by(permno) %>%
  mutate(date=as.character(date)) %>%
  arrange(date) %>%
  mutate(yymm=substr(date,1,7)) %>%
  mutate(ret=prc/lag(prc,1)-1)%>%
  mutate(ret=ifelse(is.na(ret),0,ret)) %>%
  group_by(permno,yymm) %>% 
  arrange(date) %>%
  mutate(cumret=cumprod(ret+1)) %>%
  ungroup()
  

head(mystock.sample)

mystock.ew = mystock.sample %>% 
  group_by(date) %>% 
  summarise(ew=mean(cumret),
            yymm=first(yymm)) %>%
  ungroup()
head(mystock.ew)

mystock.ew.2013 = mystock.ew %>%
  mutate(yy = substr(yymm,1,4))%>%
  filter(yy=="2013")%>%
  group_by(yymm)%>%
  arrange(date)%>%
  mutate(ret=ew/lag(ew,1)-1)%>%
  ungroup()

head(mystock.ew)
mystock.ew.2013 = mystock.ew.2013 %>%
  mutate(ret=ifelse(is.na(ret),ew-1,ret))%>%
  arrange(date)%>%
  mutate(ew.2013=cumprod(ret+1))

#Now calculate Vw
mystock.sample = mystock.sample %>%
  mutate(me = abs(prc)*shrout)%>%
  select(permno,date,yymm,ret,me,cumret)%>%
  mutate(permno=as.character(permno))

mystock.vw = mystock.sample %>%
  group_by(permno)%>%
  arrange(date)%>%
  mutate(lag.me = lag(me,1))%>%
  ungroup()%>%
  group_by(permno,yymm)%>%
  arrange(date)%>%
  mutate(lag.me=first(lag.me))%>%
  ungroup()
head(mystock.vw)
mystock.vw = mystock.vw %>%
  filter(yymm!=first(yymm))

head(mystock.vw)

mystock.vw = mystock.vw %>%
  group_by(date)%>%
  mutate(wgt=lag.me/sum(lag.me,na.rm = T))%>%
  ungroup()

mystock.vw = mystock.vw %>%
  ungroup()

head(mystock.vw)
mystock.vw = mystock.vw %>%
  group_by(yymm,permno)%>%
  arrange(date)%>%
  mutate(cumret=cumprod(ret+1))%>%
  ungroup()

mystock.vw = mystock.vw %>%
  arrange(date,permno)

head(mystock.vw)

mystock.vw = mystock.vw %>%
  mutate(vw = wgt*cumret)
head(mystock.vw)

mystock.vw = mystock.vw %>%
  group_by(date)%>%
  summarise(vw=sum(vw),yymm=first(yymm))%>%
  ungroup()

mystock.vw.2013 = mystock.vw %>%
  mutate(yy = substr(yymm,1,4))%>%
  filter(yy=="2013")%>%
  group_by(yymm)%>%
  arrange(date)%>%
  mutate(ret=vw/lag(vw,1)-1)%>%
  ungroup()
  
head(mystock.vw.2013)  

mystock.vw.2013 = mystock.vw.2013 %>%
  mutate(ret=ifelse(is.na(ret),vw-1,ret))%>%
  arrange(date)%>%
  mutate(vw.2013=cumprod(ret+1))

yrange = range(c(mystock.Rp.2013$Rp.2013,mystock.vw.2013$vw.2013))
plot(mystock.Rp.2013$Rp.2013~as.Date(mystock.Rp.2013$date),
     type = "l", xlab = "date", ylab = "Cumret",
     main = 'Index Performance 2013',
     ylim = yrange)
lines(mystock.ew.2013$ew.2013~as.Date(mystock.ew.2013$date),
     col="red")
lines(mystock.vw.2013$vw.2013~as.Date(mystock.vw.2013$date),
      col='blue')
legend("topleft",c("Rp","Ew","VW"),col=c('Black','red',"black"),lty=c(1,1))

