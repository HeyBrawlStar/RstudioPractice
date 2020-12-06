setwd("/Users/guangyaohe/Desktop/FIN3380/Fin3380_HW1")
library(zoo)
library(dplyr)

#Q1
dj= read.csv("dj.csv")
head(dj)
str(dj)

dj$date = as.Date(dj$date) 
any(is.na(dj$dji))
summary(dj)
#TRUE means there are NA in the data


#Q2
source("/Users/guangyaohe/Desktop/FIN3380/Fin3380_HW1/getwrds.R")


q="select distinct ticker, permno from CRSP.DSENAMES where ticker in ('MSFT','AAPL')"
res = dbSendQuery(wrds, q)
stock.id = dbFetch(res, n=-1)
dbClearResult(res)
print(stock.id)

q=paste0("select permno,date,bidlo,askhi,openprc,prc,vol from CRSP.DSF where permno in (",paste0(stock.id$permno,collapse = ','),") and date>='2015-01-01'")
print(q)

res = dbSendQuery(wrds, q)
mystock = dbFetch(res, n=-1)
dbClearResult(res)
print(head(mystock))

library(dplyr)
mystock = mystock %>% arrange(date,permno)
print(head(mystock))

msft = mystock %>% filter(permno=="10107")
aapl = mystock %>% filter(permno=="14593")
plot(msft$prc~msft$date,type='l',ylab='price',xlab='date',
     main='MSFT: CLOSE PRICE')

plot(aapl$prc~aapl$date,type='l',ylab='price',xlab='date',
     main='AAPL: CLOSE PRICE')

dim(aapl)
dim(msft)

msft = msft[-which(msft$vol==1),]

msftret= msft %>% select(date,prc)
print(head(msftret))

msftret.ari = msft %>% select(date,prc) %>%
  arrange(date) %>%
  mutate(lag.prc=lag(prc,1)) %>%
  mutate(msft.ret.ari=prc/lag.prc-1) %>%
  select(date,msft.ret.ari)
print(head(msftret.ari))

msftret.log = msft %>% select(date,prc) %>%
  arrange(date) %>%
  mutate(lag.prc=lag(prc,1)) %>%
  mutate(msft.ret.log=log(prc/lag.prc)) %>%
  select(date,msft.ret.log)
print(head(msftret.log))

aaplret.ari = aapl %>% select(date,prc) %>% 
  arrange(date) %>%
  mutate(lag.prc=lag(prc,1)) %>%
  mutate(aapl.ret.ari=prc/lag.prc-1) %>%
  select(date,aapl.ret.ari)
print(head(aaplret.ari))

aaplret.log = aapl %>% select(date,prc) %>% 
  arrange(date) %>%
  mutate(lag.prc=lag(prc,1)) %>%
  mutate(aapl.ret.log=log(prc/lag.prc)) %>%
  select(date,aapl.ret.log)
print(head(aaplret.log))

msft.aapl.ari=aaplret.ari %>% inner_join(msftret.ari,by="date")
print(head(msft.aapl.ari))

msft.aapl.log=aaplret.log %>% inner_join(msftret.log,by="date")
print(head(msft.aapl.log))

msft.aapl.ari = msft.aapl.ari%>% mutate(msft.aapl.ret=msft.ret.ari-aapl.ret.ari)
msft.aapl.log = msft.aapl.log%>% mutate(msft.aapl.ret=msft.ret.log-aapl.ret.log)

# cumulative returns of long-short position
msft.aapl.ari = msft.aapl.ari %>% arrange(date) %>% 
  filter(!is.na(msft.aapl.ret)) %>%
  mutate(cumret=cumprod(msft.aapl.ret+1))

msft.aapl.log = msft.aapl.log %>% arrange(date) %>% 
  filter(!is.na(msft.aapl.ret)) %>%
  mutate(cumret=cumprod(msft.aapl.ret+1))

head(msft.aapl.ari)

# plot your cummulative return 
#Arithmetic ret
plot(msft.aapl.ari$cumret~as.Date(msft.aapl.ari$date),
     col = "blue",
     type='l',ylab='price',xlab='date', 
     main='Long MSFT-Short AAPL')

#Logarithmic ret
lines(msft.aapl.log$cumret~as.Date(msft.aapl.log$date), col ='red')
legend("topleft",
       c('Arithmetic ret','Logarithmic ret'),
       lty=c(1,1,1),
       col=c('blue','red'))


#Q2.C

msftret.log = msft %>% select(date,prc) %>%
  arrange(date) %>%
  mutate(lag.prc=lag(prc,1)) %>%
  mutate(msft.ret.log=log(prc/lag.prc)) %>%
  select(date,msft.ret.log)

aaplret.log = aapl %>% select(date,prc) %>% 
  arrange(date) %>%
  mutate(lag.prc=lag(prc,1)) %>%
  mutate(aapl.ret.log=log(prc/lag.prc)) %>%
  select(date,aapl.ret.log)
print(head(aaplret.log))

msft.aapl.log = msft.aapl.log%>% mutate(msft.aapl.ret=msft.ret.log-aapl.ret.log)

msft.aapl.log = msft.aapl.log %>% arrange(date) %>%
  mutate(flag = ifelse(msft.aapl.ret>0,1,-1)) %>%
  mutate(lag.flag = lag(flag,1)) %>%
  filter(!is.na(msft.aapl.ret)) %>%
  filter(!is.na(lag.flag)) %>%
  select(date, aapl.ret.log, msft.ret.log, msft.aapl.ret, lag.flag)

head(msft.aapl.log)


if (msft.aapl.log$lag.flag == 1){
    msft.aapl.log = msft.aapl.log %>%
      mutate(msft.aapl.ret2 = msft.ret.log-aapl.ret.log)
} else {
  msft.aapl.log = msft.aapl.log %>%
    mutate(msft.aapl.ret2 = aapl.ret.log-msft.ret.log)
}

msft.aapl.log = msft.aapl.log %>% 
  mutate(cumret2=cumprod(msft.aapl.ret2+1)) %>%
  mutate(cumret=cumprod(msft.aapl.ret+1))

  
plot(msft.aapl.log$cumret2~as.Date(msft.aapl.log$date),
     type='l',ylab='price',xlab='date', 
     main='New Strategy')

