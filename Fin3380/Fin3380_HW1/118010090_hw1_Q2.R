setwd("/Users/guangyaohe/Desktop/FIN3380/Fin3380_HW1")
library(zoo)
library(dplyr)
msft= read.csv("msft.csv")
aapl= read.csv("aapl.csv")
print(summary(msft))
print(summary(aapl))

# First calculate arithmetic return

msftret = msft %>% select(date,prc) %>%
  arrange(date) %>%
  mutate(lag.prc=lag(prc,1)) %>%
  mutate(msft.ret=prc/lag.prc-1) %>%
  select(date,msft.ret)
print(head(msftret))

aaplret = aapl %>% select(date,prc) %>%
  arrange(date) %>%
  mutate(lag.prc=lag(prc,1)) %>%
  mutate(aapl.ret=prc/lag.prc-1) %>%
  select(date,aapl.ret)
print(head(aaplret))

msft.aapl = aaplret %>% inner_join(msftret,by="date") %>%
  mutate(msft.aapl.ret=msft.ret-aapl.ret)
summary(msft.aapl$msft.aapl.ret)

msft.aapl = msft.aapl %>% arrange(date) %>%
  filter(!is.na(msft.aapl.ret)) %>%
  mutate(cumret=cumprod(msft.aapl.ret+1))
print(tail(msft.aapl))
print(summary(msft.aapl$cumret))

plot(msft.aapl$cumret~as.Date(msft.aapl$date),
     type="l",ylab="Return",xlab="date",
     main="Long MSFT-Short AAPL")

# Now calculate logarithmic return
msftlogret = msft %>% select(date,prc) %>%
  arrange(date) %>%
  mutate(lag.prc=lag(prc,1)) %>%
  mutate(msft.logret=log(prc/lag.prc)) %>%
  select(date,msft.logret)
print(head(msftlogret))

aapllogret = aapl %>% select(date,prc) %>%
  arrange(date) %>%
  mutate(lag.prc=lag(prc,1)) %>%
  mutate(aapl.logret=log(prc/lag.prc)) %>%
  select(date,aapl.logret)
print(head(aapllogret))

msft.aapl.log = aapllogret %>% inner_join(msftlogret,by="date") %>%
  mutate(msft.aapl.logret=msft.logret-aapl.logret)
summary(msft.aapl.log$msft.aapl.logret)

msft.aapl.log = msft.aapl.log %>% arrange(date) %>%
  filter(!is.na(msft.aapl.logret)) %>%
  mutate(cumlogret=cumprod(msft.aapl.logret+1))
print(tail(msft.aapl.log))
print(summary(msft.aapl.log$cumlogret))

plot(msft.aapl.log$cumlogret~as.Date(msft.aapl.log$date),
     type="l",ylab="Log Return",xlab="date",
     main="Log Long MSFT-Short AAPL")

# b. cumulative return
msft.aapl = msft.aapl %>% arrange(date) %>%
  mutate(aaplcumret=cumprod(aapl.ret+1)) %>%
  mutate(msftcumret=cumprod(msft.ret+1))
plot(msft.aapl$msftcumret~as.Date(msft.aapl$date),
     type="l",ylab="Cum Return",xlab="date",
     main="MSTF/AAPL Cumulative Return",col="red")
lines(msft.aapl$aaplcumret~as.Date(msft.aapl$date),col="blue")

# c.New Strategy
msft.aapl = msft.aapl %>% arrange(date) %>%
  mutate(aaplormsft=ifelse(msft.aapl.ret>0,1,-1)) %>%
  mutate(lag.aaplormsft=lag(aaplormsft,1)) %>%
  filter(!is.na(lag.aaplormsft)) %>%
  mutate(adj.msft.aapl.ret=lag.aaplormsft*msft.aapl.ret) %>%
  mutate(adj.cum.ret=cumprod(adj.msft.aapl.ret+1))

plot(msft.aapl$adj.cum.ret~as.Date(msft.aapl$date),
     type="l",ylab="Cum Ret",xlab="date",
     main="New/Old Strategy",col="red")
lines(msft.aapl$cumret~as.Date(msft.aapl$date),col="blue")
legend("topleft",
       c('New Strategy', 'Old Strategy'),
       lty=c(1,1,1),
       col=c("red","blue"))

# obvious, for most of the time, new strategy would be better
