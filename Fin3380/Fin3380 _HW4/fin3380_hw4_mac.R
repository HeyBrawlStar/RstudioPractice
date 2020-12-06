setwd("/Users/guangyaohe/Desktop/FIN3380/RstudioPractice/Fin3380/Fin3380 _HW4")
library(RPostgres)
library(dplyr)
library(tidyr)
load("ret.all.RData")
load("ticker.RData")
#Q1.a
portn.Var = function(cov.m, wgt.all, n) {
  port.var = 0
  for (i in 1:n) {
    for(j in 1:n){
      port.var = wgt.all$wgt[i]* wgt.all$wgt[j]* cov.m[i,j] + port.var
    }
  }
  return(port.var)
}

#Q1.b
ret.all = merge(ret.all,tickers,by.x="permno",by.y="permno",all.x=T, all.y = T)
head(ret.all)

ret.all = ret.all %>% select(-permno)
head(ret.all)

ret.all = spread(ret.all,"ticker","ret")
ret.all[1:3,]

ret.all = ret.all[complete.cases(ret.all),]
ret.all[1:3,]

cov.m = cov(ret.all[,2:ncol(ret.all)])
print(cov.m)

cov.m = cov.m * 252
print(cov.m)

wgt.all = data.frame(ticker=c("AAPL","MSFT","IBM","BABA","GE","KO"),
                     wgt = c(0.1,0.2,0.15,0.25,0.1,0.2))
wgt.all = wgt.all[order(wgt.all$ticker),]
wgt.all


#Use function to calculate the risk of portfolio
portn.Var(cov.m, wgt.all, 6)

#Use matrix to calculate the risk of portfolio
wgt = matrix(wgt.all$wgt,1)
port.var = wgt %*% cov.m %*% t(wgt)
port.var


#Q2
load("crsp.sample.RData")
#Daily return
stocks = crsp.sample %>%
  filter(permno %in% c(39731,79089, 83148)) %>%
  select(permno, date, ret)

stocks = subset(stocks, stocks$date>='2017-01-01'& stocks$date<='2018-12-31')

head(stocks)

# daily return

stocks = stocks%>%
  spread(key = permno, value = ret)
head(stocks)

ret.d.39731 = mean(stocks$"39731")
ret.d.79089 = mean(stocks$"79089")
ret.d.83148 = mean(stocks$"83148")

sd.39731 = sd(stocks$"39731")
sd.79089 = sd(stocks$"79089")
sd.83148 = sd(stocks$"83148")

ret.d.39731 
ret.d.79089 
ret.d.83148 

sd.39731 
sd.79089 
sd.83148 


stocks = stocks[complete.cases(stocks),]

cov.m = cov(stocks[,2:ncol(stocks)])
cov.m

cor.m = cor(stocks[,2:ncol(stocks)])
cor.m

#Q2.b
ret.d.39731 = ret.d.39731*252
ret.d.79089 = ret.d.79089*252
ret.d.83148 = ret.d.83148*252

ret.d.39731 
ret.d.79089 
ret.d.83148 

sd.39731 = sd.39731*sqrt(252)
sd.79089 = sd.79089*sqrt(252)
sd.83148 = sd.83148*sqrt(252)

sd.39731 
sd.79089 
sd.83148 

cov.m = cov.m*252
cov.m
cor.m

#Q2.c

w1 = c()
w2 = c()
w3 = c()
for (a in seq(0,1,0.01)){
  for (b in seq(0,1,0.01)) {
    w1 = c(w1,a)
    w2 = c(w2,b)
    w3 = c(w3, 1-a-b)
  }
}

ef = data.frame(w1=w1,w2=w2,w3=w3) %>%
  filter(w3 >= 0)
head(ef)

ef = ef %>% mutate(sd.port = sqrt(w1^2*sd.39731^2 + w2^2*sd.79089^2 
                                  + w3^2*sd.83148^2 
                                  + 2*w1*w2*cov.m[1,2] 
                                  + 2*w1*w3*cov.m[1,3] 
                                  + 2*w2*w3*cov.m[2,3]),
                                  ret.port = w1*ret.d.39731 + w2*ret.d.79089 +w3*ret.d.83148)
head(ef)

#Remember a dataframe[a,b], must contains both a and b
ef[which(ef$w1 == 0.4 & ef$w2 == 0.2),]
ef[which(ef$w1 == 0.8 & ef$w2 == 0.1),]

#Q2.d
ef = round(ef,3)
head(ef)

ef.edited = ef %>%
  group_by(ret.port)%>%
  summarise(sd.port.small = min(sd.port)) %>%
  ungroup()

head(ef.edited)


plot(ef.edited$ret.port~ef.edited$sd.port.small,
     type ="l",
     lwd=2,
     main='Efficient frontier',
     xlab='Portfolio Std',
     ylab='Portfolio Return',
     ylim=c(min(ef.edited$ret.port)-0.01,max(ef.edited$ret.port)+0.01))

gmv=ef.edited[which(ef.edited$sd.port.small==min(ef.edited$sd.port.small)),c("sd.port.small","ret.port")]
points(gmv,pch=19,col='red')

#Q2.e
risky = ef.edited%>%
  mutate(sharpe = ret.port/sd.port.small)%>%
  filter(sharpe==max(sharpe))
risky

cat('The portfolio allocation that gives the highest sharpe ratio is (',risky$sd.port.small,risky$ret.port,")")

rf = data.frame(ret.rf = 0, sd.rf=0)

cal = data.frame(wgt.rf = seq(-1,1,0.1))
head(cal)

cal = cal %>% mutate(wgt.risky = 1-wgt.rf,
                     ret.rf=rf$ret.rf,
                     ret.risky=risky$ret.port, 
                     sd.risky=risky$sd.port.small)
cal = cal %>% 
  mutate(ret.cal = wgt.rf*ret.rf + wgt.risky*ret.risky,
         sd.cal = wgt.risky*sd.risky) 
head(cal)

plot(ef.edited$ret.port~ef.edited$sd.port.small,type='l',
     lwd=2,
     main='Efficient frontier',
     xlab='Portfolio Std',
     ylab='Portfolio Return',
     ylim=c(min(ef.edited$ret.port)-0.05,max(ef.edited$ret.port)+0.05),
     xlim=c(0,max(ef.edited$sd.port.small)+0.05))
lines(cal$ret.cal~cal$sd.cal,
      type='l',
      xlab='',
      ylab='',
      main='',
      col ='red',
      lwd=2)