setwd("/Users/guangyaohe/Desktop/FIN3380/RstudioPractice/Fin3380/Fin3380_HW5")

#Q1: Remember to add the pict
library(dplyr)
library(tidyr)


#Q2.a
load("indices.RData")
load("ind.g.price.RData")
summary(ind.g.price)

invest = 1e6

inds = ind.g.price %>%
  spread(key = gvkeyx, value = prccd)

colnames(inds) = c('date',"prc.150036","prc.153435")

inds = inds %>%
  mutate(ret.150036 = lag(prc.150036,1)/prc.150036-1,
         ret.153435 = lag(prc.150036,1)/prc.153435-1)

mu.150036 = mean(inds$ret.150036,na.rm=T)
mu.153435 = mean(inds$ret.153435,na.rm=T)
sigma.150036 = sd(inds$ret.150036,na.rm=T)
sigma.153435 = sd(inds$ret.153435,na.rm=T)

invest = 1e6
#q=0.05
#G VaR
var.150036 = -(sigma.150036*qnorm(0.05)+mu.150036) * invest
var.153435 = -(sigma.153435*qnorm(0.05)+mu.153435) * invest

#H VaR
var.hist.150036 = -invest * quantile(inds$ret.150036, probs=0.05, type=3, na.rm=T)
var.hist.153435 = -invest * quantile(inds$ret.153435, probs=0.05, type=3, na.rm=T)

names(var.hist.150036) = 'id.150036'
names(var.hist.153435) = 'id.153435'
#G ES
es.150036 = invest * (mu.150036+sigma.150036*dnorm(qnorm(0.05))/0.05)
es.153435 = invest * (mu.153435+sigma.153435*dnorm(qnorm(0.05))/0.05)

#H ES
inds$pl.150036 = inds$ret.150036 * invest
inds$pl.153435 = inds$ret.153435 * invest

inds$dummy.150036=NA
inds$dummy.153435=NA
inds[which(inds$pl.150036<=(-var.hist.150036)),]$dummy.150036=1
inds[which(inds$pl.153435<=(-var.hist.150036)),]$dummy.153435=1

inds$es.150036 = -inds$dummy.150036 * inds$pl.150036
inds$es.153435 = -inds$dummy.153435 * inds$pl.153435
es.hist.150036 = mean(inds$es.150036,na.rm=T)
es.hist.153435 = mean(inds$es.153435,na.rm=T)

#Summary
var.g = c(var.150036,var.153435)
var.h = c(var.hist.150036,var.hist.153435)
es.g = c(es.150036,es.153435)
es.h = c(es.hist.150036,es.hist.153435)
risk.tb = cbind(var.g,es.g,var.h,es.h)
risk.tb

#Q2.b
#VaR, ES of Portfolio
inds = inds[-1,]
cor = cor(inds$ret.150036,inds$ret.153435)
sigma.p = sqrt(0.25*sigma.150036^2+0.25*sigma.153435^2+0.5*cor*sigma.150036*sigma.153435)
mu.p = mean(inds$ret.150036+inds$ret.153435, na.rm = T)
var.p = -(sigma.p*qnorm(0.05)+mu.p) * invest

var.hist.p = -invest * quantile(0.5*(inds$ret.150036+inds$ret.153435), probs=0.05, type=3, na.rm=T)
names(var.hist.p) = 'port'

es.p = invest * (mu.p+sigma.p*dnorm(qnorm(0.05))/0.05)

#Historical ES of Portfolio
inds$pl.p = 0.5*(inds$ret.150036+inds$ret.153435) * invest
inds$dummy.p=NA
inds[which(inds$pl.p<=(-var.hist.p)),]$dummy.p=1
inds$es.p = -inds$dummy.p * inds$pl.p
es.hist.p = mean(inds$es.p,na.rm=T)

var.g = c(var.150036,var.153435,var.p)
var.h = c(var.hist.150036,var.hist.153435,var.hist.p)
es.g = c(es.150036,es.153435,es.p)
es.h = c(es.hist.150036,es.hist.153435,es.hist.p)
risk.tb = cbind(var.g,es.g,var.h,es.h)
risk.tb 
#Notice that var and ed of portfolio become smaller


#Q2.c
#Drawdown

inds.dd = inds %>%
  select(date, prc.150036, prc.153435)

inds.dd$dd.150036 = (inds.dd$prc.150036-cummax(inds.dd$prc.150036))/cummax(inds.dd$prc.150036)
inds.dd$dd.153435 = (inds.dd$prc.153435-cummax(inds.dd$prc.153435))/cummax(inds.dd$prc.153435)
#Plot
plot(inds.dd$dd.153435~inds.dd$date,type='l',
     xlab = "date",ylab='dd',
     main='2 stocks dd', col="red")
lines(inds.dd$dd.150036~inds.dd$date,type='l', col ='blue')

#Max dd and longest length
dd.max.150036 = max(-inds.dd$dd.150036)
dd.max.150036
dd.max.153435 = max(-inds.dd$dd.153435)
dd.max.153435  

inds.dd$dd.150036 = round(inds.dd$dd.150036,4)
inds.dd$dd.153435 = round(inds.dd$dd.153435,4)

inds.dd = inds.dd %>% 
  mutate(dd.flag.150036 = ifelse(dd.150036<0,1,0)) %>% 
  mutate(dd.flag.153435 = ifelse(dd.153435<0,1,0))

rles.150036 = rle(inds.dd$dd.flag.150036)
summary(rles.150036$lengths[rles.150036$values ==1])

rles.153435 = rle(inds.dd$dd.flag.153435)
summary(rles.153435$lengths[rles.153435$values ==1])

# dd of Portfolio
inds.dd$prc.p = 0.5*(inds.dd$prc.150036+inds.dd$prc.153435)
inds.dd$dd.p = (inds.dd$prc.p-cummax(inds.dd$prc.p))/cummax(inds.dd$prc.p)
dd.max.p = max(-inds.dd$dd.p)
dd.max.p
inds.dd$dd.p = round(inds.dd$dd.p,4)

inds.dd = inds.dd %>% 
  mutate(dd.flag.p = ifelse(dd.p<0,1,0)) 

rles.p = rle(inds.dd$dd.flag.p)
summary(rles.p$lengths[rles.p$values ==1])

#Q3
#the probability that returns of the two indices are both negative


#co-drawdown
inds.dd = inds.dd %>%
  mutate(anydown = pmin(dd.150036,dd.153435),
         codown = pmax(dd.150036,dd.153435))
head(inds.dd)

codd = sum(inds.dd$codown)/sum(inds.dd$anydown)
codd
#Remember Q3.1!!!!!!!!

#Q4












