setwd("F:/RstudioPractice/RstudioPractice/Fin3380/Fin3380_HW3")
library(RPostgres)
library(tidyr)
library(dplyr)
#source("F:\RstudioPractice\RstudioPractice\Fin3380")
#Q1
load("crsp.sample2.RData")

IPO.summary = crsp.sample2 %>% 
  group_by(permno) %>%
  arrange(date) %>%
  summarise(min(date)) %>%
  ungroup()

IPO.summary

#Q2
# spread function from library(tidyr) to turn a long table into a wide table
# after spread, the days with missing ret will be filled with NA value.
crsp.sample2 = crsp.sample2 %>% select(permno,date,ret)
head(crsp.sample2,3)
crsp.sample2 = spread(crsp.sample2,key = permno, value = ret)

# the gather function reformat the wide table into long table 
# the NA value will be kept 
crsp.sample2 = gather(crsp.sample2,key = permno,value = ret,-date)
tmp.summary = crsp.sample2 %>% group_by(permno) %>% summarise(n=n()) %>%
  ungroup()
tmp.summary

crsp.sample2 = crsp.sample2 %>% na.omit()
tmp.summary = crsp.sample2 %>% group_by(permno) %>% summarise(n=n()) %>%
  ungroup()
tmp.summary

crsp.sample2 = crsp.sample2 %>% 
  mutate(ret=ifelse(is.na(ret),0,ret)) %>%
  mutate(date=as.character(date)) %>%
  mutate(yymm=substr(date,1,7)) %>%
  group_by(permno,yymm) %>% 
  arrange(date) %>%
  mutate(cumret=cumprod(ret+1)) %>%
  ungroup()
head(crsp.sample2,20)

crsp.ew = crsp.sample2 %>% 
  group_by(date) %>% 
  summarise(ew=mean(cumret),
            yymm=first(yymm)) %>%
  ungroup()

head(crsp.ew)

crsp.ew.2011.01=crsp.ew %>% filter(yymm=='2011-01')
plot(crsp.ew.2011.01$ew~
       as.Date(crsp.ew.2011.01$date),
     type='l',xlab='date',ylab='ew.id',
     main='Equal Weighted Index 2011-01')

crsp.ew.2013.01=crsp.ew %>% filter(yymm=='2013-01')
plot(crsp.ew.2013.01$ew~
       as.Date(crsp.ew.2013.01$date),
     type='l',xlab='date',ylab='ew.id',
     main='Equal Weighted Index 2013-01')

#Q2

  
    
  
  







