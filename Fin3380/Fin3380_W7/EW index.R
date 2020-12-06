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
summary(crsp.sample)
crsp.sample = crsp.sample %>% select(permno, date, ret)
head(crsp.sample) 

#Now we get permno, date, ret of these 10 stocks -> crsp.sample
tmp.summary = crsp.sample %>% 
  group_by(permno) %>% 
  summarise(nOfDate=n()) %>%
  ungroup()

#summarise(): nOfDate means col name, and n() means data size, 
#usually, we use n = n() to represtent the size, other name is acceptable
#summarise will produce a new data frame, using group_by will group the data
#data will then summarise by group
#And it seems that ungroup is not necessary, so its usage may be just a cumstom
tmp.summary

load('ret.example.RData')
ret.example
# ret.example is an long data frame

#spread is function of package tidyr
#First, add an data frame
#key means col name or position, value means what data should be under key col
ret.example.wide = spread(ret.example, key = stock_id, value = ret)
ret.example.wide
#spread is convert a long data frame into a wide one, gather is opposite


#below is new code:pivot_wider() to replace spread()
ret.example.wide2 = ret.example %>% pivot_wider(names_from = stock_id, values_from = ret)
ret.example.wide2

# -date means
ret.example.long = gather(ret.example.wide, key = stock_id, value = ret, -date)
ret.example.long

#below is new code:pivot_longer() to replace gather()
ret.example.long2 = ret.example.wide2 %>% pivot_longer(names_to = 'stock_id', values_to = 'ret', -date)
ret.example.long2



#In previous code, crsp.sample has been modified into this format (10 stocks)
#Remember our goal is to realocate equal weighted portfolio
#In the previous code, we found that the data size of each stock is not the same
#After spread(), which will fill NA automatically when there is  missing data
#Now the data size should be equal now, althought there are NAs
head(crsp.sample)
crsp.sample = spread(crsp.sample, key = permno, value = ret)
crsp.sample = gather(crsp.sample, key = permno, value = ret, -date)
tmp.summary = crsp.sample %>% 
  group_by(permno) %>%
  summarise(n = n()) %>%
  ungroup()
tmp.summary

#Now we should replace NA with 0
# ifelse(test, yes, no), it will test each row, 
#in this case, it will text whether the value under col ret is NA
#if yes, then it will be replaced by 0,
#if no, keep its value of ret
#substr(date, 1, 7) means like select 1-7 characters in "2010-01-01", then it is "2010-01"

### I don't understand why should group by yymm
crsp.sample = crsp.sample %>%
  mutate(ret=ifelse(is.na(ret), 0, ret)) %>%
  mutate(date = as.character(date)) %>%
  mutate(yymm = substr(date, 1, 7)) %>%
  group_by(permno,yymm) %>%
  arrange(date) %>%
  mutate(cumret=cumprod(ret+1)) %>%
  ungroup()

#cumprod is cumlative products
head(crsp.sample)
summary(crsp.sample)

crsp.ew = crsp.sample %>%
  group_by(date) %>%
  summarise(ew=mean(cumret), yymm = first(yymm)) %>%
  ungroup()

head(crsp.ew)

#filter is a select function, it will return all data which satisify the condition
crsp.ew.2010.02=crsp.ew %>% filter(yymm=='2010-02')
head(crsp.ew.2010.02)
plot(crsp.ew.2010.02$ew~
       as.Date(crsp.ew.2010.02$date),
     type='l',xlab='date',ylab='ew.id',
     main='Equal Weighted Index 2010-02')

# calculate return on index on each day
#Code below has calculated the ret of each month
crsp.ew = crsp.ew %>% 
  group_by(yymm) %>% 
  arrange(date) %>%
  mutate(ret=ew/lag(ew,1)-1) %>% ungroup()
head(crsp.ew)
summary(crsp.ew)
# fill the first date of each month with the return on that day
crsp.ew = crsp.ew %>% 
  mutate(ret=ifelse(is.na(ret),ew-1,ret))

# cumulative returns over all periods
crsp.ew = crsp.ew %>% 
  arrange(date) %>% 
  mutate(ew.all = cumprod(ret+1))
head(crsp.ew)

# visualise equal weight index cumulative returns 
plot(crsp.ew$ew.all~as.Date(crsp.ew$date),
     type='l',xlab='date',ylab='ew.id',
     main='Equal Weighted Index')

save(crsp.ew,file='crsp.ew.RData')


