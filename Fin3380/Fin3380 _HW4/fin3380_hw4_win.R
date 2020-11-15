source("F:/RstudioPractice/RstudioPractice/Fin3380/Fin3380_HW4/mywrds.R")
setwd("F:RstudioPractice/RstudioPractice/Fin3380/Fin3380 _HW4")
load("ret.all.Rdata")
library(dplyr)
library(tidyr)

#Q1
var.6 = function(w1,w2,w3,w4,w5,
                 var1,var2,var3,var4,var5,var6,
                 cov1.1,cov1.2,cov1.3,cov1.4,cov1.5,cov,){
  
}













tickers = c("AAPL","MSFT","IBM","BABA","GE","KO")
tickers = paste0(tickers,collapse = "','")
q=paste0("select distinct permno,ticker
         from CRSP.DSENAMES where ticker in 
         ('",tickers,"')")
res = dbSendQuery(wrds, q)
tickers = dbFetch(res, n=-1)
dbClearResult(res)
q=paste0("select permno,date,ret
         from CRSP.DSF where permno in (",
         paste0(tickers$permno,collapse = ','),
         ") and date>='2010-01-01' and date<='2018-12-31'")
res = dbSendQuery(wrds, q)
ret.all = dbFetch(res, n=-1)
dbClearResult(res)
save(ret.all,file="ret.all.RData")

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

cor.m = cor(ret.all[,2:ncol(ret.all)])
cor.m


#Q2
#Daily return
load("crsp.sample.Rdata")
stocks = crsp.sample %>%
  filter(permno %in% c(39731,79089, 83148)) %>%
  group_by(date) %>%
  arrange(date) %>%
  mutate(ret.daily = mean(ret)) %>%
  ungroup()

head(stocks)














