setwd("F:/RstudioPractice/RstudioPractice/Fin3380/Fin3380_risk")
library(RPostgres)
library(tidyr)
library(dplyr)

source("F:/RstudioPractice/RstudioPractice/Fin3380/Fin3380_risk/mywrds.R")
tickers = c("AAPL","MSFT","IBM","BABA","GE","KO")
# get stocks' permno
q=paste0("select distinct ticker, permno from CRSP.DSENAMES where ticker in ('",
         paste0(tickers,collapse = "','"),"')")
print(q)
res = dbSendQuery(wrds, q)
stock.id = dbFetch(res, n=-1)
dbClearResult(res)
print(stock.id)
# get stocks' prices 
q=paste0("select permno,date,prc from CRSP.DSF where permno in (",paste0(stock.id$permno,collapse = ','),") and date>='2015-01-01'")
print(q)
res = dbSendQuery(wrds, q)
mystock = dbFetch(res, n=-1)
dbClearResult(res)
print(head(mystock))



