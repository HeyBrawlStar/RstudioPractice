library(RPostgres)
q = "select distinct ticker, permno from CRSP.DSENAMES where ticker in ('MSFT','AAPL')"
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
plot(msft$prc~as.Date(msft$date),type='l',ylab='price',xlab='date',
     main='MSFT: CLOSE PRICE')

