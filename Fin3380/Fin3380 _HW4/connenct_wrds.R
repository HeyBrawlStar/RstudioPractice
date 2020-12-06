#fin3380_hw4
#Connenct to wrds
source("/Users/guangyaohe/Desktop/FIN3380/RstudioPractice/Fin3380/Fin3380 _HW4/mywrds.R")
tickers = c("AAPL","MSFT","IBM","BABA","GE","KO")
tickers = paste0(tickers,collapse = "','")
q=paste0("select distinct permno,ticker
         from CRSP.DSENAMES where ticker in 
         ('",tickers,"')")
res = dbSendQuery(wrds, q)
tickers = dbFetch(res, n=-1)
dbClearResult(res)
save(tickers, file = "ticker.RData")
q=paste0("select permno,date,ret
         from CRSP.DSF where permno in (",
         paste0(tickers$permno,collapse = ','),
         ") and date>='2010-01-01' and date<='2018-12-31'")
res = dbSendQuery(wrds, q)
ret.all = dbFetch(res, n=-1)
dbClearResult(res)
save(ret.all,file="ret.all.RData")