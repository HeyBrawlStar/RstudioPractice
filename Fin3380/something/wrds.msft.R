library(RPostgres)
wrds = dbConnect(Postgres(),
                 host = 'wrds-pgdata.wharton.upenn.edu',
                 port = 9737,
                 user = 'peterhe',
                 password = 'wrds118010090',
                 dbname = 'wrds',
                 sslmode = 'require')

res = dbSendQuery(wrds, "select date,dji from djones.djdaily")

dj = dbFetch(res, n=-1)
dbClearResult(res)
print(head(dj) )

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

plot(aapl$prc~aapl$date,type='l',ylab='price',xlab='date',
     main='AAPL: CLOSE PRICE')

dim(msft)
dim(aapl)
all(dim(msft)==dim(aapl))

print(summary(aapl))
print(summary(msft))

print(head(msft))
print(head(msft[-1,]))
which(msft$vol==1)

msft = msft[-which(msft$vol == 1),]
summary(msft)


msftret = msft %>% select(date,prc)
print(head(msftret))

msftret = msftret %>% 
  arrange(date) %>%
  mutate(lag.prc = lag(prc,1))

print(head(msftret))

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

# merge aapl and msft returns based on date
msft.aapl=aaplret %>% inner_join(msftret,by="date")
print(head(msft.aapl))

# correlation between two returns
print(cor(msft.aapl$aapl.ret,msft.aapl$msft.ret,use = "complete.obs"))


# relative performance of msft and aapl
msft.aapl = msft.aapl %>% mutate(msft.aapl.ret=msft.ret-aapl.ret)

# summarise your long-short returns 

summary(msft.aapl$msft.aapl.ret)

# cumulative returns of long-short position
msft.aapl = msft.aapl %>% arrange(date) %>% 
  filter(!is.na(msft.aapl.ret)) %>%
  mutate(cumret=cumprod(msft.aapl.ret+1))

# plot your cummulative return 
plot(msft.aapl$cumret~as.Date(msft.aapl$date),
     type='l',ylab='price',xlab='date', 
     main='Long MSFT-Short AAPL')

























