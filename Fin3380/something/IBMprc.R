library(RPostgres)
wrds = dbConnect(Postgres(),
                 host = 'wrds-pgdata.wharton.upenn.edu',
                 port = 9737,
                 user = 'peterhe',
                 password = 'wrds118010090',
                 dbname = 'wrds',
                 sslmode = 'require')
q = "select distinct ticker, permno from CRSP.DSENAMES where ticker = 'IBM'"
res = dbSendQuery(wrds, q)
IBM.id = dbFetch(res, n=-1)
dbClearResult(res)
print(IBM.id)

q=paste0("select date,prc from CRSP.DSF where permno=", IBM.id$permno, "and date>='2015-01-01'")
print(q)
res = dbSendQuery(wrds, q)
IBMprc = dbFetch(res, n=-1)
dbClearResult(res)
print(head(IBMprc))