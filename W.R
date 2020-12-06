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


