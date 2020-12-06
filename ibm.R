library(RPostgres)
wrds = dbConnect(Postgres(),
                 host = 'wrds-pgdata.wharton.upenn.edu',
                 port = 9737,
                 user = 'peterhe',
                 password = 'wrds118010090',
                 dbname = 'wrds',
                 sslmode = 'require')

text1 = "select date, prc from CRSP.DSF where permno = "
text2 = IBM$permno
text3 = "and date>= '2015-01-01'"
q = paste0(text1, text2, text3)

res = dbSendQuery(wrds, q)
IBM = dbFetch(res, n=-1)
dbClearResult(res)

print(head(IBM))