library(RPostgres)
wrds = dbConnect(Postgres(),
                 host='wrds-pgdata.wharton.upenn.edu', port=9737,
                 user='peterhe',
                 password='wrds118010090', dbname='wrds',
                 sslmode='require')
