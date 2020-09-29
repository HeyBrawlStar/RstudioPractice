price = c(5.2,3.3,7.0,9.0,8.0,9)
stockid = c("AAPL","AAPL","MSFT","MSFT","BP","BP")
date = as.Date(c("2018-01-02","2018-01-05","2018-01-02","2018-01-05","2018-01-02","2018-01-05"))
sector = c(rep("IT",4),rep("energy",2))
stock.df = data.frame(stock.id = stockid, date = date, price = price, sector = sector,
                      stringsAsFactors = T)

print(stock.df)
str(stock.df)
stock.df[c("stock.id","date")]
stock.df[-which(colnames(stock.df) == "stock.id")]

stock.df$shares = c(100,100,300,300,1000,1000)
stock.df$mcap = stock.df$price * stock.df$shares
stock.df[c("new1","new2")] = NA
stock.df
subset(stock.df, select = -c(new1,new2))
stock.df
subset(stock.df, select = c(stock.id, date, price))

stock.df$flag = stock.df$stock.id == "MSFT" 
stock.df[stock.df$flag==F,]
stock.df = subset(stock.df, select = -c(new1,new2,flag))
order(stock.df$date)
stock.df[order(stock.df$price),]
stock.df
stock.df$mcap[6] = NA
stock.df
stock.df[order(stock.df$mcap,na.last = T, decreasing = T),]
4
aggregate(mcap~stock.id, data = stock.df, FUN = function(x) mean(x,na.rm = T))






