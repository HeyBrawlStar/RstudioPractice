setwd("/Users/guangyaohe/Desktop/FIN3380/RstudioPractice/Fin3380/Fin3380_W12")
library(dplyr)

#load A share stick monthly return, market cap data
load("a.shares.RData")
head(a.shares)

#ret.mkt is the excess ret of the stock
size.ret = a.shares %>% 
  select(-rf) %>%
  mutate(ret.mkt = ret-mkt)

head(size.ret)

#Step1:Break Points

#use 10%, 20%, 40%, 60%, 80%, 90% to break the size for stocks
#unnest() can unlist the list columns into a regular dataframe
p = c(0.1,0.2,0.4,0.6,0.8,0.9)
library(tidyr)
bp = size.ret %>% 
  group_by(yymm) %>%
  summarise(qt = list(paste0(p*100,"%")),
            break.point = list(quantile(mcap,p,type = 3)))
head(bp)

bp = bp %>% unnest(cols = c(qt, break.point)) %>%
  ungroup()
head(bp)

library(ggplot2)
library(zoo)

ggplot(data=bp,
       aes(x=as.yearmon(yymm),
           y=break.point,
           col=as.factor(qt))) + 
  scale_x_yearmon()+
  geom_line() +
  labs(title="Break Points: Size", 
       y="Size", 
       x="Date")




