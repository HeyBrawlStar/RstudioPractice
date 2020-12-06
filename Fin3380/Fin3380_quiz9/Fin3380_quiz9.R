setwd("/Users/guangyaohe/Desktop/FIN3380/RstudioPractice/Fin3380/Fin3380_quiz9")
library(zoo)
library(tidyr)
library(dplyr)
load('china.stock.sample.quiz8.RData')
sse = read.csv("SH000001.csv")
head(china.stock.sample)

#SSE daily ret
sse = sse %>% 
  mutate(ret.sse = close/lag(close,1)-1) %>%
  select(-close) %>%
  mutate(date = as.Date(date))

#stcoks sample daily ret
stocks = china.stock.sample %>%
  select(issue_id,date,close) %>%
  group_by(issue_id) %>%
  arrange(date) %>%
  mutate(ret.stocks = close/lag(close,1)-1) %>%
  ungroup() %>%
  mutate(date = as.Date(date))

class(stocks$date)


capm = inner_join(stocks, sse, by = "date") %>%
  select(-close) %>%
  na.omit()


summary = capm %>%
  group_by(issue_id) %>%
  arrange(date) %>%
  summarise(start=first(date),
            end=last(date),
            n=n())

summary

capm = capm %>%
  group_by(issue_id) %>%
  mutate_at(vars(ret.sse,ret.stocks), ~.-0.0025/30) %>%
  ungroup() %>%
  mutate(date = as.character(date)) %>%
  mutate(yymm = substr(date,1,7)) %>%
  group_by(issue_id, yymm) %>%
  summarise(beta = summary(lm(ret.stocks~ret.sse), data = capm)$coefficients[2]) %>%
  ungroup()

capm = spread(capm, key = issue_id, value =beta)
capm

  

