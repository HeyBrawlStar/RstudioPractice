setwd("F:/RstudioPractice/RstudioPractice/Fin3380/Fin3380_quiz7/")
library(dplyr)
dj = read.csv("dj.csv")

dj = dj %>% 
  arrange(date) %>%
  mutate(ret = dji/lag(dji,1)-1) %>%
  na.omit()
invest = 1e6
dj.2006 = dj %>% filter(substr(date,1,4) == "2006")
var.hist.q5 = -invest * quantile(dj.2006$ret, probs=0.05, type=3, na.rm=T)
print(paste0("VaR(0.05) = ",var.hist.q5))

      