setwd("/Users/guangyaohe/Desktop/FIN3380/RstudioPractice/Fin3380/Fin3380_Quiz8")
ff.raw = read.fwf("F-F_Research_Data_Factors.txt",widths=c(6,8,8,8,8),skip=4)
ff.raw = ff.raw[-which(is.na(ff.raw$V1))[1]:-nrow(ff.raw),]
colnames(ff.raw) = c("text.date","rmxrf","smb","hml","rf")
dt = ff.raw$text.date
ff.raw = ff.raw[,c(-1)]

ff.raw = data.frame(apply(ff.raw,2,function(x) as.numeric(x)/100))

ff.raw$date = seq(as.Date("1926-07-01"),as.Date("2019-06-30"),by="months")

head(ff.raw)
save(ff.raw,file='ff.raw.RData')

