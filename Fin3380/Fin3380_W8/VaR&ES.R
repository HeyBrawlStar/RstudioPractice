# Gaussian VaR


#Var = -(sigma * Za + mean) * Invest
#We need to calculate VaR(0.01) and VaR(0.05) in 2013
#Invest  = 100m
setwd("/Users/guangyaohe/Desktop/FIN3380/RstudioPractice/Fin3380/Fin3380_W8")
load("crsp.ew.RData")
head(crsp.ew)
crsp.ew = crsp.ew[which(substr(crsp.ew$date,1,4) == "2013"), c("date", "ret")]
head(crsp.ew)

#remember to add na.rm = T when calculate mean and sd
sigma = sd(crsp.ew$ret, na.rm = T)
print(paste0("sigma = ",sigma))

mu = mean(crsp.ew$ret, na.rm = T)
print(paste0("mu = ",mu))

q1 = qnorm(0.01)
q2 = qnorm(0.05)


#In here, VaR means the amount you will lose will be less than VaR.q1, the possiblity is 1-q1;
#which means the possibility that you will lose over than VaR.q1 will be only q1
invest = 1e6
VaR.q1 = -(sigma * q1 + mu)* invest
print(paste0("VaR(0.01) = ", VaR.q1))

VaR.q2 = -(sigma * q2 + mu)* invest
print(paste0("VaR(0.05) = ", VaR.q2))


# Historical VaR
var.hist.q1 = -invest * (quantile(crsp.ew$ret, probs = 0.01, na.rm = T, type = 3 ))
var.hist.q2 = -invest * (quantile(crsp.ew$ret, probs = 0.05, na.rm = T, type = 3 ))

print(paste0("VaR(0.01) = ", var.hist.q1))
print(paste0("VaR(0.05) = ", var.hist.q2))

#use density() to built a kernal density estimate
ret.d = density(crsp.ew$ret)
plot(ret.d,
     xlab = "return",
     ylab = "",
     yaxt = "n",
     main = "Return Density 2013 And 1%, 5%, 1-Day Historical VaR"
     )

# the x lab is return, for historical VaR, the return should be -var.hist.q1 / invest
abline(v = -var.hist.q1/invest, col = "gray", lty = 1)
abline(v = -var.hist.q2/invest, col = "black", lty = 2)

#then draw a normal distrubution grath by using seq() and dnorm()

x = seq(min(crsp.ew$ret), max(crsp.ew$ret), length = 1000)
y = dnorm(x, mu, sigma)
lines(x, y, type = "l", col = "black", lwd = 1, lty = 3)

#lty: 1为实线；2为虚线；3为离散点
legend("topleft",
       c("Return Dist",
         "Normal Dist",
         "1% 1-Day VaR",
         "5% 1-Day VaR"),
       col = c("black", "black", "gray", "black"),
       lty = c(1,3,1,2)
)


#Gaussian ES; remember multiply invest!!!
ES.q1 = (mu+sigma* dnorm(qnorm(0.01))/0.01) * invest
print(paste0("ES.q1 = ",ES.q1))

ES.q2 = (mu+sigma* dnorm(qnorm(0.05))/0.05) * invest
print(paste0("ES.q2 = ",ES.q2))

#historical ES: the mean of the loss exceed historical VaR
crsp.ew$pl = crsp.ew$ret * invest

#First set two empty cols
crsp.ew$dummy.q1 = NA
crsp.ew$dummy.q2 = NA

#some are 1, the rest are NA
crsp.ew[which(crsp.ew$pl <= -var.hist.q1),]$dummy.q1 = 1
crsp.ew[which(crsp.ew$pl <= -var.hist.q2),]$dummy.q2 = 1

#calculate es.hist
crsp.ew$es.q1 = - crsp.ew$dummy.q1 * crsp.ew$pl
crsp.ew$es.q2 = - crsp.ew$dummy.q2 * crsp.ew$pl

es.hist.q1 = mean(crsp.ew$es.q1)
es.hist.q2 = mean(crsp.ew$es.q2)


