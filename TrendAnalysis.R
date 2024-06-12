source("GrowthRateAnalysis.R")

# Part B create trends for the series

# 1
# create a liner trend 
t <- time(ExpQ, offset = 0.5)
coefT <- coef(lm(ExpQ~t))
trend <- coefT[1] + coefT[2] * t

# create a quadratic trend
t2 <- t ^ 2
coefT2 <- coef(lm(ExpQ ~ t + t2))
trend2 <- coefT2[1] + coefT2[2] * t + coefT2[3] * t2

# plot both trends
plot(ExpQ, lwd = 2, col = "cornflowerblue", 
     main = "Expenditure With a Linear and Quadratic Trends", 
     ylab = "Expenditure")
lines(trend, col = "green", lty = 2, lwd = 2)
lines(trend2, col = "red", lty = 3, lwd=2)
legend("topleft", c("Expenditure", "Linear", "Quadratic"), 
       col = c("cornflowerblue", "green", "red"), lty = c(1, 2, 3),
       lwd=2, bty='n')

# 2 create trends for log-scale series
lcoefT <- coef(lm(lExpQ ~ t))
lcoefT2 <- coef(lm(lExpQ ~ t + t2))
ltrend <- lcoefT[1] + lcoefT[2] * t
ltrend2 <- lcoefT2[1] + lcoefT2[2] * t + lcoefT2[3] * t2
plot(lExpQ, lwd=2, col = "cornflowerblue", 
     main="Expenditure With a Linear and Quadratic Trends (Log-Scale)",
     ylab="log(Expenditure)")
lines(ltrend, col = "green", lty=1, lwd = 3)
lines(ltrend2, col = "red", lty=3, lwd = 4)
legend("topleft", c("log(Expenditure)", "Linear","Quadratic"), 
       col=c("cornflowerblue", "green", "red"), lty = c(1, 1, 3),
       lwd = c(1, 3, 4), bty='n')

