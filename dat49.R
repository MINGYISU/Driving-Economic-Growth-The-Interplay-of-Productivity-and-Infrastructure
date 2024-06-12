data49 <- read.csv("dat49.csv")
ExpQ49 <- ts(data49[3], frequency = 4, start = c(1950, 1))
lExpQ49 <- log(ExpQ49)

t <- time(lExpQ49, offset = 0.5)
t2 <- t ^ 2

# fit Trend
lcoefT_49 <- coef(lm(lExpQ49 ~ t))
lcoefT2_49 <- coef(lm(lExpQ49 ~ t + t2))
ltrend_49 <- lcoefT_49[1] + lcoefT_49[2] * t
ltrend2_49 <- lcoefT2_49[1] + lcoefT2_49[2] * t + lcoefT2_49[3] * t2
plot(lExpQ49, lwd=2, col = "cornflowerblue", 
     main="Expenditure 49 With a Linear and Quadratic Trends (Log-Scale)",
     ylab="log(Expenditure) 49")
lines(ltrend_49, col = "green", lty=1, lwd = 3)
lines(ltrend2_49, col = "red", lty=3, lwd = 4)
legend("topleft", c("log(Expenditure) 49", "Linear","Quadratic"), 
       col=c("cornflowerblue", "green", "red"), lty = c(1, 1, 3),
       lwd = c(1, 3, 4), bty='n')

CSI_49 <- lExpQ49 - ltrend2_49
plot(CSI_49, col = "cornflowerblue", lwd = 2,
     main="Detrended Expenditure Series (Log-Scale) 49 using a Quadratic Trend",
     ylab="log(Expenditure)")

dec_49 <- decompose(CSI_49, filter = rep(1/5, 5))
C_49 <- dec_49$trend

plot(C_49, col = "red", lwd = 2, lty = 1, 
     main = "Cyclical Component of Detrended Expenditure Series (Log-Scale) 49", 
     ylab = "log(Expenditure) 49")


