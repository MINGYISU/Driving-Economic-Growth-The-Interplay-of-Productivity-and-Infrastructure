data = read.csv("dat67.csv")
ExpQ <- ts(data[3], frequency = 4, start = c(1950, 1))

# Part A
# 1
plot(ExpQ, col = "red", lwd = 2, lty = 1, 
     ylab = "Expenditure", 
     xlab = "Time", main = "Expenditure")


# 2
# Log Scale
lExpQ <- log(ExpQ)
plot(lExpQ, col = "blue", lwd = 2, lty = 1, 
     ylab = "log(Expenditure)", 
     xlab = "Time", main = "Expenditure: Log Scale")

# 3
# Annualized Growth Rate of ExpQ
g <- diff(ExpQ)/lag(ExpQ, -1)
g <- (1 + g) ^ 12 - 1
g <- g * 100
plot(g, col = "yellow", lwd = 2, lty = 1, 
     ylab = "% (in percentage)", 
     xlab = "Time", main = "Annualized Growth Rate of Expenditure")

# Annualized Growth Rate of log(ExpQ)
lg <- diff(lExpQ) * 100
plot(lg, col = "green", lwd = 2, lty = 1, 
     ylab = "% (in percentage)", 
     xlab = "Time", main = "Annualized Growth Rate of Expenditure: Log Scale")



# Part B

# 1
# create liner trend 
t <- time(ExpQ, offset = 0.5)
coefT <- coef(lm(ExpQ~t))
trend <- coefT[1] + coefT[2] * t

# create quadratic trend
t2 <- t ^ 2
coefT2 <- coef(lm(ExpQ ~ t + t2))
trend2 <- coefT2[1] + coefT2[2] * t + coefT2[3] * t2

# plot
plot(ExpQ, lwd = 2, col = "cornflowerblue", 
     main = "Expenditure With a Linear and Quadratic Trends", 
     ylab = "Expenditure")
lines(trend, col = "green", lty = 2, lwd = 2)
lines(trend2, col = "red", lty = 3, lwd=2)
legend("topleft", c("Expenditure", "Linear", "Quadratic"), 
       col = c("cornflowerblue", "green", "red"), lty = c(1, 2, 3),
       lwd=2, bty='n')

# 2
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


# 3
# detrended series
CSI <- lExpQ - ltrend2
plot(CSI, col = "cornflowerblue", lwd = 2,
     main="Detrended Expenditure Series (Log-Scale) using a Quadratic Trend",
     ylab="log(Expenditure)")


# 4
# cycle
dec <- decompose(CSI, filter = rep(1/5, 5))
C <- dec$trend

plot(C, col = "red", lwd = 2, lty = 1, 
     main = "Cyclical Component of Detrended Expenditure Series (Log-Scale)", 
     ylab = "log(Expenditure)")


# 5
# low frequency
CT <- ltrend2 + C

plot(CT, lwd = 2, col = "red", main = "The Low Frequency Component of Expenditure(Log-Scale)", 
     ylab = "log(Expenditure)")

# 6
# seasonal
S <- dec$figure
barplot(S, main="Expenditure Seasonal Component", xlab="Time", 
        ylab = "% (in percentage)", 
        col = "cornflowerblue", names.arg = c("Qtr1","Qtr2", "Qtr3", "Qtr4"))

# Not Required
# deseasonalizing 
des_ExpQ <- ExpQ - S

plot(ExpQ, main="Expenditure Series with its Deseasonalized Series", ylab="$ (million dollars)", lwd=3)
lines(des_ExpQ, col=2, lty=2, lwd=2)
legend("topleft", c("Non-adjusted", "adjusted"), col=1:2, lty=1:2,
       lwd=2, bty='n')

# Part C

# 1
plot(lExpQ, lExpQ49, pch = 21, col = "darkgreen", 
     bg = "green", xlab = "log(Expenditure) Data 67", 
     ylab = "log(Expenditure) Data 49", 
     main = "Comovement between log(Expenditure) Data 67 And 49")
# con: positive relationship

# 2
plot(C, C_49, pch = 21, col = "darkred", 
     bg = "red", xlab = "log(Expenditure) Data 67", 
     ylab = "log(Expenditure) Data 49", 
     main = "Comovement Between the Cyclical Components of log(Expenditure) Data 67 and 49")
# con: No positive relationship

dat <- cbind(C, C_49)
plot(dat,
     main="Cyclical Components of log(Expenditure) Data 67 and 49", 
     ylab = c("Data 67", "Data 49"))
