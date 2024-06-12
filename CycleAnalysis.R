source("TrendAnalysis.R")

# create a plot for the detrended series
CSI <- lExpQ - ltrend2
plot(CSI, col = "cornflowerblue", lwd = 2,
     main="Detrended Expenditure Series (Log-Scale) using a Quadratic Trend",
     ylab="log(Expenditure)")


# 4
# create a cyclical component chart for the series
dec <- decompose(CSI, filter = rep(1/5, 5))
C <- dec$trend

plot(C, col = "red", lwd = 2, lty = 1, 
     main = "Cyclical Component of Detrended Expenditure Series (Log-Scale)", 
     ylab = "log(Expenditure)")


# 5
# create a low frequency component chart for the series
CT <- ltrend2 + C

plot(CT, lwd = 2, col = "red", main = "The Low Frequency Component of Expenditure(Log-Scale)", 
     ylab = "log(Expenditure)")

