source("datapreprocess.R")

# create time series for the source data
data = read.csv("dat67.csv")
ExpQ <- ts(data[3], frequency = 4, start = c(1950, 1))

# Part A
# create a line chart for the time series
plot(ExpQ, col = "red", lwd = 2, lty = 1, 
     ylab = "Expenditure", 
     xlab = "Time", main = "Expenditure")


# create a line chart for the time series Log Scale
lExpQ <- log(ExpQ)
plot(lExpQ, col = "blue", lwd = 2, lty = 1, 
     ylab = "log(Expenditure)", 
     xlab = "Time", main = "Expenditure: Log Scale")

# create a line chart for the annualized growth rate of the series
# Annualized Growth Rate of ExpQ
g <- diff(ExpQ)/lag(ExpQ, -1)
g <- (1 + g) ^ 12 - 1
g <- g * 100
plot(g, col = "yellow", lwd = 2, lty = 1, 
     ylab = "% (in percentage)", 
     xlab = "Time", main = "Annualized Growth Rate of Expenditure")

# Annualized Growth Rate using log method
lg <- diff(lExpQ) * 100
plot(lg, col = "green", lwd = 2, lty = 1, 
     ylab = "% (in percentage)", 
     xlab = "Time", main = "Annualized Growth Rate of Expenditure: Log Scale")

