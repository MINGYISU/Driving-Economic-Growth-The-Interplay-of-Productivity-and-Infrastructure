source("SeasonalAnalysis")

# Part C Analyze the comovements between two series

# create a scatter plot for the two series
plot(lExpQ, lExpQ49, pch = 21, col = "darkgreen", 
     bg = "green", xlab = "log(Expenditure) Data 67", 
     ylab = "log(Expenditure) Data 49", 
     main = "Comovement between log(Expenditure) Data 67 And 49")
# conclusion: positive relationship

# create ascatter plot for the cycles of two series
plot(C, C_49, pch = 21, col = "darkred", 
     bg = "red", xlab = "log(Expenditure) Data 67", 
     ylab = "log(Expenditure) Data 49", 
     main = "Comovement Between the Cyclical Components of log(Expenditure) Data 67 and 49")
# conclusion: No relationship

# plot the two cycles
dat <- cbind(C, C_49)
plot(dat,
     main="Cyclical Components of log(Expenditure) Data 67 and 49", 
     ylab = c("Data 67", "Data 49"))

