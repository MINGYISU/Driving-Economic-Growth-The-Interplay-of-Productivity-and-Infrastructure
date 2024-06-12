source("CycleAnalysis.R")

# create the seasonal component chart for the series
S <- dec$figure
barplot(S, main="Expenditure Seasonal Component", xlab="Time", 
        ylab = "% (in percentage)", 
        col = "cornflowerblue", names.arg = c("Qtr1","Qtr2", "Qtr3", "Qtr4"))


# deseasonalizing the series
des_ExpQ <- ExpQ - S

plot(ExpQ, main="Expenditure Series with its Deseasonalized Series", lwd=3)
lines(des_ExpQ, col=2, lty=2, lwd=2)
legend("topleft", c("Non-adjusted", "adjusted"), col=1:2, lty=1:2,
       lwd=2, bty='n')

