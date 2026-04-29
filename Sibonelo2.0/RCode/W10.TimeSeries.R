install.packages(c("ggplot2", "forecast", "fpp2"))
library(ggplot2)
library(forecast)
library(fpp2)

df <- forecast::wineind

df2  <- as.ts(df)

autoplot(df2) + autolayer(moving_average,series = "MA12")+ ggtitle("RLAb prep, chowing this in 2 mins") + ylab("wineStuff")

moving_average <- ma(df2, 12)
autolayer(moving_average)

decompMod <- decompose(df2, type = 'multiplicative')
plot(decompMod)
