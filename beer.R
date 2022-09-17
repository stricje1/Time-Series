################################################################
# BEER SALES
################################################################
require(TSA)
library(timeSeries)
library(ggplot2)
library(forecast)
library(dplyr)
library(colortools)
library(TTR)
library(fpp2)
library(TSA)
library(stlplus)
library(graphics)

#Read the Beer data
data(beersales)
class(beersales)
beersales1<-window(beersales, start=1975)
#Build a time series
beer2.ts <- ts(beersales1, start=c(1991, 1), end=c(1995, 12), 
               frequency=12)
#par(mfrow=c(1,1), cex = 1)
plot(beer2.ts, col=4, lwd=3, 
     cex = 1.5, xlab = '', ylab = '', yaxt = 'n', xaxt='n', 
     panel.first = grid(10, lty = 3, lwd = 2, col = 'darkgray'))
mtext(side=1, line=3, 'Years', col = 'navy', font=2, cex=1.5)
mtext(side=2, line=3, 'Monthly Beer Sales', col = 'navy', font=2, cex=1.5)
mtext(side=3, line=0.5, "Monthly Beer Sales in Millons of Barrels", 
      col="darkorchid3", font=2, cex=2)
axis(1, cex.axis = 1.5, font=2)
axis(2, cex.axis = 1.5, font=2, las = 2)

#Apply exponential smoothing
beer.fit <- hw(beersales, h=48)
#Build a forecast
beer.for<-forecast(beer.fit, h=48)

autoplot(beer.for) +
  autolayer(beersales, series="Monthly Beer Sales", color="purple", size = 1.2) +
  autolayer(beer.fit, series="Holt-Wintrs additive model", PI=FALSE, color="blue", size = 1.2) +
  autolayer(beer.for, series="Holt-Wintrs additive forecast", PI=FALSE, color="red", size = 1.2) +
  ggtitle("Forecasts from Holt-Wintrs additive method") + xlab("Year") +
  ylab("Monthly Beer Sales in Millions of Barrels") +
  guides(colour=guide_legend(title="Forecast")) +
  theme(legend.position="bottom")

#Develop the ACF and PACF
par(mfrow=c(2,1))
acf(beer.fit, lag.max=48, col="red", lwd=2, main="Holt-Winters additive model  ACF")
acf(beer.fit$residuals, lag.max=48, col="red", lwd=2, main="Holt-Winters additive model residuals PACF")

## acf of the CO2 data
beer.acf3 <- acf(beer.fit, lag.max = 2)
## correlogram of the CO2 data
plot.acf(beer.acf3)
## PACF of the CO2 data
beer.acf4 <- acf(beer.fit$residuals, lag.max = 50)
## correlogram of the CO2 data
plot.acf(beer.acf4)
ccf(beer.fit$x, beer.fit$residuals, lag.max = 20, type="correlation")

par(mfrow=c(2,1), cex.axis = 1.25, cex.lab = 1.5, font=2, mar = c(5,6,3,1))
plot.acf(beer.acf3)
title(main="Beer ACF using Holt-Winters", cex.main=1.5, font.main=2, 
      col.main="navy", xlab = "Lag", ylab = "Correlation", col.lab ="darkorchid")
plot.acf(beer.acf4)
title(main="Beer PACF using Holt-Winters", cex.main=1.5,  font.main=2, 
      col.main="navy", xlab = "Lag", ylab = "Correlation", col.lab ="darkorchid")

plot(beer.acf3)
mtext(side=1, line=3, 'Lags', col = 'navy', font=2, cex=1.5)
mtext(side=2, line=3, 'Correlation', col = 'navy', font=2, cex=1.5)
mtext(side=3, line=0.5, "Beer ACF using Holt-Winters", 
      col="darkorchid3", font=2, cex=2)
axis(1, cex.axis = 1.25, font=2)
axis(2, cex.axis = 1.25, font=2, las = 2)

plot(beer.acf5)
mtext(side=1, line=3, 'Lags', col = 'navy', font=2, cex=1.5)
mtext(side=2, line=3, 'Correlation', col = 'navy', font=2, cex=1.5)
mtext(side=3, line=0.5, "Beer ACF using Holt-Winters", 
      col="darkorchid3", font=2, cex=2)
axis(1, cex.axis = 1.5, font=2)
axis(2, cex.axis = 1.5, font=2, las = 2)

#Apply one difference to the time series data
beer.diff1 <- diff(beer2.ts, differences=1)
#Fit the differenced time series
beer.fit1 <- hw(beer.diff1, h=48)
#Build the forecast for the differenced time series
beer.for1<-forecast(beer.fit1, h=48)
#Develop the ACF and PACF for the differenced time series
acf(beer.for1$residuals, lag.max=48, col="red", lwd=2,
    main="Holt-Wintrs additive model residuals ACF")
pacf(beer.for1$residuals, lag.max=48, col="red", lwd=2, main="Holt-Wintrs additive model residuals PACF")

beer.fit <- holt(beersales, damped=TRUE, h=2)
## acf of the CO2 data
beer.acf2 <- acf(beer.diff1, lag.max = 36)
## correlogram of the CO2 data
plot.acf(beer.acf2)



# Compare differencing to holt
beer.diff1<-diff(beer2.ts, differences=1)
## PACF of the CO2 data
beer.acf1 <- acf(beer.diff1, lag.max = 36)
## correlogram of the CO2 data
plot.acf(beer.acf1)

beer.fit <- holt(beersales, damped=TRUE, h=2)
## acf of the CO2 data
beer.acf2 <- acf(beer.fit, lag.max = 36)
## correlogram of the CO2 data
plot.acf(beer.acf2)



plot.acf(beer.acf1)
title(main="Beer using differencing = 1", cex.main=1.5, font.main=2, col.main="blue", col.lab ="darkblue")
plot.acf(beer.acf2)
title(main="Beer using Holt damped ACF", cex.main=1.5,  font.main=2, col.main="blue", col.lab ="darkblue")

# Compare differncing to holt
beer.diff1<-diff(beer2.ts, differences=2)
# ACF of the differenced beer data
beer.acf1 <- acf(beer.diff1, lag.max = 36)
# PACF of the differenced beer data
beer.pacf1 <- pacf(beer.diff1, lag.max = 36)
# correlograms of the differenced beer data
par(mfrow=c(2,1), cex.axis = 1.25, cex.lab = 1.5, font=2, mar = c(5,6,3,1))
plot.acf(beer.acf1)
title(main="Beer ACF using differencing = 2", cex.main=1.5, font.main=2, 
      col.main="navy", xlab = "Lag", ylab = "Correlation", col.lab ="darkorchid")
plot.acf(beer.pacf1)
title(main="Beer PACF using differencing = 2", cex.main=1.5,  font.main=2, 
      col.main="navy", xlab = "Lag", ylab = "Correlation", col.lab ="darkorchid")




plot.acf(beer.acf1)
title(main="Beer ACF using differencing = 2", cex.main=1.25, font.main=2, col.main="blue", col.lab ="darkblue")
plot.acf(beer.pacf1)
title(main="Beer PACF using differencing = 2", cex.main=1.25,  font.main=2, col.main="blue", col.lab ="darkblue")

beer.fit <- holt(beersales, damped=TRUE, h=2)
## acf of the CO2 data
beer.pacf2 <- pacf(beer.fit, lag.max = 36)
## correlogram of the CO2 data
plot.acf(beer.pacf2)

plot.acf(beer.pacf1)
plot.acf(beer.pacf2)

plot(beer.fit, type = "h", lwd = 2, col="red")
plot(beer2.ts,col=4)
################################################################
################################################################

devtools::install_github("FinYang/tsdl")
install.packages("Rtools")
births <- read.csv("D:\\Documents\\DATA\\nyc_births.txt")

birthstimeseries <- ts(births, frequency=12, start=c(1946,1))
birthstimeseries
plot.ts(birthstimeseries, col = "blue")
birthstimeseriescomponents <- decompose(birthstimeseries)
birthstimeseriescomponents$seasonal # get the estimated values of the seasonal component
plot(birthstimeseriescomponents, plot.type="m", col=c("blue", "orange", "darkgreen","red"))
birthstimeseriesseasonallyadjusted <- birthstimeseries - birthstimeseriescomponents$seasonal
plot(birthstimeseriesseasonallyadjusted)
par(mfrow=c(4,1), mai = c(.25, .55, .2, .1))
p1<-plot(birthstimeseriescomponents$x, col = "blue", ylim=c(1,50), ylab="observed", cex.lab=3, 
         cex.main=1.5, main="Decomposition of additive time series")
p2<-plot(birthstimeseriescomponents$trend, col = "red", ylab="trend", cex.lab=3)
p3<-plot(birthstimeseriescomponents$seasonal, col = "dark green", ylab="seasonal", cex.lab=3)
p4<-plot(birthstimeseriescomponents$random, col = "purple", ylab="random", cex.lab=3)
mtext("Decomposition of additive time series", outer=TRUE,  cex=1, line=-0.5)