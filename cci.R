library(vars)
library(fpp2)
library(fma)
library(timeSeries)
library(ggplot2)
library(forecast)
library(dplyr)
library(colortools)
library(TTR)
library(TSA)
library(stlplus)
library(graphics)

################################################################
data(unemp.cci) 

cci <- ts(unemp.cci[,"cci"])
cci<-window(unemp.cci[,"cci"],start=1997)
plot.ts(cci, main="Consumer Cost Index",  
        lwd=3, col = "purple", lty = 1, size = 1.1) 

geom_line(color = "purple", lty = 2, size = 1.1)

cci.add<- HoltWinters(cci, beta=FALSE, gamma=FALSE,  seasonal = "additive")
cci.add

cci.mult<-HoltWinters(cci, beta=FALSE, gamma=FALSE,  seasonal = "multiplicative")
cci.mult

plot(cci.add$fitted, col=2, lwd=3, cex.axis=1.5, 
     cex.lab = 1.75, main = "Consumer Confidence Index - Unemployment")


plot(cci.add$fitted, col=2, lwd=2)
plot(cci.mult$fitted, col=2, lwd=2)

cci.smoother<- HoltWinters(cci, gamma=FALSE)
cci.smoother


plot(cci.smoother$fitted, col="purple", lwd=3, cex.axis=2, cex.lab = 1.75, main = "Consumer Confidence Index - Unemployment")


autoplot(cci.smoother$fitted) +    
  autolayer(cci.smoother$fitted, series="Observed", size = 1.75) +      
  #autolayer(fc, series="Forecast", size = 1.2) +      
  #autolayer(fc2, series="Damped trend", size = 1.2) +
  theme(legend.position="bottom", legend.text = element_text(size = rel(1.2))) + 
  guides(colour=guide_legend(title="LEGEND:", size = rel(1.52))) +
  ylab("Index Value") + xlab("Years") +
  ggtitle("Consumer Confidence Index - Unemployment)") +
  theme(plot.title = element_text(size = rel(1.75))) +
  theme(axis.title.x = element_text(size = rel(1.5), color="blue")) +
  theme(axis.title.y = element_text(size = rel(1.65), color="blue3")) +
  theme(panel.background = element_rect(fill = "grey90", size=rel(1.2)), 
        axis.text = element_text(colour = "purple", size = rel(1.5))) 



library(forecast)
cci<-window(unemp.cci[,"cci"],start=1998)
f1<-hw(cci, seasonal="additive")
f2<-hw(cci, seasonal="multiplicative")
f3<-hw(cci, damped=TRUE, seasonal="multiplicative")
f4<-hw(cci, damped=TRUE, seasonal="additive")
f5<-holt(cci,h=24,damped=TRUE,exponential=TRUE)
f6<-holt(cci,h=24,initial="optimal")
f1$model
f2$model
f3$model
f4$model
f5$model
f6$model
Box.test(f1$fitted, lag=48, type="Ljung-Box")

ts_lags(cci)

########################################
par(mar = c(2, 5, 3, .15), cex.axis = 1, cex.lab = 1.5, font = 2, bg = 'white')
data(unemp.cci) 
cci <- ts(unemp.cci[,"cci"])
cci <- window(unemp.cci[,"cci"], start = 1997)
cci.fit<-hw(cci, seasonal="additive")
cci.acf <- acf(cci.fit$fitted, 36)
cci.pacf <- pacf(cci.fit$fitted, 36)
par(mfrow=c(2,1), lwd = 3)
plot.acf(cci.acf)
axis(1, cex.lab = 3, font=2)
axis(2, cex.lab = 3, font=2, las = 2)
title(main = "CCI ACF fit with Holt_Winters Additibe method", 
      cex.main = 1.5, font.main = 2, col.main = 'blue', 
      col.lab = 'blue')
plot.acf(cci.pacf)
axis(1, cex.lab = 3, font=2)
axis(2, cex.lab = 3, font=2, las = 2)
title(main = "CCI PACF fit with Holt_Winters Additibe method", 
      cex.main = 1.5, font.main = 2, col.main = 'blue', 
      col.lab = 'blue')
Box.test(f1$fitted, lag=36, type="Ljung-Box")
################################USE THIS PLOT#############################
autoplot(cci) +
  autolayer(cci, color="blue", size=1.25) +
  autolayer(f1, series="HW Additive", PI=FALSE, size=1.25) +
  autolayer(f2, series="HW Multiplicative", PI=FALSE, size=1.25) +
  autolayer(f3, series="HW Multiplicative damped", PI=FALSE, size=1.25) +
  autolayer(f4, series="HW Additive damped", PI=FALSE, size=1.25) +
  autolayer(f5, series="Holt damped", PI=FALSE, size=1.25) +
  autolayer(f6, series="Holt", PI=FALSE, size=1.25) +
  xlab("Year") + 
  ylab("Unemployment Index") +
  ggtitle("Consumer Confidence Index (CCI)") +
  guides(colour=guide_legend(title="LEGEND")) +
  theme(legend.position="bottom")


autoplot(cci) +
  autolayer(cci, color="blue", size=1.25) +
  autolayer(f1, series="HW Additive", PI=FALSE, size=1.25) +
  autolayer(f2, series="HW Multiplicative", PI=FALSE, size=1.25) +
  autolayer(f3, series="HW Multiplicative damped", PI=FALSE, size=1.25) +
  autolayer(f4, series="HW Additive damped", PI=FALSE, size=1.25) +
  autolayer(f5, series="Holt damped", PI=FALSE, size=1.25) +
  autolayer(f6, series="Holt", PI=FALSE, size=1.25) +
  theme(legend.position="bottom", legend.text = element_text(size = rel(1.2))) + 
  guides(colour=guide_legend(title="LEGEND:", size = rel(1.52))) +
  ylab("Index Value") + xlab("Years") +
  ggtitle("Consumer Confidence Index - Unemployment)") +
  theme(plot.title = element_text(size = rel(1.75))) +
  theme(axis.title.x = element_text(size = rel(1.5), color="blue")) +
  theme(axis.title.y = element_text(size = rel(1.65), color="blue3")) +
  theme(panel.background = element_rect(fill = "grey90", size=rel(1.2)), 
        axis.text = element_text(colour = "purple", size = rel(1.5))) 
###########################################################################
print(paste0("HW Additive RMSE = ", r1))


r1<-rmse(f1$fitted,f1$lower)
r2<-rmse(f2$fitted,f2$lower)
r3<-rmse(f3$fitted,f3$lower)
r4<-rmse(f4$fitted,f4$lower)
r5<-rmse(f5$fitted,f5$lower)
r6<-rmse(f6$fitted,f6$lower)
print(paste0("HW Additive RMSE = ", r1))
print(paste0("HW Multiplicative RMSE = ", r2))
print(paste0("HW Multiplicative damped RMSE = ", r3))
print(paste0("HW Additive dampled RMSE = ", r4))
print(paste0("Holt damped RMSE = ", r5))
print(paste0("Holt RMSE = ", r6))


print(paste0("HW Additive RMSE:"));round(accuracy(f1), 4)
print(paste0("HW Multiplicative RMSE:"));round(accuracy(f2), 4)
print(paste0("HW Multiplicative damped:"));round(accuracy(f3), 4)
print(paste0("HW Additive dampled RMSE:"));round(accuracy(f4), 4)
print(paste0("Holt damped RMSE:"));round(accuracy(f5), 4)
print(paste0("Holt RMSE:"));round(accuracy(f6), 4)

accuracy(f1)
accuracy(f2)
accuracy(f3)
accuracy(f4)
accuracy(f5)
accuracy(f6)

cci<-window(unemp.cci[,"cci"],start=1997)
fit_1<-hw(cci,seasonal="additive")
fit_2<-hw(cci,seasonal="multiplicative")
fit_3<-hw(cci,damped=TRUE, seasonal="multiplicative")
fit_1$model
fit_2$model
fit_3$model

ts_lags(f1$lower)
ts_lags(f1$upper)
ts_lags(f2$residuals)
ts_lags(f3)
ts_lags(f4)
ts_lags(f5)


cci.pacf1 <- pacf(for1$residuals, lag.max = 36)

# Compare differncing to holt
cci.diff1<-diff(f1, differences=2)
# ACF of the differenced beer data
cci.acf1 <- acf(cci, lag.max = 36)
# PACF of the differenced beer data
cci.pacf1 <- pacf(f1, lag.max = 36)
# correlograms of the differenced beer data
par(bg='white', lwd=2, mar=c(3, 4, 2, 2), cex=1.5, cex.axis=.85, cex.lab=1)
plot.acf(cci.acf1)
title(main="CCI ACF fit with Holt_Winters Additibe method", 
      cex.main=1, font.main=2, col.main='blue', col.lab ='darkblue')
plot.acf(cci.pacf1)
title(main="CCI PACF fit with Holt_Winters Additibe method", 
      cex.main=1, font.main=2, col.main='blue', col.lab ='darkblue')


par(mfrow=c(2,1))
acf(f1$residuals, main="Holt-Winters Additive Model Residuals ACF")
pacf(f1$residuals, main="Holt-Winters Additive Model Residuals PACF")
acf(f2$residuals, main="Holt-Winters Multiplicative Model Residuals ACF")
pacf(f2$residuals, main="Holt-Winters Multiplicative Model Residuals PACF")
acf(f3$residuals, main="Holt-Winters Multiplicative Damped CCI Model Residuals ACF")
pacf(f3$residuals, main="Holt-Winters Multiplicative Damped CCI Model Residuals PACF")
acf(f4$residuals, main="Holt-Winters Additive Damped Model Residuals ACF")
pacf(f4$residuals, main="Holt-Winters Additive Damped Model Residuals PACF")
acf(f5$residuals, main="Holt Additive Damped Model Residuals ACF")
pacf(f5$residuals, main="Holt Additive Damped Model Residuals PACF")
acf(f6$residuals, main="Holt Additive Model Residuals ACF")
pacf(f6$residuals, main="Holt Additive Model Residuals PACF")

library(ModelMetrics)
rmse(fit_1$fitted,fit_1$lower)
rmse(fit_2$fitted,fit_2$lower)
rmse(fit_3$fitted,fit_3$lower)

accuracy(fit_1)
accuracy(fit_2)
accuracy(fit_3)

################################USE THIS PLOT#############################
autoplot(cci) +
  autolayer(cci, color="blue", size=1.25) +
  autolayer(fit_1, series="Additive", PI=FALSE, size=1.25) +
  autolayer(fit_2, series="Multiplicative", PI=FALSE, size=1.25) +
  autolayer(fit_3, series="Multiplicative damped", PI=FALSE, size=1.25) +
  xlab("Year") + 
  ylab("Unemployment Index") +
  ggtitle("Consumer Confidence Index (CCI)") +
  guides(colour=guide_legend(title="HW Forecast"))


autoplot(cci) +
  autolayer(cci, color="blue", size=1.25) +
  autolayer(fit_1, series="Additive", PI=FALSE, size=1.25) +
  autolayer(fit_2, series="Multiplicative", PI=FALSE, size=1.25) +
#  autolayer(fit_3, series="Multiplicative damped", PI=FALSE, size=1.25) +
  theme(legend.position="bottom", legend.text = element_text(size = rel(1.2))) + 
  guides(colour=guide_legend(title="LEGEND:", size = rel(1.52))) +
  ylab("Index Value") + xlab("Years") +
  ggtitle("Consumer Confidence Index - Unemployment)") +
  theme(plot.title = element_text(size = rel(1.75))) +
  theme(axis.title.x = element_text(size = rel(1.5), color="blue")) +
  theme(axis.title.y = element_text(size = rel(1.65), color="blue3")) +
  theme(panel.background = element_rect(fill = "grey90", size=rel(1.2)), 
        axis.text = element_text(colour = "purple", size = rel(1.5))) 
###########################################################################
library(forecast)
data(unemp.cci) 
cci <- ts(unemp.cci[,"cci"])
cci <- window(unemp.cci[,"cci"], start = 1997)
cci.fit_3<-hw(cci,damped=TRUE, seasonal="multiplicative")
cci.for_3<-forecast(cci.fit_3, h=48)
cci.pacf1 <- pacf(f1$residuals, lag.max = 36)
Box.test(f6$residuals, lag=48, type="Ljung-Box")

autoplot(cci) +
  autolayer(cci, color = "blue", size = 1.25) +
  autolayer(f1, series = "HW Additive", PI = FALSE, 
            size = 1.25) +
  autolayer(f2, series = "HW Multiplicative", PI = FALSE, 
            size = 1.25) +
  autolayer(f3, series = "HW Multiplicative damped", PI = FALSE,
            size = 1.25) +
  autolayer(f4,series = "HW Additive damped", PI = FALSE, 
            size = 1.25) +
  autolayer(f5, series = "Holt damped", PI = FALSE, 
            size = 1.25) +
  autolayer(f6, series = "Holt", PI = FALSE, size = 1.25) +
  xlab("Year") + 
  ylab("Unemployment Index") +
  ggtitle("Consumer Confidence Index (CCI)") +
  guides(colour=guide_legend(title = "LEGEND")) +
  theme(legend.position = "bottom")