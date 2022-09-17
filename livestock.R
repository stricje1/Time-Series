################################################################
# LIVESTOCK
################################################################

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

autoplot(livestock) + 
  autolayer(livestock, color="blue", size = 1.2) +
  xlab("Year") + ylab("Livestock, sheep in Asia (millions)")

autoplot(fc) +    
  autolayer(livestock, series="Observed", size = 1.75) +      
  autolayer(fc, series="Forecast", size = 1.2) +      
  #autolayer(fc2, series="Damped trend", size = 1.2) +
  theme(legend.position="bottom", legend.text = element_text(size = rel(1.2))) + 
  guides(colour=guide_legend(title="LEGEND:", size = rel(1.52))) +
  ylab("Livestock, sheep in Asia (millions)") + xlab("Years") +
  ggtitle("Livestock, sheep in Asia (millions)") +
  theme(plot.title = element_text(size = rel(1.75))) +
  theme(axis.title.x = element_text(size = rel(1.5), color="blue")) +
  theme(axis.title.y = element_text(size = rel(1.65), color="blue3")) +
  theme(panel.background = element_rect(fill = "grey90", size=rel(1.2)), 
        axis.text = element_text(colour = "purple", size = rel(1.5))) 


e1 <- tsCV(livestock, ses, h=1)
e2 <- tsCV(livestock, holt, h=1)
e3 <- tsCV(livestock, hw, damped=TRUE, h=1)
e3
# Compare MSE:
mean(e1^2, na.rm=TRUE)
mean(e2^2, na.rm=TRUE)
mean(e3^2, na.rm=TRUE)
# Compare MAE:
mean(abs(e1), na.rm=TRUE)
mean(abs(e2), na.rm=TRUE)
mean(abs(e3), na.rm=TRUE)

fc <- hw(livestock, damped=TRUE)
# Estimated parameters:
fc[["model"]]

autoplot(fc) +
  autolayer(livestock, color="blue", size = 1.2) +
  xlab("Year") + ylab("Livestock, sheep in Asia (millions)")

ts_lags(e3)
acf(fc)
pacf(fc)
acf(e3)
pacf(e3)
acf(fc$residuals)
pacf(fc$residuals)

