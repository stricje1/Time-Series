################################################################
# Australian Air
################################################################
library(fpp2)
library(timeSeries)
library(ggplot2)
library(forecast)
library(dplyr)
library(colortools)
library(TTR)
library(fpp2)
library(TSA)
library(aTSA)
library(stlplus)
library(graphics)
library(TSstudio)
library(spatial)
library(correlation)

air <- window(ausair, start=1990)
fc <- holt(air, h=10)
fc
fc$model
acf(fc$residuals)
pacf(fc$residuals)

air %>% holt(h = 5) %>% autoplot

fc1 <- holt(air, h = 15, PI = FALSE) 
fc2 <- holt(air, damped = TRUE, h = 15, PI = FALSE) 
autoplot(air) + xlab("Year") + ylab("millions") +     
  autolayer(fc1, series="Linear trend") +      
  autolayer(fc2, series="Damped trend")

autoplot(ausair)
air
autoplot(air) +
  autolayer(air, series="Australian Air Travel", color="purple", size = 1.2) +
  theme(plot.title = element_text("Australian Air Travel, Forcast 2017-2016", 
                                  size = rel(1.75))) +
  theme(axis.title.x = element_text("Years", size = rel(1.5), color="blue")) +
  theme(axis.title.y = element_text("Air Passengers in Australia (millions)", 
                                    size = rel(1.65), color="blue3")) 
  theme(panel.background = element_rect(fill = "grey90", size=rel(1.2)), 
        axis.text = element_text(colour = "purple", size = rel(1.5))) 

  
  ylab("Passengers (in millions)") + xlab("Year") +
  geom_line(color = "dark red", linetype = 1, size = 1.5)
  autolayer(air, series="Australian Air Travel", color="purple", size = 1.2) +
  autolayer(fc1, series="Holt's method", PI=FALSE, color="blue", size = 1.2) +
  autolayer(fc2, series="Damped Holt's method", PI=FALSE, color="red", size = 1.2) +
  guides(colour=guide_legend(title="Forecast")) +
  theme(legend.position="bottom") 

fc$lower

fc2 <- hw(air, damped = TRUE, seasonal="multiplicative")

fc2 <- hw(subset(air,end=length(air)-12),
          damped = TRUE, seasonal="multiplicative", h=12)
fc1 <- holt(air, h = 15, PI = FALSE) 
fc2 <- holt(air, damped = TRUE, h = 15, PI = FALSE) 

autoplot(air) +    
  autolayer(air, series="Observed", size = 1.2) +      
  autolayer(fc1, series="Linear trend", size = 1.2) +      
  autolayer(fc2, series="Damped trend", size = 1.2) +
  theme(legend.position="bottom", legend.text = element_text(size = rel(1.2))) + 
  guides(colour=guide_legend(title="LEGEND:", size = rel(1.52))) +
  ylab("Passengers (millions)") + xlab("Years") +
  ggtitle("Total Annual Australian Air Travel") +
  theme(plot.title = element_text(size = rel(1.75))) +
  theme(axis.title.x = element_text(size = rel(1.5), color="blue")) +
  theme(axis.title.y = element_text(size = rel(1.65), color="blue3")) +
  theme(panel.background = element_rect(fill = "grey90", size=rel(1.2)), 
        axis.text = element_text(colour = "purple", size = rel(1.5))) 
  


air <- window(ausair, start=1990)
fc1 <- holt(air, h=15)
fc2 <- holt(air, damped=TRUE, phi = 0.9, h=15)
autoplot(fc1) +  
  autolayer(air, series="Total Annual Australian Air Travel", color="purple", size = 1.2) +
  autolayer(fc1, series="Holt's Linear Trend", color="blue", size = 1.2) +
  autolayer(fc2, series="Damped Holt'sTrend", color="red", size = 1.2) +
  ylab("Passengers (millions)") + xlab("Years") +
  ggtitle("Total Annual Australian Air Travel") + 
  theme(legend.position="bottom") +
  theme(plot.title = element_text(size = rel(1.75))) +
  theme(axis.title.x = element_text(size = rel(1.5), color="blue")) +
  theme(axis.title.y = element_text(size = rel(1.65), color="blue3")) +
  theme(panel.background = element_rect(fill = "grey90", size=rel(1.2)), 
        axis.text = element_text(colour = "purple", size = rel(1.5))) +
  guides(colour=guide_legend(title="Forecast")) +
  theme(legend.position="bottom") 

fit <- ets(air)
fc3 <- forecast(fit)
plot(fc3)

autoplot(air)+
  autolayer(air, series="Total Annual Air Travel", color="darkorchid", size = 1.2) +
  autolayer(fc3, series="Forecasted Annual Air Travel", color="dodgerblue", size = 1.2) +
  ggtitle("Total Annual Australian Air Travel") + 
  ylab("Passengers (millions)") + xlab("Years") +
  theme(plot.title = element_text(size = rel(1.75))) +
  theme(axis.title.x = element_text(size = rel(1.5), color="blue")) +
  theme(axis.title.y = element_text(size = rel(1.65), color="blue3")) +
  theme(panel.background = element_rect(fill = "grey90", size=rel(1.2)), 
        axis.text = element_text(colour = "purple", size = rel(1.5)))+

 
  theme(legend.position='left', legend.margin = margin(6, 6, 6, 6),
  legend.title = element_text(colour="blue", size=10, face="bold"),
  legend.text = element_text(colour="blue", size=10, face="bold"))


air %>% ets() %>% forecast() %>% autoplot() +
  autolayer(air, series="Forecast, 2017-2026", color="purple", size = 1.2) +
  ggtitle("Total Annual Australian Air Travel")



acf(ausair)
pacf(ausair)
acf(fc1)
acf(fc1$residuals)
pacf(fc1$residuals)
acf(fc2)
acf(fc2$residuals)
pacf(fc2$residuals)
airdiff1 <- diff(airpass, differences = 1)
airdiff2 <- diff(airpass, differences = 2)
plot.ts(airdiff1 , lwd = 3, col = 'red', main="Air Passenger Series First Order Differencing")
plot.ts(airdiff2 , lwd = 3, col = 'blue', main="Air Passenger Series Second Order Differencing")

p1<-plot.ts(airdiff1, lwd = 2, col = 'red', xaxt = 'n',yaxt = 'n',  
            main="Air Passenger Series First & Second Order Differencing")
par(new=TRUE)
p2<-plot.ts(airdiff2, lwd = 2, col = 'blue')

#forecast using the ARIMA model
plot(for1, col = 'purple', lwd = 3)

library(TSstudio)
ts_cor(ts.obj = for1$x, lag.max = 72, type = 'acf')
ts_cor(ts.obj = for1$x, lag.max = 72, type = 'pacf') 

ggtaperedpacf(airpass, nsim=50)  +
  ggtitle("Air Passengers PACF") + theme_light()

