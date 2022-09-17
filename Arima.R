library(TSA)
library(stlplus)
library(ggplot2)
library(stats)
library(datasets)
library(forecast)
library(aTSA)

par(mfrow = c(1, 1))
# plot series
plot(airpass, col='blue', lwd=2)

display.brewer.pal(n = 12, name = 'Paired')

library("RColorBrewer")
ggseasonplot(airpass, 
             col=brewer.pal(n = 12, name = "Paired"), 
             year.labels=TRUE) + 
  geom_line(lwd=1.1) + 
  theme_gray()


# Seasonal decomposition
fit <- stl(airpass, s.window="period")
plot(fit, col='purple', lwd=2) 

trend.test(airpass, plot = TRUE)

# additional plots
monthplot(AirPassengers, col="blue", lwd=2)
monthplot(AirPassengers, choice = "seasonal", col.base="red", col="blue", lwd=2, 
          lty.base=1, lwd.base=3, xlab="Month", 
          ylab="Number of Passengers", 
          main="Monthly AirPassengers") 


library("RColorBrewer")
display.brewer.pal(n = 12, name = 'Paired')
ggseasonplot(AirPassengers, col=brewer.pal(n = 12, name = "Paired"), year.labels=TRUE) + 
  geom_line(lwd=1.1) + theme_gray()

(seasonal <- ggplot(AirPassengers, aes(x = date, 
                                  y = milk_prod_per_cow_kg, group = year)) +
    geom_line(aes(color = year), lwd=1.1) +
    theme_light() + 
    scale_color_manual(values = year_pal))


for.fit<-forecast::forecast(fit, h=12)
accuracy(for.fit)


plot(for.fit, lwd=2, col="purple") 

# simple exponential - models level  
fit1 <- HoltWinters(airpass, beta=FALSE, gamma=FALSE)
# predict next 12 future values
for1 <- forecast::forecast(fit1, h=12)
# predictive accuracy
accuracy(for1)

# double exponential - models level and trend
fit2 <- HoltWinters(airpass, gamma=FALSE)
# predict next 12 future values
for2<-forecast::forecast(fit2, h=12)
# predictive accuracy
accuracy(for2)


# triple exponential - models level, trend, and seasonal 
# components
fit3 <- HoltWinters(airpass)
# predict next 12 future values
for3<-forecast::forecast(fit2, h=12)
# predictive accuracy
accuracy(for3)


par(new=TRUE)
par(cex=1.5)
airdiff1 <- diff(airpass, differences=1)
plot.ts(airdiff1, lwd=2, col="red", main = "First Difference for Air Passengers Series")

airdiff2 <- diff(airpass, differences=2)
plot.ts(airdiff2, lwd=2, col="blue", main = "Second Difference for Air Passenger Series")

p1<-plot.ts(airdiff1, lwd=2, col="red", xaxt='n', yaxt='n', ylab=NULL)
p2<-plot.ts(airdiff2, lwd=2, col="blue", ylab=" Differenced Series")

p1<-plot.ts(airdiff1, lwd=2, col= "red", xaxt='n', yaxt='n')
par(new=TRUE)
p2<-plot.ts(airdiff2, lwd=2, col= "blue", main = "First & Second Differences for Air Passengers Series")


#forecast using the ARIMA model
plot(for1, col="purple", lwd=2, xlab="Date", ylab="Number of Passenges")

stationary.test(ts(airpass))

fit1 <- HoltWinters(airpass, beta=FALSE, gamma=FALSE)
for1 <- forecast::forecast(fit1, h=12)
for1.acf <- Acf(for1, lag.max = 12)
plot.acf(for1.acf1)
title(main="Beer ACF using differencing = 2", cex.main=1.25, 
      font.main=2, col.main="blue", col.lab ="darkblue")

Acf(airpass, lag.max=12)
Pacf(airpass)
forecast::Acf(airpass, lag.max=12)

taperedpacf(airpass, nsim=50)


for2 %>% Acf(plot=TRUE) %>% autoplot

airpass %>% Pacf(plot=TRUE) %>% autoplot

library(forecast)
fit <- ets(airpass)
fc<- forecast(fit)
plot_forecast(fc)

airfit <- arima(airpass, order=(c(0,1,1)))
 

data(airpass)
library(forecast)
fit1 <- ets(airpass)
fc1<- forecast::forecast(fit1, h = 60)
plot_forecast(fc1)

air_fit1 <- Arima(window(airpass,end=1972+11/12),
                    order=c(0,1,1),seasonal=list(order=c(0,1,1),
                                                 period=12),lambda=0)
air_fc1<- forecast::forecast(air_fit1, h = 60)
plot_forecast(air_fc1,
              title = 'Air Passenger Forecast: ARIMA(0,1,1)(0,1,1)[12]', 
              Xtitle = 'Years',Ytitle = 'Number of Passengers (millions)', 
              color = 'dodgerblue3', width = 2)

air_fit2 <- Arima(window(airpass,end=1967+11/12),
                  order=c(0,1,1),seasonal=list(order=c(0,1,1),
                                               period=12),lambda=0)
air_fc2<- forecast::forecast(air_fit2, h = 60)
par(cex.lab = 1, cex.axis = 1)
plot_forecast(air_fc2, 
              title = 'Air Passenger Forecast: ARIMA(0,1,1)(0,1,1)[12]', 
              Xtitle = 'Years',Ytitle = 'Number of Passengers (millions)', 
              width = 2)

library(PerformanceAnalytics)
chart.ACFplus(airpass, maxlag = 12, elementcolor = "red", main = "air passengers")

library(TSstudio)
ts_cor(ts.obj = airpass, lag.max = 72, type = "acf")
ts_cor(ts.obj = airpass, lag.max = 72, type = "pacf")

ts_cor(ts.obj = airpass, type = 'both', seasonal = TRUE, ci = 0.95,
       lag.max = 72, seasonal_lags = NULL)
ts_cor(ts.obj = airpass, type = 'acf', seasonal = TRUE, ci = 0.95,
       lag.max = 72, seasonal_lags = NULL)
ts_cor(ts.obj = airpass, type = 'pacf', seasonal = TRUE, ci = 0.95,
       lag.max = 72, seasonal_lags = NULL)

Acf(for1$fitted, lag.max=72)
Pacf(for1$fitted, lag.max=72)

(theme01 <- theme_light()+ theme(legend.position="right") +
  theme(plot.title = element_text(size = rel(1.25))) +
  theme(axis.title.x = element_text(size = rel(1.05), color="dodgerblue3")) +
  theme(axis.title.y = element_text(size = rel(1.05), color="dodgerblue3")) +
  theme(panel.background = element_rect(fill = "gray99", size=rel(1.05)),
        axis.text = element_text(colour = "navy", size = rel(1.05))) +
  theme(axis.line = element_line(size = 2, color = "gray")))

ggtaperedacf(fit1$x, nsim=50)
ggtaperedacf(for1$upper, nsim=50) + 
  ggtitle("Air Passengers ACF") + theme01

ggtaperedacf(for1$lower, nsim=50) + 
  ggtitle("Air Passengers ACF") + theme01

ggtaperedacf(airpass, nsim=72) +  
  ggtitle("Air Passengers ACF") + theme01
  
ggtaperedpacf(airpass, nsim=72) +
  ggtitle("Air Passengers PACF") + theme01

for1$residuals %>% Acf(plot=TRUE) %>% autoplot

for1$residuals %>% Pacf(plot=TRUE) %>% autoplot

library(PerformanceAnalytics)
chart.ACFplus(for1$residuals, maxlag = 12, elementcolor = "blue", main = "air passengers")

library(TSstudio)
ts_cor(ts.obj = for1$x, lag.max = 72, type = "acf")
ts_cor(ts.obj = for1$x, lag.max = 72, type = "pacf")

Box.test(for1$fitted, lag=72, type="Ljung-Box")

######### better ACF plot #########
plot.acf <- function(ACFobj) {
  rr <- ACFobj$acf[-1]
  kk <- length(rr)
  nn <- ACFobj$n.used
  plot(seq(kk), rr, type = "h", lwd = 2, col="red", yaxs = "i", xaxs = "i",
       ylim = c(floor(min(rr)), 1), xlim = c(0, kk + 1), xlab = "Lag", 
       ylab = "Correlation", las = 1)
  abline(h = -1/nn + c(-2, 2)/sqrt(nn), lty = "dashed", col = "blue")
  abline(h = 0)
}

airpass.fit1 <- HoltWinters(airpass, beta=FALSE, gamma=FALSE)
airpass.for1 <- forecast::forecast(fit1, h=12)

airpass.diff<-diff(airpass, differences=1)
## acf of the CO2 data
airpass.acf <- acf(airpass.for1$x, lag.max = 72)
## correlogram of the CO2 data
plot.acf(airpass.acf)

######### better PACF plot #########
plot.pacf <- function(PACFobj) {
  rr <- PACFobj$acf
  kk <- length(rr)
  nn <- PACFobj$n.used
  plot(seq(kk), rr, type = "h", lwd = 2, col="red", yaxs = "i", xaxs = "i", 
       ylim = c(floor(min(rr)), 1), xlim = c(0, kk + 1), xlab = "Lag", 
       ylab = "PACF", las = 1)
  abline(h = -1/nn + c(-2, 2)/sqrt(nn), lty = "dashed", col = "blue")
  abline(h = 0)
}

airpass.diff<-diff(airpass, differences=6)
## acf of the CO2 data
airpass.pacf <- pacf(airpass.for1$x, lag.max = 72)
## correlogram of the CO2 data
plot.acf(airpass.pacf)

Box.test(for1$residuals, lag=20, type="Ljung-Box")
Box.test(for1$fitted, lag=20, type="Ljung-Box")

require(forecast)
data(AirPassengers)
###################################
par(mfrow=c(1,1))
air.model1 <- Arima(window(airpass,end=1967+11/12),
      order=c(0,1,1),seasonal=list(order=c(0,1,1),
      period=12),lambda=0)
air.model1

#plot(forecast::forecast(air.model1,h = 48), lwd = 3, col = "darkorchid2")
#abline(v = 1968, lty = 3, lwd = 2, col = 'navy')

plot(forecast::forecast(air.model1 ,h = 48), main = '', cex = 1, lwd = 3, col = 'darkorchid2')
title(ylab = 'Number of Passengers (millions)', cex.lab = 1.35, line = 2.5)
abline(v=1968 , lty = 3, col = 'navy', lwd = 2)
text(1965,800,'Forecast: \nJan 1968 - Dec 1971', cex = 1.35, col = 'red2')
text(1968,200,'1968', cex = 1.35, col = 'red2')
title(main = 'Air Passenger Forecast: ARIMA(0,1,1)(0,1,1)[12]', cex = 1.35, line = 1)
Map(axis, side=1, col.axis='navy', cex.lab= 1)
Map(axis, side=2, col.axis='navy', cex.lab= 1)
###################################
par(mfrow=c(1,1), mar=c(4,5,4,1)+0.1)
air.model2 <- Arima(window(airpass,start=1960), 
      model=air.model1)
air.model2

par(mfrow=c(1,1), mar=c(4,5,4,1))
plot(forecast::forecast(air.model2 ,h = 48), main = '', cex = 1, lwd = 3, col = 'darkorchid2')
title(ylab = 'Number of Passengers (millions)', cex.lab = 1.35, line = 2.5)
abline(v=1972,lty=3)
text(1969,800,'Forecast: \nJan 1972 - Dec 1975', cex = 1.35, col = 'red2')
text(1972,200,'1972', cex = 1.35, col = 'red2')
title(main = 'Air Passenger Forecast: ARIMA(0,1,1)(0,1,1)[12]', cex = 1.35, line = 1)
Map(axis, side=1, col.axis='navy', cex.lab= 1)
Map(axis, side=2, col.axis='navy', cex.lab= 1)
#axis(1,at=1:3,labels=FALSE)
###################################
air.model3 <- Arima(airpass,order=c(0,1,1))
air.forecast3<-forecast::forecast(air.model3, h = 48)
plot(forecast::forecast(air.model3 ,h = 48), main = '', cex = 1, lwd = 3, col = 'darkorchid2')
title(ylab = 'Number of Passengers (millions)', cex.lab = 1.35, line = 2.5)
abline(v=1972 , lty = 3, col = 'navy', lwd = 2)
lines(airpass, lwd = 2)
text(1969,800,'Forecast: \nJan 1972 - Dec 1975', cex = 1.35, col = 'red2')
text(1972,100,'1972', cex = 1.35, col = 'red2')
title(main = 'Air Passenger Forecast: ARIMA(0,1,1)', cex = 1.35, line = 1)
Map(axis, side=1, col.axis='navy', cex.lab= 1)
Map(axis, side=2, col.axis='navy', cex.lab= 1)

###################################
air.model4 <- auto.arima(airpass)
air.forecast4<-forecast::forecast(air.model4,h=48)
plot(forecast::forecast(air.model4 ,h = 48), main = '', cex = 1, lwd = 3, col = 'darkorchid2')
title(ylab = 'Number of Passengers (millions)', cex.lab = 1.35, line = 2.5)
abline(v=1972,lty=3)
text(1969,800,'Forecast: \nJan 1972 - Dec 1975', cex = 1.35, col = 'red2')
text(1972,200,'1972', cex = 1.35, col = 'red2')
title(main = 'Air Passenger Forecast: ARIMA(2,1,1)(0,1,0)[12]', cex = 1.35, line = 1)
Map(axis, side=1, col.axis='navy', cex.lab= 1)
Map(axis, side=2, col.axis='navy', cex.lab= 1)
accuracy(air.model4)

########################################
par(mfrow=c(2,2), mar=c(2.5,5,3,1))
for (t in 1964:1967){
  air.model1 = Arima(window(airpass, end = t+11/12),
      order = c(0,1,1), seasonal = list(order = c(0,1,1),
      period=12),lambda=0)
  plot(forecast::forecast(air.model1, h = 84-12*(t-1964)), 
       cex = 1.5, main = '',lwd = 2, col = 'dodgerblue',
       xlim = c(1960,1972),
       ylim = c(100,1400))
  lines(airpass, lwd = 2, col = 'magenta')
  abline(v = t+1,lty = 3, lwd = 2, col = 'navy')
  title(main = 'Forecast: ARIMA(0,1,1)(0,1,1)[12]', 
        cex = 1.35, line = 1, col = 'dodgerblue')
  Map(axis, side=1, col.axis='navy', cex.lab= 1.2)
  Map(axis, side=2, col.axis='navy', cex.lab= 1.2)
}
###################################

specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k)) 

acc1 <- specify_decimal(accuracy(air.forecast1),4)
acc2 <- specify_decimal(accuracy(air.forecast2),4)
acc3 <- specify_decimal(accuracy(air.forecast3),4)
acc4 <- specify_decimal(accuracy(air.forecast4),4)

row.names <- c('ME','RMSE','MAE','MPE','MAPE','MASE','ACF1')
column.names <- c("Model1","Model2","Model3", 'Model4')
result <- array(c(acc1,acc2,acc3,acc4),dim = c(7,4),
                dimnames = list(row.names,column.names))
print(result,quote=FALSE)

###################################
plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd   <- sd(forecasterrors)
  mymin  <- min(forecasterrors) - mysd*3
  mymax  <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

plotForecastErrors(air.forecast1$residuals)

airpass.diff<-diff(airpass, differences=6)
## acf of the CO2 data
airpass.acf <- acf(air.forecast1$fitted, lag.max = 72)
## correlogram of the CO2 data
plot.acf(airpass.acf)
airpass.acf <- acf(air.forecast1$residuals, lag.max = 72)
## correlogram of the CO2 data
plot.acf(airpass.acf)
title(main = "Air Passenger ACF", 
      cex.main = 1, font.main = 2, col.main = 'blue', 
      col.lab = 'blue')
airpass.pacf <- pacf(air.forecast1$fitted, lag.max = 72)
## correlogram of the CO2 data
plot.pacf(airpass.pacf)
title(main = "Air Passenger ACF", 
      cex.main = 1, font.main = 2, col.main = 'blue', 
      col.lab = 'blue')

par(mar = c(5,5,2,1), cex.lab = 1.5, cex.axis = 1.5, font = 2, lwd = 2, col = 'navy')
library(WeightedPortTest)
Weighted.Box.test(air.forecast1, lag = 10, type = "Ljung")
estimate(airpass, p = 0, d = 1, q = 1, PDQ = c(0,1,1))

library(AnalyzeTS)
Descriptives(air.model1$residuals,plot=TRUE)
Box.test(air.forecast1$residuals, lag=48, type="Ljung-Box")
Box.test(air.forecast1$residuals, lag=48, type="Box-Pierce")

# out-of-sample multi-step forecasts
air.model4 <- Arima(window(airpass,end=1960),order=c(0,1,1),seasonal=list(order=c(0,1,1),period=12),lambda=0)
air.forecast1-forecast(air.model4,h=48,lambda=NULL)
accuracy(air.forecast1,log(window(airpass,start=1960)))

air.model5 <- Arima(window(airpass, end=1965), 
                    order=c(0,1,1),
                    seasonal=list(order=c(0,1,1), 
                                  period=12), 
                    lambda=0)
air.forecast5 <- forecast::forecast(air.model5, h=48, lambda=NULL)
accuracy(air.forecast5,log(window(airpass,start=1960)))


# out-of-sample multi-step forecasts
air.model5 <- Arima(window(airpass, end=1957), 
                    order=c(0,1,1),
                    seasonal=list(order=c(0,1,1), 
                    period=12), 
                    lambda=0)
air.forecast5 <- forecast::forecast(airpass, h=48, lambda=NULL)
specify_decimal(accuracy(air.forecast5,log(window(airpass, start=1949))),4)
library(ZRA)
plot(ZRA(air.forecast1$fitted), zero = TRUE)

View(result) 

par(mfrow=c(1,1))
for (t in 1953:1956){
  air.model1 = Arima(window(AirPassengers,end=t+11/12),
  order=c(0,1,1),seasonal=list(order=c(0,1,1),period=12),lambda=0)
  plot(forecast::forecast(air.model1,h=84-12*(t-1953)),xlim=c(1949,1961),
  ylim=c(100,1400))
  lines(AirPassengers)
  abline(v=t+1,lty=3)
}


for (t in 1956:1961){
  air.model2 = Arima(window(AirPassengers,start=1956), model=air.model1,
  order=c(0,1,1), seasonal=list(order=c(0,1,1), period=12), lambda=0)
  plot(forecast::forecast(air.model,h=84-12*(t-1956)), xlim=c(1956,1961),
  ylim=c(100,1400))
  lines(AirPassengers)
  abline(v=t+1,lty=3)
}

airpass.acf <- acf(air.forecast1$x, lag.max = 72)
plot.acf(airpass.acf)


airpass.pacf <- pacf(air.forecast1$residuals, lag.max = 72)
plot.pacf(airpass.pacf)

######################## plot.acf definition #############################
plot.acf <- function(ACFobj) {
  rr <- ACFobj$acf[-1]
  kk <- length(rr)
  nn <- ACFobj$n.used
  plot(seq(kk), rr, type = "h", lwd = 3, col="red2", yaxs = "i", xaxs = "i",
       ylim = c(floor(min(rr)), 1), xlim = c(0, kk + 1), xlab = "Lag", 
       ylab = "Correlation", las = 1, cex.lab = 1.5, cex.axis = 1.25)
  abline(h = -1/nn + c(-2, 2)/sqrt(nn), lty = 3, col = "blue", lwd = 3)
  abline(h = 0)
}
######################## plot.pacf definition #############################
plot.pacf <- function(PACFobj) {
  rr <- PACFobj$acf
  kk <- length(rr)
  nn <- PACFobj$n.used
  plot(seq(kk), rr, type = "h", lwd = 3, col="red3", yaxs = "i", xaxs = "i", 
       ylim = c(floor(min(rr)), 1), xlim = c(0, kk + 1), xlab = "Lag", 
       ylab = "PACF", las = 1, cex.lab = 1.5, cex.axis = 1.25)
  abline(h = -1/nn + c(-2, 2)/sqrt(nn), lty = 3, col = "blue", lwd = 3)
  abline(h = 0)
}
##########################################################################
air_fc <- forecast::forecast(air.model1 ,h = 48)
airpass.acf <- acf(air_fc$fitted, lag.max = 72)
airpass.pacf <- pacf(air_fc$residuals, lag.max = 72)

par(mfrow=c(2,2), mar=c(5,5,4,.5), col = 'dodgerblue3', 
    cex.lab = 1.45, cex.axis = 1.45, cex.main = 1.5, font = 2, 
    col.main = 'dodgerblue3', col.lab = 'dodgerblue3')

plot(air.forecast1$residuals) + 
title(main = "Air Passenger Residuals")

plotForecastErrors(air.forecast1$residuals) +
title(main = "")

plot.acf(airpass.acf) +
title(main = "Air Passenger ACF")

plot.pacf(airpass.pacf) +
title(main = "Air Passenger PACF")



##########################################################################
# TSstudio::train_model
# Defining the models and their arguments

methods <- list(ets1 = list(method = "ets",
                       method_arg = list(opt.crit = "lik"), 
                       notes = "ETS model with opt.crit = lik"),
                ets2 = list(method = "ets",
                       method_arg = list(opt.crit = "amse"), 
                       notes = "ETS model with opt.crit = amse"),
                arima1 = list(method = "arima",
                       method_arg = list(order = c(2,1,0)), 
                       notes = "ARIMA(2,1,0)"),
                      
                arima2 = list(method = "arima",
                       method_arg = list(order = c(2,1,2),
                            seasonal = list(order = c(1,1,1))), 
                       notes = "SARIMA(2,1,2)(1,1,1)"),
                hw = list(method = "HoltWinters",
                       method_arg = NULL,
                       notes = "HoltWinters Model"), 
                tslm = list(method = "tslm",
                       method_arg = list(formula = input ~ trend + season), 
                       notes = "tslm model with trend and seasonal components"))

# Training the models with backtesting 

md <- train_model(input = airpass,
methods = methods,
train_method = list(partitions = 4,
                    sample.out = 12,
                    space = 3),
                    horizon = 12, error = "MAPE")
fc <- forecast(md)

# View the model performance on the backtesting partitions

md$leaderboard
library(zoo)
library(TSstudio)
theme_01 <- (theme(plot.title = element_text(size = rel(1.75))) +
               theme(axis.title.x = element_text(size = rel(1.5), color="blue")) +
               theme(axis.title.y = element_text(size = rel(1.5), color="blue3")) +
               theme(panel.background = element_rect(fill = "grey90", size=rel(1.25)),
                     axis.text = element_text(colour = "purple", size = rel(1.2))) +
               theme(axis.line = element_line(size = 2, colour = "grey80")))

# Forecasting applications
# Setting training and testing partitions
milk_s <- ts_split(ts.obj = milk_mon_ts, sample.out = 12)
train <- milk_s$train
test <- milk_s$test

# Forecasting with auto.arima
library(forecast)
md <- auto.arima(train)
fc <- forecast::forecast(md, h = 12)

# Plotting actual vs. fitted and forecasted
test_forecast(actual = milk_mon_ts, forecast.obj = fc, test = test)
accuracy(fc)
