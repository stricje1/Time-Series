library(readr)
library(TSA)
library(stlplus)
library(ggplot2)
library(stats)
library(datasets)
library(forecast)
library(aTSA)
library(dplyr)
library(ggsci)

par(mfrow=c(1,1))
dta <- as.data.frame(read_csv("D:/Documents/Data/glb_ts.csv"))
head(dta)
class(dta)
myvector=dta[,2]
class(myvector)
head(myvector)
dta.ts <- ts(myvector, start=c(1880, 1), end=c(2019, 12), frequency=12)
class(dta.ts)
head(dta.ts)

plot.ts(dta.ts, col='dodgerblue3', lwd=1.85, xlab='', ylab='') 
labels=axis(1, seq(1880,2030,10)) 
title(main="Percent Change in Temperature", col.main="dodgerblue2", cex.main=1.5, 
      xlab="Year", ylab="Percent Change", 
      col.lab="dodgerblue3", font.lab=2, cex.lab=1.5)
      grid(lty = 3, col = 'cadetblue3', lwd = 2)

theme(text=element_text(face = 'bold'),
      panel.background = element_rect(fill = 'royalblue2'),
      panel.grid = element_line(color = 'lightblue1', 
                                linetype = 3))

tempdecomposition<- decompose(window(dta.ts, start=1960))
plot(tempdecomposition, col='deeppink4', lwd=2, cex.axis = 1.5)

fit <- stl(window(dta.ts, start=1960), s.window="period")
summary(fit)

plot(fit, col='deeppink3', lwd=1.5)
title(main="Percent Change in Temperature Decomposition", 
      col.main="deeppink3", cex.main=1.25, 
      col.lab="deeppink3", font.lab=2, cex.lab=1.25)

library("RColorBrewer")
par(mar=c(4,5,3,1))
ggseasonplot(window(dta.ts, start=2000),  continuous=TRUE,
             year.labels=TRUE, year.labels.left=TRUE) + 
  geom_line(lwd=1.1) + ylab('Year') +
  theme(axis.text = element_text(size=14, face = 'bold', color = 'navy'),
      axis.title = element_text(size=18, face='bold', color = 'navy'),
      plot.title = element_text(color = 'navy', size = 18, face = 'bold', hjust = 0.5),
      legend.title = element_text(color= 'navy', size=16,  face='bold'),
      legend.text = element_text(color = 'navy', size = 16),
      legend.position = 'bottom')


library(colorRamps)
colmat<- blue2green(400)
par(cex= 1.25)
monthplot(window(dta.ts, start=1950),
          main = "Monthplot of Percent Change in Temperature",
          xlab="Months", ylab="Percent Change", 
          lwd=2, col = 'deepskyblue3', cex.axis = 1, cex.lab = 1.25) +
  theme(title.etxt = element_text(color = 'navy', size = 1, face = 'bold', hjust = 0.5))

temp.diff1 <- diff(dta.ts, differences=1)
plot.ts(temp.diff1)
library(rucm)

library(astsa)
par(mar=c(4,6,4,1),cex.axis = 1.25, cex.lab = 1.25, cex = 2, col = 'dodgerblue3', lwd = 2)
sarima(dta.ts,0,1,1,P=0, D=1, Q=1, S=12)


# Reject nonstationarity if p <= 0.05
stationary.test(ts(dta.ts),method="kpss")
library(tseries)
tseries::kpss.test(diff(dta.ts, 1), null = "Trend")
adf.test(dta.ts)

temp.fit<-timeSeries::diff(dta.ts, differences=6)
plot(temp.fit, lwd = 1.5, col="red")


par(mfrow=c(2,1))
library(mgcv)
library(oddsratio)
temp.acf2 <- (acf(temp.fit$fitted, lag.max = 72))
temp.acf3 <- (pacf(temp.fit$residuals, lag.max = 72))

plot.acf(temp.acf2)
title(main="Percent Change in Temperature ACF", col.main='deeppink3')
plot.acf(temp.acf3)
title("Percent Change in Temperature PACF")


par(mfrow=c(2,2), mar=c(3,3,2,1))

temp.model1 <- Arima(window(dta.ts, start=1950),order=c(1,0,0))
temp.forecast1<-forecast::forecast(temp.model1,h=48)

temp.model2 <- Arima(window(dta.ts, start=1950),order=c(1,1,0))
temp.forecast2<-forecast::forecast(temp.model2,h=48)

temp.model3 <- Arima(window(dta.ts, start=1950),order=c(0,2,2))
temp.forecast3<-forecast::forecast(temp.model3,h=48)

temp.model4 <- Arima(window(dta.ts, start=1950),order=c(1,1,2))
temp.forecast4<-forecast::forecast(temp.model4,h=48)

par(mfrow=c(2,2), mar=c(3,3,2,1))

plot(temp.forecast1, col = 'dodgerblue', main = "") 
lines(dta.ts, col="red", lwd=1.5)
title("Forecast: ARIMA(1,0,0); non-zero mean")

plot(temp.forecast2, col = 'green', main = "") 
lines(dta.ts, col="red", lwd=1.5)
title("Forecast: ARIMA(1,1,0)")

plot(temp.forecast3, col = 'lightblue3', main = "") 
lines(dta.ts, col="red", lwd=1.5)
title("Forecast: ARIMA(0,2,2)")

plot(temp.forecast4, col = 'orange', main = "") 
lines(dta.ts, col="red", lwd=1.5)
title("Forecast: ARIMA(1,1,2)")

summary(temp.model4)

par(mfrow=c(2,2))
temp.acf1 <- pacf(temp.model1$residuals, lag.max = 36)
temp.acf2 <- pacf(temp.model2$residuals, lag.max = 36)
temp.acf3 <- pacf(temp.model3$residuals, lag.max = 36)
temp.acf4 <- pacf(temp.model4$residuals, lag.max = 36)
plot.acf(temp.acf1)
title("Percent Change in Temperature ACF")
plot.acf(temp.acf2)
title("Percent Change in Temperature ACF")
plot.acf(temp.acf3)
title("Percent Change in Temperature ACF")
plot.acf(temp.acf4)
title("Percent Change in Temperature ACF")

par(mfrow=c(1,1))
ggtaperedpacf(temp.forecast2$fitted, nsim=50) + 
  ggtitle("Change in Temperature Model3 Forecast PACF") + 
  theme_light() + theme(legend.position="bottom")
ggtaperedpacf(temp.forecast3$fitted, nsim=50)  +
  ggtitle("Change in Temperature Model4 Forecast PACF") + 
  theme_light() + theme(legend.position="bottom")

par(mfrow=c(2,1))
temp.forecast2$residuals %>% Acf(plot=TRUE) 
title("Percent Change in Temperature ACF")
temp.forecast2$residuals %>% Pacf(plot=TRUE) 
title("Percent Change in Temperature PACF")

library(astsa)
acf2(temp.forecast2$fitted)
acf2(temp.forecast2$residuals)

par(mfrow=c(1,1))
specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k)) 
acc1 <- specify_decimal(accuracy(temp.forecast1),4)
acc2 <- specify_decimal(accuracy(temp.forecast2),4)
acc3 <- specify_decimal(accuracy(temp.forecast3),4)
acc4 <- specify_decimal(accuracy(temp.forecast4),4)
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
par(mfrow=c(2,2), mar=c(5,5,3,1))
plotForecastErrors(temp.forecast1$residuals)
plotForecastErrors(temp.forecast2$residuals)
plotForecastErrors(temp.forecast3$residuals)
plotForecastErrors(temp.forecast4$residuals)
########################################
library(WeightedPortTest)
Weighted.Box.test(temp.forecast3, lag = 10, type = "Ljung")
Weighted.Box.test(temp.forecast4, lag = 10, type = "Ljung")

estimate(dta.ts, p = 0, d = 1, q = 1, PDQ = c(0,1,1))
estimate(dta.ts, p = 1, d = 1, q = 2, PDQ = c(1,1,2))

library(AnalyzeTS)
Descriptives(temp.model3$residuals,plot=TRUE)
Box.test(temp.forecast3$residuals, lag=48, type="Ljung-Box")
Box.test(temp.forecast3$residuals, lag=48, type="Box-Pierce")
Descriptives(temp.model4$residuals,plot=TRUE)
Box.test(temp.forecast4$residuals, lag=48, type="Ljung-Box")
Box.test(temp.forecast4$residuals, lag=48, type="Box-Pierce")
################################################

# out-of-sample multi-step forecasts
temp.model5 <- Arima(window(dta.ts, start=1950, end=1980), 
                    order=c(1,1,2))
temp.forecast5 <- forecast::forecast(temp.model5, h=96)
specify_decimal(accuracy(temp.forecast5,log(window(dta.ts,start=1980))),4)

library(ZRA)
plot(ZRA(temp.forecast5$fitted), zero = TRUE)

View(result) 
################################################
par(mfrow=c(2,2), mar=c(3,3,3,1))
for (t in 2011:2014){
  temp.model4 = Arima(window(dta.ts,end=t+11/12),
                     order=c(1,1,2))
  plot(forecast::forecast(temp.model4,h=84-12*(t-2011)),xlim=c(1980,2020),
       ylim=c(-1,2))
  lines(window(dta.ts, start=1980,end=t+1), col="blue", lwd=2)
  labels=axis(1, seq(1980,2020,5))
  abline(v=t+1,lty=3, col="red")
}

Box.test(temp.forecast4, lag=24, type="Ljung-Box")
Box.test(temp.forecast3, lag=24, type="Ljung-Box")


temp.model1 <- Arima(window(dta.ts, start=1950),order=c(1,0,0))
temp.model2 <- Arima(window(dta.ts, start=1950),order=c(1,1,0))
temp.model3 <- Arima(window(dta.ts, start=1950),order=c(0,1,1))
temp.model4 <- Arima(window(dta.ts, start=1950),order=c(1,1,2))

par(mfrow=c(1,1), mar=c(5,6,3,1))
plot(window(dta.ts, start=2000,end=t+1), 
     ylab = 'Percent Change (%)',
     xlab = 'Years', cex.lab = 1.25) 
  lines(temp.model1$fitted, col="green3", lwd=2) 
  lines(temp.model2$fitted, col="blue2", lwd=2)
  lines(temp.model3$fitted, col="red3", lwd=2) 
  lines(temp.model4$fitted, col="orange", lwd=2)   
  title("Fitted ARIMA Models for Globle Temperature Change") 
  grid(col = 'lightblue')
  legend(2011, 0.45, 
         c("ARIMA(1,0,0)", "ARIMA(1,1,0)", "ARIMA(0,1,1)","ARIMA(1.1.2)"),
         col=c("green", "blue2", "red3", "orange"), 
         lwd = 3, lty=1, cex=1.2, bty="n", y.intersp=1.5)

X<-dta.ts
adf.test(dta.ts)
pp.test(dta.ts)

library(urca)
df=ur.df(X,type="none",lags=0)
summary(df)

df=ur.df(X,type="drift",lags=1)
summary(df)

temps=(lags+1):n
summary(lm(z.diff~1+temps+z.lag.1+z.diff.lag ))

temps=(lags+1):n
summary(lm(z.diff~1+temps+z.lag.1+z.diff.lag ))

summary(ur.kpss(X,type="mu"))
summary(ur.kpss(X,type="tau"))

kpss.test(X,"Level")
kpss.test(X,"Trend")

pp.test(X)
PP.test(X)

google <- as.data.frame(read_csv("D:/Documents/Data/google.csv"))
library(fma)
data(pigs)
goog<-ts(google, start=c(2015,10), frequency=1)
data(pigs)

par(mfrow=c(4,2))
par(mar=c(2,2.5,2,.55), bg = '#fff2c9', font = 2, cex = 1.05, cex.axis = .85, cex.lab = .85)

ppigs<-plot(pigs, lwd=3, col="blue1", main="Annual Pork Production") 
plynx<-plot(lynx, lwd=3, col="blue2", main="Annual Lynx Trappings") 
pbeer<-plot(beer, lwd=3, col="blue3", main="Monthly  Beer Production")
peggs<-plot(eggs, lwd=3, col="magenta1", main="Monthly Electricity Production")
phsales<-plot(hsales, lwd=3, col="magenta2", main="Monthly One-Family Home Sales")
pstrikes<-plot(strikes, lwd=3, col="magenta3", main="Annual Number US Strikes")
pgoog200<-plot(goog[,4], lwd=3, col="magenta4", ylab="Stock Price", main="Daily Google Stock $")
pgoog<-plot(goog[,8], lwd=3, col="purple", ylab="% change",main="Daily Change Google Stock $")



par(mfrow=c(4,2))
pgoog200
pgoog
pstrikes
phsales
peggs
ppigs
plynx
pbeer
pelec

par(mfrow=c(1,1))
E=rnorm(240)
X=cumsum(E)
plot(X,type="l")

lags=1
z=diff(X)
n=length(z)
z.diff=embed(z, lags+1)[,1]
z.lag.1=X[(lags+1):n]
k=lags+1
z.diff.lag = embed(z, lags+1)[, 2:k]
summary(lm(z.diff~0+z.lag.1+z.diff.lag ))

df=ur.df(X,type="none",lags=1)
summary(df)

PP.test(X)
kpss.test(X,"Level")
kpss.test(X,"Trend")
ur.kpss(X,type="tau")
ur.kpss(X,type="mu")
ur.df(X,type="trend",lags=1)
adf.test(X,k=1)
ur.df(X,type="none",lags=1)

n=100
P=matrix(NA,3,31)
M1=matrix(NA,1000,length(X))
M2=matrix(NA,1000,length(X))
M3=matrix(NA,1000,length(X))


length(dta.ts)
X<-dta.ts
length(X)



for(i in 1:(length(X)+1)){
  for(s in 1:1000){
    if(i==1) X=dta.ts
    if(i!=1) X=dta.ts
    library(urca)
    M2[s,i]=as.numeric(pp.test(X)$p.value)
    M1[s,i]=as.numeric(kpss.test(X)$p.value)
    M3[s,i]=as.numeric(adf.test(X)$p.value)
    }}

prop05=function(x){ mean(x>.05)
+ P[1,]=1-apply(M1,2,prop05)
+ P[2,]=apply(M2,2,prop05)
+ P[3,]=apply(M3,2,prop05)
 }
plot(X,P[1,],type="l",col="red",ylim=c(0,1),ylab="proportion of non-stationnary series",xlab="autocorrelation coefficient")
 lines(X,P[2,],type="l",col="blue")
 lines(X,P[3,],type="l",col="green")
 legend(.7,1,c("ADF","KPSS","PP"),col=c("green","red","blue"),lty=1,lwd=1)

 summary(lm(z.diff~1+z.lag.1+z.diff.lag ))
 adf.test(X,k=1)
 
 Box.test(temp.model3$fitted, lag=24, type= "Ljung-Box")
 Box.test(temp.model4$fitted, lag=24, type= "Ljung-Box")
 