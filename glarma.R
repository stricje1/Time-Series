library(glarma)
data(Polio)

opar <- par()
par(opar)
par(mfrow = c(1,1))
plot(Polio)

Polio[0,]

myvector = Polio[, 2]
polio.ts <- ts(myvector, start=c(1970, 1), end=c(1983, 12), frequency=12) 
plot(polio.ts, col=6)

y <- Polio[, 2]
X <- as.matrix(Polio[, 3:8])

glarmamod <- glarma(y, X, thetaLags = c(1,2,5), type = "Poi", method = "FS", residuals = "Pearson", maxit = 100, grad = 1e-6)
glarmamod
plot.ts(glarmamod$y, lwd = 1.5, col = 'red3', ylab = "")
lines(glarmamod$fitted.values, lwd = 2, col = 'deepskyblue2')
title(main = "Reported Cases of Poliomyelitis (1970 to 1983)",
      lwd = 2, col = 'deepskyblue2', ylab = "Number of Cases")
panel.first = grid(10, lty = 3, lwd = 1, col = 'lightblue')
legend("top", legend = c("observations","", "fitted"),
       cex = 1, bty = "n", col = c('red3',"",'deepskyblue2'),
       lwd = c(2,0,2), lty = c(2,0,2))

summary(glarmamod)

## Score Type (GAS) Residuals, Fisher Scoring
glarmamod <- glarma(y, X, thetaLags = c(1,2,5), type = "Poi", method = "FS", residuals = "Score", maxit = 100, grad = 1e-6)
glarmamod

summary(glarmamod)

## Score Type (GAS) Residuals, Newton Raphson
## Note: Newton Raphson fails to converge from GLM initial estimates.
## Setting up the initial estimates by ourselves
init.delta <- glarmamod$delta
beta <- init.delta[1:6]
thetaInit <- init.delta[7:9]
glarmamod <- glarma(y, X, beta = beta, thetaLags = c(1, 2, 5), thetaInit = thetaInit, type ="Poi", method = "NR", residuals = "Score", maxit = 100, grad = 1e-6)
glarmamod

summary(glarmamod)

## AR(1,5), Pearson Residuals, Fisher Scoring
glarmamod <- glarma(y, X, phiLags = c(1, 5), type = "Poi", method = "FS", residuals = "Pearson", maxit = 100, grad = 1e-6)
glarmamod

summary(glarmamod)

par(cex = 1.5, lwd = 2)
## The default plots are shown
plot(glarmamod)
Hit <Return> to see next plot: 
  
### Model number of cases
data(Polio)
y <- Polio[, "Cases"]
X <- as.matrix(Polio[, 2:5])
Population <- Polio[, "Trend"]
### Offset included
glarmamod <- glarma(y, X, offset = (Population/1),  phiLags = c(12),  thetaLags = c(1),  type = "Poi", method = "FS",residuals = "Pearson", maxit = 100, grad = 1e-6)
print(summary(glarmamod))

allX <- X
allFits <- fitted(glarmamod)
ally <- y
### Look at a succession of forecasts
### Using actual values in forecasts
forecasts <- numeric(72)
for (i in (62:71)){
  y <- DriverDeaths[1:i, "Deaths"]
  X <- as.matrix(DriverDeaths[1:i, 2:5])
  Population <- DriverDeaths[1:i, "Population"]
  ## Offset included
  glarmamod <- glarma(y, X, offset = log(Population/100000),phiLags = c(12),thetaLags = c(1),type = "Poi", method = "FS",residuals = "Pearson", maxit = 100, grad = 1e-6)
  XT1 <- matrix(allX[i + 1, ], nrow = 1)
  offsetT1 <- log(DriverDeaths$Population[i + 1]/100000)
  mu <- forecast(glarmamod, 1, XT1, offsetT1)$mu
  if (i == 62){
    forecasts[1:62] <- fitted(glarmamod)
  }
  forecasts[i+1] <- mu
}




XT1 <- matrix(X[72,], nrow = 1)
offsetT1 <- (Population/1)[72]
mu <- forecast(glarmamod, 1, XT1, offsetT1)$mu
print(mu)

### Save some values
allX <- X
allFits <- fitted(glarmamod)
ally <- y
### Look at a succession of forecasts
### Using actual values in forecasts
forecasts <- numeric(72)
for (i in (62:71)){y <- Polio[1:i, "Cases"]
X <- as.matrix(Polio[1:i, 2:5])
Population <- Polio[1:i, "Trend"]
## Offset included
glarmamod <- glarma(y, X, offset = (Population/1),phiLags = c(12),  thetaLags = c(1),  type = "Poi", method = "FS",residuals = "Pearson", maxit = 100, grad = 1e-6)
XT1 <- matrix(allX[i + 1, ], nrow = 1)
offsetT1 <- (Polio$Trend[i + 1]/1)
mu <- forecast(glarmamod, 1, XT1, offsetT1)$mu
if (i == 62){
  forecasts[1:62] <- fitted(glarmamod)
}
forecasts[i+1] <- mu
}
par(mfrow = c(1,1))
forecasts <- ts(forecasts[63:72], start = c(1987, 10), deltat = 1/12)
fitted <- ts(allFits, start = c(1980, 8), deltat = 1/12)
obs <- ts(Polio$Cases, start = c(1980, 8), deltat = 1/12)
plot(obs, ylab = "Polio Cases", lty = 2, col = 'green4', main = "Polio Cases")
points(obs, col = 'purple2')
lines(fitted, lwd = 2, col = 'dodgerblue3')
lines(forecasts, col = 'red2')
par(xpd = NA)
graph.param <-legend("top",legend = c("observations",expression(estimated~mu[t]), expression(predicted~mu[t])),ncol = 3,cex = 0.7,bty = "n", plot = FALSE)
legend(graph.param$rect$left,graph.param$rect$top+graph.param$rect$h,legend = c("observations", expression(estimated~mu[t]),expression(predicted~mu[t])),col = c("black","black","red"),lwd = c(1,2,1), lty = c(2,1,1),pch = c(1, NA_integer_, NA_integer_),ncol = 3,cex = 0.7,bty = "n",text.font = 4)
par(xpd = FALSE)

### Generate a sample of Y values 2 steps ahead and examine the distribution
data(Polio)
y <- Polio[, "Cases"]
X <- as.matrix(Polio[, 2:5])
Population <- Polio[, "Trend"]
### Fit the glarma model to the first 70 observations
glarmamod <- glarma(y[1:62], X[1:62, ],offset = (Population/1)[1:62], phiLags = c(12), thetaLags = c(1), type = "Poi", method = "FS", residuals = "Pearson", maxit = 100, grad = 1e-6)
nObs <- NROW(X)
n.ahead <- 2
### Specify the X matrix and offset for the times where 
### predictions are required
XT1 <- as.matrix(X[(nObs - n.ahead + 1):nObs, ])
offsetT1 <- (Population/1)[(nObs - n.ahead + 1):nObs]
nSims <- 500
forecastY <- matrix(ncol = n.ahead, nrow = nSims)
forecastMu <- matrix(ncol = n.ahead, nrow = nSims)
### Generate sample predicted values
for(i in 1:nSims){
  temp <- forecast(glarmamod, n.ahead, XT1, offsetT1)
  forecastY[i, ] <- temp$Y
  forecastMu[i, ] <- temp$mu
}
### Examine distribution of sample of Y values n.ahead
table(forecastY[, 2])


par(mfrow = c(2,1))
barplot(table(forecastY[, 2]), main = "Barplot of Sample Y Values 2 Steps Ahead")
hist(forecastY[, 2], xlab = "Sample Y values",  breaks=seq(0,max(forecastY[, 2])),  main = "Histogram of Sample Y Values 2 Steps Ahead\nwith 0.025 and 0.975 Quantiles")
abline(v = quantile(forecastY[, 2], c(0.025, 0.975)), col = "red")


data(Polio)
print(y <- Polio[, 2])
X <- as.matrix(Polio[, 3:8])
str(X)
head(X)
glarmamod <- glarma(y, X, thetaLags = c(1, 2, 5), type = "Poi",method = "FS", residuals = "Pearson",maxit = 100, grad = 1e-6)
str(model.frame(glarmamod))
head(model.frame(glarmamod))
par(lwd=2)
plot(glarmamod)


data(Polio)
print(y <- Polio[, 2])
X <- as.matrix(Polio[, 3:8])
str(X)
head(X)
glarmamod <- glarma(y, X, thetaLags = c(1, 2, 5), type = "Poi",method = "FS", residuals = "Pearson",maxit = 100, grad = 1e-6)
str(model.frame(glarmamod))
head(model.frame(glarmamod))


par(mfrow = c(1,1))
require(zoo)
### Model number of deaths
data(DriverDeaths)
y <- DriverDeaths[, "Deaths"]
X <- as.matrix(DriverDeaths[, 2:5])
Population <- DriverDeaths[, "Population"]
### Offset included
glarmamod <- glarma(y, X, offset = log(Population/100000),
                    phiLags = c(12),
                    thetaLags = c(1),
                    type = "Poi", method = "FS",
                    residuals = "Pearson", maxit = 100, grad = 1e-6)
print(summary(glarmamod))

XT1 <- matrix(X[72,], nrow = 1)
offsetT1 <- log(Population/100000)[72]
mu <- forecast(glarmamod, 1, XT1, offsetT1)$mu
print(mu)

### Save some values
allX <- X
allFits <- fitted(glarmamod)
ally <- y
### Look at a succession of forecasts
### Using actual values in forecasts
forecasts <- numeric(72)
for (i in (62:71)){
  y <- DriverDeaths[1:i, "Deaths"]
  X <- as.matrix(DriverDeaths[1:i, 2:5])
  Population <- DriverDeaths[1:i, "Population"]
  ## Offset included
  glarmamod <- glarma(y, X, offset = log(Population/100000),phiLags = c(12),thetaLags = c(1),type = "Poi", method = "FS",residuals = "Pearson", maxit = 100, grad = 1e-6)
  XT1 <- matrix(allX[i + 1, ], nrow = 1)
  offsetT1 <- log(DriverDeaths$Population[i + 1]/100000)
  mu <- forecast(glarmamod, 1, XT1, offsetT1)$mu
  if (i == 62){
    forecasts[1:62] <- fitted(glarmamod)
  }
  forecasts[i+1] <- mu
}
opar <- par()
opar
graphics::par(mfrow = c(1,1), mar = (c(5,5,6,1)))
forecasts <- ts(forecasts[63:72], start = c(1985, 10), deltat = 1/12)
fitted <- ts(allFits, start = c(1980, 8), deltat = 1/12)
obs <- ts(DriverDeaths$Deaths, start = c(1980, 8), deltat = 1/12)
plot(obs, ylab = "Driver Deaths", lty = 2, col = 'dodgerblue3', main = "Single Vehicle Nighttime Driver Deaths in Utah")
points(obs, col = 'green3')
lines(fitted, lwd = 2, col = 'purple2')
lines(forecasts, col = "red", lwd = 2)
par(xpd = NA)

graph.param <-legend("top",legend = c("observations",expression(estimated~mu[t]),expression(predicted~mu[t])), ncol = 3,cex = 1, bty = "n", plot = FALSE)
legend(graph.param$rect$left,graph.param$rect$top + graph.param$rect$h,legend = c("observations", expression(estimated~mu[t]),expression(predicted~mu[t])),col = c('dodgerblue3','purple2','red'),lwd = c(2,2,2), lty = c(2,1,1),pch = c(1, NA_integer_, NA_integer_),ncol = 3,cex = 1, bty = "n",text.font = 4)
par(xpd = FALSE)
### Generate a sample of Y values 2 steps ahead and examine the distribution

data(DriverDeaths)
y <- DriverDeaths[, "Deaths"]
X <- as.matrix(DriverDeaths[, 2:5])
Population <- DriverDeaths[, "Population"]

### Fit the glarma model to the first 70 observations
glarmamod <- glarma(y[1:70], X[1:70, ],offset = log(Population/100000)[1:70],phiLags = c(12),thetaLags = c(1),type = "Poi", method = "FS",residuals = "Pearson", maxit = 100, grad = 1e-6)
nObs <- NROW(X)
n.ahead <- 2
### Specify the X matrix and offset for the times where predictions are required
XT1 <- as.matrix(X[(nObs - n.ahead + 1):nObs, ])
offsetT1 <- log(Population/100000)[(nObs - n.ahead + 1):nObs]
nSims <- 500
forecastY <- matrix(ncol = n.ahead, nrow = nSims)
forecastMu <- matrix(ncol = n.ahead, nrow = nSims)

### Generate sample predicted values
for(i in 1:nSims){
  temp <-  forecast(glarmamod, n.ahead, XT1, offsetT1)
  forecastY[i, ] <- temp$Y
  forecastMu[i, ] <- temp$mu
}
### Examine distribution of sample of Y values n.ahead
table(forecastY[, 2])
par(mfrow = c(2,1))
barplot(table(forecastY[, 2]),main = "Barplot of Sample Y Values 2 Steps Ahead")
hist(forecastY[, 2], xlab = "Sample Y values",breaks=seq(0,max(forecastY[, 2])),main = "Histogram of Sample Y Values 2 Steps Ahead\nwith 0.025 and 0.975 Quantiles")
abline(v = quantile(forecastY[, 2], c(0.025, 0.975)), lwd = 3, col = 'dodgerblue2')
opar
par(mfrow = c(2,1), cex.main = 1.5, lwd = 2, col.main = 'dodgerblue3', col = 'red3' )
graph.param <- legend("top",legend = c("obs", "fixed", "glarma"), 
                      ncol = 3, cex = 1.25, bty = "n", plot = FALSE)
legend(graph.param$rect$left,graph.param$rect$top + graph.param$rect$h,
       legend = c("observations", expression(estimated~mu[t]),
                  expression(predicted~mu[t])),
       col = c('dodgerblue3','purple2','red'),lwd = c(2,2,2), 
       lty = c(2,1,1),pch = c(1, NA_integer_, NA_integer_),
       ncol = 3,cex = 1.25, bty = "n",text.font = 4)
par(mfrow = c(2,1), cex.main = 1.5, lwd = 2, 
    col.main = 'dodgerblue3', col = 'red3' )
plot(glarmamod)

polio_slim <- Polio[,1:4]
plot(polio_slim)

myvector = polio_slim[, 2]
polio.ts <- ts(myvector, start=c(1970, 1), end=c(1983, 12), frequency=12) 
plot(polio.ts, col=4)

y <- polio_slim[, 2]
X <- as.matrix(polio_slim[, 3:4])

glarmamod <- glarma(y, X, thetaLags = c(1,2,5), type = "Poi", method = "FS", residuals = "Pearson", maxit = 100, grad = 1e-6)
glarmamod
plot.ts(glarmamod$y)
plot.ts(glarmamod$fitted.values)
summary(glarmamod)

### Model number of deaths
data(DriverDeaths)
y <- DriverDeaths[, "Deaths"]
X <- as.matrix(DriverDeaths[, 2:5])
Population <- DriverDeaths[, "Population"]

### Offset included
glarmamodOffset <- glarma(y, X, offset = log(Population/100000),
                          phiLags = c(12),
                          type = "Poi", method = "FS",
                          residuals = "Pearson", maxit = 100, grad = 1e-6)
print(summary(glarmamodOffset))
par(mfrow =c(3,2))
plot(glarmamodOffset)


### No offset included
glarmamodNoOffset <- glarma(y, X, phiLags = c(12),
                            type = "Poi", method = "FS",
                            residuals = "Pearson", maxit = 100, grad = 1e-6)
print(summary(glarmamodNoOffset))
par(mfrow=c(3,2))
plot(glarmamodNoOffset)

###########################################################
library(glarma)
data(Polio)

myvector = Polio[, 2]
polio.ts <- ts(myvector, start=c(1970, 1), end=c(1983, 12), frequency=12) 
plot(polio.ts, col=6)

y <- Polio[, 2]
X <- as.matrix(Polio[, 3:8])

## Modeling with MA Orders
polio_fit1 <- glarma(y, X, thetaLags = c(1,2,5), type = "Poi", method = "FS", residuals = "Pearson", maxit = 100, grad = 1e-6)
summary(polio_fit1)

## Score Type (GAS) Residuals, Fisher Scoring
polio_fit2 <- glarma(y, X, thetaLags = c(1,2,5), type = "Poi", method = "FS", residuals = "Score", maxit = 100, grad = 1e-6)
summary(polio_fit2)

## Score Type (GAS) Residuals, Newton Raphson
## Note: Newton Raphson fails to converge from GLM initial estimates.

## Setting up the initial estimates by ourselves
init.delta <- glarmamod$delta
beta <- init.delta[1:6]
thetaInit <- init.delta[7:8]
polio_fit3 <- glarma(y, X, beta = beta, thetaLags = c(1, 2, 5), thetaInit = thetaInit, type ="Poi", method = "NR", residuals = "Score", maxit = 200, grad = 1e-6)
summary(polio_fit3)

## AR(1,5), Pearson Residuals, Fisher Scoring
polio_fit4 <- glarma(y, X, phiLags = c(1, 5), type = "Poi", 
                     method = "FS", residuals = "Pearson", 
                     maxit = 100, grad = 1e-6)
summary(polio_fit4)

delta <- c("Intcpt" = 0.2069383, "Trend" = -4.7986615 ,"CosAnnual" = -0.1487333, 
           "SinAnnual" = -0.5318768,"CosSemiAnnual" = 0.1690998, 
           "SinSemiAnnual" = -0.4321435,"theta_1" = 0, "theta_2"= 0, "theta_5"= 0) 

glarmaPoissonScore(y, X, offset = NULL, delta=delta, phiLags= c(1, 5), thetaLags= c(1, 2, 5), method =  "FS")

summary.glarma(polio_fit4)

##################################
### Example with asthma data
par(opar)
par(mfrow=c(1,1))
data(Asthma)
y <- Asthma[,1]
X <- as.matrix(Asthma[,2:16])
## Model in Davis, Dunsmuir and Streett (2003)
## MA(7) specification - see Davis, Dunsmuir and Streett (2003)
## Pearson Residuals, Fisher Scoring
glarmamod <- glarma(y, X, thetaLags = 7, type = "Poi", method = "FS",residuals = "Pearson", maxit = 100, grad = 1e-6)
glarmamod
summary(glarmamod)
likTests(glarmamod)
plot.glarma(glarmamod)
## Not run:
## Example is specified as \dontrun because it takes too long
## for package inclusion on CRAN
## Pearson Residuals, Newton Raphson, Negative Binomial
## Initial value of the shape parameter take to be zero
glarmamod <- glarma(y, X, thetaLags = 7, type = "NegBin", method = "NR",residuals = "Pearson", alphaInit = 0,maxit = 100, grad = 1e-6)
glarmamodsummary(glarmamod)
likTests(glarmamod)
plot.glarma(glarmamod)
## End(Not run)
##############################################
library(tscount)
data(influenza)
influenza
infl_vector <- influenza[,3]
infl_fit <- tsglm(ts = infl_vector, model=list(past_obs=1, past_mean=c(7,13)))
summary(infl_fit)
plot(infl_fit)

campyfit <- tsglm(ts=campy, model=list(past_obs=1, past_mean=c(7,13)))
summary(campyfit)
plot(campyfit)
