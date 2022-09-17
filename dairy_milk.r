# If library is needed and not already installed
if(!require(timeSeries)) install.packages("timeSeries")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(graphics)) install.packages("graphics")
if(!require(forecast)) install.packages("forecast")
if(!require(dplyr)) install.packages("dplyr")
if(!require(colortools)) install.packages("colortools")
if(!require(forecast)) install.packages("forecast")
if(!require(TTR)) install.packages("TTR")
if(!require(fpp2)) install.packages("fpp2")
if(!require(TSA)) install.packages("TSA")
if(!require(stlplus)) install.packages("stlplus")

# Load installed libraries
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

################################################################
# MILK PRODUCTION
################################################################
# Read milk monthly data
milk_mon <- read.csv("D:\\Documents\\DATA\\milk_monthly.txt")
milk_day <- read.csv("D:\\Documents\\DATA\\milk_daily.txt")
head(milk_mon)
class(milk_mon)
class(milk_mon$month)
head(milk_mon)

milk_mon_ts
# Coerce to `Date` class
milk_mon$month_date <- as.Date(milk_mon$month, format = "%Y-%m-%d")
# Check it worked
class(milk_mon$month_date)

class(format(milk_mon$month_date, format = "%Y-%B-%u")) 
format(milk_mon$month_date, format = "%Y-%B-%u")

milk_mon$bad_date <- format(milk_mon$month_date, format = "%d/%b/%Y-%u")
head(milk_mon$bad_date)  # Awful... 
class(milk_mon$bad_date)  # Not in `Date` class

milk_mon$good_date <- as.Date(milk_mon$bad_date, format = "%d/%b/%Y-%u")
head(milk_mon$good_date)
class(milk_mon$good_date)

milk_mon$year <- format(milk_mon$month_date, format = "%Y")

milk_day$date_time_posix <- as.POSIXct(milk_day$date_time, format = "%Y-%m-%d %H:%M:%S")
class(milk_day$date_time_posix)

par(mar = c(5, 5, 3,.25))
par(col.lab = 'dodgerblue3', font.lab = 2, cex.main = 3, cex.axis = 2, cex.lab = 3)
ggplot(milk_day, aes(x = date_time_posix, y = milk_prod_per_cow_kg)) +
    geom_line(color = "purple", linetype = 1, size = 2) +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
    ggtitle("Monthly Milk Production") +
    ylab("Production in kg") +
    xlab("Years") + 
    theme(plot.title = element_text(size=24),
          axis.text=element_text(size=14, face="bold"),
          axis.title=element_text(size=18, face="bold"))

par(mar = c(5, 5, 3,.25))
par(col.lab = 'dodgerblue3', font.lab = 2, cex.main = 3, cex.axis = 2, cex.lab = 3)
ggplot(milk_day, aes(x = date_time_posix, y = milk_prod_per_cow_kg)) +
  geom_line(color = "green3", linetype = 1, size = 2) +
  #scale_x_date(date_labels = "%d", date_breaks = "1 year") +
  ggtitle("Daily Milk Production") +
  ylab("Production in kg") +
  xlab("Days") + 
  theme(plot.title = element_text(size=24),
        axis.text=element_text(size=14, face="bold"),
        axis.title=element_text(size=18, face="bold"))


par(mar=c(1,3,1,1))
par(mfrow=c(1,1), mai = c(1, 1, .75, .5))
par(col.lab = 'dodgerblue3', font.lab = 2, cex.axis = 1.25, cex.lab = 1.25)
plot(milk_mon, col = 'navy',  
     lwd = 3,  col.lab = 'dodgerblue3', cex.lab = 2,
     panel.first = grid(10, lty = 3, lwd = 2, col = 'lightblue')) 
title(main = "New York City births from January 1946 to December 1949", 
      cex.main = 1.5,   font.main= 4, col.main= "dodgerblue4",
      cex.sub = 0.75, font.sub = 3, col.sub = "dodgerblue2")
lines(modelNile$s.level, col = 'purple1', lwd=2)
legend("bottomleft",legend=c("Observed flow","S_level"), 
       col= c('navy','purple1'), lwd = 3, lty = 1,  
       x.intersp=2, y.intersp=2, bty='n')

par(mar = c(5, 5, 3,.25))
par(col.lab = 'dodgerblue3', font.lab = 2.5, cex.axis = 1.5, cex.lab = 1.75)
plot(forecast(fit7, 36), lwd=3, main = "Unsecured Consumer Loans",  xlab = "Months", 
     ylab= "Volume in Loans", cex.main = 2,  col.main = 'dodgerblue3',  
     panel.first = grid(10, lty = 3, lwd = 1.5, col = 'dodgerblue2'))

(time_plot_2 <- ggplot(milk_day, aes(x = date_time_posix, y = milk_prod_per_cow_kg)) +
    geom_line(color = "dark green", linetype = 1, size = 1.1) +
    ggtitle("Forecasts from Holt-Winters Multiplicative Damped method") +
    scale_x_datetime(date_labels = "%m-%d", date_breaks = "1 week") +
    theme_classic())

par(mfrow=c(1,1), cex = 1.5, cex.lab = 1, cex.axis = 1)
trendpattern = stats::filter(milk_mon_ts, c(1/4, 1/4, 1/4, 1/4), sides=1)
plot(trendpattern, type= "b", col="blue", 
     main = "Quarterly moving average annual trend")
lines(trendpattern, col="blue", lwd=3)

par(mfrow=c(1,1))
trendpattern = stats::filter(milk_mon_ts, c(1/12, 1/12, 1/12, 1/12, 1/6, 1/6, 1/12, 1/12, 1/12, 1/12), sides=2)
plot(trendpattern, type= "b", col="blue", main = "Monthly moving average annual trend")
lines(trendpattern, col="blue", lwd=3)

# Convert milk monthly data to a time series
milk_ts <- ts(milk_mon$milk_prod, start = 1962, end = 1975, freq = 12)  # Specify start and end year, measurement frequency (monthly = 12)
# Smooth milk monthly time series data using Holt model
mm1 <- holt(milk_ts, h=15)
# Smooth milk monthly time series data using Holt ammped model
mm2 <- holt(milk_ts, damped=TRUE, phi = 0.9, h=15)
# Plot milk monthly time series and forecasts
par(mfrow=c(1,1))
autoplot(mm1) +
  autolayer(milk_ts, series="Australian Air Travel", color="purple", size = 1.2) +
  autolayer(mm1, series="Holt's method", PI=FALSE, color="blue", size = 1.2) +
  autolayer(mm2, series="Damped Holt's method", PI=FALSE, color="red", size = 1.2) +
  ggtitle("Forecasts from Holt's method") + xlab("Year") +
  ylab("Milk Production (kg)") +
  guides(colour=guide_legend(title="Forecast")) +
  theme(legend.position="bottom")
# Plot milk monthly Holt mdeol lags
par(mfrow=c(1,1))
ts_lags(milk_ts)
ts_lags(mm1$residuals)
# Plot milk monthly Holt damped model lags
ts_lags(milk_ts)
ts_lags(mm2$residuals)
# Plot milk monthly Holt ACF & PACF
par(mfrow=c(3,1))
acf(mm1, col="red", lwd=2, main="Holt's additive model ACF")
acf(mm1$residuals, col="red", lwd=2, main="Holt's additive model residuals ACF")
pacf(mm1$residuals, col="red", lwd=2, main="Holt's additive model residuals PACF")
# Plot milk monthly Holt damped ACF & PACF
acf(mm2, col="red", lwd=2, main="Holt's damped model ACF")
acf(mm2$residuals, col="red", lwd=2, main="Holt's damped model residuals ACF")
pacf(mm2$residuals, col="red", lwd=2, main="Holt's damped model residuals PACF")

milk.diff<-diff(milk_ts, differences=3)
mm3 <- holt(milk.diff, damped=TRUE, phi = 0.9, h=15)
par(mfrow=c(2,1))
acf(mm1, col="red", lwd=3, main="Holt's additive model residuals ACF")
acf(mm3, col="red", lwd=3, main="Holt's additive model residuals ACF")
acf(mm1$residuals, col="red", lwd=3, main="Holt's additive model residuals ACF")
acf(mm3$residuals, col="red", lwd=3, main="Holt's additive model diff residuals ACF")
pacf(mm1$residuals, col="red", lwd=3, main="Holt's additive model residuals PACF")
pacf(mm3$residuals, col="red", lwd32, main="Holt's additive model diff residuals PACF")

acf(milk_ts, lag.max = 36)
pacf(milk_ts, lag.max = 36)

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

milk.diff<-diff(milk_ts, differences=6)
## acf of the CO2 data
milk_ts.acf <- acf(milk.diff, lag.max = 36)
## correlogram of the CO2 data
plot.acf(milk_ts.acf)

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

## PACF of the CO2 data
milk.pacf <- pacf(milk.diff)
## correlogram of the CO2 data
plot.acf(milk.pacf)

theme02 <- (theme(plot.title = element_text(size = rel(1.75))) +
  theme(panel.background = element_rect(fill = "snow2")) +
  theme(axis.title.x = element_text(size = rel(1.75), color="dodgerblue3")) +
  theme(axis.title.y = element_text(size = rel(1.75), color="dodgerblue3")) +
  theme(axis.text.x = element_text(color="coral4", size=rel(1.65), face="bold"),
        axis.text.y = element_text(color="coral4", size=rel(1.65), face="bold")) +
  theme(axis.line = element_line(size = 2, colour = "grey80")))

theme03 <- (theme(plot.title = element_text(size = rel(1.5))) +
              theme(panel.background = element_rect(fill = "snow2")) +
              theme(axis.title.x = element_text(size = rel(1.35), color="dodgerblue3")) +
              theme(axis.title.y = element_text(size = rel(1.35), color="dodgerblue3")) +
              theme(axis.text.x = element_text(color="coral4", size=rel(1.25), face="bold"),
                    axis.text.y = element_text(color="coral4", size=rel(1.25), face="bold")) +
              theme(axis.line = element_line(size = 2, colour = "grey80")))

par(mar(4,5,5,1))
(decomp_1 <- ggplot(milk_mon, aes(x = month_date, y = milk_prod_per_cow_kg)) +
    geom_line(color = "dark red", linetype = 1, size = 1.1) +
    ggtitle("Monthly Milk Production per Cow") + 
    xlab("Years") + 
    ylab("Milk Production (kg)") +
    scale_x_date(date_labels = "%Y", date_breaks = "2 year")) + theme02


(decomp_2 <- ggplot(milk_mon, aes(x = month_date, y = milk_prod_per_cow_kg)) +
    geom_line(color = "dark red", linetype = 1, size = 1.1) +
    geom_smooth(method = "loess", se = FALSE, span = 0.6, size = 1.5) +
    ggtitle("Monthly Milk Production per Cow") + 
    xlab("Years") + 
    ylab("Milk Production (kg)") +
    scale_x_date(date_labels = "%Y", date_breaks = "2 year")) + theme02

# Extract month and year and store in separate columns
milk_mon$year <- format(milk_mon$month_date, format = "%Y")
milk_mon$month_num <- format(milk_mon$month_date, format = "%m")

# Create a colour palette using the `colortools` package 
year_pal <- sequential(color = "darkturquoise", percentage = 5, what = "value")

# Make the plot
par(mfrow=c(1,1), cex.main = 2, cex.lab = 1.5)
(seasonal <- ggplot(milk_mon, aes(x = month_num, y = milk_prod_per_cow_kg, group = year)) +
    geom_line(aes(colour = year), size = 1.5) +
    ggtitle("Monthly Milk Production per Cow") + 
    xlab("Month Number") + 
    ylab("Milk Production (kg)") +
    theme02 + 
    scale_color_manual(values = year_pal))

# Transform to `ts` class
milk_mon_ts <- ts(milk_mon$milk_prod, start = 1962, end = 1975, freq = 12)  # Specify start and end year, measurement frequency (monthly = 12)

# Decompose using `stl()`
milk_mon_stl <- stl(milk_mon_ts, s.window = "period")

# Generate plots
par(mfrow=c(1,1), font.lab = 2, cex.main = 2, cex.lab = 1.5, cex.axis = 1.5)
# top=original data, second=estimated seasonal, third=estimated smooth trend, bottom=estimated irregular element i.e. unaccounted for variation
plot(milk_mon_stl, col="purple", lwd=3,  main="Milk Production Time Series Decomposition")  
# variation in milk production for each month
monthplot(milk_mon_ts, choice = "seasonal", col.base=1:6, 
          lty.base=1, lwd.base=5, lwd = 2, xlab="Month", 
          ylab="Milk per Cow (kg)", 
          main="Monthly Milk Production") 

par(mfrow=c(1,1), font.lab = 2, cex.main = 2, cex.lab = 1.5, cex.axis = 1.5)  
seasonplot(milk_mon_ts, col=1:6, lty=1, lwd=2.5, pch = 15, year.labels = TRUE, 
           labelgap = .25, year.labels.left=TRUE, xlab="Month",
           ylab="Milk per Cow (kg)", main="Monthly Milk Production")

par(mfrow=c(1,1), font = 2, font.lab = 2, cex.main = 2, cex.lab = 1.5, cex.axis = 1.5)  
ggseasonplot(milk_mon_ts, polar=TRUE) +
  ylab("Milk Production (ML)") + 
  geom_line(linetype = 1, size = 1.1)+
  ggtitle("Monthly milk production per month") + theme03


milk_mon_model <- window(x = milk_mon_ts, start = c(1962), end = c(1970))
milk_mon_test <- window(x = milk_mon_ts, start = c(1970))

# Creating model objects of each type of ets model
milk_ets_auto <- ets(milk_mon_model)
milk_ets_mmm <- ets(milk_mon_model, model = "MMM")
milk_ets_zzz<- ets(milk_mon_model, model = "ZZZ")
milk_ets_mmm_damped <- ets(milk_mon_model, model = "MMM", damped = TRUE)

# Creating forecast objects from the model objects
milk_ets_fc <- forecast::forecast(milk_ets_auto, h = 60)  # `h = 60` means that the forecast will be 60 time periods long, in our case a time period is one month
milk_ets_mmm_fc <- forecast::forecast(milk_ets_mmm, h = 60)
milk_ets_zzz_fc <- forecast::forecast(milk_ets_zzz, h = 60)
milk_ets_mmm_damped_fc <- forecast::forecast(milk_ets_mmm_damped, h = 60)

# Convert forecasts to data frames
milk_ets_fc_df <- cbind("Month" = rownames(as.data.frame(milk_ets_fc)), as.data.frame(milk_ets_fc))  # Creating a data frame
names(milk_ets_fc_df) <- gsub(" ", "_", names(milk_ets_fc_df))  # Removing whitespace from column names
milk_ets_fc_df$Date <- as.Date(paste("01-", milk_ets_fc_df$Month, sep = ""), format = "%d-%b %Y")  # prepending day of month to date
milk_ets_fc_df$Model <- rep("ets")  # Adding column of model type

milk_ets_mmm_fc_df <- cbind("Month" = rownames(as.data.frame(milk_ets_mmm_fc)), as.data.frame(milk_ets_mmm_fc))
names(milk_ets_mmm_fc_df) <- gsub(" ", "_", names(milk_ets_mmm_fc_df))
milk_ets_mmm_fc_df$Date <- as.Date(paste("01-", milk_ets_mmm_fc_df$Month, sep = ""), format = "%d-%b %Y")
milk_ets_mmm_fc_df$Model <- rep("ets_mmm")

milk_ets_zzz_fc_df <- cbind("Month" = rownames(as.data.frame(milk_ets_zzz_fc)), as.data.frame(milk_ets_zzz_fc))
names(milk_ets_zzz_fc_df) <- gsub(" ", "_", names(milk_ets_zzz_fc_df))
milk_ets_zzz_fc_df$Date <- as.Date(paste("01-", milk_ets_zzz_fc_df$Month, sep = ""), format = "%d-%b %Y")
milk_ets_zzz_fc_df$Model <- rep("ets_zzz")

milk_ets_mmm_damped_fc_df <- cbind("Month" = rownames(as.data.frame(milk_ets_mmm_damped_fc)), as.data.frame(milk_ets_mmm_damped_fc))
names(milk_ets_mmm_damped_fc_df) <- gsub(" ", "_", names(milk_ets_mmm_damped_fc_df))
milk_ets_mmm_damped_fc_df$Date <- as.Date(paste("01-", milk_ets_mmm_damped_fc_df$Month, sep = ""), format = "%d-%b %Y")
milk_ets_mmm_damped_fc_df$Model <- rep("ets_mmm_damped")

# Combining into one data frame
forecast_all <- rbind(milk_ets_fc_df, milk_ets_mmm_fc_df, milk_ets_zzz_fc_df, milk_ets_mmm_damped_fc_df)

# Plotting with ggplot
par(mar = c(4,3,3,.15))
dat <- milk_mon[1:108,]
ggplot() + 
    geom_line(data = dat, aes(x = month_date, y = milk_prod_per_cow_kg), lwd=1.5) +  
    geom_line(data = forecast_all, aes(x = Date, y = Point_Forecast, color = Model), lwd=1.5) +  
    ggtitle("Monthly Milk Production Forecast (Jan 1971 - Dec-1975)") + 
    ylab("Milk Production per Cow (ML)") +
    guides(colour=guide_legend(title="Legend")) +
    theme(axis.text = element_text(size=16, face = "bold", color = 'navy'),
          axis.title = element_text(size=18, face="bold", color = 'navy'),
          plot.title = element_text(color = "navy", size = 18, face = "bold", hjust = 0.5),
          legend.title = element_text(color="navy", size=16,  face="bold"),
          legend.text = element_text(color = "navy", size = 16),
          legend.position = "bottom")

scale_x_continuous(breaks = round(seq(min(dat$year), max(dat$year), by = 0.5),1))
dat$year

milk_mon[1:108,]

accuracy(milk_ets_fc, milk_mon_test)
accuracy(milk_ets_mmm_fc, milk_mon_test)
accuracy(milk_ets_zzz_fc, milk_mon_test)
accuracy(milk_ets_mmm_damped_fc, milk_mon_test)


milk_ets_fc_df %>%
  filter(Month == "Jan 1975") %>%
  select(Month, Point_Forecast)

milk_ets_zzz_fc_df %>%
  filter(Month == "Jan 1975") %>%
  select(Month, Point_Forecast)

plot(milk_mon, col="blue", lwd=2, main="Holt-Winters Multiplicative Damped method")


milk_mon_model <- window(x = milk_mon_ts, start = c(1962), end = c(1970))
milk_mon_test <- window(x = milk_mon_ts, start = c(1970))
fit<-hw(milk_mon_model, damped=TRUE, seasonal="multiplicative")
fit$model
accuracy(fit, milk_mon_test)
accuracy(milk_ets_mmm_fc, milk_mon_test)
accuracy(milk_ets_zzz_fc, milk_mon_test)
accuracy(milk_ets_mmm_damped_fc, milk_mon_test)

fc <- hw(subset(milk_mon_ts,end=length(milk_mon_ts)),
         damped = TRUE, seasonal="multiplicative", h=35)
autoplot(milk_mon_ts) +
  autolayer(milk_mon_ts, series="HW multi damped", PI=FALSE, size=1.25, color="blue") +
  autolayer(fc, series="HW multi damped", PI=FALSE, size=1.25) + 
  ggtitle("Forecasts from Holt-Winters Multiplicative Damped method") + xlab("Year") +
  ylab("Milk Production per Cow per month (kg)") +
  guides(colour=guide_legend(title="Monthly forecasts")) +
  theme(legend.position = "bottom")

(forecast_plot <- ggplot() +
    # Plotting original data
    geom_line(data = milk_mon, aes(x = month_date, y = milk_prod_per_cow_kg)) +  
    # Plotting model forecasts
    geom_line(data = forecast_all, aes(x = Date,   y = Point_Forecast, colour = Model)) +  
    theme_classic())

library(TSstudio)
ts_seasonal(milk_mon_ts, type = "all")

ts_heatmap(milk_mon_ts)



par(cex.main = 1.75, cex.axis = 1.25, cex.lab = 1.5,
    col.main = 'dodgerblue3', col.axis = 'darkorchid3', col.lab = 'dodgerblue3')
fit <- stl(milk_mon_ts, s.window="period")
ts_lags(milk_mon_ts)
plot(milk_mon_ts, main = "Decomposition of Loans Time Series",
     lwd = 2, col = 'darkblue')

par(cex = 2)
ts_lags(milk_mon_ts)


################################################################
# BEER SALES
################################################################
require(TSA)
#Read the Beer data
data(beersales)
class(beersales)
beersales1<-window(beersales, start=1975)
#Build a time series
beer2.ts <- ts(beersales1, start=c(1991, 1), end=c(1995, 12), 
               frequency=12)
par(mfrow=c(1,1))
plot(beer2.ts, col=4, lwd=2, main="Monthly Beer Sales", ylab="Beer Sales in Million of Barrels")

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

plot.acf(beer.acf3)
title(main="Beer ACF using Holt-Winters", cex.main=1.5, font.main=2, col.main="blue", col.lab ="darkblue")
plot.acf(beer.acf4)
title(main="Beer PACF using Holt-Winters", cex.main=1.5,  font.main=2, col.main="blue", col.lab ="darkblue")

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


## Plot MSFT Example Data Set - 
if(!require(timeSeries)) install.packages("timeSeries")
library(timeSeries)
data(MSFT)
plot(MSFT[, 1:4], type = "l")
plot(MSFT[, 5], type = "h")
acf(MSFT)
ts_lags(MSFT)


################################################################
# OIL DATA
################################################################

oildata<-read.table("http://www.stat.sc.edu/~habing/courses/data/oildata.txt",header=TRUE) 
oildata
oildata <- window(oildata, start=1996)
autoplot(oildata) +
  ylab("Oil (millions of tonnes)") + xlab("Year")

corrgram(oildata, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Milk Production Time Series Correlogram") 

corrgram(oildata, order=NULL, lower.panel=panel.shade,
         upper.panel=NULL, text.panel=panel.txt,
         main="Milk Production Time Series Correlogram") 

library(GGally)
data$v4 = oildata$Econ ** 2 
data$v5 = -(oildata$Low ** 2) 
ggcorr(milk_mon_ts, method = c("everything", "pearson"))
ggpairs(milk_mon, columns = 1:2)

remove.packages(purrr)
library(corrplot)
devtools::install_github("laresbernardo/lares")
library(lares)
corr_cross(oil, # name of dataset
                         max_pvalue = 0.05, # display only significant correlations (at 5% level)
                         top = 10 # display top 10 couples of variables (by correlation coefficient)
)
library(spatia)
R <- cor(oildata)
correlogram(R)

install.packages('fpp2', dependencies = TRUE)
library(fpp2)
library(Correlplot)


dmlist("http://data.is/nyFeP9")
dminfo("http://datamarket.com/data/set/17tm/#ds=17tm!kqc=17.v.i")
#### oil
dminit("71bcd20bb05e4251a8b4cbaaa8c0cdf9")
dta <- dmlist("https://datamarket.com/data/set/17tm/oil-production-tonnes#!ds=17tm!kqc=4")
oil <- ts(a$Value, start = 1965)
plot(oil)
lines(fpp::oil, col = "red")
devtools::use_data(oil, overwrite = TRUE)

autoplot(oil) +
  autolayer(oil, color="purple", size = 1.2) +
  ggtitle("Annual oil production, Saudi Arabia, 1965-2013") + 
  ylab("Oil (millions of metric tons)") + xlab("Year") + 
  title('Annual oil production, Saudi Arabia, 1965-2013') +
  theme(plot.title = element_text("Annual oil production, Saudi Arabia, 1965-2013", size = rel(1.75))) +
  theme(axis.title.x = element_text("Years", size = rel(1.5), color="blue")) +
  theme(axis.title.y = element_text("Oil (millions of metric tons)", size = rel(1.65), color="blue3")) +
  theme(panel.background = element_rect(fill = "grey90", size=rel(1.2)), 
        axis.text = element_text(colour = "purple", size = rel(1.5)))
  
  theme(axis.line = element_line(size = 2, colour = "grey80"))

autoplot(oil) + geom_line(color = "purple", linetype = 1, size = 1.1) +
  autolayer(oil, color="purple", size = 1.2) 
  ylab("Oil (millions of metric tons)") + xlab("Year") + 
  title('Annual oil production (millions of metric tons), Saudi Arabia, 1965-2013')
  

oildata <- window(oil, start=1996)
oildata
# Estimate parameters
fc1 <- ses(oildata, h=5)
# Accuracy of one-step-ahead training errors
round(accuracy(fc1),2)

fc$model
autoplot(oil)

autoplot(fc1, size=1.2) + 
  autolayer(oildata, size=1.2, color="purple") +
  autolayer(fitted(fc1), series="Fitted", color="red", size = 1.2) + 
  ylab("Oil (millions of metric tons)") + xlab("Year") +
  theme(plot.title = element_text("Annual oil production, Saudi Arabia, 1965-2013", size = rel(1.75))) +
  theme(axis.title.x = element_text("Years", size = rel(1.5), color="blue")) +
  theme(axis.title.y = element_text("Oil (millions of metric tons)", size = rel(1.65), color="blue3")) +
  theme(panel.background = element_rect(fill = "grey90", size=rel(1.2)), 
        axis.text = element_text(colour = "purple", size = rel(1.5)))

################################################################
# Australian Air
################################################################
library(fpp2)
air <- window(ausair, start=1990)
fc <- holt(air, h=10)
fc
fc$model
acf(fc$residuals)
pacf(fc$residuals)

ausair
air
par(bg = 'white', mar = c(4,4,2,1), 
    cex = 1.75, cex.lab = .75, cex.axis = .75)
plot(airpass, cex = 1.5, cex.lab = 1, cex.axis = 1, 
     col = "blue", lwd = 3,
     ylab = " Air Passengers (millions)", xlab = "Year",
     main ="Annual Air passengers in Australia")  
 

fc2 <- hw(air, damped = TRUE, seasonal="multiplicative")

fc2 <- hw(subset(air,end=length(air)-12),
          damped = TRUE, seasonal="multiplicative", h=12)

air <- window(ausair, start=1990)
fc1 <- holt(air, h=15)
fc2 <- holt(air, damped=TRUE, phi = 0.9, h=15)
autoplot(fc1) +
  autolayer(air, series="Australian Air Travel", color="purple", size = 1.2) +
  autolayer(fc1, series="Holt's method", PI=FALSE, color="blue", size = 1.2) +
  autolayer(fc2, series="Damped Holt's method", PI=FALSE, color="red", size = 1.2) +
  ggtitle("Forecasts from Holt's method") + xlab("Year") +
  ylab("Air passengers in Australia (millions)") +
  guides(colour=guide_legend(title="Forecast")) +
  theme(legend.position="bottom")

acf(ausair)
pacf(ausair)
acf(fc1)
acf(fc1$residuals)
pacf(fc1$residuals)
acf(fc2)
acf(fc2$residuals)
pacf(fc2$residuals)
################################################################
# LIVESTOCK
################################################################

autoplot(livestock) + 
  autolayer(livestock, color="blue", size = 1.2) +
  xlab("Year") + ylab("Livestock, sheep in Asia (millions)")

e1 <- tsCV(livestock, ses, h=1)
e2 <- tsCV(livestock, holt, h=1)
e3 <- tsCV(livestock, holtwinters, damped=TRUE, h=1)
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

autoplot(e3) +
  autolayer(livestock, color="blue", size = 1.2) +
  xlab("Year") + ylab("Livestock, sheep in Asia (millions)")

ts_lags(e3)
acf(fc)
pacf(fc)
acf(e3)
pacf(e3)
acf(fc$residuals)
pacf(fc$residuals)
################################################################
# CONSUMER CONFIDENCE INDEX - UNEMPLOYMENT
################################################################
data(unemp.cci) 

cci <- ts(unemp.cci[,"cci"])
cci<-window(unemp.cci[,"cci"],start=1997)
par(mar=c(4,4,2,1), oma = c(1,1,1,.25))
plot.ts(cci, main="Consumer Cost Index",  
        lwd=3, col = "purple", lty = 1) 
  
geom_line(color = "purple", lty = 1, size = 1.1)
  
cci.add<- HoltWinters(cci, beta=FALSE, gamma=FALSE,  seasonal = "additive")
cci.add

cci.mult<-HoltWinters(cci, beta=FALSE, gamma=FALSE,  seasonal = "multiplicative")
cci.mult

plot(cci.add$fitted, col=2, lwd=2)
plot(cci.mult$fitted, col=2, lwd=2)
  
cci.smoother<- HoltWinters(cci, gamma=FALSE)
cci.smoother

plot(cci.smoother$fitted, col="purple", lwd=2)

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

pacf()
########################################
par(mar = c(2, 5, 3, .15), cex.axis = 1, cex.lab = 1.5, font = 2, bg = 'white')
data(unemp.cci) 
cci <- ts(unemp.cci[,"cci"])
cci <- window(unemp.cci[,"cci"], start = 1997)
cci.fit<-hw(cci, seasonal="additive")
cci.acf <- acf(cci.fit$fitted, 36)
cci.pacf <- pacf(cci.fit$fitted, 36)
par(mfrow=c(2,1))
plot.acf(cci.acf)
title(main = "CCI ACF fit with Holt_Winters Additibe method", 
      cex.main = 1.5, font.main = 2, col.main = 'blue', 
      col.lab = 'blue')
plot.acf(cci.pacf)
title(main = "CCI PACF fit with Holt_Winters Additibe method", 
      cex.main = 1.5, font.main = 2, col.main = 'blue', 
      col.lab = 'blue')
Box.test(f1$fitted, lag=36, type="Ljung-Box")
#######################################

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
beer.diff1<-diff(f1, differences=2)
# ACF of the differenced beer data
beer.acf1 <- acf(cci, lag.max = 36)
# PACF of the differenced beer data
beer.pacf1 <- pacf(f1, lag.max = 36)
# correlograms of the differenced beer data
par(bg='white', lwd=2, mar=c(3, 4, 2, 2), cex=1.5, cex.axis=.85, cex.lab=1)
plot.acf(beer.acf1)
title(main="CCI ACF fit with Holt_Winters Additibe method", 
      cex.main=1, font.main=2, col.main='blue', col.lab ='darkblue')
plot.acf(beer.pacf1)
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

autoplot(cci) +
  autolayer(cci, color="blue", size=1.25) +
  autolayer(fit_1, series="Additive", PI=FALSE, size=1.25) +
  autolayer(fit_2, series="Multiplicative", PI=FALSE, size=1.25) +
  autolayer(fit_3, series="Multiplicative damped", PI=FALSE, size=1.25) +
  xlab("Year") + 
  ylab("Unemployment Index") +
  ggtitle("Consumer Confidence Index (CCI)") +
  guides(colour=guide_legend(title="HW Forecast"))

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

par(mfrow=c(1,1))

aust <- window(austourists,start=2005)
fit1 <- hw(aust,seasonal="additive")
fit2 <- hw(aust,seasonal="multiplicative")
fit1$model
fit1$upper

autoplot(aust) +
  autolayer(aust, color="blue", size=1.25) +
  autolayer(fit1, series="HW additive forecasts", PI=FALSE, size=3) +
  autolayer(fit2, series="HW multiplicative forecasts", PI=FALSE, size=1.25) +
  xlab("Year") +
  ylab("Visitor nights (millions)") +
  ggtitle("International visitors nights in Australia") +
  guides(colour=guide_legend(title="Forecast"))

cci <- ts(unemp.cci[,"cci"])
cci <- window(unemp.cci[,"cci"], start = 1997)
plot.ts(cci, main = "Consumer Cost Index", col = 4, lwd = 2) 
geom_line(color = "purple", linetype = 1, size = 1.1)
title(main = "CCI ACF fit with Holt_Winters Additibe method", 
      cex.main = 1, font.main = 2, col.main = 'blue', 
      col.lab = 'darkblue')
plot.acf(cci.pacf)
title(main = "CCI PACF fit with Holt_Winters Additibe method", 
      cex.main = 1, font.main = 2, col.main = 'blue', 
      col.lab = 'darkblue')


fit1 <- HoltWinters(diff(livestock, differences=1), beta=FALSE, gamma=FALSE)
for1<-forecast(fit1, h=48)
acf(for1$residuals, lag.max=48)
pacf(for1$residuals, lag.max=48)
Box.test(for1$residuals, lag=48, type="Ljung-Box")

################################################################
# AIR PASSENGERS
################################################################
library(fpp2)
require(TSA)
require(stlplus)
data(airpass)
Air_stl <- stlplus(airpass, t = as.vector(time(airpass)), 
                   n.p = 12, l.window = 13, t.window = 19, 
                   s.window = 35, s.degree = 1, 
                   sub.labels = substr(month.name, 1, 3))
plot(Air_stl, ylab = "Air Passenger Concentration (ppm)", 
     xlab = "Time (years)", lwd=3)
plot_seasonal(Air_stl, col = 'royalblue',lwd=3, pch = 19)
plot_trend(Air_stl, lwd=3, col="purple")
plot_cycle(Air_stl, col = 'navy', lwd=4)
plot_rembycycle(Air_stl, col = 'magenta', lwd=3, pch = 19)

################################################################
################################################################
par(mfrow=c(1,1), mar = c(2, 3, 2, 2)) 
par(cex.axis = 1.5, cex.lab = 1.5, lwd = 3, cex.main = 1.75, font.lab = 2,
    col.main = 'dodgerblue3', col.lab = 'dodgerblue3', col.axis = 'dodgerblue3')
x <- c(1,3,4,7,8,6,9,4,3,5,2)
length(x)
plot(x, type = 'b', col =' blue', lwd = 4)

plot(x[-1], x[-11], type = 'b', col = 'blue', lwd = 4)
abline(0, 1, lty = 2, lwd = 3, col = 'gray')

lag.plot(x, col = 'blue', lwd = 4, dig = TRUE, diag.col = 'gray')
lag.plot(x, 9, col = 'dodgerblue4', lwd = 3)

ggplot(x, )
gglagplot(x, lwd = 3)


gglagplot(woolyrnq)
gglagplot(woolyrnq,seasonal=FALSE)

lungDeaths <- cbind(mdeaths, fdeaths)
gglagplot(lungDeaths, lags=2)
gglagchull(lungDeaths, lags=6)

gglagchull(woolyrnq)

################################################################
# LOANS
################################################################
# Import loan data
loans <- read.csv("D:\\Documents\\DATA\\loans.csv")
myvector=loans[,2]
# Transform loan data to a times series
loans.ts <- ts(myvector, start=c(2002, 1), end=c(2013, 4), frequency=12)
is.timeSeries(loans.ts)
par(mfrow=c(1,1))
plot.ts(loans.ts, col=4, lwd=2, main="Loans Times")
# Apply differences = 1
loandiff1 <- diff(loans.ts, differences=1)
# Second order differencing
par(mfrow=c(2,1), mar = c(5,4,4,2) + 0.1, cex =.75)
loandiff2 <- diff(loans.ts, differences=6)
plot.ts(loandiff1, col=4, lwd=2, main="Loans Times Series First Order Differencing")
plot.ts(loandiff2, col=4, lwd=2, main="Loans Times Series Second Order Differencing")

library(tseries)
adf.test(myvector, alternative = "explosive")
adf.test(myvector2, alternative = "stationary")
adf.test(myvector, alternative = "stationary")
adf.test(loandiff2, alternative = "stationary")

myvector2<-loandiff1
kpss.test(diff(myvector, 1), null = "Trend")

################################################################ 
#RANDOM WALK
###############################################################
par(mfrow=c(1,1), title(cex=1.35), cex.lab = 1.3, cex.axis = 1.25)
x <- w <- rnorm(1000)
for (t in 2:1000) x[t]<-x[t-1]+w[t]
layout(1:2)
plot(x, type="l", col="orangered", lwd=2.5, main="Random Walk")
acf(x, col="green3", lwd=4, main="Lag Plot of Random Walk")
acf(diff(x), col="magenta2", lwd=4, main="Random Walk First-Order Differencing")

layout(1:2)
Z <- read.csv("D:\\Documents\\DATA\\pounds_nz.txt", header=T)
Z.ts<-ts(Z,st=1991,fr=4)
plot(Z.ts, col="purple", lwd=3, main="Pounds Random Walk")
acf(diff(Z.ts), col="red", lwd=3, main="First-Order Difference Random Walk")

ts_lags(milk_mon_ts)

require(TSA)
require(stlplus)
data(airpass)
Air_stl <- stlplus(airpass, t = as.vector(time(airpass)), 
                  n.p = 12, l.window = 19, t.window = 19, 
                  s.window = 35, s.degree = 1, 
                  sub.labels = substr(month.name, 1, 3))

plot(Air_stl, ylab = "Air Passenger Concentration (ppm)", 
     xlab = "Time (years)", font = 2, size = 2, lwd = 2,
     col = 'blue')
plot_seasonal(Air_stl, font = 2, size = 2, lwd = 2, col = 'blue', pch = 19)
plot_trend(Air_stl, font = 2, size = 2, lwd = 3, col = 'blue', pch = 19)
plot_cycle(Air_stl)
plot_rembycycle(Air_stl)

elecsales.ma<-ma(elecsales, order=5)
plot(elecsales, main="Residential electricity sales",ylab="GWh", xlab="Year", col="magenta", lwd=2)
lines(elecsales.ma, col="blue", lwd=2)

elecsales.ma3<-ma(elecsales, order=4)
elecsales.ma5<-ma(elecsales, order=6)
elecsales.ma7<-ma(elecsales, order=8)
elecsales.ma9<-ma(elecsales, order=10)

old.par <- par(mfrow=c(2,2), cex.lab = 1.75, cex.axis = 1.5)

plot(elecsales, main="Residential electricity sales",ylab="GWh", xlab="Year", col="blue", lwd=3)
lines(elecsales.ma3,col="red", lwd=3)
legend("bottomright","MA-3", pch = 1)
plot(elecsales, main="Residential electricity sales",ylab="GWh", xlab="Year", col="blue", lwd=3)
lines(elecsales.ma5,col="red", lwd=2)
legend("bottomright","MA-5", pch = 1)
plot(elecsales, main="Residential electricity sales",ylab="GWh", xlab="Year", col="blue", lwd=3)
lines(elecsales.ma7,col="red", lwd=2)
legend("bottomright","MA-7", pch = 1)
plot(elecsales, main="Residential electricity sales",ylab="GWh", xlab="Year", col="blue", lwd=3)
lines(elecsales.ma9,col="red", lwd=2)
legend("bottomright","MA-9", pch = 1)

elecsales2 <- window(elecsales,start=1989)
ma4 <- ma(elecsales2 , order=4, centre=FALSE)
ma4x4 <- ma(elecsales2 , order=4, centre=TRUE)
tselecsales<-array(c(elecsales,ma4,ma4x4),c(19,3))
tselecsales

par(mfrow=c(1, 1))

plot(tselecsales, main="Residential electricity sales",ylab="GWh", xlab="Year", col="blue", lwd=2)
lines(tselecsales,col="red", lwd=3)



data(elecequip)
elecequip

plot(elecequip, ylab="New orders index", col="dark gray", 
     lwd=3, main="Electrical equipment manufacturing (Euro area)")
lines(ma(elecequip, order=12), col="red", lwd=3)

beertimeseriescomponents <- decompose(beer2.ts)
par(cex.axis = 1.25)
plot(beertimeseriescomponents, col="darkorchid2", lwd=3, cex = 1.25)


plot.acf(beer.acf1)
title(main="Beer ACF using differencing = 2", cex.main=1.5, font.main=2, 
      col.main="navy", xlab = "Lag", ylab = "Correlation", col.lab ="darkorchid")
plot.acf(beer.pacf1)
title(main="Beer PACF using differencing = 2", cex.main=1.5,  font.main=2, 
      col.main="navy", xlab = "Lag", ylab = "Correlation", col.lab ="darkorchid")


beertimeseries <- ts(beersales, start=c(1991, 1), 
                     end=c(1995, 12), frequency=12)
beertimeseriescomponents <- decompose(beertimeseries)
beertimeseriesseasonallyadjusted <- beertimeseries + beertimeseriescomponents$seasonal
plot(beertimeseriesseasonallyadjusted, ylab = 'Adjusted Value', col=4, lwd=3, main="Beer Seasonally Adjusted")

library(TTR)
library(dplyr)
library(ggplot2)

data(ttrc)
TTRC<-ttrc["Volume"]

ema.20 <-   EMA(milk_mon_ts, 20)
sma.20 <-   SMA(milk_mon_ts, 20)
dema.20 <-  DEMA(milk_mon_ts, 20)
evwma.20 <- EVWMA(milk_mon_ts, 20)
zlema.20 <- ZLEMA(milk_mon_ts, 20)

plot(ema.20)

autoplot(milk_mon_ts) +
  geom_line(data=ema.20, series="EMAd", size=2, color="blue") +
  geom_line(data=sma.20, series="SMA", size=1.1, color="orange") + 
  geom_line(data=dema.20, series="DEMA", size=1.1, color="purple") + 
  geom_line(data=evwma.20, series="EVWMA", size=1.1, color="red") + 
  geom_line(data=zlema.20, series="ZLEMA", size=1.1, color="green") + 
  ggtitle("Forecasts from Holt-Winters Multiplicative Damped method") + xlab("Year") +
  ylab("Milk Production per Cow per month (kg)") +
  guides(colour=guide_legend(title="Monthly forecasts")) +
  theme(legend.position = "bottom") +
  theme_minimal()

library(tstools)
library(astsa)
list("EMA"=ema.20,"SMA"=sma.20,"DEMA"=dema.20,"EVWMA"=evwma.20)
col=c("magenta","green","orange","blue")
par(oma=c(.5,.5,.5,.5))
tt <- init_tsplot_theme(lwd=c(4,2,2,2), line_colors = c("red","purple","blue","green"))
par(mfrow=c(1,1), cex.main = 1,5, cex = 1.5, cex.axis = .8, mar=c(4,5,4,1))
tstools::tsplot(list("EMA"=ema.20,"SMA"=sma.20,"DEMA"=dema.20,"EVWMA"=evwma.20), auto_legend = T, theme = tt)
title(main = 'Milk ProductionMoving Averages')
legend('top', milk_mon_ts,
       c('EMA', 'SMA', 'DEMA', 'EVWMA'),
       fill = c('Red', 'purple', 'blue', 'green2'), cex = .75, box.lty = 0,  bg = 'snow', y.intersp = 1.5)

old.par<-par(mfrow=c(2,1))
plot(milk_mon_ts)
plot(ema.20)
plot(evwma.20)
plot(dema.20)

par(font=1, lwd = 2)
tstools::tsplot(ema.20,sma.20, dema.20,evwma.20)


monthOrder <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
milk_mon$month <- factor(format(milk_mon$month_date, "%b"), levels = monthOrder)
ggplot(df, aes(month, milk_ets_fc$residuals)) + geom_boxplot() + stat_boxplot(geom ='errorbar') + ggtitle("UKRPI Additive Deviation")

par(mfrow=c(1,1), mar=c(4,5,4,1))


p1<-boxplot(milk_mon_ts ~ cycle(milk_mon_ts), xlab = "Month", ylab = "ML", col="light blue", main = "Monthly Milk Production per Cow - Boxplot")

boxplot(milk_mon_ts ~ cycle(milk_mon_ts), aes(x = month_num, y = milk_prod_per_cow_kg, group = year))

plot(airpass, col="blue", lwd=2)

Z <- read.csv("D:\\Documents\\DATA\\pounds_nz.txt", header=T)
Z.ts<-ts(Z,st=1991,fr=4)
plot(Z.ts, col="purple", lwd=2, main="Pounds Random Walk")
acf(diff(Z.ts), col="red", lwd=2, main="First-Order Difference Random Walk")

