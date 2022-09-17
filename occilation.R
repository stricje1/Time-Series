library(astsa)
library(forecast)
library(ggplot2)
library(readr)
library(ocedata)
data(soi, package="astsa")
soi <- data(soi, package="astsa")
soi <- read_csv("D:/Documents/Data/soi.csv")
myvector<-soi
soi1.ts <- ts(myvector, start=c(1950, 1), end=c(1995, 12), 
              frequency=12) 
# subset the time series (Jan 1985 to December 1995)
soi2.ts <- window(myvector, start=c(1985, 1), end=c(1995, 12))


head(soi)
class(soi)
myvector=soi[,2:13]
class(myvector)
head(myvector)
soi.ts <- ts(myvector, start=c(1880, 1), end=c(2019, 12), frequency=12)
class(soi.ts)
head(soi.ts)
recent <- subset(soi, Year > 1950)

# save a numeric vector containing 48 monthly observations
# from Jan 1950 to Dec 1995 as a time series object
myvector<-soi
soi1.ts <- ts(myvector, start=c(1950, 1), end=c(1995, 12), frequency=12) 
# subset the time series (Jan 1985 to December 1995)
soi2.ts <- window(soi.ts, start=c(1985, 1), end=c(1995, 12)) 


###################

library(astsa)
library(forecast)
library(readr)
#data(soi, package="astsa")
soi <- read_csv("D:/Documents/Data/soi_new.csv")
# save a numeric vector containing 48 monthly observations
# from Jan 1950 to Dec 1995 as a time series object
myvector<-soi[,2]
soi1.ts <- ts(myvector, start=c(1950, 1), end=c(1995, 12), 
              frequency=12) 
# subset the time series (Jan 1985 to December 1995)
soi2.ts <- window(soi1.ts, start=c(1985, 1), end=c(1995, 12))
class(soi2.ts)
soi3.ts <- window(soi1.ts, start=c(1975, 1), end=c(1985, 12))
class(soi3.ts)

autoplot(soi2.ts) +
geom_line(series=soi2.ts, lwd=1.25, col="magenta4") +
ggtitle("Southern Oscillation Index (SOI)") + 
  xlab("Year") +
  ylab("Oscilation") + theme_01
  
theme_01 <- (theme(plot.title = element_text(size = rel(1.75))) +
  theme(axis.title.x = element_text(size = rel(1.5), color="blue")) +
  theme(axis.title.y = element_text(size = rel(1.5), color="blue3")) +
  theme(panel.background = element_rect(fill = "grey90", size=rel(1.25)),
        axis.text = element_text(colour = "purple", size = rel(1.2))) +
  theme(axis.line = element_line(size = 2, colour = "grey80")))

fit1 <- stl(soi2.ts, s.window="period")
autoplot(fit1) +
  geom_line(series=soi2.ts, lwd=1.5, col="magenta4") +
  ggtitle("Southern Oscillation Index (SOI)") +
  theme_01

library("RColorBrewer")
ggseasonplot(soi2.ts, 
             col=brewer.pal(n = 12, name = "Set3"), 
             year.labels = TRUE, year.labels.left = TRUE) + 
  ggtitle("Southern Oscillation Index (SOI) Monthly Values") +
  geom_line(lwd=1.25) + ylab("Oscillation Values") +
  theme(text=element_text(face="bold"), axis.text = element_text(colour = "dodgerblue3", size = rel(1.05)), 
    panel.background = element_rect(fill = 'royalblue2'),
    panel.grid = element_line(color="lightblue1", linetype=3)) +
  theme(axis.title.x = element_text(size = rel(1.25), color="navy"),
        axis.title.y = element_text(size = rel(1.25), color="navy")) 
  
  
axis.text = element_text(colour = "purple", size = rel(1.5))
theme_set(theme_gray(base_size = 12))


library("RColorBrewer")
library("ggplot2")
ggmonthplot(soi2.ts, 
            col=brewer.pal(n = 12, name = "Paired"), 
            year.labels=TRUE) + 
  geom_line(lwd=1.25, col="slateblue3") +
  ggtitle("Southern Oscillation Index (SOI) Monthly") +
  ylab("Oscillation Values") +
  theme_01 

fit1 <- hw(soi2.ts)
  # predict next 12 future values
For1 <- forecast(fit1, 12)
  
autoplot(fit1$x, size= 1, color= "purple") + 
  autolayer(For1, color="blue", 
            size = 1.2) +
  ggtitle("Southern Oscillation Index (SOI) Forecast Single Smoothing") +
  xlab("Year") +ylab("Oscillation Values") +
  theme(
    panel.background = element_rect(fill = "snow2"),
    plot.title = element_text(size = rel(1.5), color="navy", face='bold'),
    axis.title.x = element_text(size = rel(1.35), color="navy"),
    axis.title.y = element_text(size = rel(1.35), color="navy"),
    axis.text = element_text(colour = "red4", size = rel(1.25)),
    axis.line = element_line(size = 2, colour = "grey80")) +
  theme(legend.position="bottom", legend.text = element_text(size = rel(1.2))) + 
  guides(colour=guide_legend(title="LEGEND:", size = rel(1.52)))

    lines(fit1$x, lwd=1, col="magenta4") + 
    ggtitle("Southern Oscillation Index (SOI)") + 
    xlab("Year") +
    ylab("Oscilation") +
    theme(plot.title = element_text(size = rel(1.5))) +
    theme(axis.title.x = element_text(size = rel(1.25), color="blue")) +
    theme(axis.title.y = element_text(size = rel(1.25), color="blue3")) +
    theme(panel.background = element_rect(fill = "grey90", size=rel(1.2)),
          axis.text = element_text(colour = "purple", size = rel(1.05))) +
    theme(axis.line = element_line(size = 2, colour = 'grey80'))

# simple exponential - models level
fit2 <- HoltWinters(soi2.ts, beta=FALSE, gamma=FALSE)
# predict next 12 future values
For2 <- forecast(fit2, 12)
summary(For2)
autoplot(fit2$x, size= 1.25, color= "purple") + 
  autolayer(For1, color="blue", 
            size = 1.25) +
  ggtitle("Southern Oscillation Index (SOI) Forecast Single Smoothing") +
  xlab("Year") +ylab("Oscillation Values") +
  theme(
    panel.background = element_rect(fill = "snow2"),
    plot.title = element_text(size = rel(1.5), color="navy", face='bold'),
    axis.title.x = element_text(size = rel(1.35), color="navy"),
    axis.title.y = element_text(size = rel(1.35), color="navy"),
    axis.text = element_text(colour = "red4", size = rel(1.25)),
    axis.line = element_line(size = 2, colour = "grey80")) +
  theme(legend.position="bottom", legend.text = element_text(size = rel(1.2))) + 
    guides(colour=guide_legend(title="LEGEND:", size = rel(1.52)))
  

# double exponential - models level and trend
fit3 <- HoltWinters(soi2.ts, gamma=FALSE)
# predict next 12 future values
for3 <- forecast(fit3, 12)
summary(for3)
autoplot(fit3$x, size= 1.25, color= "purple") + 
  autolayer(for3, color="blue", 
            size = 1.1) +
  ggtitle("Southern Oscillation Index (SOI) Forecast Triple Smoothing") +
  xlab("Year") +ylab("Oscillation Values") +
  theme(
    panel.background = element_rect(fill = "snow2"),
    plot.title = element_text(size = rel(1.5), color="navy", face='bold'),
    axis.title.x = element_text(size = rel(1.35), color="navy"),
    axis.title.y = element_text(size = rel(1.35), color="navy"),
    axis.text = element_text(colour = "red4", size = rel(1.25)),
    axis.line = element_line(size = 2, colour = "grey80")) +
  theme(legend.position="bottom", legend.text = element_text(size = rel(1.2))) + 
  guides(colour=guide_legend(title="LEGEND:", size = rel(1.52)))

# triple exponential - models level, trend, and seasonal 
# components
fit3 <- HoltWinters(soi2.ts)
# predict next 12 future values
For3 <- forecast(fit3, 12)
plot(forecast(fit3, 12), xlab="Year", ylab="Oscillation", main="Southern Oscillation Index (SOI)")
lines(soi2.ts, col="magenta4", lwd=2)
autoplot(For2) +
  ggtitle("Southern Oscillation Index (SOI) Forecast Triple" ) +
  lines(fit2$x) +
  xlab("Year") +ylab("Oscillation Values") +
  theme(line=element_line(color = 'msgenta4', size = 1.5),
        plot.title = element_text(size = rel(1.25)),
        panel.background = element_rect(fill = "snow2"),
        axis.title.x = element_text(size = rel(1.2), color="navy"),
        axis.text = element_text(colour = "red4", size = rel(0.75)),
        axis.line = element_line(size = 2, colour = "grey80"))

# predictive accuracy for single exponential smoothing
accuracy(forecast(fit1, 12))
# predictive accuracy for double exponential smoothing
accuracy(forecast(fit2, 12))
#predictive accuracy for triple exponential smoothing
accuracy(forecast(fit3, 12))



milk_mon <- read.csv("D:\\Documents\\DATA\\milk_monthly.txt")
milk_day <- read.csv("D:\\Documents\\DATA\\milk_daily.txt")

head(milk_mon)
class(milk_mon)
class(milk_mon$month)
head(milk_mon)
class(milk_mon)
class(milk_mon$month)
milk_mon$month_date <- as.Date(milk_mon$month, format = "%Y-%m-%d")
class(milk_mon$month_date)
class(format(milk_mon$month_date, format = "%Y-%B-%u"))  
format(milk_mon$month_date, format = "%Y-%B-%u")
class(milk_day$date_time)
milk_day$date_time_posix <- as.POSIXct(milk_day$date_time, 
                                       format = "%Y-%m-%d %H:%M:%S")
class(milk_day$date_time_posix)
milk_mon$bad_date <- format(milk_mon$month_date, 
                            format = "%d/%b/%Y-%u")
head(milk_mon$bad_date)  
class(milk_mon$bad_date)  
milk_mon$good_date <- as.Date(milk_mon$bad_date, 
                              format = "%d/%b/%Y-%u")
head(milk_mon$good_date)
class(milk_mon$good_date)
head(milk_mon)
autoplot(milk.ts)

milk.ts <- ts(milk_mon$milk_prod, start = 1962, end = 1975, freq = 12)  # Specify start and end year, measurement frequency (monthly = 12)
f1<-hw(milk.ts, seasonal="additive")
f2<-hw(milk.ts, seasonal="multiplicative")
f3<-hw(milk.ts, damped=TRUE, seasonal="multiplicative")
f4<-hw(milk.ts, damped=TRUE, seasonal="additive")
f5<-holt(milk.ts,h=24, damped=TRUE, exponential=TRUE)
f6<-holt(milk.ts,h=24, initial="optimal")


autoplot(milk.ts) +
  autolayer(milk.ts, color="blue", size=1.25) +
  autolayer(f1, series="HW Additive", PI=FALSE, size=1.25) +
  autolayer(f2, series="HW Multiplicative", PI=FALSE, 
            size=1.25) +
  autolayer(f3, series="HW Multiplicative damped", PI=FALSE,
            size=1.25) +
  autolayer(f4,series="HW Additive damped", PI=FALSE, 
            size=1.25) +
  autolayer(f5,series="Holt damped", PI=FALSE, size=1.25) +
  autolayer(f6,series="Holt", PI=FALSE, size=1.25) +
  xlab("Year") + 
  ylab("Unemployment Index") +
  ggtitle("Consumer Confidence Index (CCI)") +
  guides(colour=guide_legend(title="LEGEND")) +
  theme(legend.position="bottom")


