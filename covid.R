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
library(raster)
library(Hmisc)
library(zoo)
library(xts)


################################################################

library(tsbox)
dta <- as.data.frame(read_csv("D:/Documents/Data/covid_co.csv"))
dta_death<-dta[,c(1, 6)]
covid_co=dta[1,5,6] 
death.xts <- tsbox::ts_xts(dta_death)

new_death_03da <- zoo::rollmean(death.xts, k = 3, fill = NA)
new_death_05da <- zoo::rollmean(death.xts, k = 5, fill = NA)
new_death_07da <- zoo::rollmean(death.xts, k = 7, fill = NA)
new_death_14da <- zoo::rollmean(death.xts, k = 14, fill = NA)
new_death_21da <- zoo::rollmean(death.xts, k = 21, fill = NA)
new_death_30da <- zoo::rollmean(death.xts, k = 31, fill = NA)

cum_death<-dta[,c(1, 5)]
cum_death.xts <- tsbox::ts_xts(cum_death)

cum_death_03da <- zoo::rollmean(cum_death.xts, k = 3, fill = NA)
cum_death_05da <- zoo::rollmean(cum_death.xts, k = 5, fill = NA)
cum_death_07da <- zoo::rollmean(cum_death.xts, k = 7, fill = NA)
cum_death_14da <- zoo::rollmean(cum_death.xts, k = 14, fill = NA)
cum_death_21da <- zoo::rollmean(cum_death.xts, k = 21, fill = NA)
cum_death_30da <- zoo::rollmean(cum_death.xts, k = 31, fill = NA)



library(PerformanceAnalytics)
par(bg="snow")
chart.ACFplus(new_death_21da, maxlag = 21, elementcolor = "red3", 
              main = "COVID-19 Daily New Deaths", cex.main = 1.5,
              col.main = "slateblue3", xlab = "lags", cex.lab = 1.25,
              col.lab = "red3", col.axis ="violetred", cex.axis = 1.25)


par(oma = c(.15, .15, .15, .15))
op <- options(tsbox.lwd=3, tsbox.col="blue1")
ts_plot(ts_trend(death.xts), title = "Daily Trend for New Deaths From COVID-19") 


ts_plot("New Deaths" = ts_scale(death.xts))
ts_plot(ts_lag(death.xts,7))

library(plotfunctions)
xtick<-seq(0, 10, by=5)

par(oma = c(3, .15, .15, .15))
plot(cum_death.xts, lwd=3, col="blue3", main="COVID-19 Cummulative and Daily New Deaths - Daily", cex=.85, 
     ylab="Number of Deaths", xlab = "Date", x) 
  lines(death.xts, lwd=4, col="red2")
  legend_margin('top', legend=c(''), pch=0, box.lty=0)
    legend("center", deaths.xts,
         c("Cummulative Deaths", "Daily New Deaths"),
         fill=c("blue2", "red2"),cex=1, box.lty=0)
    
    
# Create some mock data
par(mar = c(5, 5, 3, 5))
plot(dta[1:225, 5], type ="l", ylab = "Cummulative Deaths",
     main = "COVID-19 Cummulative and Daily New Deaths - Daily", xlab = "Date",
     col = "blue")
par(new = TRUE)
plot(dta[1:225, 7], type = "l", xaxt = "n", yaxt = "n",
      ylab = "", xlab = "", col = "red", lty = 2)
#axis(side = 4)
mtext("New Deaths 21-Day MA", side = 4, line = 3)
legend("topleft", c("Cummulative Deaths", "New Deaths 21-Day MA"),
        col = c("blue", "red"), lty = c(1, 2))



plot(dta[1:225, 5])
par(new = TRUE)
plot(dta[1:225, 7])





par(cex=2)
TSstudio::ts_plot(death.xts, line.mode = "lines", width = 2, dash = NULL,
          color = "green2", slider = FALSE, type = "single", Xtitle = "Days",
          Ytitle = "New Deaths", title = "COVID-19 New Deaths - Daily", Xgrid = TRUE, Ygrid = TRUE)
par(mfrow=c(1,1))  
op <- options(
  tsbox.lwd = 2,
  tsbox.col = c("blue2", "red2", "orange2"),
  tsbox.lty = "solid"
  ) 
par(cex=1.25)
ts_plot(
    'New Deaths' = death.xts,
    'New Deaths 7-Day Moving Average' = new_death_07da,
    'New Deaths 21-day Moving Average' = new_death_21da,
    title = "New Deaths From COVID-19"
  )
theme(legend.position = "right")

legend("right","Legend", pch = 1)
op
    

theme(plot.title = element_text("COVID-19 New Deaths - Daily", size = rel(1.5))) +
  theme(axis.title.x = element_text(size = rel(1.25), color="blue")) +
  theme(axis.title.y = element_text(size = rel(1.25), color="blue3")) +
  theme(panel.background = element_rect(fill = "grey90", size=rel(1.2)),
        axis.text = element_text(colour = "purple", size = rel(1.05))) +
  theme(axis.line = element_line(size = 2, colour = "grey80"))


dta_cases<-dta[,c(1,3)]
cases.xts <- tsbox::ts_xts(dta_cases)

new_case_03da <- zoo::rollmean(cases.xts, k = 3, fill = NA)
new_case_05da <- zoo::rollmean(cases.xts, k = 5, fill = NA)
new_case_07da <- zoo::rollmean(cases.xts, k = 7, fill = NA)
new_case_14da <- zoo::rollmean(cases.xts, k = 14, fill = NA)
new_case_21da <- zoo::rollmean(cases.xts, k = 21, fill = NA)
new_case_30da <- zoo::rollmean(cases.xts, k = 31, fill = NA)

summary(new_case_21da)

library(plotfunctions)

par(bg="snow", cex = 1.5, oma=c(.75,1,.5,1))
chart.ACFplus(new_case_21da, maxlag = 21, elementcolor = "red3",
              main = "COVID-19 Daily New Cases", cex.main = 1.55,
              col.main = "slateblue3", xlab = "lags", cex.lab = 1.25,
              col.lab = "red3", col.axis ="violetred")

par(mar = c(7, 4, 1, 3))
plot(cases.xts, lwd = 2, col = 'blue3',  main = 'COVID-19 New Cases vs Deaths - Daily', cex = .85, 
     ylab = 'Number of Cases vs Deaths', las = 2)
lines(new_case_21da, lwd = 3, col = 'orange2')
lines(new_death_21da, lwd = 4, col = 'red3')
legend_margin('top', legend = c(''), pch = 0, box.lty = 0)
legend('center', cases.xts,
       c('New cases', 'New Cases 21-Day MA', 'new Deaths 21-Day MA'),
       fill = c('blue2', 'orange', 'red2'), cex = 1, box.lty = 0,  bg = 'snow', y.intersp = 1.5)

legend(2011, 0.45, 
       c('New cases", "New Cases 21-Day MA", "new Deaths 21-Day MA'),
       col=c('blue2', 'orange', 'red2'), 
       lwd = 3, lty=1, cex=1.2, bty="n", y.intersp=1.5)


plot(cases.xts, lwd=2, col="blue3",  main="COVID-19 New Cases vs Deaths - Daily", cex=.85, 
     ylab="Number of Cases vs Deaths",las=2)
lines(cummulative_21da, lwd=3, col="orange2")
lines(new_death_21da, lwd=4, col="red3")
legend_margin('top', legend=c(''), pch=0, box.lty=0)
legend("center", cases.xts,
       c("New cases", "New Cases 21-Day MA", "new Deaths 21-Day MA"),
       fill=c("blue2", "orange", "red2"),cex=1, box.lty=0)

dta <- as.data.frame(read_csv("D:/Documents/Data/covid_co.csv"))
dta_conf_death<-dta[,c(1,5)]
conf_death.xts <- tsbox::ts_xts(dta_conf_death)

conf_death_03da <- zoo::rollmean(conf_death.xts, k = 3, fill = NA)
conf_death_05da <- zoo::rollmean(conf_death.xts, k = 5, fill = NA)
conf_death_07da <- zoo::rollmean(conf_death.xts, k = 7, fill = NA)
conf_death_14da <- zoo::rollmean(conf_death.xts, k = 14, fill = NA)
conf_death_21da <- zoo::rollmean(conf_death.xts, k = 21, fill = NA)

summary(conf_death_21da)
dta_conf_death %>% dplyr::select("date", "confirmed_deaths")


par(oma = c(.15, .15, .15, .15))
op <- options(tsbox.lwd=3, tsbox.col="blue1")
plot(conf_death.xts)
segments(x1, y1, x2, y2, col= 'blue')
lines(ts_trend(conf_death.xts),type='l')

s <- seq(length(conf_death.xts)-1)

x <- 223
y <- 2222
slope <- diff(y)/diff(x)
intercept <- y[1]-slope*x[1]
plot(x, y)
abline(intercept, slope, col="red")

ts_plot("New Deaths" = ts_scale(conf_death.xts))
ts_plot(ts_lag(death.xts,7))

par(oma = c(3, .15, .15, .15))
plot(conf_death.xts, lwd=3, col="blue3", main="COVID-19 Confirmed Deaths - Cumulative", cex=.85, 
     ylab="Deaths",las=2) 
#lines(conf_death_07da, lwd=4, col="red2") 
lines(conf_death_30da, lwd=4, col="green3")
legend_margin('top', legend=c(''), pch=0, box.lty=0)
legend("right", deaths.xts,
       c("Confirmed Deaths", "30-Day MA"),
       fill=c("blue2", "green2"),cex=.85, box.lty=0)

x1 <- 0
x2 <- 223
y1 <- 223
y2 <- 2222
plot(c(x1,223), c(x2,2222))
segments(x1, y1, x2, y2, col= 'blue')

plot(conf_death.xts$confirmed_deaths)
segments(x1, y1, x2, y2, col= 'blue')
2222/23
      
