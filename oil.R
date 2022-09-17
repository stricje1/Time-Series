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