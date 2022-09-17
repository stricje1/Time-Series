install.packages("rucm")
library(rucm)
library(forecast)

par()              # view current settings
opar <- par()      # make a copy of current settings
par(col.lab = 'dodgerblue3', font.lab = 2) # red x and y labels
                   # create a plot with these new settings
par(opar)          # restore original settings 

modelNile <- ucm(formula = Nile~0, data = Nile, level = TRUE, 
                 slope = TRUE , season = TRUE, season.length = 12)
modelNile #Printing method for class ucm

par(col.lab = 'dodgerblue3', font.lab = 2, cex.axis = 1.5, cex.lab = 1.5)
plot(Nile, col = 'navy', xlab = "Months", ylab= "Monthly Flow", 
     lwd = 3,  col.lab = 'dodgerblue3', cex.lab = 2,
     panel.first = grid(10, lty = 3, lwd = 2, col = 'lightblue')) 
title(main = "Nile overflow data 1871-1984", 
      cex.main = 2,   font.main= 4, col.main= "dodgerblue4",
      cex.sub = 0.75, font.sub = 3, col.sub = "dodgerblue2")
lines(modelNile$s.level, col = 'purple1', lwd=3)
legend(cex = 1.5, "bottomleft",legend=c("Observed flow","S_level"), 
       col= c('navy','purple1'), font = 2, lwd = 3, lty = 1,  
       x.intersp=2, y.intersp=2, bty='n')



modelNile <- ucm(formula = Nile~0, data = Nile, level = TRUE, slope = TRUE)
pred <- predict(modelNile$model, n.ahead = 12) # Forecasting

plot(modelNile$s.level, ylim = c(-100, 1100), lwd=3)
lines(modelNile$s.season, col = "blueviolet", lwd=3)
lines(modelNile$s.slope, col = "blue", lwd=3)
lines(modelNile$s.cycle, col = "violet", lwd=3)
plot(modelNile$s.level)
plot(modelNile$irr.var)
plot(modelNile$vs.level)
plot(modelNile$vs.slope)



fit <- StructTS(Nile, type = c("level", "trend", "BSM"))
fit

par()              
opar <- par()      
par(opar) 

library(ggthemes)
par(cex=1.5, cex.lab = 1.25, cex.axis = 1, lwd = 2)
par(col.lab="chocolate3")
par(col.axis="brown4")
par(bg="ivory")
par(fg="orange")
plot(Nile, col = "deepskyblue", lwd=3,
     panel.first = grid(10, lty = 3, lwd = 1, col = 'orange'))
lines(fitted(fit), col = "blueviolet", lwd=3) 
  title(main="Nile overflow data 1871-1984", cex.main=1.5, 
      font.main=2, col.main="goldenrod4", col.lab ="darkblue")
  labels=axis(1, seq(1870,1970,20))
  axis(1,at=seq(at.labels[1], at.labels[length(at.labels)],
                (at.labels[2]-at.labels[1])/4), labels=F )
par(opar) 

par(cex=2, lwd = 2)
par(col="blue")
par(pch=19)
par(lwd=1.5)
par(col.lab="darkred")
par(col.axis="darkred")
par(bg="white")
par(fg="red")
tsdiag(fit, lwd = 2)

dfx = data.frame(ev1=1:10, ev2=sample(10:99, 10), ev3=10:1)
with(dfx, symbols(x=ev1, y=ev2, circles=ev3, inches=1/3,
                  ann=F, bg="steelblue2", fg=NULL))

par(bg="white")
par(fg="darkblue")
par(opar) 


Nile.for<-forecast(fit, h=12)
plot(forecast::forecast(fit, level = c(50, 90), h = 10, 
              fan=TRUE, robust=TRUE), 
     xlim = c(1950, 1980), lwd=3, col="red2", main="",
     xlab="Year", ylab="Annual Flow")
lines(fit$fitted, lwd=3, col="blue2")
title(main="Flow of the Nile River", cex.main=1.5, 
      font.main=2, col.main="darkblue")
legend(cex = 1.25, "bottomleft",legend=c("Observed flow","fitted flow"), 
       col= c("red2","blue"), lty = 1, box.lwd = 2, lwd=2, 
       x.intersp=2, y.intersp=2, box.col = 'purple', bty='n',
       inset=.02)

par(mfrow = c(1, 1))  
plot(cbind(fitted(fit), resids=resid(fit)), main = "Nile Water Levels", lwd=2, col="orange3")

summary(Nile.for)

par(col="blue")
par(font.lab=2)
par(col.lab="darkblue")
par(col.axis="darkblue")
plot(Nile, type = "o", col='purple', xlab="Year", ylab="Annual Flow", ldw = 2)
lines(fitted(fit), lty = "dashed", col='red2',lwd = 3)
lines(tsSmooth(fit), lty = "dotted", col='blue',lwd = 3)
title(main="Flow of the Nile River", cex.main=1.5, 
      font.main=2, col.main="darkblue")
legend(CEX = 1.5, "bottomleft",legend=c("Fitted flow","tsSmooth", "Series"), 
       col= c('red2','blue','purple'), lty = c(2,3,1), box.lwd = 1, lwd=3, 
       x.intersp=1, y.intersp=2, box.col = 'purple', bty='n',
       inset=.02)


plot(mtcars, oma=c(2, 3, 5, 2))
mytitle = "I want main title NOT bold and left aligned"
mysubtitle = "Sub title should be under the main title left aligned"
mtext(side=3, line=3, at=-0.07, adj=0, cex=1, mytitle)
mtext(side=3, line=2, at=-0.07, adj=0, cex=0.7, mysubtitle)

data(nile)
