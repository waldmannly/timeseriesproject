# read in data ----
data<- read.csv("ysnp.csv")

# format data set
mat <- do.call(rbind, strsplit(as.character(data$Year.Month.Day), '/'))
day<-  as.numeric(mat[,3])
month<- as.numeric(mat[,2])
year<- as.numeric(mat[,1])
hour <- substring(data$Time, 1,2)
monthLetter <- c( "J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")
DATE<- as.POSIXlt(paste0( year,'/',month,'/', day, " ", "00",':', "00:00 EST") )
df <- data.frame( data$Recreation.Visits, data$Unemployment.Rate, data$MeanTemperature.F., data$Consumer.Price.Index , month, year, DATE, monthLet =monthLetter[month], fracTime= (year + month/12 + day/365)) 

library(TSA)
library(tseries)
library(forecast)
# data before log transform ----
visits <- (ts(df$data.Recreation.Visits[order(df$DATE)], frequency = 12, start = c(1986)))


png(filename="visits-ts-plot-outliers.png", 
    type="cairo",
    units="in", 
    width=8, 
    height=8, 
    pointsize=14, 
    res=200)
plot(visits)
dev.off()

visitcomponents <- decompose(visits)

png(filename="visits-components-ts-plot-outliers.png",   type="cairo",units="in",   width=8, height=8, pointsize=14, res=200)
plot(visitcomponents)
dev.off()

trainV <- head(visits, n= (length(visits) -12) )
testV <- tail(visits, n=12) 

trainVcomponents <- decompose(trainV)
png(filename="trainV-ts-plot-outliers.png",   type="cairo",units="in",   width=8, height=8, pointsize=14, res=200)  
plot(trainVcomponents)
dev.off()

adf.test(trainV) 
# Augmented Dickey-Fuller Test
# 
# data:  trainV
# Dickey-Fuller = -20.333, Lag order = 7, p-value = 0.01
# alternative hypothesis: stationary

auto.arima(trainV)

fitV <- Arima(trainV, order=c(1, 0, 0), seasonal = c(0,1,1), xreg=as.numeric(seq(trainV ==32))) 

#https://stats.stackexchange.com/questions/169564/arimax-prediction-using-forecast-package
# gotta do some shit probably. 


tf<-filter(1*(seq(1:(length(trainV)+5))==69),filter=0.5521330,method='recursive',side=1)*(-159917.76)
forecast.arima<-Arima(log(trainV),order=c(0,0,1),xreg=tf[1:(length(tf)-5)])
forecast.arima


accuracy(fitV)
castV <- forecast(fitV, h=12, 0)
castV
accuracy(castV, testV,newxreg=0)
png(filename="forecast95-fitV-plot-outliers.png",   type="cairo",units="in",   width=8, height=8, pointsize=14, res=200)  
plot(forecast(fitV, 12,level = .95,newxreg=0))
dev.off()

AR.mean.V <-forecast(fitV,h=12)$mean
png(filename="forecastMean-fitV-plot-outliers.png",   type="cairo",units="in",   width=8, height=8, pointsize=14, res=200)  
plot(testV, main="Log Visits", ylab="", xlab="Months", col="darkblue")  
lines(AR.mean.V, col="red")
dev.off()

# probably the accuracy plot we want 
png(filename="forecastGOOD-fitV-plot-outliers.png",   type="cairo",units="in",   width=8, height=8, pointsize=14, res=200)  
plot(castV,xlim=c(2014,2017), lwd=2)
lines(testV, col="red",lwd=2)
legend("topleft",lty=1,bty = "n",col=c("red","blue"),c("testData","ARIMAPred"))
dev.off()

png(filename="tsdiag-fitV-plot-outliers.png",   type="cairo",units="in",   width=8, height=8, pointsize=14, res=200)
tsdiag(fitV)
dev.off()

#residuals of just visits
png(filename="residualsStandard-fitV-plot-outliers.png",   type="cairo",units="in",   width=8, height=8, pointsize=14, res=200) 
tsdisplay(residuals(fitV))
dev.off()

#https://rpubs.com/RatherBit/90267
png(filename="residualsCustom-fitV-plot-outliers.png",   type="cairo",units="in",   width=8, height=8, pointsize=14, res=200) 
res.fr <- residuals(fitV)

par(mfrow=c(1,3))

plot(res.fr, main="Residuals from ARIMA method on visits",
     ylab="", xlab="Years")


Acf(res.fr, main="ACF of residuals")

u <- residuals(fitV)

m<-mean(u)
std<-sqrt(var(u))
hist(u, breaks=20, col="gray", prob=TRUE, 
     xlab="Residuals", main="Histogram of residuals\n with Normal Curve")
curve(dnorm(x, mean=m, sd=std), 
      col="black", lwd=2, add=TRUE)
par(mfrow=c(1,1))

dev.off()

# log transform  ---- 
logvisits <- log(visits)

logvisitscomponents <- decompose(logvisits)
png(filename="Logvisits-ts-plot-outliers.png",   type="cairo",units="in",   width=8, height=8, pointsize=14, res=200)  
plot(logvisitscomponents)
dev.off()

trainLV <- head(logvisits, n= (length(logvisits) -12) )
testLV <- tail(logvisits, n=12) 

trainLVcomponents <- decompose(trainLV)
png(filename="trainLV-components-ts-plot-outliers.png",   type="cairo",units="in",   width=8, height=8, pointsize=14, res=200)  
plot(trainLVcomponents)
dev.off()

adf.test(trainLV) 
# Augmented Dickey-Fuller Test
# 
# data:  trainLV
# Dickey-Fuller = -23.802, Lag order = 7, p-value = 0.01
# alternative hypothesis: stationary

auto.arima(trainLV)

#https://stats.stackexchange.com/questions/169564/arimax-prediction-using-forecast-package
# gotta do some shit probably. 

fitLV <- Arima(trainLV, order=c(0, 0, 1), seasonal = c(2,1,1), io=(32)) 


accuracy(fitLV)
castLV <- forecast(fitLV, 12)
castLV
accuracy(castLV, testLV)
png(filename="forecast95-fitLV-plot-outliers.png",   type="cairo",units="in",   width=8, height=8, pointsize=14, res=200)  
plot(forecast(fitLV, 12,level = .95))
dev.off()

AR.mean.LV <-forecast(fitLV,h=12)$mean
png(filename="forecastMean-fitLV-plot-outliers.png",   type="cairo",units="in",   width=8, height=8, pointsize=14, res=200)  
plot(testLV, main="Log Visits", ylab="", xlab="Months", col="darkblue")  
lines(AR.mean.LV, col="red")
dev.off()

# probably the accuracy plot we want 
png(filename="forecastGOOD-fitLV-plot-outliers.png",   type="cairo",units="in",   width=8, height=8, pointsize=14, res=200)  
plot(castLV,xlim=c(2014,2017), lwd=2)
lines(testLV, col="red",lwd=2)
legend("topleft",lty=1,bty = "n",col=c("red","blue"),c("testData","ARIMAPred"))
dev.off()

png(filename="tsdiag-fitLV-plot-outliers.png",   type="cairo",units="in",   width=8, height=8, pointsize=14, res=200)
tsdiag(fitLV)
dev.off()

#residuals of log visits 
png(filename="residualsStandard-fitLV-plot-outliers.png",   type="cairo",units="in",   width=8, height=8, pointsize=14, res=200) 
tsdisplay(residuals(fitLV))
dev.off()

#https://rpubs.com/RatherBit/90267
png(filename="residualsCustom-fitLV-plot-outliers.png",   type="cairo",units="in",   width=8, height=8, pointsize=14, res=200) 
res.fr <- residuals(fitLV)

par(mfrow=c(1,3))


plot(res.fr, main="Residuals from ARIMA method on log visits",
     ylab="", xlab="Years")

Acf(res.fr, main="ACF of residuals")

u <- residuals(fitLV)

m<-mean(u)
std<-sqrt(var(u))
hist(u, breaks=20, col="gray", prob=TRUE, 
     xlab="Residuals", main="Histogram of residuals\n with Normal Curve")
curve(dnorm(x, mean=m, sd=std), 
      col="black", lwd=2, add=TRUE)

dev.off()







# distribution plots over months 
png(filename="visits-distribution-plot-outliers.png",   type="cairo",units="in",   width=8, height=8, pointsize=14, res=200)
boxplot(split(visits, cycle(visits)), names = month.abb, col = "gold")
dev.off()

png(filename="logvisits-distribution-plot-outliers.png",   type="cairo",units="in",   width=8, height=8, pointsize=14, res=200)
boxplot(split(logvisits, cycle(logvisits)), names = month.abb, col = "gold")
dev.off()

png(filename="sqrtvisits-distribution-plot-outliers.png",   type="cairo",units="in",   width=8, height=8, pointsize=14, res=200)
boxplot(split(sqrt(visits), cycle(sqrt(visits))), names = month.abb, col = "gold")
dev.off()

#sqrt transformation??? 
sqrtvisitscomponents <- decompose(sqrt(visits))
plot(sqrtvisitscomponents)


