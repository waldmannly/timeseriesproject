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
visits <- sqrt(visits)
plot(visits)

visitcomponents <- decompose(visits)
plot(visitcomponents)

trainV <- head(visits, n= (length(visits) -12) )
testV <- tail(visits, n=12) 

trainVcomponents <- decompose(trainV)
plot(trainVcomponents)


#get rid of the 1987 and use the non log transform 


adf.test(trainV) 
# Augmented Dickey-Fuller Test
# 
# data:  trainV
# Dickey-Fuller = -20.333, Lag order = 7, p-value = 0.01
# alternative hypothesis: stationary

auto.arima(trainV)

fitV <- Arima(trainV, order=c(1, 0, 0), seasonal = c(0,1,1)) 

accuracy(fitV)
castV <- forecast(fitV, h=12)
castV
accuracy(castV, testV)
plot(forecast(fitV, 12,level = .95))

AR.mean.V <-forecast(fitV,h=12)$mean
plot(testV, main="Log Visits", ylab="", xlab="Months", col="darkblue")  
lines(AR.mean.V, col="red")

# probably the accuracy plot we want 
plot(castV,xlim=c(2014,2017), lwd=2)
lines(testV, col="red",lwd=2)
legend("topleft",lty=1,bty = "n",col=c("red","blue"),c("testData","ARIMAPred"))

tsdiag(fitV)

#residuals of just visits 
tsdisplay(residuals(fitV))

#https://rpubs.com/RatherBit/90267
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

