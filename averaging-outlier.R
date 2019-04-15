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


newtotal <- df$data.Recreation.Visits[df$year == 1986]+df$data.Recreation.Visits[df$year == 1987]+df$data.Recreation.Visits[df$year == 1988]+df$data.Recreation.Visits[df$year == 1989]+ df$data.Recreation.Visits[df$year == 1990]
avgover5years <- floor(newtotal/5 )

df$data.Recreation.Visits[df$year == 1988] <- avgover5years

library(TSA)
library(tseries)
library(forecast)
# data before log transform ----
visits <- (ts(df$data.Recreation.Visits[order(df$DATE)], frequency = 12, start = c(1986)))


png(filename="outlier/visits-ts-plot-outlier.png", 
    type="cairo",
    units="in", 
    width=8, 
    height=8, 
    pointsize=14, 
    res=200)
plot(visits)
dev.off()

visitcomponents <- decompose(visits)

png(filename="outlier/visits-components-ts-plot-outlier.png",   type="cairo",units="in",   width=8, height=8, pointsize=14, res=200)
plot(visitcomponents)
dev.off()

trainV <- head(visits, n= (length(visits) -12) )
testV <- tail(visits, n=12) 

trainVcomponents <- decompose(trainV)
png(filename="outlier/trainV-ts-plot-outlier.png",   type="cairo",units="in",   width=8, height=8, pointsize=14, res=200)  
plot(trainVcomponents)
dev.off()

adf.test(trainV) 
# Augmented Dickey-Fuller Test
# 
# data:  trainV
# Dickey-Fuller = -20.333, Lag order = 7, p-value = 0.01
# alternative hypothesis: stationary

auto.arima(trainV)

# Series: trainV 
# ARIMA(1,0,0)(1,1,1)[12] with drift 
# 
# Coefficients:
#   ar1    sar1     sma1     drift
# 0.6049  0.1837  -0.5640  330.0516
# s.e.  0.0431  0.1310   0.1133  152.5286
# 
# sigma^2 estimated as 597765731:  log likelihood=-4009.51
# AIC=8029.02   AICc=8029.2   BIC=8048.28

fitV <- Arima(trainV, order=c(1, 0, 0), seasonal = c(1,1,1)) 

accuracy(fitV)
castV <- forecast(fitV, h=12)
castV
accuracy(castV, testV)
png(filename="outlier/forecast95-fitV-plot-outlier.png",   type="cairo",units="in",   width=8, height=8, pointsize=14, res=200)  
plot(forecast(fitV, 12,level = .95))
dev.off()

AR.mean.V <-forecast(fitV,h=12)$mean
png(filename="outlier/forecastMean-fitV-plot-outlier.png",   type="cairo",units="in",   width=8, height=8, pointsize=14, res=200)  
plot(testV, main="Visits", ylab="", xlab="Months", col="darkblue")  
lines(AR.mean.V, col="red")
dev.off()

# probably the accuracy plot we want 
png(filename="outlier/forecastGOOD-fitV-plot-outlier.png",   type="cairo",units="in",   width=8, height=8, pointsize=14, res=200)  
plot(castV,xlim=c(2014,2017), lwd=2)
lines(testV, col="red",lwd=2)
legend("topleft",lty=1,bty = "n",col=c("red","blue"),c("testData","ARIMAPred"))
dev.off()

png(filename="outlier/tsdiag-fitV-plot-outlier.png",   type="cairo",units="in",   width=8, height=8, pointsize=14, res=200)
tsdiag(fitV)
dev.off()

#residuals of just visits
png(filename="outlier/residualsStandard-fitV-plot-outlier.png",   type="cairo",units="in",   width=8, height=8, pointsize=14, res=200) 
tsdisplay(residuals(fitV))
dev.off()

#https://rpubs.com/RatherBit/90267
png(filename="outlier/residualsCustom-fitV-plot-outlier.png",   type="cairo",units="in",   width=8, height=8, pointsize=14, res=200) 
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
png(filename="outlier/Logvisits-ts-plot-outlier.png",   type="cairo",units="in",   width=8, height=8, pointsize=14, res=200)  
plot(logvisitscomponents)
dev.off()

trainLV <- head(logvisits, n= (length(logvisits) -12) )
testLV <- tail(logvisits, n=12) 

trainLVcomponents <- decompose(trainLV)
png(filename="outlier/trainLV-components-ts-plot-outlier.png",   type="cairo",units="in",   width=8, height=8, pointsize=14, res=200)  
plot(trainLVcomponents)
dev.off()

adf.test(trainLV) 
# Augmented Dickey-Fuller Test
# 
# data:  trainLV
# Dickey-Fuller = -23.802, Lag order = 7, p-value = 0.01
# alternative hypothesis: stationary

auto.arima(trainLV)
# Series: trainLV 
# ARIMA(1,0,0)(2,1,2)[12] with drift 
# 
# Coefficients:
#   ar1    sar1     sar2     sma1    sma2  drift
# 0.2796  0.5664  -0.1298  -1.3482  0.4921  7e-04
# s.e.  0.0558  0.2434   0.1135   0.2294  0.2232  4e-04
# 
# sigma^2 estimated as 0.03753:  log likelihood=74.35
# AIC=-134.69   AICc=-134.36   BIC=-107.73

fitLV <- Arima(trainLV, order=c(1, 0, 0), seasonal = c(2,1,2)) 

accuracy(fitLV)
castLV <- forecast(fitLV, 12)
castLV
accuracy(castLV, testLV)
png(filename="outlier/forecast95-fitLV-plot-outlier.png",   type="cairo",units="in",   width=8, height=8, pointsize=14, res=200)  
plot(forecast(fitLV, 12,level = .95))
dev.off()

AR.mean.LV <-forecast(fitLV,h=12)$mean
png(filename="outlier/forecastMean-fitLV-plot-outlier.png",   type="cairo",units="in",   width=8, height=8, pointsize=14, res=200)  
plot(testLV, main="Log Visits", ylab="", xlab="Months", col="darkblue")  
lines(AR.mean.LV, col="red")
dev.off()

# probably the accuracy plot we want 
png(filename="outlier/forecastGOOD-fitLV-plot-outlier.png",   type="cairo",units="in",   width=8, height=8, pointsize=14, res=200)  
plot(castLV,xlim=c(2014,2017), lwd=2)
lines(testLV, col="red",lwd=2)
legend("topleft",lty=1,bty = "n",col=c("red","blue"),c("testData","ARIMAPred"))
dev.off()

png(filename="outlier/tsdiag-fitLV-plot-outlier.png",   type="cairo",units="in",   width=8, height=8, pointsize=14, res=200)
tsdiag(fitLV)
dev.off()

#residuals of log visits 
png(filename="outlier/residualsStandard-fitLV-plot-outlier.png",   type="cairo",units="in",   width=8, height=8, pointsize=14, res=200) 
tsdisplay(residuals(fitLV))
dev.off()

#https://rpubs.com/RatherBit/90267
png(filename="outlier/residualsCustom-fitLV-plot-outlier.png",   type="cairo",units="in",   width=8, height=8, pointsize=14, res=200) 
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



