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


png(filename="normalplots/visits-ts-plot.png", 
    type="cairo",
    units="in", 
    width=8, 
    height=8, 
    pointsize=14, 
    res=200)
plot(visits)
dev.off()

visitcomponents <- decompose(visits)

png(filename="normalplots/visits-components-ts-plot.png",   type="cairo",units="in",   width=8, height=8, pointsize=14, res=200)
plot(visitcomponents)
dev.off()

trainV <- head(visits, n= (length(visits) -12) )
testV <- tail(visits, n=12) 

trainVcomponents <- decompose(trainV)
png(filename="normalplots/trainV-ts-plot.png",   type="cairo",units="in",   width=8, height=8, pointsize=14, res=200)  
plot(trainVcomponents)
dev.off()

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
png(filename="normalplots/forecast95-fitV-plot.png",   type="cairo",units="in",   width=8, height=8, pointsize=14, res=200)  
plot(forecast(fitV, 12,level = .95))
dev.off()

AR.mean.V <-forecast(fitV,h=12)$mean
png(filename="normalplots/forecastMean-fitV-plot.png",   type="cairo",units="in",   width=8, height=8, pointsize=14, res=200)  
plot(testV, main="Visits", ylab="", xlab="Months", col="darkblue")  
lines(AR.mean.V, col="red")
dev.off()

# probably the accuracy plot we want 
png(filename="normalplots/forecastGOOD-fitV-plot.png",   type="cairo",units="in",   width=8, height=8, pointsize=14, res=200)  
plot(castV,xlim=c(2014,2017), lwd=2)
lines(testV, col="red",lwd=2)
legend("topleft",lty=1,bty = "n",col=c("red","blue"),c("testData","ARIMAPred"))
dev.off()

png(filename="normalplots/tsdiag-fitV-plot.png",   type="cairo",units="in",   width=8, height=8, pointsize=14, res=200)
tsdiag(fitV)
dev.off()

#residuals of just visits
png(filename="normalplots/residualsStandard-fitV-plot.png",   type="cairo",units="in",   width=8, height=8, pointsize=14, res=200) 
tsdisplay(residuals(fitV))
dev.off()

#https://rpubs.com/RatherBit/90267
png(filename="normalplots/residualsCustom-fitV-plot.png",   type="cairo",units="in",   width=8, height=8, pointsize=14, res=200) 
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
png(filename="normalplots/Logvisits-ts-plot.png",   type="cairo",units="in",   width=8, height=8, pointsize=14, res=200)  
plot(logvisitscomponents)
dev.off()

trainLV <- head(logvisits, n= (length(logvisits) -12) )
testLV <- tail(logvisits, n=12) 

trainLVcomponents <- decompose(trainLV)
png(filename="normalplots/trainLV-components-ts-plot.png",   type="cairo",units="in",   width=8, height=8, pointsize=14, res=200)  
plot(trainLVcomponents)
dev.off()

adf.test(trainLV) 
# Augmented Dickey-Fuller Test
# 
# data:  trainLV
# Dickey-Fuller = -23.802, Lag order = 7, p-value = 0.01
# alternative hypothesis: stationary

auto.arima(trainLV)

fitLV <- Arima(trainLV, order=c(0, 0, 1), seasonal = c(2,1,1)) 

accuracy(fitLV)
castLV <- forecast(fitLV, 12)
castLV
accuracy(castLV, testLV)
png(filename="normalplots/forecast95-fitLV-plot.png",   type="cairo",units="in",   width=8, height=8, pointsize=14, res=200)  
plot(forecast(fitLV, 12,level = .95))
dev.off()

AR.mean.LV <-forecast(fitLV,h=12)$mean
png(filename="normalplots/forecastMean-fitLV-plot.png",   type="cairo",units="in",   width=8, height=8, pointsize=14, res=200)  
plot(testLV, main="Log Visits", ylab="", xlab="Months", col="darkblue")  
lines(AR.mean.LV, col="red")
dev.off()

# probably the accuracy plot we want 
png(filename="normalplots/forecastGOOD-fitLV-plot.png",   type="cairo",units="in",   width=8, height=8, pointsize=14, res=200)  
plot(castLV,xlim=c(2014,2017), lwd=2)
lines(testLV, col="red",lwd=2)
legend("topleft",lty=1,bty = "n",col=c("red","blue"),c("testData","ARIMAPred"))
dev.off()

png(filename="normalplots/tsdiag-fitLV-plot.png",   type="cairo",units="in",   width=8, height=8, pointsize=14, res=200)
tsdiag(fitLV)
dev.off()

#residuals of log visits 
png(filename="normalplots/residualsStandard-fitLV-plot.png",   type="cairo",units="in",   width=8, height=8, pointsize=14, res=200) 
tsdisplay(residuals(fitLV))
dev.off()

#https://rpubs.com/RatherBit/90267
png(filename="normalplots/residualsCustom-fitLV-plot.png",   type="cairo",units="in",   width=8, height=8, pointsize=14, res=200) 
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
png(filename="normalplots/visits-distribution-plot.png",   type="cairo",units="in",   width=8, height=8, pointsize=14, res=200)
boxplot(split(visits, cycle(visits)), names = month.abb, col = "gold")
dev.off()

png(filename="normalplots/logvisits-distribution-plot.png",   type="cairo",units="in",   width=8, height=8, pointsize=14, res=200)
boxplot(split(logvisits, cycle(logvisits)), names = month.abb, col = "gold")
dev.off()

png(filename="normalplots/sqrtvisits-distribution-plot.png",   type="cairo",units="in",   width=8, height=8, pointsize=14, res=200)
boxplot(split(sqrt(visits), cycle(sqrt(visits))), names = month.abb, col = "gold")
dev.off()

#sqrt transformation??? 
sqrtvisitscomponents <- decompose(sqrt(visits))
plot(sqrtvisitscomponents)



### original accuracy normal--- 
# Series: trainV 
# ARIMA(1,0,0)(0,1,1)[12] with drift 
# 
# Coefficients:
#   ar1     sma1     drift
# 0.5951  -0.5113  332.4268
# s.e.  0.0433   0.0525  160.6342
# 
# sigma^2 estimated as 835467736:  log likelihood=-4068.89
# AIC=8145.78   AICc=8145.89   BIC=8161.19
# 
# > accuracy(fitV)
#                   ME     RMSE      MAE       MPE     MAPE      MASE       ACF1
# Training set 2956.927 28466.38 16547.06 -4.970651 22.79798 0.7689806 0.02234708
# > castV <- forecast(fitV, h=12)
# > castV
# Point Forecast      Lo 80     Hi 80     Lo 95     Hi 95
# Jan 2016       28475.57  -8736.309  65687.45 -28435.09  85386.22
# Feb 2016       32595.40 -11031.810  76222.60 -34126.66  99317.45
# Mar 2016       21556.76 -24242.409  67355.92 -48487.02  91600.53
# Apr 2016       38174.21  -8412.340  84760.77 -33073.77 109422.20
# May 2016      338444.02 291565.977 385322.07 266750.24 410137.80
# Jun 2016      718288.69 671301.939 765275.44 646428.66 790148.72
# Jul 2016      918758.75 871731.357 965786.15 846836.56 990680.95
# Aug 2016      809912.82 762870.214 856955.44 737967.36 881858.29
# Sep 2016      618407.01 571358.701 665455.32 546452.83 690361.19
# Oct 2016      202270.45 155220.012 249320.89 130313.01 274227.89
# Nov 2016       11515.51 -35535.726  58566.75 -60443.15  83474.17
# Dec 2016       19221.46 -27830.076  66273.00 -52737.66  91180.58
# > accuracy(castV, testV)
#                     ME     RMSE      MAE       MPE     MAPE      MASE       ACF1 Theil's U
# Training set  2956.927 28466.38 16547.06 -4.970651 22.79798 0.7689806 0.02234708        NA
# Test set     41629.695 58997.57 41629.69 16.042694 16.04269 1.9346297 0.51035213 0.2975208


### log accuracy normal----
# Series: trainLV 
# ARIMA(0,0,1)(2,1,1)[12] with drift 
# 
# Coefficients:
#   ma1    sar1     sar2     sma1  drift
# 0.2582  0.0308  -0.0577  -0.8064  6e-04
# s.e.  0.0508  0.0808   0.0727   0.0633  3e-04
# 
# sigma^2 estimated as 0.04128:  log likelihood=56.83
# AIC=-101.66   AICc=-101.41   BIC=-78.55
# 
# > accuracy(fitLV)
#                     ME      RMSE       MAE       MPE     MAPE      MASE          ACF1
# Training set 0.03070647 0.2000634 0.1322511 0.2220471 1.226473 0.8639502 -0.0007861004
# > castLV <- forecast(fitLV, 12)
# > castLV
# Point Forecast     Lo 80     Hi 80     Lo 95    Hi 95
# Jan 2016      10.231217  9.968931 10.493504  9.830085 10.63235
# Feb 2016      10.348323 10.077007 10.619639  9.933381 10.76326
# Mar 2016       9.906186  9.634870 10.177501  9.491244 10.32113
# Apr 2016      10.350557 10.079242 10.621873  9.935616 10.76550
# May 2016      12.548401 12.277085 12.819716 12.133459 12.96334
# Jun 2016      13.405033 13.133717 13.676348 12.990091 13.81997
# Jul 2016      13.686124 13.414808 13.957439 13.271182 14.10107
# Aug 2016      13.558471 13.287155 13.829787 13.143530 13.97341
# Sep 2016      13.206680 12.935365 13.477996 12.791739 13.62162
# Oct 2016      11.929529 11.658213 12.200845 11.514588 12.34447
# Nov 2016       9.398428  9.127112  9.669744  8.983486  9.81337
# Dec 2016       9.822305  9.550989 10.093621  9.407363 10.23725
# > accuracy(castLV, testLV)
#                     ME      RMSE       MAE       MPE     MAPE      MASE          ACF1 Theil's U
# Training set 0.03070647 0.2000634 0.1322511 0.2220471 1.226473 0.8639502 -0.0007861004        NA
# Test set     0.28554966 0.3589114 0.2855497 2.4749930 2.474993 1.8653962  0.1195574051 0.3615089