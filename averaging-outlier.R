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


### original accuracy outlier removed----
# ME     RMSE      MAE       MPE     MAPE      MASE        ACF1
# Training set 2648.622 24054.88 14584.08 -5.408762 22.51167 0.7638682 -0.04020516
# > castV <- forecast(fitV, h=12)
# > castV
# Point Forecast       Lo 80     Hi 80     Lo 95     Hi 95
# Jan 2016       28713.41  -2777.1847  60204.01 -19447.30  76874.13
# Feb 2016       33278.46  -3840.2473  70397.16 -23489.70  90046.62
# Mar 2016       22078.59 -17013.0592  61170.24 -37706.93  81864.11
# Apr 2016       40346.59    513.1238  80180.06 -20573.44 101266.62
# May 2016      349515.40 309396.7885 389634.02 288159.28 410871.53
# Jun 2016      733642.90 693413.8047 773872.00 672117.81 795168.00
# Jul 2016      934653.98 894381.9459 974926.02 873063.22 996244.75
# Aug 2016      821145.49 780856.7395 861434.23 759529.17 882761.81
# Sep 2016      633042.74 592747.4933 673338.00 571416.48 694669.01
# Oct 2016      214378.38 174080.5952 254676.16 152748.24 276008.52
# Nov 2016       11427.75 -28871.0211  51726.52 -50203.90  73059.40
# Dec 2016       19527.62 -20771.5361  59826.77 -42104.62  81159.85
# > accuracy(castV, testV)
# ME     RMSE      MAE       MPE     MAPE      MASE        ACF1 Theil's U
# Training set  2648.622 24054.88 14584.08 -5.408762 22.51167 0.7638682 -0.04020516        NA
# Test set     34618.807 50153.09 34618.81 14.030880 14.03088 1.8132242  0.47683322 0.2662011


### log accuracy outlier removed----
# ME      RMSE       MAE       MPE     MAPE      MASE        ACF1
# Training set 0.02455324 0.1900528 0.1209964 0.1754727 1.133322 0.8397706 -0.03268428
# > castLV <- forecast(fitLV, 12)
# > castLV
# Point Forecast     Lo 80     Hi 80     Lo 95     Hi 95
# Jan 2016      10.193354  9.943829 10.442879  9.811738 10.574970
# Feb 2016      10.321185 10.061145 10.581224  9.923489 10.718881
# Mar 2016       9.905457  9.644533 10.166382  9.506408 10.304507
# Apr 2016      10.354771 10.093770 10.615771  9.955605 10.753936
# May 2016      12.555348 12.294341 12.816355 12.156173 12.954524
# Jun 2016      13.423932 13.162924 13.684939 13.024755 13.823108
# Jul 2016      13.705902 13.444894 13.966910 13.306725 14.105079
# Aug 2016      13.579463 13.318455 13.840470 13.180286 13.978639
# Sep 2016      13.236929 12.975921 13.497936 12.837752 13.636106
# Oct 2016      11.941649 11.680641 12.202656 11.542472 12.340825
# Nov 2016       9.412005  9.150997  9.673013  9.012828  9.811182
# Dec 2016       9.808389  9.547381 10.069397  9.409212 10.207566
# > accuracy(castLV, testLV)
# ME      RMSE       MAE       MPE     MAPE      MASE        ACF1 Theil's U
# Training set 0.02455324 0.1900528 0.1209964 0.1754727 1.133322 0.8397706 -0.03268428        NA
# Test set     0.28162220 0.3527428 0.2816222 2.4574017 2.457402 1.9545869  0.12340362 0.3558942