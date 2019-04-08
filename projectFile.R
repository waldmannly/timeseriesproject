# read in data
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
# data before log transform 
visits <- (ts(df$data.Recreation.Visits[order(df$DATE)], frequency = 12, start = c(1986)))
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
plot(forecast(fitV, 12,level = .95))

tsdiag(fitV)

# log transform 
logvisits <- log(visits)

logvisitscomponents <- decompose(logvisits)
plot(logvisitscomponents)


trainLV <- head(logvisits, n= (length(logvisits) -12) )
testLV <- tail(logvisits, n=12) 

trainLVcomponents <- decompose(trainLV)
plot(trainLVcomponents)

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
plot(forecast(fitLV, 12,level = .95))

tsdiag(fitLV)



