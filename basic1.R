# fire data probably isnt a good choice. The graph seems too random. 
fire <- read.csv("number-of-acres-burned-in-forest.csv")

plot(fire)
head(fire)
colnames(fire) <- c("Year", "AcresBurned")
head(fire)

as.POSIXct(fire$Year)
tail(fire)
fire <- fire[c(-72), ]
DATE<- as.POSIXlt(paste0( fire$Year,'/',01,'/', 1, " ", 0,':', "00:00 EST") )

monthLetter <- c("J", "J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")

dat <- cbind(fire, DATE)

dat$Year<- NULL
plot(dat)


# air quality data 
data<- read.csv("AirQualityUCI.csv")
head(data)
tail(data)

mat <- do.call(rbind, strsplit(as.character(data$Date), '/'))
day<-  as.numeric(mat[,1])
month<- as.numeric(mat[,2])
year<- as.numeric(mat[,3])
hour <- substring(data$Time, 1,2)

DATE<- as.POSIXlt(paste0( year,'/',month,'/', day, " ", hour,':', "00:00 EST") )

plot(DATE, data$PT08.S1.CO.)


# yellow stone visitor data -- seems to be pretty good so far... 
data<- read.csv("ysnp.csv")
head(data)
tail(data)

mat <- do.call(rbind, strsplit(as.character(data$Year.Month.Day), '/'))
day<-  as.numeric(mat[,3])
month<- as.numeric(mat[,2])
year<- as.numeric(mat[,1])
hour <- substring(data$Time, 1,2)

monthLetter <- c( "J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")

DATE<- as.POSIXlt(paste0( year,'/',month,'/', day, " ", "00",':', "00:00 EST") )

plot(DATE, data$Recreation.Visits)

df <- data.frame( data$Recreation.Visits, data$Unemployment.Rate, data$MeanTemperature.F., data$Consumer.Price.Index , month, year, DATE, monthLet =monthLetter[month], fracTime= (year + month/12 + day/365)) 

head(df)

plot(df)

library(ggplot2)
ggplot(df, aes(x=DATE, y = data.Recreation.Visits )) +geom_point()+  geom_line()+  theme_bw()+
  geom_text(aes(label=df$monthLet),hjust=-.5, vjust=0)

#https://a-little-book-of-r-for-time-series.readthedocs.io/en/latest/src/timeseries.html 
visits <- (ts(df$data.Recreation.Visits[order(df$DATE)], frequency = 12, start = c(1986)))
plot(visits)

visitcomponents <- decompose(visits)
plot(visitcomponents)

visitsseasonallyadjusted <- visits - visitcomponents$seasonal
plot(visitsseasonallyadjusted)

acf(visits)
pacf(visits)
visits <- log(visits)

#http://r-statistics.co/Time-Series-Analysis-With-R.html
library(forecast)
ts.stl <- stl(visits,"periodic")  # decompose the TS
ts.sa <- seasadj(ts.stl)  # de-seasonalize
plot(visits, type="l")  # original series
plot(ts.sa, type="l")  # seasonal adjusted
seasonplot(ts.sa, 12, col=rainbow(12), year.labels=TRUE, main="Seasonal plot: visits") # seasonal frequency set as 12 for monthly data.


library(tseries)

#unit root test? 
adf.test(visits) # p-value < 0.05 indicates the TS is stationary

# Augmented Dickey-Fuller Test
# 
# data:  visits
# Dickey-Fuller = -19.209, Lag order = 7, p-value = 0.01
# alternative hypothesis: stationary

trainV <- head(visits, n= (length(visits) -12) )

test<- tail(visits, n=12) 
test

fit <- arima(trainV, order=c(1, 0, 2), seasonal = c(0,1,1)) 
      
plot(fit)
       
accuracy(fit)
cast <- forecast(fit, 12)
cast
plot(forecast(fit, 12))


visitsdiff1 <- diff(visits, differences=1)
plot.ts(visitsdiff1)

visitsdiff2 <- diff(visits, differences=2)
plot.ts(visitsdiff2)

acf(visitsdiff1, lag.max=20)             # plot a correlogram
acf(visitsdiff1, lag.max=20, plot=FALSE) # get the autocorrelation values

pacf(visitsdiff1, lag.max=20)             # plot a correlogram
pacf(visitsdiff1, lag.max=20, plot=FALSE) # get the autocorrelation values

auto.arima(visits)
