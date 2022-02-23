# import library
library(imputeTS)
library(ggplot2)
library(TTR)
library(forecast)

# set the work directory and import the data
setwd("C:/Users/Haci/Desktop/Metode Peramalan Deret Waktu/Project-20220127")
df <- read.csv("IHSG.csv", sep=";")
View(df)

# as we can see, our data has many missing values, let's find out how many are there
sum(is.na(df))

# handling NA using interpolation 
df.imputed <- na_interpolation(df)

# making sure that our data has no more missing value
sum(is.na(df.imputed))

# transform into time series data. 
# seasonal will be used for holtwinter additive/multiplicative model
data.ts <- ts(df.imputed$JKSE.Adjusted)
seasonal <- ts(df.imputed$JKSE.Adjusted, frequency=12)

# look at the time series plot of JKSE closing price
ts.plot(data.ts, xlab="Time Period", ylab="JKSE Adjusted Closing Price", 
        main="JKSE Adjusted Closing Price Overtime")

# we wanna split the data into 80% training and 20% testing
# let's calculate the number of instances
floor(nrow(df.imputed)*0.8)

# splitting the data
train.ts <- head(data.ts, 6460)
test.ts <- tail(data.ts, 1616)
seasonal.train <- head(seasonal, 6460)
seasonal.test <- tail(seasonal, 1616)

# single moving average
data.sma<-SMA(train.ts, n=20)
data.fc<-c(NA,data.sma)
data.gab<-data.frame(cbind(actual=c(train.ts,rep(NA,1616)),smoothing=c(data.sma,rep(NA,1616)),
                forecast=c(data.fc,rep(data.fc[length(data.fc)],1615))))

error.sma = train.ts-data.fc[1:length(train.ts)]
RMSE.sma = sqrt(mean(error.sma[1617:length(train.ts)]^2))
test.RMSE.SMA <- sqrt(mean((tail(data.gab$forecast, 1616)-test.ts)^2))

ts.plot(data.gab[,1], xlab="Time Period ", ylab="JKSE Adjusted Closing Price",
        main= "Time series plot of JKSE Adjusted Closing Price")
lines(data.gab[,2],col="green",lwd=2)
lines(data.gab[,3],col="red",lwd=2)
legend("topleft",c("Actual","Smoothed","Forecast"), lty=8, 
       col=c("black","green","red"), cex=0.8)

# double moving average
dma <- SMA(data.sma, n = 30)
At <- 2*data.sma - dma
Bt <- 2/(30-1)*(data.sma - dma)
data.dma<- At+Bt
data.fc2<- c(NA, data.dma)

t = 1:1617
f = c()

for (i in t) {
  f[i] = At[length(At)] + Bt[length(Bt)]*(i)
}

data.gab2 <- data.frame(cbind(aktual = c(train.ts,rep(NA,1617)), 
                   pemulusan1 = c(data.sma,rep(NA,1617)),
                   pemulusan2 = c(data.dma, rep(NA,1617)),
                   At = c(At, rep(NA,1617)), 
                   Bt = c(Bt,rep(NA,1617)),
                   forecast = c(data.fc2, f[-1])))

error.dma = train.ts-data.fc2[1:length(train.ts)]
RMSE.dma = sqrt(mean(error.dma[60:length(train.ts)]^2))
test.RMSE.DMA <- sqrt(mean((tail(data.gab2$forecast, 1616)-test.ts)^2))

ts.plot(data.gab2[,1], xlab="Time Period ", ylab="JKSE Adjusted Closing Price",
        main= "Time series plot of JKSE Adjusted Closing Price")
lines(data.gab2[,3],col="green",lwd=2)
lines(data.gab2[,6],col="red",lwd=2)
legend("topleft",c("Actual","Smoothed","Forecast"), lty=8, 
       col=c("black","green","red"), cex=0.8)

# single exponential smoothing
ses.1 <- HoltWinters(train.ts, gamma = F, beta = F, alpha = 0.2)
ses.2 <- HoltWinters(train.ts, gamma = F, beta = F, alpha = 0.7)
ses.opt <- HoltWinters(train.ts, gamma = F, beta = F) 

RMSE.ses1 <- sqrt(ses.1$SSE/length(train.ts))
RMSE.ses2 <- sqrt(ses.2$SSE/length(train.ts))
RMSE.sesopt <- sqrt(ses.opt$SSE/length(train.ts))

fc.ses1 <- predict(ses.1, n.ahead = 1616)
fc.ses2 <- predict(ses.2, n.ahead = 1616)
fc.sesopt <- predict(ses.opt, n.ahead = 1616)

test.RMSE.SES1 <- sqrt(mean((fc.ses1-test.ts)^2))
test.RMSE.SES2 <- sqrt(mean((fc.ses2-test.ts)^2))
test.RMSE.SESopt <- sqrt(mean((fc.sesopt-test.ts)^2))

plot(train.ts,main="SES with Optimal parameter",type="l",col="black",pch=12)
lines(ses.opt$fitted[,2],type="l",col="red")
lines(fc.sesopt,type="l",col="blue")
lines(test.ts,type="l")
legend("topleft",c("Actual Data","Fitted Data","Forecast"),
       col=c("black","red","blue"),lty=1)

# double exponential smoothing
des.1 <- HoltWinters(train.ts,alpha = 0.2, beta=0.3, gamma=F)
des.2 <- HoltWinters(train.ts,alpha = 0.7, beta=0.004, gamma=F)
des.opt <- HoltWinters(train.ts, gamma = F)

RMSE.des1 <- sqrt(des.1$SSE/length(train.ts))
RMSE.des2 <- sqrt(des.2$SSE/length(train.ts))
RMSE.desopt <- sqrt(des.opt$SSE/length(train.ts))

fc.des1 <- predict(des.1, n.ahead = 1616)
fc.des2 <- predict(des.2, n.ahead = 1616)
fc.desopt <- predict(des.opt, n.ahead = 1616)

test.RMSE.DES1 <- sqrt(mean((fc.des1-test.ts)^2))
test.RMSE.DES2 <- sqrt(mean((fc.des2-test.ts)^2))
test.RMSE.DESopt <- sqrt(mean((fc.desopt-test.ts)^2))

plot(train.ts,main="DES with Optimal parameter",type="l",col="black",pch=12)
lines(des.opt$fitted[,2],type="l",col="red")
lines(fc.desopt,type="l",col="blue")
lines(test.ts,type="l")
legend("topleft",c("Actual Data","Fitted Data","Forecast"),
       col=c("black","red","blue"),lty=1)


# holtwinter additive
HWA <- HoltWinters(seasonal.train, seasonal = "additive")
fc.HWA <- forecast(HWA, h=1616)
RMSE.HWA <- sqrt(HW.1$SSE/length(seasonal.train))
test.RMSE.HWA <- sqrt(mean((fc.HWA$mean[1:1616]-test.ts)^2))
predictHWA <- predict(HWA, n.ahead=1616)

plot(seasonal.train,main="Holt Winter Additive",type="l",col="black",pch=12)
lines(HWA$fitted[,2],type="l",col="red")
lines(predictHWA,type="l",col="blue")
lines(seasonal.test,type="l")
legend("topleft",c("Actual Data","Fitted Data","Forecast"),
       col=c("black","red","blue"),lty=1)

# holtwinter multiplicative
HWM <- HoltWinters(seasonal.train, seasonal = "multiplicative")
fc.HWM <- forecast(HWM, h=1616)
RMSE.HWM <- sqrt(HWM$SSE/length(seasonal.train))
test.RMSE.HWM <- sqrt(mean((fc.HWA$mean[1:1616]-test.ts)^2))
predictHWM <- predict(HWM, n.ahead=1616)


plot(seasonal.train,main="Holt Winter Multiplicative",type="l",col="black",pch=12)
lines(HWM$fitted[,2],type="l",col="red")
lines(predictHWM,type="l",col="blue")
lines(seasonal.test,type="l")
legend("topleft",c("Actual Data","Fitted Data","Forecast"),
       col=c("black","red","blue"),lty=1)


# comparing RMSE of the train dataset
err <- data.frame(metode=c("SMA","DMA","SES 1","SES 2","SES opt",
                           "DES 1", "DES 2", "DES opt"),
                  RMSE=c(RMSE.sma, RMSE.dma, RMSE1.ses, RMSE2.ses, RMSEopt.ses,
                         RMSE1.des, RMSE2.des, RMSEopt.des))
err

# comparing RMSE of the test dataset
test.err <- data.frame(metode=c("SMA","DMA","SES 1","SES 2","SES opt",
                                "DES 1", "DES 2", "DES opt"),
                       RMSE=c(test.RMSE.SMA, test.RMSE.DMA, 
                              test.RMSE.SES1, test.RMSE.SES2, test.RMSE.SESopt, 
                              test.RMSE.DES1, test.RMSE.DES2, test.RMSE.DESopt))
test.err

# comparing MAPE of the train dataset


# comparing MAPE of the test dataset
