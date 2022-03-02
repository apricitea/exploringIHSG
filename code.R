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

# plot to check whether interpolating the data changes its trend
ggplot_na_imputations(df$JKSE.Adjusted, df.imputed$JKSE.Adjusted)

# making sure that our data has no more missing value
sum(is.na(df.imputed))

# transform into time series data. 
# seasonal will be used for holtwinter additive/multiplicative model
data.ts <- ts(df.imputed$JKSE.Adjusted)
seasonal <- ts(df.imputed$JKSE.Adjusted, frequency=30)

# look at the time series plot of IHSG closing price
ts.plot(data.ts, xlab="Time Period", ylab="IHSG Adjusted Closing Price", 
        main="IHSG Adjusted Closing Price Overtime")

#compare proportion
#0.9 train 0.1 test
#0.8 train 0.2 test
#0.7 train 0.3 test

# we wanna split the data into 80% training and 20% testing
# let's calculate the number of instances
floor(nrow(df.imputed)*0.9)
floor(nrow(df.imputed)*0.8)
floor(nrow(df.imputed)*0.7)


trainprop <- floor(nrow(df.imputed)*0.9)
testprop <- nrow(df.imputed)-trainprop

# splitting the data
train.ts <- head(data.ts, trainprop)
test.ts <- tail(data.ts, testprop)
seasonal.train <- head(seasonal, trainprop)
seasonal.test <- tail(seasonal, testprop)

# single moving average
data.sma<-SMA(train.ts, n=30)
data.fc<-c(NA,data.sma)
data.gab<-data.frame(cbind(actual=c(train.ts,rep(NA,testprop)),smoothing=c(data.sma,rep(NA,testprop)),
                forecast=c(data.fc,rep(data.fc[length(data.fc)],testprop-1))))

error.sma = train.ts-data.fc[1:length(train.ts)]
RMSE.sma = sqrt(mean(error.sma[testprop+1:length(test.ts)]^2))
test.RMSE.SMA <- sqrt(mean((tail(data.gab$forecast, testprop)-test.ts)^2))

ts.plot(data.gab[,1], xlab="Time Period ", ylab="IHSG Adjusted Closing Price",
        main= "SMA of IHSG Adjusted Closing Price, m=30",ylim=c(200,7000))
lines(data.gab[,2],col="green",lwd=2)
lines(data.gab[,3],col="red",lwd=2)
lines(test.ts)
legend("topleft",c("Actual","Smoothed","Forecast"), lty=1, 
       col=c("black","green","red"), cex=0.8)

# try different moving average
# periode = 7
# periode = 30
periode = 365

# double moving average
dma <- SMA(data.sma, n = period)
At <- 2*data.sma - dma
Bt <- 2/(period-1)*(data.sma - dma)
data.dma<- At+Bt
data.fc2<- c(NA, data.dma)

t = 1:testprop+1
f = c()

for (i in t) {
  f[i] = At[length(At)] + Bt[length(Bt)]*(i)
}

data.gab2 <- data.frame(cbind(aktual = c(train.ts,rep(NA,testprop+1)), 
                   pemulusan1 = c(data.sma,rep(NA,testprop+1)),
                   pemulusan2 = c(data.dma, rep(NA,testprop+1)),
                   At = c(At, rep(NA,testprop+1)), 
                   Bt = c(Bt,rep(NA,testprop+1)),
                   forecast = c(data.fc2, f[-1])))

error.dma = train.ts-data.fc2[1:length(train.ts)]
RMSE.dma = sqrt(mean(error.dma[60:length(train.ts)]^2))
test.RMSE.DMA <- sqrt(mean((tail(data.gab2$forecast, testprop)-test.ts)^2))

ts.plot(data.gab2[,1], xlab="Time Period ", ylab="IHSG Adjusted Closing Price",
        main= "DMA of IHSG Adjusted Closing Price m=365",ylim=c(200,7000))
lines(data.gab2[,3],col="green",lwd=2)
lines(data.gab2[,6],col="red",lwd=2)
lines(test.ts)
legend("topleft",c("Actual","Smoothed","Forecast"), lty=1, 
       col=c("black","green","red"), cex=0.8)

# single exponential smoothing
ses.1 <- HoltWinters(train.ts, gamma = F, beta = F, alpha = 0.5)
ses.2 <- HoltWinters(train.ts, gamma = F, beta = F, alpha = 0.9)
ses.opt <- HoltWinters(train.ts, gamma = F, beta = F) 

ses.opt #optimum parameter for ses

RMSE.ses1 <- sqrt(ses.1$SSE/length(train.ts))
RMSE.ses2 <- sqrt(ses.2$SSE/length(train.ts))
RMSE.sesopt <- sqrt(ses.opt$SSE/length(train.ts))

fc.ses1 <- predict(ses.1, n.ahead = testprop)
fc.ses2 <- predict(ses.2, n.ahead = testprop)
fc.sesopt <- predict(ses.opt, n.ahead = testprop)

test.RMSE.SES1 <- sqrt(mean((fc.ses1-test.ts)^2))
test.RMSE.SES2 <- sqrt(mean((fc.ses2-test.ts)^2))
test.RMSE.SESopt <- sqrt(mean((fc.sesopt-test.ts)^2))

plot(train.ts,main="SES with Optimal parameter alpha=0.9999368",type="l",col="black",pch=12,
     ylab="IHSG Adjusted Closing Price",
     xlim=c(0,8000),ylim=c(200,7000))
lines(ses.opt$fitted[,2],type="l",col="red")
lines(fc.sesopt,type="l",col="blue")
lines(test.ts,type="l")
legend("topleft",c("Actual Data","Fitted Data","Forecast"),
       col=c("black","red","blue"),lty=1, cex=0.8)

plot(train.ts,main="SES with alpha=0.5",type="l",col="black",pch=12,
     ylab="IHSG Adjusted Closing Price",
     xlim=c(0,8000),ylim=c(200,7000))
lines(ses.1$fitted[,2],type="l",col="red")
lines(fc.ses1,type="l",col="blue")
lines(test.ts,type="l")
legend("topleft",c("Actual Data","Fitted Data","Forecast"),
       col=c("black","red","blue"),lty=1, cex=0.8)

plot(train.ts,main="SES with alpha=0.9",type="l",col="black",pch=12,
     ylab="IHSG Adjusted Closing Price",
     xlim=c(0,8000),ylim=c(200,7000))
lines(ses.2$fitted[,2],type="l",col="red")
lines(fc.ses2,type="l",col="blue")
lines(test.ts,type="l")
legend("topleft",c("Actual Data","Fitted Data","Forecast"),
       col=c("black","red","blue"),lty=1, cex=0.8)

# double exponential smoothing
des.1 <- HoltWinters(train.ts, alpha = 1, beta=0.024, gamma=F)
des.2 <- HoltWinters(train.ts, alpha = 0.86, beta=0.01, gamma=F)
des.opt <- HoltWinters(train.ts, gamma=F)

#check the parameters of optimum des
des.opt #a=1, b=0.002951429 for 90% train data

RMSE.des1 <- sqrt(des.1$SSE/length(train.ts))
RMSE.des2 <- sqrt(des.2$SSE/length(train.ts))
RMSE.desopt <- sqrt(des.opt$SSE/length(train.ts))

fc.des1 <- predict(des.1, n.ahead = testprop)
fc.des2 <- predict(des.2, n.ahead = testprop)
fc.desopt <- predict(des.opt, n.ahead = testprop)

test.RMSE.DES1 <- sqrt(mean((fc.des1-test.ts)^2))
test.RMSE.DES2 <- sqrt(mean((fc.des2-test.ts)^2))
test.RMSE.DESopt <- sqrt(mean((fc.desopt-test.ts)^2))

plot(train.ts,main="DES with Optimal parameter alpha=1 beta=0.003",
     type="l",col="black",pch=12, ylab="IHSG Adjusted Closing Price",
     xlim=c(0,8000),ylim=c(200,7000))
lines(des.opt$fitted[,2],type="l",col="red")
lines(fc.desopt,type="l",col="blue")
lines(test.ts,type="l")
legend("topleft",c("Actual Data","Fitted Data","Forecast"),
       col=c("black","red","blue"),lty=1, cex=0.8)

plot(train.ts,main="DES with alpha=1 beta=0.024",
     type="l",col="black",pch=12, ylab="IHSG Adjusted Closing Price",
     xlim=c(0,8000),ylim=c(200,7000))
lines(des.1$fitted[,2],type="l",col="red")
lines(fc.des1,type="l",col="blue")
lines(test.ts,type="l")
legend("topleft",c("Actual Data","Fitted Data","Forecast"),
       col=c("black","red","blue"),lty=1, cex=0.8)

plot(train.ts,main="DES with alpha=0.86 beta=0.01",
     type="l",col="black",pch=12, ylab="IHSG Adjusted Closing Price",
     xlim=c(0,8000),ylim=c(200,7000))
lines(des.2$fitted[,2],type="l",col="red")
lines(fc.des2,type="l",col="blue")
lines(test.ts,type="l")
legend("topleft",c("Actual Data","Fitted Data","Forecast"),
       col=c("black","red","blue"),lty=1, cex=0.8)

# holtwinter additive
HWA <- HoltWinters(seasonal.train, seasonal = "additive")
fc.HWA <- forecast(HWA, h=testprop)
RMSE.HWA <- sqrt(HWA$SSE/length(seasonal.train))
test.RMSE.HWA <- sqrt(mean((fc.HWA$mean[1:testprop]-test.ts)^2))
predictHWA <- predict(HWA, n.ahead=testprop)

plot(seasonal.train,main="Holt Winter Additive",type="l",col="black",pch=12,
     ylim=c(200,7000),xlim=c(0,270), 
     ylab="IHSG Adjusted Closing Price (per seasonal)")
lines(HWA$fitted[,2],type="l",col="red")
lines(predictHWA,type="l",col="blue")
lines(seasonal.test,type="l")
legend("topleft",c("Actual Data","Fitted Data","Forecast"),
       col=c("black","red","blue"),lty=1, cex=0.8)

# holtwinter multiplicative
HWM <- HoltWinters(seasonal.train, seasonal = "multiplicative")
fc.HWM <- forecast(HWM, h=testprop)
RMSE.HWM <- sqrt(HWM$SSE/length(seasonal.train))
test.RMSE.HWM <- sqrt(mean((fc.HWM$mean[1:testprop]-test.ts)^2))
predictHWM <- predict(HWM, n.ahead=testprop)

plot(seasonal.train,main="Holt Winter Multiplicative",type="l",col="black",pch=12,
     ylim=c(200,7000),xlim=c(0,270), 
     ylab="IHSG Adjusted Closing Price (per seasonal)")
lines(HWM$fitted[,2],type="l",col="red")
lines(predictHWM,type="l",col="blue")
lines(seasonal.test,type="l")
legend("topleft",c("Actual Data","Fitted Data","Forecast"),
       col=c("black","red","blue"),lty=1,cex=0.8)

# comparing RMSE of the train dataset
err <- data.frame(metode=c("SMA","DMA","SES 1","SES 2","SES opt",
                           "DES 1", "DES 2", "DES opt",
                           "HW Additive", "HW Multiplicative"),
                  RMSE=c(RMSE.sma, RMSE.dma, RMSE.ses1, RMSE.ses2, RMSE.sesopt,
                         RMSE.des1, RMSE.des2, RMSE.desopt, RMSE.HWA, RMSE.HWM))
View(err)

# comparing RMSE of the test dataset
test.err <- data.frame(metode=c("SMA","DMA","SES 1","SES 2","SES opt",
                                "DES 1", "DES 2", "DES opt",
                                "HW Additive","HW Multiplicative"),
                       RMSE=c(test.RMSE.SMA, test.RMSE.DMA, 
                              test.RMSE.SES1, test.RMSE.SES2, test.RMSE.SESopt, 
                              test.RMSE.DES1, test.RMSE.DES2, test.RMSE.DESopt,
                              test.RMSE.HWA, test.RMSE.HWM))
View(test.err)

# comparing MAPE
MAPE.sma <- mean(abs((test.ts-tail(data.gab$forecast, testprop))/test.ts))*100
MAPE.dma <- mean(abs((test.ts-tail(data.gab2$forecast, testprop))/test.ts))*100

MAPE.ses1 <- mean(abs((fc.ses1 - test.ts)/test.ts)) * 100
MAPE.ses2 <- mean(abs((fc.ses2 - test.ts)/test.ts)) * 100
MAPE.sesopt <- mean(abs((fc.sesopt - test.ts)/test.ts)) * 100

MAPE.des1 <- mean(abs((fc.des1 - test.ts)/test.ts)) * 100
MAPE.des2 <- mean(abs((fc.des2 - test.ts)/test.ts)) * 100
MAPE.desopt <- mean(abs((fc.desopt - test.ts)/test.ts)) * 100

MAPE.HWA <- mean(abs((fc.HWA$mean - seasonal.test)/seasonal.test)) * 100
MAPE.HWM <- mean(abs((fc.HWM$mean - seasonal.test)/seasonal.test)) * 100


MAPE <- data.frame(metode=c("SMA","DMA","SES 1","SES 2","SES opt",
                                "DES 1", "DES 2", "DES opt",
                                "HW Additive","HW Multiplicative"),
                       MAPE=c(MAPE.sma, MAPE.dma, 
                              MAPE.ses1, MAPE.ses2, MAPE.sesopt, 
                              MAPE.des1, MAPE.des2, MAPE.desopt,
                              MAPE.HWA, MAPE.HWM))
View(MAPE)

