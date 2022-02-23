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
data.ts <- ts(df.imputed$JKSE.Close)
seasonal <- ts(df.imputed$JKSE.Close, frequency=12)

# look at the time series plot of JKSE closing price
ts.plot(data.ts, xlab="Time Period", ylab="JKSE Closing Price", 
        main="JKSE Closing Price Overtime")

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
data.gab<-cbind(actual=c(train.ts,rep(NA,20)),smoothing=c(data.sma,rep(NA,20)),
                forecast=c(data.fc,rep(data.fc[length(data.fc)],19)))

error.sma = train.ts-data.fc[1:length(train.ts)]
RMSE.sma = sqrt(mean(error.sma[21:length(train.ts)]^2))

# double moving average
dma <- SMA(data.sma, n = 20)
At <- 2*data.sma - dma
Bt <- 2/(20-1)*(data.sma - dma)
data.dma<- At+Bt
data.fc2<- c(NA, data.dma)

t = 1:21
f = c()

for (i in t) {
  f[i] = At[length(At)] + Bt[length(Bt)]*(i)
}

data.gab2 <- cbind(aktual = c(train.ts,rep(NA,21)), 
                   pemulusan1 = c(data.sma,rep(NA,21)),
                   pemulusan2 = c(data.dma, rep(NA,21)),
                   At = c(At, rep(NA,21)), 
                   Bt = c(Bt,rep(NA,21)),
                   ramalan = c(data.fc2, f[-1]))

error.dma = train.ts-data.fc2[1:length(train.ts)]
RMSE.dma = sqrt(mean(error.dma[40:length(train.ts)]^2))

# single exponential smoothing
ses.1 <- HoltWinters(train.ts, gamma = F, beta = F, alpha = 0.2)
ses.2 <- HoltWinters(train.ts, gamma = F, beta = F, alpha = 0.7)
ses.opt <- HoltWinters(train.ts, gamma = F, beta = F) 

RMSE1.ses <- sqrt(ses.1$SSE/length(train.ts))
RMSE2.ses <- sqrt(ses.2$SSE/length(train.ts))
RMSEopt.ses <- sqrt(ses.opt$SSE/length(train.ts))

#double exponential smoothing
des.1 <- HoltWinters(train.ts,alpha = 0.2, beta=0.3, gamma=F)
des.2 <- HoltWinters(train.ts,alpha = 0.7, beta=0.004, gamma=F)
des.opt <- HoltWinters(train.ts, gamma = F)

RMSE1.des <- sqrt(des.1$SSE/length(train.ts))
RMSE2.des <- sqrt(des.2$SSE/length(train.ts))
RMSEopt.des <- sqrt(des.opt$SSE/length(train.ts))

#holtwinter additive

HWA <- HoltWinters(seasonal.train, seasonal = "additive")
fc.HWA <- forecast(HWA, h=1616)
RMSE.HWA <- sqrt(HW.1$SSE/length(seasonal.train))

#holtwinter multiplicative

HWM <- HoltWinters(seasonal.train, seasonal = "multiplicative")
fc.HWM <- forecast(HWM, h=1616)
RMSE.HWM <- sqrt(HWM$SSE/length(seasonal.train))
