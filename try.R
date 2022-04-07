library(TTR)
library(forecast)
library(aTSA)
library(TSA)
library(imputeTS)

ihsg <- read.csv("C:/Users/Haci/Desktop/Metode Peramalan Deret Waktu/Project-20220127/IHSG7days.csv", sep=";")
ihsg.imputed <- na_interpolation(ihsg)
ggplot_na_imputations(ihsg$JKSE.Adjusted, ihsg.imputed$JKSE.Adjusted)
View(ihsg.imputed)

data.ts <- ts(ihsg.imputed$JKSE.Adjusted)
ts.plot(data.ts)
adf.test(data.ts)

seasonal.ts <- ts(ihsg.imputed$JKSE.Adjusted, frequency = 183)
diff.seasonal.ts <- diff(seasonal.ts,1)
acf(diff.seasonal.ts, lag.max=100)

eacf(data.ts)
eacf(ihsg.diff)
#ARIMA(0,1,2)
#ARIMA(0,1,3)
#ARIMA(1,1,3)

model1 <- Arima(data.ts, order=c(0,1,2), method="ML")
model2 <- Arima(data.ts, order=c(0,1,2), include.drift = TRUE, method="ML")
model3 <- Arima(data.ts, order=c(0,1,3), method="ML")
model4 <- Arima(data.ts, order=c(1,1,3), method="ML")
model1$aic; model2$aic; model3$aic; model4$aic;

data.ts <- ts(ihsg.imputed$JKSE.Adjusted)
ts.plot(data.ts, xlab="Time Period", ylab="IHSG Adjusted Closing Price", 
        main="IHSG Adjusted Closing Price Overtime")

acf(data.ts, lag.max = 50, main = "ACF IHSG Adjusted Closing Price") 
pacf(data.ts, lag.max = 50, main = "PACF IHSG Adjusted Closing Price")

ihsg.diff <- diff(data.ts, difference=1)
acf(ihsg.diff, lag.max = 50, main = "ACF IHSG Adjusted Closing Price") 
pacf(ihsg.diff, lag.max = 50, main = "PACF IHSG Adjusted Closing Price")

bc <- boxcox(data.ts ~ 1)
lambda <- bc$x[which.max(bc$y)]; lambda
bc.ihsg <- log(data.ts)

acf(bc.ihsg, lag.max = 50, main = "ACF IHSG Adjusted Closing Price") 
pacf(bc.ihsg, lag.max = 50, main = "PACF IHSG Adjusted Closing Price")

diff.bc.ihsg <- diff(data.ts, difference=1)
acf(diff.bc.ihsg, lag.max = 50, main = "ACF IHSG Adjusted Closing Price") 
pacf(diff.bc.ihsg, lag.max = 50, main = "PACF IHSG Adjusted Closing Price")

adf.test(ihsg.diff)
ts.plot(diff.bc.ihsg)

bcd <- boxcox((ihsg.diff+329) ~ 1)
lambda <- bcd$x[which.max(bcd$y)]; lambda

bcd.ihsg <- log(ihsg.diff)
ts.plot(bcd.ihsg)
ts.plot(ihsg.diff)

#acf pacf
acf(ihsg.diff, max.lag=30)


#
ihsg.sqrt <- ihsg.diff^(1/2)
ihsg.cbrt <- ihsg.diff^(1/3)
ihsg.log <- log(ihsg.diff+abs(min(ihsg.diff)))
ihsg.inv <- -1/ihsg.diff

ts.plot(ihsg.sqrt)
ts.plot(ihsg.cbrt)
ts.plot(ihsg.log)
ts.plot(ihsg.inv)
ts.plot(ihsg.diff)

#pola musiman
seasonal.ts <- ts(ihsg.imputed$JKSE.Adjusted, frequency=15)
diff.seasonal.ts <- diff(seasonal.ts,1)
acf(diff.seasonal.ts, lag.max = 100)
plot.ts(ds.ts)
plot.ts(ihsg.diff)

par(mfrow=c(1,2))
ts.plot(data.ts, main="IHSG Adjusted Closing Price 1991-2020")
ts.plot(data.ts[0:2000], main="IHSG Adjusted Closing Price 1991-1996")
