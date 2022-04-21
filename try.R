#import library
library(imputeTS)
library(quantmod)
library(aTSA)
library(TSA)
library(forecast)
library(tseries)
library(MASS)
library(lmtest)

#import data
start <- as.POSIXct("2017-01-01")
end <- as.POSIXct("2022-03-31")
getSymbols(Symbols = "^JKSE",src = "yahoo", from = start, to = end)
data <- as.data.frame(JKSE)
data$Date <- as.Date(rownames(data))

#export as csv
#write.csv(data, file="E:/Github/exploringIHSG/IHSG.csv", row.names = T)

#menambahkan tanggal yang kosong di dataset (sabtu-minggu dan tanggal merah)
fulldates <- data.frame(Date=seq(as.Date("2017-01-01"), as.Date("2022-03-31"), by="days"))
ihsg <- merge(fulldates,data,by="Date",all.x=T)

#imputasi missing value
ihsg.imputed <- na_interpolation(ihsg, option="spline")
ggplot_na_imputations(ihsg$JKSE.Adjusted, ihsg.imputed$JKSE.Adjusted)

#bikin data time series + seasonal ts
ihsg.ts <- ts(ihsg.imputed$JKSE.Adjusted)
ihsg.seasonal <- ts(ihsg.imputed$JKSE.Adjusted, frequency=7) #belum tau/yakin

#train-test split
train.prop <- floor(nrow(ihsg.imputed)*0.8)
test.prop <- nrow(ihsg.imputed)-train.prop

ihsg.train <- head(ihsg.ts, train.prop)
ihsg.test <- tail(ihsg.ts, test.prop)
seasonal.train <- head(ihsg.seasonal, train.prop)
seasonal.test <- tail(ihsg.seasonal, test.prop)

#plot time series
plot(ihsg.imputed$JKSE.Adjusted~ihsg.imputed$Date,
     type = "l", xlab = "Tahun", main = "IHSG Adjusted Closing Price Overtime",
     ylab = "Harga Adjusted Closing IHSG")

#plot acf pacf
acf(ihsg.ts, lag.max = 30 ,main = "Plot ACF IHSG Adjusted Closing Price")
pacf(ihsg.ts, lag.max = 30, main = "Plot PACF IHSG Adjusted Closing Price") 
#acf turun secara eksponensial, ga stasioner

#uji formal adf
adf.test(ihsg.ts) #kesimpulan: tidak stasioner

#differencing 1 kali
ihsg.diff <- diff(ihsg.ts, 1)
ts.plot(ihsg.diff)

#plot acf pacf
acf(ihsg.diff, lag.max = 30, 
    main = "Plot ACF yang telah Differencing 1 kali")
#cutoff pada lag ke-1 atau ke-3
pacf(ihsg.diff, lag.max = 30, 
     main = "Plot PACF yang telah Differencing 1 kali") 
#cutoff pada lag ke-1 atu ke-3

#uji formal adf
adf.test(ihsg.diff)

#menentukan ordo p, q, dan r
eacf(ihsg.diff) #ARIMA(0,1,3) ARIMA(0,1,4) ARIMA(1,1,4) 

#menentukan model terbaik
ihsg.model1 <- Arima(ihsg.ts, order=c(0,1,3), method="ML")
ihsg.model2 <- Arima(ihsg.ts, order=c(0,1,4), method="ML")
ihsg.model3 <- Arima(ihsg.ts, order=c(1,1,4), method="ML")
ihsg.model4 <- Arima(ihsg.ts, order=c(0,1,1), method="ML")
ihsg.model1$aic; ihsg.model2$aic; ihsg.model3$aic; ihsg.model4$aic;
ihsg.bestmodel <- ihsg.model4
ihsg.residual <- ihsg.bestmodel$residuals

coeftest(ihsg.model1) 
coeftest(ihsg.model2) 
coeftest(ihsg.model3) 
coeftest(ihsg.model4) 

#uji diagnostik semuanya :)
jarque.bera.test(ihsg.residual)
#acf pacf
acf(ihsg.residual)
pacf(ihsg.residual)
#autocorrelation
Box.test(ihsg.residual, type="Ljung")

#uji diagnostik
#normality
#qqplot
qqnorm(ihsg.residual)
qqline(ihsg.residual)
#formal test
jarque.bera.test(ihsg.residual)
ks.test(ihsg.residual,"pnorm")
#acf pacf
acf(ihsg.residual)
pacf(ihsg.residual)
#autocorrelation
Box.test(ihsg.residual, type="Ljung")

#overfitting
#akan dicoba ARIMA(3,0,2) dan (2,0,3)
ihsg.overfitting1 <- Arima(ihsg.ts, order=c(1,1,3), method="ML")
ihsg.overfitting1$aic; ihsg.bestmodel$aic
#model ovterfitting ARIMA(3,0,2) memiliki AIC lebih besar,
#maka overfitting tidak akan dilakukan



#forecasting
ihsg.forecast <- forecast(Arima(ihsg.train, order=c(0,1,3), method="ML"), 
                          h=test.prop)
plot(ihsg.forecast)
accuracy(ihsg.forecast$mean, ihsg.test)

#SARIMA
ihsg.seasonal <- ts(ihsg.imputed$JKSE.Adjusted, frequency=7)
ihsg.seasonal.diff <- diff(diff(ihsg.seasonal,1),7)

ts.plot(ihsg.seasonal.diff)
adf.test(ihsg.seasonal.diff)

#nentuin ordo
par(mfrow=c(1,2))
acf(ihsg.seasonal.diff, lag.max = 14, main="ACF lag max = 14") 
acf(ihsg.seasonal.diff, lag.max = 210, main="ACF lag max = 1000") 
#cutoff di lag ke-1 atau ke-3. selain itu komponen musiman di ACF cutoff 
#di lag ke-14 (satu lag musiman)
#MA(1) atau MA(3) non musiman
pacf(ihsg.seasonal.diff, lag.max = 14, main="PACF lag max = 14")
pacf(ihsg.seasonal.diff, lag.max = 210, main="PACF lag max = 1000")
par(mfrow = c(1,1))
#cutoff di lag ke-1 atau ke-4, tampak tail-off di lag musiman 
#AR(1) atau AR(4) non musiman 
#(terjadi penurunan namun secara perlahan).
eacf(ihsg.seasonal.diff)

eacf(ihsg.diff)
#EACF merekomendasikan model ARIMA(0,1,1), ARIMA(0,1,3), ARIMA(0,1,)
#kandidat model 

#ARIMA(0,1,3)(0,1,1)14
ihsg.seasonal <- ts(ihsg.imputed$JKSE.Adjusted, frequency=7)
ihsg.bestmodel

#cek model
ihsg.seasonal.model1 <- Arima(ihsg.seasonal, order=c(0,1,3), seasonal=c(0,1,1), method="ML")
ihsg.seasonal.model2 <- Arima(ihsg.seasonal, order=c(0,1,4), seasonal=c(0,1,1), method="ML")
ihsg.seasonal.model3 <- Arima(ihsg.seasonal, order=c(1,1,4), seasonal=c(0,1,1), method="ML")
ihsg.seasonal.model4 <- Arima(ihsg.seasonal, order=c(0,1,1), seasonal=c(0,1,1), method="ML")

ihsg.seasonal.model1$aic; ihsg.seasonal.model2$aic; ihsg.seasonal.model3$aic; 
ihsg.seasonal.model4$aic; 
ihsg.model1$aic
ihsg.seasona.bestmodel <- ihsg.seasonal.model1
ihsg.seasonal.residual <- ihsg.seasonal.bestmodel$residuals

#uji diagnostik
#normality
#qqplot
qqnorm(ihsg.seasonal.residual)
qqline(ihsg.seasonal.residual)
#formal test
jarque.bera.test(ihsg.seasonal.residual)
#acf pacf
acf(ihsg.seasonal.residual)
pacf(ihsg.seasonal.residual)
#autocorrelation
Box.test(ihsg.seasonal.residual, type="Ljung")

#coba boxcox

bc <- boxcox(ihsg.ts ~ 1)
(lambda <- bc$x[which.max(bc$y)])

ihsg.ts <- ihsg.ts^(lambda)
ts.plot(ihsg.ts)

auto.arima(ihsg.ts)

ihsg.model1 <- Arima(ihsg.ts, order=c(0,1,3), method="ML")
ihsg.model2 <- Arima(ihsg.ts, order=c(0,1,4), method="ML")
ihsg.model3 <- Arima(ihsg.ts, order=c(1,1,4), method="ML")
ihsg.model1$aic; ihsg.model2$aic; ihsg.model3$aic;
ihsg.bestmodel <- ihsg.model1
ihsg.residual <- ihsg.bestmodel$residuals

ihsg.model1
coeftest(ihsg.model1)
#forecasting
ihsg.forecast <- forecast(Arima(ihsg.train, order=c(0,1,3), method="ML"), 
                          h=test.prop)
plot(ihsg.forecast)
ihsg.forecast$mean
lines(ihsg.test)
accuracy(ihsg.forecast$mean, ihsg.test)

ihsg.seasonal.forecast <- forecast(Arima(seasonal.train, order=c(0,1,3), seasonal=c(0,1,1), method="ML"), 
                          h=test.prop)
plot(ihsg.seasonal.forecast)
lines(seasonal.test)
accuracy(ihsg.seasonal.forecast$mean, seasonal.test)
