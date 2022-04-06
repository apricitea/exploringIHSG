ihsg <- read.csv("C:/Users/Haci/Desktop/Metode Peramalan Deret Waktu/Project-20220127/IHSG7days.csv", sep=";")
ts.ihsg <- ts(ihsg$JKSE.Adjusted)
ts.plot(ts.ihsg)
View(ihsg)
library(imputeTS)
ihsg.imputed <- na_interpolation(ihsg)
ggplot_na_imputations(ihsg$JKSE.Adjusted, ihsg.imputed$JKSE.Adjusted)

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

library(forecast)
library(aTSA)
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
