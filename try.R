#import library
library(aTSA)
library(imputeTS)
library(knitr)
library(MASS)
library(quantmod)

#import data
start <- as.POSIXct("2017-01-01")
end <- as.POSIXct("2022-03-31")
getSymbols(Symbols = "^JKSE",src = "yahoo", from = start, to = end)
data <- as.data.frame(JKSE)
data$Date <- as.Date(rownames(data))
View(data)

#export as csv
#write.csv(data, file="E:/Github/exploringIHSG/IHSG.csv", row.names = T)

#menambahkan tanggal yang kosong di dataset (sabtu-minggu dan tanggal merah)
fulldates <- data.frame(Date=seq(as.Date("2017-01-01"), as.Date("2022-03-31"), by="days"))
ihsg <- merge(fulldates,data,by="Date",all.x=T)
View(ihsg)

#imputasi missing value
ihsg.imputed <- na_interpolation(ihsg)
ggplot_na_imputations(ihsg$JKSE.Adjusted, ihsg.imputed$JKSE.Adjusted)

