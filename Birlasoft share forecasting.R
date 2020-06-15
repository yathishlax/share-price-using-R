
#daily Data

install.packages('quantmod')
install.packages('quantmod')
install.packages('quantmod')
install.packages('forecast')
install.packages('zoo')
install.packages('ggplot2')
install.packages('dplyr')
install.packages('lubridate')
install.packages('tseries')
install.packages('timeSeries')
install.packages('xts')
install.packages('knitr')
install.packages('fpp2')
install.packages('seasonal')
install.packages('urca')

library('quantmod')
library('quantmod')
library('forecast')
library('zoo')
library('ggplot2')
library('dplyr')
library('lubridate')
library('tseries')
library('timeSeries')
library('xts')
library('knitr')
library('fpp2')
library('seasonal')
library('urca')
library('forecast')


df <- read.csv('C:/Users/Sazid/Desktop/yati/R/BSOFT.NS (1).csv')
head(df)

#replacing Na's
df1 = na.locf(df1)
plot.ts(df1$Close)

### Adding the rownames as date
rownames(df1) <- as.POSIXct(df1$Date, format = '%Y-%m-%d')
head(df1)

#converting to time series
df2 <- ts(df1$ Close, frequency =12, 
                 start =c(2015,6), end = c(2020,03))
class(df2)
plot.ts(df2)
start(df2)
end(df2)
summary(df2)
cycle(df2)

#decompositon
decompose(df2)
plot(decompose(df2))

# Various plots
ggsubseriesplot(df2, season.labels = c("Jan", "Feb", "Mar", "April", "May", "Jun", "July",
                                         "Aug","sept", "Oct", "Nov","Dec"))

monthplot(df2)
monthplot(df2, base = median, col.base = "red")

##since there it is a seasonal data and having a trend, the time series is not stationary
## mean is not same and varience is not constant during the journey
## lets check it using dicker fully test
## H0: It is not stationary
## H1: It is stationary

print(adf.test(df2))

## as per adf test p-value is greater than 0.05 and so it is not stationary since(H0 is true)

dflog = log(df2)
plot(dflog)

## lets check stationarity using dicker fully test

print(adf.test(dflog))

## lets apply transformation, which is used to make the varience constant
## log transformation is a popular method

decompose(dflog)
plot(decompose(dflog))

## it was observed that after transformation still it is not stationary
## lets apply difference and check for stationarity

difflog <- diff(dflog)
plot((difflog))

print(adf.test(difflog))

##now it is observed that the series is stationary and ready to apply models

### Lets implement ARIMA model on this series

AR- Autoregressive
I- Integrated
MA- Moving average

acf(difflog)

#ACF used to calculate the value of q-MA

# q = 0 from the graph as there are is no significant spike outside the threshold

#ACF used to calculate the value of p-MA
pacf(difflog)

# p = 0 from the graph as there are is no significant spike outside the threshold

#d=1 since it is first order differencing


fit = arima(difflog, c(0,1,0))

checkresiduals(fit)


# it seems the residual errors are not statistic
#also p_value is insignificant from Ljung-Box test
# lets use autoarima and check for values

dfA <- auto.arima(df2, approximation=FALSE, stepwise=FALSE)
summary(dfA)

checkresiduals(dfA)

q = (forecast(dfA, h = 12 ))
plot(q)

print(q)






