#importing required packages
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(grid)
library(gridExtra)
library(maps)
library(mapdata)
library(sp)
library(maptools)
library(rgdal)
library(mapproj)
library(scales)
library(treemapify)
library(ckanr)
library(tidyverse)
library(jsonlite)
library(magrittr)
library(readr) 
library(dplyr)
library(openxlsx)
library(foreign)
library(knitr)
library(ids)
library(lubridate)

#Reading the data and loading it
Finaldata2 <- read.csv("Finaldata1.csv", header = TRUE, na.strings = c("","NA"))

Finaldata2

Finaldata2$Date.of.Sale<-strptime(Finaldata2$Date.of.Sale,format="%Y-%m-%d")
Finaldata2$Date.of.Sale<-as.Date(Finaldata2$Date.of.Sale,format="%Y-%m-%d")

class(Finaldata2$Date.of.Sale)

#Subsetting the dataset
houseprice <- Finaldata2[,c("Date.of.Sale", "Price....")]

houseprice

plot(houseprice)


house_price <- ts(houseprice)
class(house_price)

cat("Start of year : ", start(house_price), "\n")
cat("End of year : ", end(house_price), "\n")
cat("Frequency of year : ", frequency(house_price), "\n")
print(summary(house_price))

cycle(house_price)


house_price <- ts(houseprice, start=c(2010,1), end=c(2020,2), frequency = 12)

end(house_price)

cycle(house_price)

plot(house_price)

plot(house_price, xlab="", ylab = "Price", main="Prices from 2010 to 2020")

abline(reg=lm(house_price~time(house_price)))

plot(aggregate(house_price,FUN=mean))

boxplot(house_price ~ cycle(house_price), xlab="Year", ylab = "Price in euros" , 
        main ="Yearly prices from 2010 to 2020")

seasonal_decomposition <- stl(house_price[,1], s.window="period")
plot(seasonal_decomposition)


library(forecast)
acf(house_price)

pacf(house_price)

nsdiffs(house_price[,1])

log_house_price <- log(house_price[,1])
nsdiffs(log_house_price)


#Using arima to fit(trial)
fit1 <- arima(house_price[,1], order=c(1, 0, 0))
accuracy(fit1)

forecast(fit1, 5)
plot(forecast(fit1, 5))

#Fitting an ARIMA model
fit <- arima(house_price[,1], c(1,1,1), seasonal = list(order = c(1,1,1), period = 12))
fit

prediction <- predict(fit, n.ahead = 3 * 12)
prediction

forecast_house_price <- forecast(fit, level = c(95), h = 36)
forecast_house_price

autoplot(forecast_house_price)

plot(forecast(forecast_house_price, 3), xlab = "Year", ylab = "price")

accuracy(fit)

# Using Automated forecasting (exponential model)
fit2 <- ets(house_price[,1])
accuracy(fit2)
forecast_house_price1 <- forecast(fit2, level = c(95), h = 36)
autoplot(forecast_house_price1)
plot(forecast(forecast_house_price1, 3), xlab = "Year", ylab = "price")


# Using Auto Arima
auto_arima_model <- auto.arima(house_price[,1])
auto_arima_model

summary(auto_arima_model)

accuracy(auto_arima_model)

plot(forecast(auto_arima_model, 3 * 12), xlab = "Year", ylab = "prices")


#Plotting of residuals
qqnorm(fit$residuals)
qqline(fit$residuals)

qqnorm(fit1$residuals)
qqline(fit1$residuals)


qqnorm(fit2$residuals)
qqline(fit2$residuals)


qqnorm(auto_arima_model$residuals)
qqline(auto_arima_model$residuals)


#Training and Testing
house_price_train <- window(x = house_price, start=c(2010,1), end=c(2014,12),12)
house_price_test <- window(x = house_price, start=c(2015,1))

house_price_train

house_price_test


fittrain <- arima(house_price_train[,1], 
             c(1,0,0), 
             seasonal = list(order = c(1,0,0), 
                             period = 12))
fittrain

accuracy(fittrain)

auto_arima_model <- auto.arima(house_price_train[,1])
auto_arima_model

nrow(house_price_test)

predict_auto_ARIMA <- forecast(auto_arima_model, 62)
predict_auto_ARIMA

predict_manual_ARIMA <- forecast(fit, 3 * 12)
predict_manual_ARIMA

actuals_predictions <- data.frame(cbind(actuals = house_price_test, predicted = predict_auto_ARIMA))
head(actuals_predictions)

correlation_accuracy <- cor(actuals_predictions)
correlation_accuracy



#-------------------------------------------------------------------------------------
#*************************************************************************************
#-------------------------------------------------------------------------------------





















