############################
# Code                     #
#    1. EDA                #
#    2. Data Preprocessing #
#    3. AMIRA Model        #
#    4. Forecasting        #
############################

#######
# EDA #
#######

# Read Data
data = read.csv("./data/data.csv")

#install.packages("ggplot2")
library(ggplot2)
ggplot(data, aes(x = year, y = value, group = level_1, color = level_1)) +
  geom_line() +
  labs(title = "Line Plot of Three Categories", x = "Year", y = "Value", color = 'Type')

# Fertility rate
fertilityRate = subset(data, level_1 == "Total Fertility Rate")
fertilityRate = fertilityRate[, c(1,3)]
plot(x = fertilityRate$year, y = fertilityRate$value, type = "l",
     main = "Total Fertility Rate from 1960 to 2018",
     xlab = "Year",
     ylab = "Per Female",
     col = "blue")

# ACF and PACF of raw data
acf(train[, c(2)], main = "ACF (Raw Data)", col = "red")
pacf(train[, c(2)], main = "PACF (Raw Data)", col = "red")

######################
# Data Preprocessing #
######################

# first difference
train_firstDiff = diff(train[, c(2)], difference = 1)
plot(train_firstDiff, type = "l", main = "First difference", ylab = "Differenced", col = "blue")

# log transformation
log_train = log(train$value)
plot(log_train, type = "l", main = "Transformed Data (log)", ylab = "log", col = "blue")
log_train_firstDiff = diff(log_train, difference = 1)
plot(log_train_firstDiff, type = "l", main = "First difference (log)", ylab = "Differenced", col = "blue")

# ACF and PACF of first difference (log)
acf(log_train_firstDiff, main = "ACF (Transformed Data)", col = "red")
pacf(log_train_firstDiff, main = "PACF (Transformed Data)", col = "red")

# check stationarity
#install.packages("tseries")
library(tseries)
adf.test(log_train_firstDiff)

###############
# ARIMA Model #
###############

# Function to search for optimal parameters
searchParamsARIMA <- function(pList, dList, qList, metric, ts){
  bestScore <- 0
  bestParams <- c(0,0,0)
  metric = metric
  for (p in pList){
    for (d in dList){
      for (q in qList){
        params = c(p, d, q)
        model = arima(ts, order = params, include.mean = FALSE, method = "ML")
        # change AIC() for different metrics
        score = metric(model)
        if (score < bestScore) {
          bestScore = score
          bestParams = c(p,d,q)
        }
      }
    }
  }
  msg <- sprintf("Best Params (p, d, q): %d, %d, %d, with score of %0.3f", bestParams[1], bestParams[2], bestParams[3], bestScore)
  print(msg)
}

# search for best parameters
pList = c(0:2)
dList = c(1)
qList = c(0:2)
ts = log_train_firstDiff

# metrics
m = AIC
searchParamsARIMA(pList, dList, qList, metric = m, ts)
m = BIC
searchParamsARIMA(pList, dList, qList, metric = m, ts)

# Model Evaluation
library(forecast)
bestModel = arima(log_train_firstDiff, order = c(0,1,1), include.mean = FALSE)

acf(bestModel$residual ,main = "ACF of Residuals", col='red')
pacf(bestModel$residual ,main = "PACF of Residuals", col='red')
tsdiag(bestModel)
checkresiduals(bestModel)
hist(bestModel$residual, main = "Histogram of Residuals", col='orange')
qqnorm(bestModel$residual, main = "Normal Q-Q Plot of Residuals", col='red')

# extract coefficients
bestModel

###############
# Forecasting #
###############

# install.packages("forecast")
library(forecast)
forecasts = forecast(bestModel, h = 5)

# Plot forecasts
x1 = fertilityRate$year
y1 = fertilityRate$value

fore0 = exp(tail(log_train_firstDiff, 1) + cumsum(forecasts$mean))
y2 = c(y1[1:length(train$value)], fore0)

fore97.5 = exp(tail(log_train_firstDiff, 1) + cumsum(forecasts$upper[, c(2)]))
fore2.5 = exp(tail(log_train_firstDiff, 1) + cumsum(forecasts$lower[, c(2)]))
y3 = c(y1[1:length(train$value)], fore97.5)
y4 = c(y1[1:length(train$value)], fore2.5)

fore90 = exp(tail(log_train_firstDiff, 1) + cumsum(forecasts$upper[, c(1)]))
fore10 = exp(tail(log_train_firstDiff, 1) + cumsum(forecasts$lower[, c(1)]))
y5 = c(y1[1:length(train$value)], fore90)
y6 = c(y1[1:length(train$value)], fore10)

plot(x1, y2, ylim = c(0,max(y1)), type = "l", col = "red",
     main = "Actual vs Forecasted",
     xlab = "Fertility Rate",
     ylab = "Year")
lines(x1, y3, type = "l", col = "gold")
lines(x1, y4, type = "l", col = "gold")
lines(x1, y5, type = "l", col = "orange")
lines(x1, y6, type = "l", col = "orange")
lines(x1, y1, type = "l", col = "blue")

legend(x = "topright",
       inset = 0.05,
       title = "Legend",
       legend = c("Actual", "Forecast", "80% C.I.", "95% C.I."),
       fill = c("blue", "red", "orange", "gold"))

# plot from 2010 onwards (magnified view)
plot(x1, y2, xlim = c(2010, 2018), ylim = c(0,2), type = "l", col = "red",
     main = "Actual vs Forecasted (Magnified)",
     xlab = "Fertility Rate",
     ylab = "Year")
lines(x1, y3, type = "l", col = "gold")
lines(x1, y4, type = "l", col = "gold")
lines(x1, y5, type = "l", col = "orange")
lines(x1, y6, type = "l", col = "orange")
lines(x1, y1, type = "l", col = "blue")

legend(x = "topleft",
       cex = 0.5,
       inset = 0.05,
       title = "Legend",
       legend = c("Actual", "Forecast", "80% C.I.", "95% C.I."),
       fill = c("blue", "red", "orange", "gold"))

#####################
# White Noise Model #
#####################

wn_mean = mean(log_train_firstDiff)
wn_sd = sd(log_train_firstDiff)
wn_model = arima.sim(model = list(order = c(0, 0, 0)), n = 5, mean = wn_mean, sd = wn_sd)
#plot(wn_model)

forecasts_wn = forecast(wn_model, h = 5)

# Plot forecasts
x1 = fertilityRate$year
y1 = fertilityRate$value

fore0 = exp(tail(log_train_firstDiff, 1) + cumsum(forecasts_wn$mean))
y2 = c(y1[1:length(train$value)], fore0)

fore97.5 = exp(tail(log_train_firstDiff, 1) + cumsum(forecasts_wn$upper[, c(2)]))
fore2.5 = exp(tail(log_train_firstDiff, 1) + cumsum(forecasts_wn$lower[, c(2)]))
y3 = c(y1[1:length(train$value)], fore97.5)
y4 = c(y1[1:length(train$value)], fore2.5)

fore90 = exp(tail(log_train_firstDiff, 1) + cumsum(forecasts_wn$upper[, c(1)]))
fore10 = exp(tail(log_train_firstDiff, 1) + cumsum(forecasts_wn$lower[, c(1)]))
y5 = c(y1[1:length(train$value)], fore90)
y6 = c(y1[1:length(train$value)], fore10)

# plot from 2010 onwards (magnified view)
plot(x1, y2, xlim = c(2010, 2018), ylim = c(0,2), type = "l", col = "darkgreen",
     main = "Actual vs Forecasted (Magnified)",
     xlab = "Fertility Rate",
     ylab = "Year")
lines(x1, y3, type = "l", col = "lightgreen")
lines(x1, y4, type = "l", col = "lightgreen")
lines(x1, y5, type = "l", col = "green")
lines(x1, y6, type = "l", col = "green")
lines(x1, y1, type = "l", col = "blue")

legend(x = "topleft",
       cex = 0.5,
       inset = 0.05,
       title = "Legend",
       legend = c("Actual", "Forecast", "80% C.I.", "95% C.I."),
       fill = c("blue", "red", "darkgreen", "lightgreen"))


#################################
# WN compared to ARIMA(0, 1, 1) #
#################################

forecasts_wn = forecast(wn_model, h = 5)
forecasts_arima = forecast(bestModel, h = 5)

# Plot forecasts
x1 = fertilityRate$year
y1 = fertilityRate$value

fore_wn = exp(tail(log_train_firstDiff, 1) + cumsum(forecasts_wn$mean))
y2 = c(y1[1:length(train$value)], fore_wn)

fore_arima = exp(tail(log_train_firstDiff, 1) + cumsum(forecasts_arima$mean))
y3 = c(y1[1:length(train$value)], fore_arima)

plot(x1, y2, ylim = c(0,max(y1)), type = "l", col = "red",
     main = "Actual vs Forecasted",
     xlab = "Fertility Rate",
     ylab = "Year")
lines(x1, y3, type = "l", col = "green")
lines(x1, y1, type = "l", col = "blue")

legend(x = "topright",
       cex = 0.5,
       inset = 0.05,
       title = "Legend",
       legend = c("White Noise", "ARIMA(0, 1, 1)", "Actual"),
       fill = c("red", "green", "blue"))

# plot magnified forecasts
plot(x1, y2, xlim = c(2010, 2018), ylim = c(0,2), type = "l", col = "red",
     main = "Actual vs Forecasted (Magnified)",
     xlab = "Fertility Rate",
     ylab = "Year")
lines(x1, y3, type = "l", col = "green")
lines(x1, y1, type = "l", col = "blue")

legend(x = "topleft",
       cex = 0.5,
       inset = 0.05,
       title = "Legend",
       legend = c("White Noise", "ARIMA(0, 1, 1)", "Actual"),
       fill = c("red", "green", "blue"))
