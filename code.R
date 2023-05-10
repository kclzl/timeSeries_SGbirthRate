# read data + EDA
data = read.csv("./data/data.csv", header = TRUE)
library(ggplot2)
ggplot(data, aes(x = year, y = value, group = level_1, color = level_1)) +
  geom_line() +
  labs(title = "Line Plot of Three Categories", x = "Year", y = "Value", color = 'Type')

fertilityRate = subset(data, level_1 == "Total Fertility Rate")
fertilityRate = fertilityRate[, c(1,3)]
plot(x = fertilityRate$year, y = fertilityRate$value, type = "l",
     main = "Total Fertility Rate from 1960 to 2018",
     xlab = "Per Female",
     ylab = "Year")

# ACF and PACF of raw data
acf(fertilityRate[, c(2)])
pacf(fertilityRate[, c(2)])

# log transformation
log_fertilityRate = log(fertilityRate$value)

# log first difference
firstDiff_log = diff(log_fertilityRate, difference = 1)
plot(firstDiff_log, type = "l", main = "first difference")

# test stationarity
library(tseries)
adf.test(firstDiff_log)

# ACF and PACF of log firstDiff
acf(firstDiff_log)
pacf(firstDiff_log)

# Periodogram
spec <- spec.pgram(firstDiff_log, taper = 0, log = "no", spans = 2)
plot(spec$freq, spec$spec, type = "l", xlab = "Frequency", ylab = "Periodogram")

max_index <- which.max(spec$spec)
max_freq <- spec$freq[max_index]

period <- 1/max_freq
period

# searchParamsARIMA
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

# searchParamsSARIMA
searchParamsSARIMA <- function(pList, dList, qList, PList, DList, QList, sList, metric, ts){
  bestScore <- 0
  bestParams <- c(0,0,0,0,0,0,0)
  metric = metric
  for (p in pList){
    for (d in dList){
      for (q in qList){
        for (P in PList){
          for (D in DList){
            for (Q in QList){
              for (s in sList){
                params = c(p, d, q)
                seasons = c(P, D, Q)
                periods = s
                sModel = arima(ts, order = params, seasonal = list(order = seasons, period = periods))
                score = metric(sModel)
                if (score < bestScore) {
                  bestScore = score
                  bestParams = c(p,d,q)
                  bestSeasons = c(P, D, Q)
                  bestPeriod = s
                }
              }
            }
          }
        }
      }
    }
  }
  msg <- sprintf("Best Params (p, d, q, P, D, Q, s): %d, %d, %d, %d, %d, %d, %d with score of %0.3f",
                 bestParams[1], bestParams[2], bestParams[3],
                 bestSeasons[1], bestSeasons[2], bestSeasons[3],
                 bestPeriod = s,
                 bestScore)
  print(msg)
}

# params
pList = c(11,12,13)
dList = c(1)
qList = c(12,13)
PList = c(11, 12, 13)
DList = c(1)
QList = c(11, 12)
sList = c(1)
m = AIC
ts = firstDiff_log
# aic model
searchParamsSARIMA(pList, dList, qList, PList, DList, QList, sList, metric = m, ts)
# bic model
m = BIC
searchParamsSARIMA(pList, dList, qList, PList, DList, QList, sList, metric = m, ts)




