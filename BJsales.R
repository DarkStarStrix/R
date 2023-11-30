# analyze from the BJsales dataset do a time series analysis

# load the data
data(BJsales)

# create a time series object
ts <- ts(BJsales, frequency = 4, start = c(2001, 1), end = c(2004, 4))

# plot the time series
plot(ts)

# Forecast the time series
forecast(ts)

# Plot the forecast
plot(forecast(ts))

# predict the time series
predict(ts)

# Plot the predicted time series
plot(predict(ts))

# model
model <- auto.arima(ts)

# show the model
summary(model)

# plot the model
plot(model)

# print the accuracy of the model
accuracy(model)

# plot the forecast
plot(forecast(model))

# predict the time series
predict(model)

# plot the predicted time series
plot(predict(model))

# plot the time series
plot(ts)

# analyze from the BJsales.lead dataset do a time series analysis
BJsales.lead

# create a time series object
ts <- ts(BJsales.lead, frequency = 4, start = c(2001, 1), end = c(2004, 4))

# plot the time series
plot(ts)

# Forecast the time series
forecast(ts)

# Plot the forecast
plot(forecast(ts))

# predict the time series
predict(ts)

# Plot the predicted time series
plot(predict(ts))

# model
model <- auto.arima(ts)

# show the model
summary(model)

# plot the model
plot(model)

# print the accuracy of the model
accuracy(model)

# plot the forecast
plot(forecast(model))

# predict the time series
predict(model)

# plot the predicted time series
plot(predict(model))

# plot the time series
plot(ts)

# tighten the model
model <- auto.arima(ts, stepwise = FALSE, approximation = FALSE)

# show the model
summary(model)

# plot the model
plot(model)

# Forecast the time series with the tightened model and predict the time series
forecast(model)
predict(model)

# plot the forecast
plot(forecast(model))

# plot the predicted time series
plot(predict(model))

# plot the time series
plot(ts)

# print sales for 2004
def <- ts(BJsales.lead, frequency = 4, start = c(2004, 1), end = c(2004, 4))

# plot the time series
plot(def)

# Forecast the time series
forecast(def)

# Plot the forecast
plot(forecast(def))

# predict the time series
predict(def)

# Plot the predicted time series
plot(predict(def))

# forecast to 2010
forecast(model, h = 24)

# predict to 2010
predict(model, n.ahead = 24)

# plot the forecast
plot(forecast(model, h = 24))

# plot the predicted time series
plot(predict(model, n.ahead = 24))

# plot the time series
plot(ts)

# it looks like the model is overfitting the data so we will try to tighten the model
model <- auto.arima(ts, stepwise = FALSE, approximation = FALSE)

# show the model
summary(model)

# Forecast the time series
forecasts <- forecast(model)

# Plot the forecasts
plot(forecasts)

# the model is still overfitting the data so we will try to tighten the model again do it iteratively
model <- auto.arima(ts, stepwise = FALSE, approximation = FALSE)

# show the model
summary(model)

# Forecast the time series
forecasts <- forecast(model)

# Plot the forecasts
plot(forecasts)
