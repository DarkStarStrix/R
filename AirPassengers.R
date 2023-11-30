# Do a Time series analysis on the AirPassengers data set

# Do a exploratory analysis of the data
summary(AirPassengers)

# Plot the data
plot(AirPassengers)

# Create a new variable
Passengers01 <- ifelse(AirPassengers > median(AirPassengers), 1, 0)

# Create a new data frame
newdata <- data.frame(Passengers01 = 1)

# Install the logistf package if not already installed
if (!require(logistf)) {
  install.packages("logistf")
}


# Load the logistf package
library(logistf)

# Create a new variable
Passengers01 <- ifelse(AirPassengers > median(AirPassengers), 1, 0)

# Create a data frame
df <- data.frame(Passengers = as.numeric(AirPassengers), Passengers01 = Passengers01)

# Create a logistic regression model using Firth's method
model <- logistf(Passengers01 ~ Passengers, data = df)

# Show the model
summary(model)

# Plot the model
plot(model)

# do a time series analysis
library(forecast)

# create a time series object
ts <- ts(AirPassengers, frequency = 12, start = c(1949, 1), end = c(1960, 12))

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

# primt the accuracy of the model
accuracy(model)

# plot the forecast
plot(forecast(model))

# predict the time series
predict(model)

# plot the predicted time series
plot(predict(model))

# plot the time series
plot(ts)

