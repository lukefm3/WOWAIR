# Load required packages
library(forecast)

# Load example dataset
data(AirPassengers)

# Convert dataset to a time series object
ts_data <- ts(AirPassengers, frequency = 12)

# Fit an ARIMA model to the data
fit <- auto.arima(ts_data)

# Forecast the next 9 months
forecast_values <- forecast(fit, h = 9)

# Plot the actual values and forecasted values
plot(ts_data, main = "Air Passengers", ylab = "Passengers (in thousands)", xlab = "Year")
lines(forecast_values$mean, col = "blue")
lines(forecast_values$upper[, "95%"], col = "red", lty = "dashed")
lines(forecast_values$lower[, "95%"], col = "red", lty = "dashed")
legend("topright", legend = c("Actual", "Predicted"), col = c("black", "blue"), lty = c(1,1), cex = 0.8)



library(ggplot2)
library(forecast)

data("AirPassengers")

# Through the end of 1956, but no months in 1957
air_train <- window(AirPassengers, end = 1956+11/12)

# everything in 1957 and on
air_test  <- window(AirPassengers, start = 1957)

n_test <- length(air_test)

# //////////////////////////////////////////////////////////////////////////////

# Fit model to first few years of AirPassengers data
air_model <- auto.arima(air_train)

# So we get an Arima model back
air_model
#> Series: air_train 
#> ARIMA(1,1,0)(1,1,0)[12] 
#> 
#> Coefficients:
#>           ar1     sar1
#>       -0.2250  -0.2274
#> s.e.   0.1076   0.1081
#> 
#> sigma^2 estimated as 92.5:  log likelihood=-304.98
#> AIC=615.97   AICc=616.27   BIC=623.22

# You can look at the "performance", but this is the in sample performance
# i.e. it is computing performance using the data that it was fit with
accuracy(air_model)
#>                     ME     RMSE      MAE       MPE     MAPE      MASE
#> Training set 0.4424643 8.834477 6.351387 0.1376786 2.870884 0.2174955
#>                     ACF1
#> Training set 0.003854251

# Its an "Arima" object
class(air_model)
#> [1] "ARIMA" "Arima"

# //////////////////////////////////////////////////////////////////////////////

# We could do "multistep" forecasts for as many days are in the test set
# This repeatedly feeds the predicted data point back into the prediction
# equation to get the next prediction
air_multi_forecast <- air_model %>%
  forecast(h = n_test)

air_multi_forecast %>%
  autoplot()

accuracy(air_multi_forecast, x = air_test)
#>                     ME      RMSE       MAE       MPE     MAPE      MASE
#> Training set 0.4424643  8.834477  6.351387 0.1376786 2.870884 0.2174955
#> Test set     6.8490404 26.471644 19.501243 0.8396151 4.405293 0.6677963
#>                     ACF1 Theil's U
#> Training set 0.003854251        NA
#> Test set     0.675272576 0.5053222

# And we can plot this
air_multi_forecast %>%
  autoplot() +
  geom_line(
    aes(
      x = as.numeric(time(air_test)),
      y = as.numeric(air_test)
    ),
    col = "red"
  )


# Best way to do this is going to be to create a TS dataframe
# First Column is the Actual
# Second Column is the CausalImpact
# Third Column is the ARIMA
# 4th Column is the Difference between Causal Impact & Arima
# 5th Column is the difference between ARIMA 





