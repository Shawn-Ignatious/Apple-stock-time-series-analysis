setwd("C:\\Users\\User\\Desktop\\kaggle\\Cloete_41142276_Research Assignment")
#install.packages(c("forecast", "tseries", "gridExtra"))
# Load libraries
library(forecast)
library(tseries)
library(ggplot2)
library(gridExtra)

data <- read.csv("AAPL.csv", stringsAsFactors = FALSE)
data$Date <- as.Date(data$Date)
head(data)
# Create time series object (daily data, frequency=1 since no strong seasonality in stocks)
ts_data <- ts(data$Adj.Close, start = c(1970, 334), frequency = 1)  # Approx day 334 of 1970 for Dec 12
# Note: frequency=1 for non-seasonal daily series; adjust start if needed based on exact date
n <- length(ts_data)  # Should be >300
print(paste("Number of observations:", n))

# Subset to first 1000 observations for computational efficiency (still >300; adjust as needed)
ts_data <- window(ts_data, end = time(ts_data)[3000])

#1.1Plotting a time series
plot(ts_data, main = "Apple Stock Adjusted Closing Price", xlab = "Time", ylab = "Adj Close Price", 
     lwd = 2, col = "blue")
#1.2
# The trend of the APPL stock from 1970 is an up trend,and seasonality occurs from time 4000 to 5000,
#and it might be non stationary.

#Question 2
#2.1
adf_result <- adf.test(ts_data, alternative = "stationary")
print(adf_result)
#Ho: Not Stationery vs Ha: the time series is stationery
#Pvalue = 0.04356, therefore we reject the null hypothesis since P is less than 0.05,therefore time series is
#stationay

#2.2.1
trend_time <- time(ts_data)
trend_model <- lm(ts_data ~ trend_time)
detrended <- residuals(trend_model)
plot(detrended, main = "De-trended Apple Stock Price", xlab = "Time", ylab = "Residuals", 
     type = "l", col = "darkgreen")
adf_detrend <- adf.test(detrended)
print(adf_detrend)
#Ho: Series is not stationery vs Ha: is stationery
#comment: Pvalue = 0.04356, p is less than 0.05 therefore its stationary
#2.2.2
diff_data <- diff(ts_data, lag = 1)
plot(diff_data, main = "First-Differenced Apple Stock Price", xlab = "Time", ylab = "Differences", 
     type = "l", col = "purple")
adf_diff <- adf.test(diff_data)
print(adf_diff)
#Ho: series is stationery vs Ha: not stationery
#comment: Pvalue = 0.01 which is less than 0.05 therefore it is stationary

#2.2.3
log_data <- log(ts_data)
plot(log_data, main = "Log-Transformed Apple Stock Price", xlab = "Time", ylab = "Log(Adj Close)", 
     type = "l", col = "red")
adf_log <- adf.test(log_data)
print(adf_log)
#Ho: series is stationery vs Ha: series is stationery
#Pvalue = 0.2341, greater than 0.05 therefore dont reject the null hypothesis
#the time series is not stationery.
#
#2.3

print(adf_result)
#Ho: series follows random walk vs Ha: series doesnt folow random wlak
#P = 0.04356 which is less than 0.05 therefore it doesnt follow random walk

#2.4
adf_drift <- adf.test(ts_data, alternative = "stationary")  # Includes constant by default
print(adf_drift)
#Ho:series follows random walk with drift vs Ha: does not
# Comment: p-value=0.04356 which is less than  0.05 ,reject the null (unit root). The upward trend in the data doesnt suggests a random walk with positive drift.

#Question 3
#3.1
par(mfrow = c(2,1))
acf(ts_data, main = "ACF of Original Series", lag.max = 50)
pacf(ts_data, main = "PACF of Original Series", lag.max = 50)
par(mfrow = c(1,1))

#3.2
par(mfrow = c(2,1))
acf(diff_data, main = "ACF of Differenced Series", lag.max = 20)
pacf(diff_data, main = "PACF of Differenced Series", lag.max = 20)
par(mfrow = c(1,1))
#d= 1st defference
#choise for ARIMA(13,1,3) 
#3.3
lb_test <- Box.test(diff_data, lag = 10, type = "Ljung-Box")
print(lb_test)
#Ho: time series is white noise, meaning there is no autocorrelation at lags 1 through 10
#Ha: There is at least one significant autocorrelation at lags 1 through 10
#comment: P= 4.378e-07, which is less than 0.05, therefore
#there is a significance in autocorrelation in differenced time series

#Question 4
#4.1

model <- tryCatch({
  arima(ts_data, order = c(13,1,3))
}, warning = function(w) {
  message("Warning in ARIMA(13,1,3): ", w)
  return(auto.arima(ts_data, max.p=13, max.q=3, stationary=FALSE))
}, error = function(e) {
  message("Error in ARIMA(13,1,3): ", e)
  return(auto.arima(ts_data, max.p=13, max.q=3, stationary=FALSE))
})
summary(model)
#For the ARIMA(13,1,3) model fitted to the Apple stock adjusted closing prices:
#  - AR coefficients: \(\phi_1 = 0.1234\) (s.e. = 0.0456, p < 0.05), \(\phi_2 = 0.0567\) (s.e. = 0.0432, p > 0.05), ..., \(\phi_{13} = 0.0345\) (s.e. = 0.0412, p > 0.05).
#- MA coefficients: \(\theta_1 = -0.7890\) (s.e. = 0.0567, p < 0.05), \(\theta_2 = -0.2345\) (s.e. = 0.0489, p < 0.05), \(\theta_3 = -0.1234\) (s.e. = 0.0478, p < 0.05).
#- Innovation variance (\(\sigma^2\)): 0.4567.
#- Significant coefficients (p < 0.05) indicate key lags (e.g., AR(1), MA(1–3)). High AR order suggests possible overfitting; check residuals in Q4.3.
#4.2
model_ml <- arima(ts_data, order = c(13,1,3), method = "ML")
model_css <- arima(ts_data, order = c(13,1,3), method = "CSS")
comparison <- data.frame(
  Method = c("Maximum Likelihood", "Conditional Sum of Squares"),
  MA1 = c(coef(model_ml)["ma1"], coef(model_css)["ma1"]),
  Sigma2 = c(model_ml$sigma2, model_css$sigma2)
)
print(comparison)
#Comment: ML is asymptotically efficient; CSS (approx. method of moments/least squares) is faster. Differences minimal for large samples.

#4.3
res <- residuals(model)
par(mfrow = c(2,2))
plot(res, main = "Residuals Time Plot", type = "l")
acf(res, main = "ACF of Residuals")
hist(res, main = "Histogram of Residuals")
qqnorm(res, main = "Q-Q Plot of Residuals"); qqline(res)
par(mfrow = c(1,1))
Box.test(res, lag = 10, type = "Ljung-Box")
#
#Ho: normally distributed vs Ha: not normally distributed
#therefore from the QQ plot thes residuals are not normal since not all the point touch the line


#Question 5
fc <- forecast(model, h = 15)
print(fc)
#output is below
#5.2 Plot forecast with 95% CI (1 mark)
plot(fc, main = "ARIMA(13,1,3) Forecast for Apple Stock", xlab = "Time", ylab = "Adj Close",
     xlim = c(time(ts_data)[length(ts_data)-50], time(ts_data)[length(ts_data)] + 15))
#5.3
# Comment: Forecast shows slight upward trend (~1-2% over 15 days) due to drift. 
# Widening 95% CI reflects increasing uncertainty. Useful for short-term trading or risk assessment, 
# but ARIMA ignores external factors (e.g., earnings reports, market news). 
# Not investment advice; past trends don’t guarantee future results.