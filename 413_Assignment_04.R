###############################################################################
# 413_Assignment_04.R
# Last updated: 2016-02-01 by MJG
###############################################################################

# Clear workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/Michael/Dropbox/MSPA/413-DL/Data Sets")

# Load packages
library(fpp)
library(fBasics)
library(fUnitRoots)

# Source backtest function, currently stored in:
# C:/Users/Michael/Dropbox/MSPA/413-DL/Code/Functions

#==============================================================================
# Problem 1
#==============================================================================

# Read data
da <- read.table("m-unempmean.txt", header = T)

# Assign values
unemp <- da$Value

# Explore summary stats, head, tail
summary(unemp)
head(unemp)
tail(unemp)

# Convert to time series
ts.unemp <- ts(unemp, start = c(1948,1), frequency = 12)

# Plot the time series
plot(ts.unemp, xlab = "Year", ylab = "Unemployment Rate",
     main = "Mean Duration of Unemployment
     (measured in weeks)")

par(mfcol = c(2, 2))

# ACF & PACF
acf(unemp)
pacf(unemp)

# ACF & PACF - first differenced
acf(diff(unemp))
pacf(diff(unemp))

par(mfcol = c(1, 1))

#======================================
# Q1A
#======================================

# Does the series have a unit root?

# Find unit root in series - multiple ways to try this
# If p-value is > alpha, then fail to reject null that series is unit root
# non-stationary (a unit root is present), suggests differencing is required
# (page 91 of Intro TS)

#------------------
# Method 1 - Using Augmented Dickey-Fuller test
# Returns lags (order) = 9
adf.test(unemp, alternative = "stationary")
#------------------

#------------------
# Method 2 - Using Augmented Dickey-Fuller test

# Determine order of model
unemp.diff.m1 <- ar(diff(unemp), method = "mle")
unemp.diff.m1$order

# STL - determine type of unit root regression to specify
plot(stl(ts.unemp, s.window = "periodic", robust = T),
     main = "Mean Duration of Unemployment
     (series decomposition)")

# Appears constant with no time trend, so type = "c"
adfTest(unemp, lags = 12, type = "c")
#------------------

#------------------
# Method 3 - Using ndiffs(), returns value of 1
ndiffs(unemp, test = "adf")
#------------------

# Determine if second-differenced is required:

#------------------
# Method 1
adf.test(diff(unemp), alternative = "stationary")
#------------------

#------------------
# Method 2
adfTest(diff(unemp), lags = 12, type = "c")
#------------------

#------------------
# Method 3
ndiffs(diff(unemp), test = "adf")
#------------------

#======================================
# Q1B
#======================================

# Plot the ACF - first differenced
acf(diff(unemp))

# Plot the PACF - first differenced 
pacf(diff(unemp))

# Repeat ADF test - first differenced
adfTest(diff(unemp), lags = 12, type = "c")

# Assign first differenced series
unemp.diff <- diff(unemp)

# Test the hypothesis that expected change (first differenced) of UNEMP is zero
# versus alternative that it's non-zero.
t.test(unemp.diff)

#======================================
# Q1C
#======================================

# Building AR model - use lag = 12 from ADF test in Q1A
unemp.diff.m2 <- arima(unemp.diff, order = c(12, 0, 0), include.mean = F)

# Is the model adequate? Why?
# Compare models and check fit

# Comments on Pormanteau test or Ljung-Box test:
# Large p-value suggests residuals are white noise (uncorrelated)
# If the result is significant (p-value < alpha), reject null hypothesis of 
# model adequacy
# If there is no correlation in your residuals, then you have sufficiently 
# modeled everything
# If both results suggest model adequacy, use AIC or BIC as the next criterion
# for model selection; these also consider parsimony

# Summary Stats
summary(unemp.diff.m2)
tsdiag(unemp.diff.m2, gof = 24)

# Pormanteau test or Ljung-Box test & plot
# Set fitdf = p + q from arima(order = (p, d, q))
Box.test(residuals(unemp.diff.m2), lag = 24, fitdf = 12, type = "Ljung")
plot(sapply(12:100, function(i) Box.test(residuals(unemp.diff.m2), lag = i, 
                                        fitdf = 12)$p.value), type = "l")

# ACF & PACF of residuals - first lag removed
acf(unemp.diff.m2$residuals, 25, xlim = c(1, 25), ylim = c(-0.2, 1))
pacf(unemp.diff.m2$residuals, 25, ylim = c(-0.2, 1))

# Plot
plot(forecast(unemp.diff.m2))

# Removing coefficients: t-ratio
# Test the t-ratio of each, remove if abs(x) < 1.96

# Two ways of getting the standard error (se)
unemp.diff.m2.se <- sqrt(diag(vcov(unemp.diff.m2))); unemp.diff.m2.se
unemp.diff.m2.se <- sqrt(diag(unemp.diff.m2$var.coef)); unemp.diff.m2.se

# Now calculate the t-ratio
unemp.diff.m2.tratio <- abs(unemp.diff.m2$coef / unemp.diff.m2.se)
unemp.diff.m2.tratio

#======================================
# Q1D
#======================================

# Write down the fitted model
unemp.diff.m2

#======================================
# Q1E
#======================================

# Fit a seasonal model to the data
unemp.diff.m3 <- arima(unemp.diff, order = c(2, 0, 1), 
                       seasonal = list(order = c(1, 0, 1), period = 12),
                       include.mean = F)

#======================================
# Q1F & Q1G
#======================================

# Is the model adequate? Why?
# Compare models and check fit

# Comments on Pormanteau test or Ljung-Box test:
# Large p-value suggests residuals are white noise (uncorrelated)
# If the result is significant (p-value < alpha), reject null hypothesis of 
# model adequacy
# If there is no correlation in your residuals, then you have sufficiently 
# modeled everything
# If both results suggest model adequacy, use AIC or BIC as the next criterion
# for model selection; these also consider parsimony

# Summary Stats
summary(unemp.diff.m3)
tsdiag(unemp.diff.m3, gof = 24)

# Pormanteau test or Ljung-Box test & plot
# Set fitdf = p + q from arima(order = (p, d, q))
Box.test(residuals(unemp.diff.m3), lag = 12, fitdf = 5, type = "Ljung")
Box.test(residuals(unemp.diff.m3), lag = 24, fitdf = 5, type = "Ljung")
plot(sapply(7:100, function(i) Box.test(residuals(unemp.diff.m3), lag = i, 
                                        fitdf = 5)$p.value), type = "l")

# ACF & PACF of residuals - first lag removed
acf(unemp.diff.m3$residuals, 25, xlim = c(1, 25), ylim = c(-0.2, 1))
pacf(unemp.diff.m3$residuals, 25, ylim = c(-0.2, 1))

# Plot
plot(forecast(unemp.diff.m3))

#======================================
# Q1H
#======================================

# Use backtest to compare the two models (M1 & M2) with forecast origin at
# t = 750; which model is preferred?

# Backtest - M1
backtest(unemp.diff.m2, unemp.diff, 750, 1, inc.mean = F)

# Backtest - M2
backtest(unemp.diff.m3, unemp.diff, 750, 1, inc.mean = F)

#==============================================================================
# Problem 2
#==============================================================================

# Read data
da <- read.table("w-coilwtico.txt", header = T)

# Assign values
oil <- da$Value

# Explore summary stats, head, tail
summary(oil)
head(oil)
tail(oil)

# Convert to time series
ts.oil <- ts(oil, start = c(1986,1), frequency = 52)

# Plot the time series
plot(ts.oil, xlab = "Year", ylab = "Price",
     main = "Weekly WTI Crude Prices
     (per barrel)")

par(mfcol = c(2, 2))

# ACF & PACF
acf(oil)
pacf(oil)

# ACF & PACF - first differenced
acf(diff(oil))
pacf(diff(oil))

par(mfcol = c(1, 1))

#======================================
# Q2A
#======================================

# Create log oil prices
oil.log <- log(oil)

# Take first difference of log oil prices
oil.log.diff <- diff(oil.log)

Box.test(oil.log.diff, lag = 10, type = "Ljung")

#======================================
# Q2B
#======================================

# Three parts:

#------------------
# Part 1:
#------------------
par(mfcol = c(2, 2))

# ACF & PACF - log
acf(oil.log)
pacf(oil.log)

# ACF & PACF - log & first differenced
acf(oil.log.diff)
pacf(oil.log.diff)

par(mfcol = c(1, 1))

#------------------
# Part 2:
#------------------
# Determine order of model
oil.log.diff.m1 <- ar(oil.log.diff, method = "mle")
oil.log.diff.m1$order

oil.log.diff.m2 <- arima(oil.log.diff, order = c(11,0,0))
oil.log.diff.m2

# Summary Stats
summary(oil.log.diff.m2)
tsdiag(oil.log.diff.m2)

# Pormanteau test or Ljung-Box test
# Set fitdf = p + q from arima(order = (p, d, q))
# Since this is a weekly series, start at 52, and see how large the lag value
# must be before it is less than alpha (fail to reject H0: of model adequacy)

# Loop testing
p <- 1
alpha <- 0.05
start <- 52
while(p >= alpha){
    start <- start + 1
    oil.log.diff.m2.bt <- Box.test(residuals(oil.log.diff.m2), lag = start, 
                                   fitdf = 11, type = "Ljung")
    p <- oil.log.diff.m2.bt$p.value
}
print(oil.log.diff.m2.bt)

# Pormanteau test or Ljung-Box test & plot
Box.test(residuals(oil.log.diff.m2), lag = 52, fitdf = 11, type = "Ljung")
Box.test(residuals(oil.log.diff.m2), lag = 104, fitdf = 11, type = "Ljung")
plot(sapply(0:1200, function(i) Box.test(residuals(oil.log.diff.m2), lag = i,
                                         fitdf = 11)$p.value), type = "l")

# ACF & PACF of residuals - first lag removed
acf(oil.log.diff.m2$residuals, 25, xlim = c(1, 25), ylim = c(-0.2, 1))
pacf(oil.log.diff.m2$residuals, 25, ylim = c(-0.2, 1))

# Plot
plot(forecast(oil.log.diff.m2))

# Removing coefficients: t-ratio
# Test the t-ratio of each, remove if abs(x) < 1.96

# Two ways of getting the standard error (se)
oil.log.diff.m2.se <- sqrt(diag(vcov(oil.log.diff.m2))); oil.log.diff.m2
oil.log.diff.m2.se <- sqrt(diag(oil.log.diff.m2$var.coef)); oil.log.diff.m2

# Now calculate the t-ratio
# Remove coefficients with values < 1.96 (5% significance level)
oil.log.diff.m2.tratio <- abs(oil.log.diff.m2$coef / oil.log.diff.m2.se)
oil.log.diff.m2.tratio

#------------------
# Part 3:
#------------------
# Rebuild model with coefficients removed
oil.log.diff.m3 <- arima(oil.log.diff, order = c(11, 0, 0), include.mean = F,
                         fixed = c(NA, NA, NA, 0, 0, 0, 0, NA, 0, 0, NA))
oil.log.diff.m3

# Summary Stats
summary(oil.log.diff.m3)
tsdiag(oil.log.diff.m3)

# Pormanteau test or Ljung-Box test & plot
Box.test(residuals(oil.log.diff.m3), lag = 52, fitdf = 11, type = "Ljung")
Box.test(residuals(oil.log.diff.m3), lag = 104, fitdf = 11, type = "Ljung")
plot(sapply(11:100, function(i) Box.test(residuals(oil.log.diff.m3), lag = i,
                                         fitdf = 11)$p.value), type = "l")

# ACF & PACF of residuals - first lag removed
acf(oil.log.diff.m3$residuals, 25, xlim = c(1, 25), ylim = c(-0.2, 1))
pacf(oil.log.diff.m3$residuals, 25, ylim = c(-0.2, 1))

# Plot
plot(forecast(oil.log.diff.m3))

#======================================
# Q2C
#======================================

# Fit another model using:
oil.log.diff.m4 = arima(oil.log.diff, order = c(3, 0, 2), include.mean = F)

#======================================
# Q2D
#======================================

# Based on in-sample fit, which model is preferred?

# Model 1
summary(oil.log.diff.m3)

# Model 2
summary(oil.log.diff.m4)

###############################################################################
# FIN
###############################################################################
