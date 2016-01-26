###############################################################################
# 413_Assignment_02.R
# Last updated: 2016-01-16 by MJG
###############################################################################

# Clear workspace
rm(list=ls())

# Set working directory
setwd("D:/Personal/School/MSPA/413-DL/Data Sets")

# Load packages
library(fpp)
library(fBasics)
library(fUnitRoots)

# Source backtest function, currently stored in:
# D:/Personal/School/MSPA/413-DL/Code/Functions/413_Function_backtest.R

#==============================================================================
# Problem 1
#==============================================================================

# Read data
da <- read.table("m-umcsent.txt", header = T)

# Assign values
cs <- da$VALUE

# Convert to time series
ts.cs <- ts(cs, start = c(1978,1), frequency = 12)

# Explore summary stats, head, tail
summary(cs)
head(cs)
tail(cs)

#======================================
# Q1A
#======================================

# Plot the time series
plot(ts.cs, ylab = "Values", xlab = "Year", 
     main = "Consumer Sentiment (University of Michigan)
     Monthly: January 1978 - August 2013")

# Plot the ACF
# Appears linear, suggests differencing is required
acf(cs)

# Plot the PACF
pacf(cs)

#======================================
# Q1B
#======================================

# Find unit root in series - multiple ways to try this
# If p-value is > alpha, then fail to reject null that series is unit root
# non-stationary (a unit root is present), suggests differencing is required
# (page 91 of Intro TS)
# Note: Method 1 and Method 3.3 produce same results

# Method 1 - Using Augmented Dickey-Fuller test
adf.test(cs, alternative = "stationary")

# Method 2 - Using ndiffs(), returns value of 1
ndiffs(cs, test = "adf")

# Method 3:
# 3.1 - No intercept (constant), no time trend
adfTest(cs, lags = 7, type = "nc")
# 3.2 - Intercept (constant), no time trend
adfTest(cs, lags = 7, type = "c")
# 3.3 - Intercept (constant), time trend
adfTest(cs, lags = 7, type = "ct")

#------------------
# Determine if second-differenced is required:
#------------------
# Assign first differenced series
cs.diff <- diff(cs)

# Method 1 - check against first differenced
adf.test(diff(cs), alternative = "stationary")

# Method 2 - check against first differenced, returns a value of 0
ndiffs(diff(cs), test = "adf")

# Plot the PACF - first differenced 
pacf(cs.diff)
# Select lag = 5 (or, model is of order 5)

# Repeat ADF test at differenced lag
adf.test(cs.diff, alternative = "stationary", k = 5)

# Plot the ACF - first differenced
acf(cs.diff)

#======================================
# Q1C
#======================================

# Test the hypothesis that expected change (first differenced) of CS is zero
# versus alternative that it's non-zero.
t.test(cs.diff)

#======================================
# Q1D
#======================================

# Continue using the first differenced (change) series
# Test the null hypothesis that the lag-k autocorrelation (pk) of p1 through
# p12 = 0, versus alternative that it's non-zero for some i E[1, 12]

# Note: if Ha wasn't testing E[1, 12], then would test at 24 as well:
# 12 and 24 are important for *monthly* time series
# Using m = ln(T) may be a better choice

Box.test(cs.diff, lag = 12, type = "Ljung")

#==============================================================================
# Problem 2
#==============================================================================

#======================================
# Q2A
#======================================

# Use the AR command with MLE to determine the order of the first differenced
# series (page 63 of Intro TS)
cs.diff.m1 <- ar(cs.diff, method = "mle")
cs.diff.m1$order

# Plot the ACF
acf(cs.diff, main = "ACF of Consumer Sentiment
    (first differenced)")

#======================================
# Q2B
#======================================

# Build an AR model based on the selected order for the change series;
# perform model checking to validate the fitted model
# Use include.mean = F because the model uses a differenced series

# Model using first differenced series:
cs.diff.m2 <- arima(cs.diff, order = c(5, 0, 0), include.mean = F)
tsdiag(cs.diff.m2)

# Model using raw series, specifying differencing
cs.diff.m3 <- arima(cs, order = c(5, 1, 0), include.mean = T)
tsdiag(cs.diff.m3)

# Look *very* similar, but not identical:
cs.diff.m2$residuals == cs.diff.m3$residuals

# Length of residuals differs:
length(cs.diff.m2$residuals)
length(cs.diff.m3$residuals)

#======================================
# Q2C
#======================================

# Does the model imply the existence of business cycles in consumer sentiment?
# Need to view the polynomial root (pages 56-58 of Intro TS)

# Set up the polynomial
cs.diff.m2.poly <- c(1, -cs.diff.m2$coef)

# Solve the equation
cs.diff.m2.root <- polyroot(cs.diff.m2.poly); cs.diff.m2.root

# Obtain absolute value (modulus)
Mod(cs.diff.m2.root)

#======================================
# Q2D
#======================================

# Obtain 1-step to 4-step ahead point and 95% interval forecasts for the change
# series of consumer sentiment at the forecast origin August 1, 2013 (see 
# page 78 of Intro TS)

# Predict 1-step to 4-step ahead
cs.diff.m2.fc <- predict(cs.diff.m2, 4)

# Result is list of 2: prediction and standard error
names(cs.diff.m2.fc)
cs.diff.m2.fc$pred
cs.diff.m2.fc$se

# Create confidence intervals @ 95%

# Lower confidence interval
cs.diff.m2.fc.lcl <- cs.diff.m2.fc$pred + 
    (qnorm(0.025, lower.tail = T) * cs.diff.m2.fc$se)
cs.diff.m2.fc.lcl

# Upper confidence interval
cs.diff.m2.fc.ucl <- cs.diff.m2.fc$pred + 
    (qnorm(0.025, lower.tail = F) * cs.diff.m2.fc$se)
cs.diff.m2.fc.ucl


# Plot the series
# Note: this works but looks terrible
cs.diff.plot <- ts(cs.diff, start = c(1978,1), frequency = 12)
plot(cs.diff.plot, ylab = "Values", xlab = "Months", 
     main = "Consumer Sentiment - First Differenced
     1-point to 4-point ahead forecast")
lines(ts(cs.diff.m2.fc$pred, start = c(2013,9), frequency = 12), col = 2)
lines(ts(cs.diff.m2.fc.lcl, start = c(2013,9), frequency = 12), col = 4)
lines(ts(cs.diff.m2.fc.ucl, start = c(2013,9), frequency = 12), col = 4)

#==============================================================================
# Problem 3
#==============================================================================

#======================================
# Q3A
#======================================

# Simplify the fitted AR model of Problem 2 by removing parameter estimates 
# with t-ratio less than 1.2 in absolute; use the fixed subcommand

# View the parameter estimates (coefficients)
cs.diff.m2$coef

# Two ways of doing this, will produce similar results:

#------------------
# Method 1
#------------------
# Test the t-ratio of each, remove if abs(x) < 1.2
cs.diff.m2.se <- sqrt(diag(vcov(cs.diff.m2))); cs.diff.m2.se
cs.diff.m2.tratio <- abs(cs.diff.m2$coef / cs.diff.m2.se); cs.diff.m2.tratio
#------------------

#------------------
# Method 2
#------------------
# Test the t-ratio of each, remove if abs(x) < 1.2
abs((cs.diff.m2$coef[1]) * sqrt(length(cs.diff.m2$residuals)))   # Remove
abs((cs.diff.m2$coef[2]) * sqrt(length(cs.diff.m2$residuals)))   # Keep
abs((cs.diff.m2$coef[3]) * sqrt(length(cs.diff.m2$residuals)))   # Keep
abs((cs.diff.m2$coef[4]) * sqrt(length(cs.diff.m2$residuals)))   # Remove
abs((cs.diff.m2$coef[5]) * sqrt(length(cs.diff.m2$residuals)))   # Keep
#------------------

# Set the new model
cs.diff.m4 <- arima(cs.diff, order = c(5, 0, 0), include.mean = F,
                    fixed = c(0, NA, NA, 0, NA))

#======================================
# Q3B & Q3C
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

#------------------
# Model two 
#------------------
# Summary Stats
accuracy(cs.diff.m2); summary(cs.diff.m2); tsdiag(cs.diff.m2)

# Pormanteau test or Ljung-Box test
# Set fitdf = p + q from arima(order = (p, d, q))
Box.test(residuals(cs.diff.m2), lag = 12, fitdf = 5, type = "Ljung")

# ACF & PACF of residuals - first lag removed
acf(cs.diff.m2$residuals, 25, xlim = c(1, 25), ylim = c(-0.2, 1))
pacf(cs.diff.m2$residuals, 25, ylim = c(-0.2, 1))

# Plot
plot(forecast(cs.diff.m2))
#------------------

#------------------
# Model four
#------------------
# Summary Stats
accuracy(cs.diff.m4); summary(cs.diff.m4); tsdiag(cs.diff.m4)

# Pormanteau test
# Set fitdf = p + q from arima(order = (p, d, q))
Box.test(residuals(cs.diff.m4), lag = 12, fitdf = 5, type = "Ljung")

# ACF & PACF of residuals - first lag removed
acf(cs.diff.m4$residuals, 25, xlim = c(1, 25), ylim = c(-0.2, 1))
pacf(cs.diff.m4$residuals, 25, ylim = c(-0.2, 1))

# Plot
plot(forecast(cs.diff.m4))
#------------------

#======================================
# Q3D
#======================================

# Does the model imply the existence of business cycles in consumer sentiment?
# Need to view the polynomial root (pages 56-58 of Intro TS)

# Set up the polynomial
cs.diff.m4.poly <- c(1, -cs.diff.m4$coef)

# Solve the equation
cs.diff.m4.root <- polyroot(cs.diff.m4.poly); cs.diff.m4.root

# Obtain absolute value (modulus)
Mod(cs.diff.m4.root)

#======================================
# Q3E
#======================================

# Use backtest to compare the two models (M2 & M4) with forecast origin at
# t = 380. Which model is preferred?

# Backtest - M2
backtest(cs.diff.m2, cs.diff, 380, 1, inc.mean = F)

# Backtest - M4
backtest(cs.diff.m4, cs.diff, 380, 1, inc.mean = F, fixed = c(0, NA, NA, 0, NA))

###############################################################################
# FIN
###############################################################################
