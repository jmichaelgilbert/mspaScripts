###############################################################################
# 413_Assignment_05.R
# Last updated: 2016-02-03 by MJG
###############################################################################

# Clear workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/michael.gilbert/Dropbox/MSPA/413-DL/Data Sets")

# Load packages
library(fpp)
library(fBasics)
library(fUnitRoots)

# Source backtest function, currently stored in:
# C:/Users/michael.gilbert/Dropbox/MSPA/413-DL/Code/Functions

#==============================================================================
# Problem 1
#==============================================================================

# Read data
da <- read.table("m-FamaFrench.txt", header = T)

# Assign values
ff <- da$hml

# Explore summary stats
basicStats(ff)

# Convert to time series
ts.ff <- ts(ff, start = c(1961,1), frequency = 12)

# Plot the time series
plot(ts.ff, xlab = "Year", ylab = "Returns of HML", 
     main = "Simple Returns of Factor HML")

par(mfcol = c(2, 2))

# ACF & PACF
acf(ff)
pacf(ff)

# ACF & PACF - first differenced
acf(diff(ff))
pacf(diff(ff))

par(mfcol = c(1, 1))

#======================================
# Q1A
#======================================

# Determine if differencing is required
ndiffs(ff, test = "adf")
adf.test(ff, alternative = "stationary")

# Build a time series model for the data; write down the fitted model
ff.m1 <- arima(ff, order = c(0, 0, 1))
ff.m1

#======================================
# Q1B
#======================================

# Is the model adequate? Why?

# Comments on Pormanteau test or Ljung-Box test:
# Large p-value suggests residuals are white noise (uncorrelated)
# If the result is significant (p-value < alpha), reject null hypothesis of 
# model adequacy
# If there is no correlation in your residuals, then you have sufficiently 
# modeled everything
# If both results suggest model adequacy, use AIC or BIC as the next criterion
# for model selection; these also consider parsimony

# Summary stats
summary(ff.m1)
tsdiag(ff.m1, gof = 25)

# Pormanteau test or Ljung-Box test & plot
# Set fitdf = p + q from arima(order = (p, d, q))
# Monthly series so test lags at 12 and 24
Box.test(residuals(ff.m1), lag = 12, fitdf = 1, type = "Ljung")
Box.test(residuals(ff.m1), lag = 24, fitdf = 1, type = "Ljung")
# Plot; start at value of fitdf()
plot(sapply(1:100, function(i) Box.test(residuals(ff.m1), lag = i, 
                                         fitdf = 1)$p.value), type = "l")

# ACF & PACF of residuals - first lag removed
par(mfcol = c(2, 1))
acf(ff.m1$residuals, 25, xlim = c(1, 25), ylim = c(-0.2, 0.4))
pacf(ff.m1$residuals, 25, ylim = c(-0.2, 0.4))
par(mfcol = c(1, 1))

# Plot
plot(forecast(ff.m1))

# Removing coefficients: t-ratio
# Test the t-ratio of each, remove if abs(x) < 1.96

# Compute the standard error (se)
ff.m1.se <- sqrt(diag(vcov(ff.m1))); ff.m1.se

# Now calculate the t-ratio
# Use 1.96 as cutoff - 5% significance level
ff.m1.tratio <- abs(ff.m1$coef / ff.m1.se); ff.m1.tratio

#======================================
# Q1C
#======================================

# Obtain 1-step to 2-step ahead point and 95% interval forecasts for the change
# series of consumer sentiment at the forecast origin November, 2013 (see 
# page 78 of Intro TS)

# Predict 1-step to 2-step ahead
ff.m1.fc <- predict(ff.m1, 2)

# Result is list of 2: prediction and standard error
names(ff.m1.fc)
ff.m1.fc$pred
ff.m1.fc$se

# Create confidence intervals @ 95%

# Lower confidence level (or interval)
ff.m1.fc.lcl <- ff.m1.fc$pred + (qnorm(0.025, lower.tail = T) * ff.m1.fc$se)
ff.m1.fc.lcl

# Upper confidence level (or interval)
ff.m1.fc.ucl <- ff.m1.fc$pred + (qnorm(0.025, lower.tail = F) * ff.m1.fc$se)
ff.m1.fc.ucl

#==============================================================================
# Problem 2
#==============================================================================

# Read data
da <- read.table("m-PastorStambaugh.txt", header = T)

# Assign values
ps <- da$PS_LEVEL

# Explore summary stats
basicStats(ps)

# Convert to time series
ts.ps <- ts(ps, start = c(1962,8), frequency = 12)

# Plot the time series
plot(ts.ps, xlab = "Year", ylab = "Measure", 
     main = "Monthly Market Liquidity Measure
     (Professors Pastor & Stambaugh")

par(mfcol = c(2, 2))

# ACF & PACF
acf(ps)
pacf(ps)

# ACF & PACF - first differenced
acf(diff(ps))
pacf(diff(ps))

par(mfcol = c(1, 1))

#======================================
# Q2A
#======================================

# Determine if differencing is required
ndiffs(ps, test = "adf")
adf.test(ps, alternative = "stationary")

# Build a time series model for the data; write down the fitted model
ps.m1 <- arima(ps, order = c(5, 0, 0))
ps.m1

#======================================
# Q2B
#======================================

# Is the model adequate? Why?

# Comments on Pormanteau test or Ljung-Box test:
# Large p-value suggests residuals are white noise (uncorrelated)
# If the result is significant (p-value < alpha), reject null hypothesis of 
# model adequacy
# If there is no correlation in your residuals, then you have sufficiently 
# modeled everything
# If both results suggest model adequacy, use AIC or BIC as the next criterion
# for model selection; these also consider parsimony

# Summary stats
summary(ps.m1)
tsdiag(ps.m1, gof = 25)

# Pormanteau test or Ljung-Box test & plot
# Set fitdf = p + q from arima(order = (p, d, q))
# Monthly series so test lags at 12 and 24
Box.test(residuals(ps.m1), lag = 12, fitdf = 5, type = "Ljung")
Box.test(residuals(ps.m1), lag = 24, fitdf = 5, type = "Ljung")
# Plot; start at value of fitdf()
plot(sapply(5:100, function(i) Box.test(residuals(ps.m1), lag = i, 
                                        fitdf = 5)$p.value), type = "l")

# ACF & PACF of residuals - first lag removed
par(mfcol = c(2, 1))
acf(ps.m1$residuals, 25, xlim = c(1, 25), ylim = c(-0.2, 0.4))
pacf(ps.m1$residuals, 25, ylim = c(-0.2, 0.4))
par(mfcol = c(1, 1))

# Plot
plot(forecast(ps.m1))

# Removing coefficients: t-ratio
# Test the t-ratio of each, remove if abs(x) < 1.96

# Compute the standard error (se)
ps.m1.se <- sqrt(diag(vcov(ps.m1))); ps.m1.se

# Now calculate the t-ratio
# Use 1.96 as cutoff - 5% significance level
ps.m1.tratio <- abs(ps.m1$coef / ps.m1.se); ps.m1.tratio

#======================================
# Q2C
#======================================

# Identify the largest outlier in the series
# Refine the fitted model (Q2B) by using an indicator for the outlier
which.min(ps.m1$residuals)
which.max(ps.m1$residuals)

# Identify value of outliers
abs(ps.m1$residuals[303])
abs(ps.m1$residuals[441])

# Create empty data set to remove outlier with arima(xreg = )
# xreg requires a vector or matrix with the same number of rows as x
length(ps)
i303 <- rep(0,605)
i303[303] <- 1

# Refit the model
ps.m2 <- arima(ps, order = c(5, 0, 0), xreg = i303)
ps.m2

# Summary stats
summary(ps.m2)
tsdiag(ps.m2, gof = 25)

# Pormanteau test or Ljung-Box test & plot
# Set fitdf = p + q from arima(order = (p, d, q))
# Monthly series so test lags at 12 and 24
Box.test(residuals(ps.m2), lag = 12, fitdf = 5, type = "Ljung")
Box.test(residuals(ps.m2), lag = 24, fitdf = 5, type = "Ljung")
# Plot; start at value of fitdf()
plot(sapply(5:100, function(i) Box.test(residuals(ps.m2), lag = i, 
                                        fitdf = 5)$p.value), type = "l")

# ACF & PACF of residuals - first lag removed
par(mfcol = c(2, 1))
acf(ps.m2$residuals, 25, xlim = c(1, 25), ylim = c(-0.2, 0.4))
pacf(ps.m2$residuals, 25, ylim = c(-0.2, 0.4))
par(mfcol = c(1, 1))

# Removing coefficients: t-ratio
# Test the t-ratio of each, remove if abs(x) < 1.96

# Compute the standard error (se)
ps.m2.se <- sqrt(diag(vcov(ps.m2))); ps.m2.se

# Now calculate the t-ratio
# Use 1.96 as cutoff - 5% significance level
ps.m2.tratio <- abs(ps.m2$coef / ps.m2.se); ps.m2.tratio

#======================================
# Q2D
#======================================

# Fix least significant parameter to zero (lowest t-ratio)
fixed <- c(NA, NA, NA, 0, NA, NA, NA)
ps.m3 <- arima(ps, order = c(5, 0, 0), xreg = i303, fixed = fixed)
ps.m3

# Summary stats
summary(ps.m3)
tsdiag(ps.m3, gof = 25)

# Pormanteau test or Ljung-Box test & plot
# Set fitdf = p + q from arima(order = (p, d, q))
# Monthly series so test lags at 12 and 24
Box.test(residuals(ps.m3), lag = 12, fitdf = 5, type = "Ljung")
Box.test(residuals(ps.m3), lag = 24, fitdf = 5, type = "Ljung")
# Plot; start at value of fitdf()
plot(sapply(5:100, function(i) Box.test(residuals(ps.m3), lag = i, 
                                        fitdf = 5)$p.value), type = "l")

# ACF & PACF of residuals - first lag removed
par(mfcol = c(2, 1))
acf(ps.m3$residuals, 25, xlim = c(1, 25), ylim = c(-0.2, 0.4))
pacf(ps.m3$residuals, 25, ylim = c(-0.2, 0.4))
par(mfcol = c(1, 1))

# Removing coefficients: t-ratio
# Test the t-ratio of each, remove if abs(x) < 1.96

# Compute the standard error (se)
ps.m3.se <- sqrt(diag(vcov(ps.m2))); ps.m2.se

# Now calculate the t-ratio
# Use 1.96 as cutoff - 5% significance level
ps.m3.tratio <- abs(ps.m2$coef / ps.m2.se); ps.m2.tratio

#==============================================================================
# Problem 3
#==============================================================================

# Read data
da <- read.table("q-earn-msft.txt", header = T)

# Assign values
ms <- da$value

# Create log returns
ms.log <- log(ms)

# Explore summary stats
basicStats(ms.log)

# Convert to time series
ts.ms.log <- ts(ms.log, start = c(1986,2), frequency = 4)

# Plot the time series
plot(ts.ms.log, xlab = "Year", ylab = "Log Value", 
     main = "Microsoft Quarterly Earnings
     1986-Q2 to 2013-Q3")

par(mfcol = c(2, 2))

# ACF & PACF
acf(ms.log)
pacf(ms.log)

# ACF & PACF - first differenced
acf(diff(ms.log))
pacf(diff(ms.log))

par(mfcol = c(1, 1))

#======================================
# Q3A
#======================================

# Determine if differencing is required
ndiffs(ms.log, test = "adf")
adf.test(ms.log, alternative = "stationary")
adf.test(diff(ms.log), alternative = "stationary")

# First differenced series
ms.log.diff <- diff(ms.log)

# Build a time series model for the data; write down the fitted model
# Use include.mean = F because the model uses a differenced series
ms.log.diff.m1 <- arima(ms.log.diff, order = c(1, 0, 1), include.mean = F)
ms.log.diff.m1

# Summary stats
summary(ms.log.diff.m1)
tsdiag(ms.log.diff.m1, gof = 25)

# Pormanteau test or Ljung-Box test & plot
# Set fitdf = p + q from arima(order = (p, d, q))
# Quarterly series so test lags at 4 and 8
Box.test(residuals(ms.log.diff.m1), lag = 4, fitdf = 2, type = "Ljung")
Box.test(residuals(ms.log.diff.m1), lag = 8, fitdf = 2, type = "Ljung")
# Plot; start at value of fitdf()
plot(sapply(2:100, function(i) Box.test(residuals(ms.log.diff.m1), lag = i, 
                                        fitdf = 2)$p.value), type = "l")

# ACF & PACF of residuals - first lag removed
par(mfcol = c(2, 1))
acf(ms.log.diff.m1$residuals, 25, xlim = c(1, 25), ylim = c(-0.2, 0.4))
pacf(ms.log.diff.m1$residuals, 25, ylim = c(-0.2, 0.4))
par(mfcol = c(1, 1))

# Removing coefficients: t-ratio
# Test the t-ratio of each, remove if abs(x) < 1.96

# Compute the standard error (se)
ms.log.diff.m1.se <- sqrt(diag(vcov(ms.log.diff.m1))); ms.log.diff.m1.se

# Now calculate the t-ratio
# Use 1.96 as cutoff - 5% significance level
ms.log.diff.m1.tratio <- abs(ms.log.diff.m1$coef / ms.log.diff.m1.se)
ms.log.diff.m1.tratio

# Notes: Reject null hypothesis of model adequacy
# tsdiag(), Box.test, ACF, PACF of residuals all suggest this
# p-values < alpha

#======================================
# Q3B
#======================================

# Fit the model to the series; write down the fitted model
ms.log.m1 <- arima(ms.log, order = c(0, 1, 1), 
                   seasonal = list(order = c(0, 0, 1), period = 4))
ms.log.m1

# Summary stats
summary(ms.log.m1)
tsdiag(ms.log.m1, gof = 25)

# Pormanteau test or Ljung-Box test & plot
# Set fitdf = p + q from arima(order = (p, d, q))
# Quarterly series so test lags at 4 and 8
Box.test(residuals(ms.log.m1), lag = 4, fitdf = 2, type = "Ljung")
Box.test(residuals(ms.log.m1), lag = 8, fitdf = 2, type = "Ljung")
# Plot; start at value of fitdf()
plot(sapply(2:100, function(i) Box.test(residuals(ms.log.m1), lag = i, 
                                        fitdf = 2)$p.value), type = "l")

# ACF & PACF of residuals - first lag removed
par(mfcol = c(2, 1))
acf(ms.log.m1$residuals, 25, xlim = c(1, 25), ylim = c(-0.2, 0.4))
pacf(ms.log.m1$residuals, 25, ylim = c(-0.2, 0.4))
par(mfcol = c(1, 1))

# Removing coefficients: t-ratio
# Test the t-ratio of each, remove if abs(x) < 1.96

# Compute the standard error (se)
ms.log.m1.se <- sqrt(diag(vcov(ms.log.m1))); ms.log.m1.se

# Now calculate the t-ratio
# Use 1.96 as cutoff - 5% significance level
ms.log.m1.tratio <- abs(ms.log.m1$coef / ms.log.m1.se)
ms.log.m1.tratio

#======================================
# Q3C
#======================================

# Compare the two series and determine preference based on fit

#======================================
# Q3D
#======================================

# Use backtest to compare models; determine preference based on fit

# Q3A
backtest(ms.log.diff.m1, ms.log.diff, 81, 1)

# Q3B
backtest(ms.log.m1, ms.log, 81, 1)

#==============================================================================
# Problem 4
#==============================================================================

# Read data
da <- read.table("m-FamaBlissdbndyields.txt", header = T)

# Assign values
y1 <- da$yield1
y3 <- da$yield3

# Explore summary stats
basicStats(y1)
basicStats(y3)

#======================================
# Q4A
#======================================

# Fit the linear regression model with y3 as the dependent variable and y1 as
# the independent variable
bond.m1 <- lm(y3 ~ y1)
bond.m1

# Summary stats
summary(bond.m1)

# ACF & PACF of residuals - first lag removed
par(mfcol = c(2, 1))
acf(bond.m1$residuals, 25, xlim = c(1, 25), ylim = c(-0.2, 1.0))
pacf(bond.m1$residuals, 25, ylim = c(-0.2, 1.0))
par(mfcol = c(1, 1))

#======================================
# Q4B
#======================================

# Determine if differencing is required:

# Yield 1
ndiffs(y1, test = "adf")
adf.test(y1, alternative = "stationary")
adf.test(diff(y1), alternative = "stationary")

# Yield 3
ndiffs(y3, test = "adf")
adf.test(y3, alternative = "stationary")
adf.test(diff(y3), alternative = "stationary")

# Fit a linear regression model as in Q4A, but using first differenced series
y1.diff <- diff(y1)
y3.diff <- diff(y3)

# Use of -1 excludes the constant
bond.m2 <- lm(y3.diff ~ (-1 + y1.diff))

# Summary stats
summary(bond.m2)

# ACF & PACF of residuals - first lag removed
par(mfcol = c(2, 1))
acf(bond.m2$residuals, 25, xlim = c(1, 25), ylim = c(-0.2, 0.2))
pacf(bond.m2$residuals, 25, ylim = c(-0.2, 0.2))
par(mfcol = c(1, 1))

#======================================
# Q4C
#======================================

# Is the model in Q4B adequate? If not, refine the model and write it down

# Determine order of model
bond.m3 <- ar(bond.m2$residuals, method = "mle")
bond.m3$order

#------------------
# Model 4
#------------------
# Fit the model to the series; write down the fitted model
bond.m4 <- arima(y3.diff, order = c(5, 0, 0), xreg = y1.diff, include.mean = F)
bond.m4

# Summary stats
summary(bond.m4)
tsdiag(bond.m4, gof = 25)

# Pormanteau test or Ljung-Box test & plot
# Set fitdf = p + q from arima(order = (p, d, q))
# Monthly series so test lags at 12 and 24
Box.test(residuals(bond.m4), lag = 12, fitdf = 5, type = "Ljung")
Box.test(residuals(bond.m4), lag = 24, fitdf = 5, type = "Ljung")
# Plot; start at value of fitdf()
plot(sapply(5:100, function(i) Box.test(residuals(bond.m4), lag = i, 
                                        fitdf = 5)$p.value), type = "l")

# ACF & PACF of residuals - first lag removed
par(mfcol = c(2, 1))
acf(bond.m4$residuals, 25, xlim = c(1, 25), ylim = c(-0.2, 0.2))
pacf(bond.m4$residuals, 25, ylim = c(-0.2, 0.2))
par(mfcol = c(1, 1))

# Removing coefficients: t-ratio
# Test the t-ratio of each, remove if abs(x) < 1.96

# Compute the standard error (se)
bond.m4.se <- sqrt(diag(vcov(bond.m4))); bond.m4.se

# Now calculate the t-ratio
# Use 1.96 as cutoff - 5% significance level
bond.m4.tratio <- abs(bond.m4$coef / bond.m4.se)
bond.m4.tratio

#------------------
# Model 5
#------------------
# Fit the model to the series; write down the fitted model
fixed <- c(NA, NA, 0, NA, NA, NA)
bond.m5 <- arima(y3.diff, order = c(5, 0, 0), xreg = y1.diff, include.mean = F,
                 fixed = fixed)
bond.m5

# Summary stats
summary(bond.m5)
tsdiag(bond.m5, gof = 25)

# Pormanteau test or Ljung-Box test & plot
# Set fitdf = p + q from arima(order = (p, d, q))
# Monthly series so test lags at 12 and 24
Box.test(residuals(bond.m5), lag = 12, fitdf = 5, type = "Ljung")
Box.test(residuals(bond.m5), lag = 24, fitdf = 5, type = "Ljung")
# Plot; start at value of fitdf()
plot(sapply(5:100, function(i) Box.test(residuals(bond.m5), lag = i, 
                                        fitdf = 5)$p.value), type = "l")

# ACF & PACF of residuals - first lag removed
par(mfcol = c(2, 1))
acf(bond.m5$residuals, 25, xlim = c(1, 25), ylim = c(-0.2, 0.2))
pacf(bond.m5$residuals, 25, ylim = c(-0.2, 0.2))
par(mfcol = c(1, 1))

# Removing coefficients: t-ratio
# Test the t-ratio of each, remove if abs(x) < 1.96

# Compute the standard error (se)
bond.m5.se <- sqrt(diag(vcov(bond.m5))); bond.m5.se

# Now calculate the t-ratio
# Use 1.96 as cutoff - 5% significance level
bond.m5.tratio <- abs(bond.m5$coef / bond.m5.se)
bond.m5.tratio

#======================================
# Q4D
#======================================

# Based on the refined model, describe the linear dependence between the yields

#==============================================================================
# Problem 5
#==============================================================================

#======================================
# Q5A
#======================================

# Fit an AR(6) model to y3.diff, use y1.diff as an explanatory variable
# Write down the fitted model and include the intercept
bond.m6 <- arima(y3, order = c(6, 0, 0), xreg = y1)
bond.m6

# Summary stats
summary(bond.m6)
tsdiag(bond.m6, gof = 25)

# Pormanteau test or Ljung-Box test & plot
# Set fitdf = p + q from arima(order = (p, d, q))
# Monthly series so test lags at 12 and 24
Box.test(residuals(bond.m6), lag = 12, fitdf = 6, type = "Ljung")
Box.test(residuals(bond.m6), lag = 24, fitdf = 6, type = "Ljung")
# Plot; start at value of fitdf()
plot(sapply(6:100, function(i) Box.test(residuals(bond.m6), lag = i, 
                                        fitdf = 6)$p.value), type = "l")

# ACF & PACF of residuals - first lag removed
par(mfcol = c(2, 1))
acf(bond.m6$residuals, 25, xlim = c(1, 25), ylim = c(-0.2, 0.2))
pacf(bond.m6$residuals, 25, ylim = c(-0.2, 0.2))
par(mfcol = c(1, 1))

# Removing coefficients: t-ratio
# Test the t-ratio of each, remove if abs(x) < 1.96

# Compute the standard error (se)
bond.m6.se <- sqrt(diag(vcov(bond.m6))); bond.m6.se

# Now calculate the t-ratio
# Use 1.96 as cutoff - 5% significance level
bond.m6.tratio <- abs(bond.m6$coef / bond.m6.se)
bond.m6.tratio

#======================================
# Q5B & Q5C
#======================================

# Set insignificant coefficients to zero; write down the fitted model
fixed <- c(NA, 0, NA, NA, 0, NA, NA, NA)
bond.m7 <- arima(y3, order = c(6, 0, 0), xreg = y1, fixed = fixed)
bond.m7

# Summary stats
summary(bond.m7)
tsdiag(bond.m7, gof = 25)

# Pormanteau test or Ljung-Box test & plot
# Set fitdf = p + q from arima(order = (p, d, q))
# Monthly series so test lags at 12 and 24
Box.test(residuals(bond.m7), lag = 12, fitdf = 6, type = "Ljung")
Box.test(residuals(bond.m7), lag = 24, fitdf = 6, type = "Ljung")
# Plot; start at value of fitdf()
plot(sapply(6:100, function(i) Box.test(residuals(bond.m7), lag = i, 
                                        fitdf = 6)$p.value), type = "l")

# ACF & PACF of residuals - first lag removed
par(mfcol = c(2, 1))
acf(bond.m7$residuals, 25, xlim = c(1, 25), ylim = c(-0.2, 0.2))
pacf(bond.m7$residuals, 25, ylim = c(-0.2, 0.2))
par(mfcol = c(1, 1))

# Removing coefficients: t-ratio
# Test the t-ratio of each, remove if abs(x) < 1.96

# Compute the standard error (se)
bond.m7.se <- sqrt(diag(vcov(bond.m7))); bond.m7.se

# Now calculate the t-ratio
# Use 1.96 as cutoff - 5% significance level
bond.m7.tratio <- abs(bond.m7$coef / bond.m7.se)
bond.m7.tratio

#======================================
# Q5D & Q5E
#======================================

# Does the model imply the existence of business cycles in consumer sentiment?
# Need to view the polynomial root (pages 56-58 of Intro TS)
# If results contain complex roots, suggest the existence of business cycles

# Set up the polynomial
# Only use first six coefficients; last two are intercept + additional variable
bond.m7.poly <- c(1, -bond.m7$coef[1:6])

# Solve the equation
bond.m7.root <- polyroot(bond.m7.poly); bond.m7.root

# Obtain absolute value (modulus)
Mod(bond.m7.root)

###############################################################################
# FIN
###############################################################################