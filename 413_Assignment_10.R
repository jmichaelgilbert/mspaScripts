###############################################################################
# 413_Assignment_10.R
# Last updated: 2016-03-14 by MJG
###############################################################################

# Clear workspace
rm(list=ls())

# Load packages
library(fitdistrplus)
library(forecast)
library(MTS)
library(quantmod)
library(rmgarch)

# Source functions
setwd("C:/Users/Michael/Dropbox/MSPA/413-DL/Code/Functions")
source("413_Function_ccm.R")
source("413_Function_covEWMA.R")
source("413_Function_mq.R")

#==============================================================================
# Problem 1
#==============================================================================

# Download daily stock prices for the following companies: Valero Energy (VLO)
#   and Chevron Corporation (CVX) and compute the log returns using the date
#   range January 1, 2011 - December 31, 2015.

# Download price series - use daily adjusted closing prices
getSymbols("VLO", from = "2011-01-01", to = "2016-01-31")
getSymbols("CVX", from = "2011-01-01", to = "2016-01-31")

# Use adjusted prices, merge series, compute continuously compounded returns
# Drop first observation (NA), rename columns
returns <- diff(log1p(merge(VLO$VLO.Adjusted['2011-01-01::2015-12-31'],
                            CVX$CVX.Adjusted['2011-01-01::2015-12-31'])))
returns <- returns[-1]
colnames(returns) <- c("VLO", "CVX")

# Repeat for January 2016 data
returns.jan <- diff(log1p(merge(VLO$VLO.Adjusted['2015-12-31::2016-01-31'],
                                CVX$CVX.Adjusted['2015-12-31::2016-01-31'])))
returns.jan <- returns.jan[-1]
colnames(returns.jan) <- c("VLO", "CVX")

# Merge data for later use
returns.full <- rbind(returns, returns.jan)

#--------------------------------------
# Assign vectors of data
#--------------------------------------

#------------------
# VLO
#------------------

# Historical
returns.vlo <- as.vector(returns$VLO)

# January
returns.vlo.jan <- as.vector(returns.jan$VLO)

#------------------
# CVX
#------------------

# Historical
returns.cvx <- as.vector(returns$CVX)

# January
returns.cvx.jan <- as.vector(returns.jan$CVX)

#-------------------------------------
# Plots
#-------------------------------------

#------------------
# Prices
#------------------

# VLO
plot(VLO$VLO.Adjusted, main = "Adjusted Daily Closing Prices: Valero Energy
     2011-01-03 to 2015-12-31")

# CVX
plot(CVX$CVX.Adjusted, main = "Adjusted Daily Closing Prices: Chevron
     2011-01-03 to 2015-12-31")

#------------------
# Returns
#------------------

# VLO
plot(returns$VLO, main = "Adjusted Daily Closing Log Returns: Valero Energy
     2011-01-04 to 2015-12-31")

# CVX
plot(returns$CVX, main = "Adjusted Daily Closing Log Returns: Chevron
     2011-01-04 to 2015-12-31")

#------------------
# Scatterplot
#------------------

plot(coredata(returns$VLO), coredata(returns$CVX), xlab = "VLO", ylab = "CVX",
     type = "p", pch = 16, lwd = 2, col = "blue",
     main = "Scatterplot of Adjusted Daily Closing Log Returns
     2011-01-04 to 2015-12-31")
abline(h = 0, v = 0)

#==============================================================================
# Q1A
#==============================================================================

# Compute the sample mean, standard deviation, skewness, excess kurtosis,
#   minimum, and maximum of the log returns for each series.

# Explore summary stats
basicStats(returns)

#==============================================================================
# Q1B
#==============================================================================

# Test the null hypothesis that the mean of each of the series log returns is
#   zero. Also, construct a 95% confidence interval for the daily log returns
#   of each stock.
# Test H0: mu = 0 vs. Ha: mu != 0, where mu denotes mean of the log returns

# VLO
t.test(returns.vlo)

# CVX
t.test(returns.cvx)

#==============================================================================
# Q1C
#==============================================================================

# Third Moment: Skewness
# Test H0: m3 = 0 vs. Ha: m3 != 0, where m3 denotes skewness of the log returns
# Testing the symmetry of the series with respect to the mean
# Compute p-value

# VLO
returns.vlo.skew <- skewness(returns.vlo)/sqrt(6/length(returns.vlo))
returns.vlo.skew
returns.vlo.skew.pv <- 2*(1-pnorm(abs(returns.vlo.skew))); returns.vlo.skew.pv

# CVX
returns.cvx.skew <- skewness(returns.cvx)/sqrt(6/length(returns.cvx))
returns.cvx.skew
returns.cvx.skew.pv <- 2*(1-pnorm(abs(returns.cvx.skew))); returns.cvx.skew.pv

#==============================================================================
# Q1D
#==============================================================================

# Fourth Moment: Kurtosis
# Test H0: m4 = 3 vs. Ha: m4 != 3, where m4 denotes kurtosis of the log returns
# Testing the tail behavior of the series with respect to the mean
# Compute p-value

# VLO
returns.vlo.kurt <- kurtosis(returns.vlo)/sqrt(24/length(returns.vlo))
returns.vlo.kurt
returns.vlo.kurt.pv <- 2*(1-pnorm(abs(returns.vlo.kurt))); returns.vlo.kurt.pv

# CVX
returns.cvx.kurt <- kurtosis(returns.cvx)/sqrt(24/length(returns.cvx))
returns.cvx.kurt
returns.cvx.kurt.pv <- 2*(1-pnorm(abs(returns.cvx.kurt))); returns.cvx.kurt.pv

#==============================================================================
# Q1E
#==============================================================================

# Obtain the empirical density plot of the daily log returns of each series, 
#   and select an appropriate distribution (Gaussian, t, etc.).

#------------------
# VLO
#------------------

# Fit Distribution
plot(fitdist(returns.vlo, "norm", method = "mle"), histo = F, demp = T)
plot(fitdist(returns.vlo, "cauchy", method = "mle"), histo = F, demp = T)

# Density
returns.vlo.edp <- density(returns.vlo)
plot(returns.vlo.edp$x, returns.vlo.edp$y, xlab = "Log Returns", 
     ylab = "Density", main = "Empirical Density Plot: Valero Energy (VLO)", 
     sub = "Normal Distribution Overlay (blue)", type = "l", lwd = 2)
xfit <- seq(min(returns.vlo), max(returns.vlo), length = 100)
yfit <- dnorm(xfit, mean = mean(returns.vlo), sd = sd(returns.vlo))
lines(xfit, yfit, col = "blue", lwd = 2)

# Q-Q Plot
qqnorm(returns.vlo, main = "Normal Q-Q Plot: Valero Energy (VLO)")
qqline(returns.vlo)

# Kolmogorov-Smirnoff test
ks.test(returns.vlo, "pnorm", mean(returns.vlo), sd(returns.vlo))

# Shapiro-Wilk test
shapiro.test(returns.vlo)

#------------------
# CVX
#------------------

# Fit Distribution
plot(fitdist(returns.cvx, "norm", method = "mle"), histo = F, demp = T)
plot(fitdist(returns.cvx, "cauchy", method = "mle"), histo = F, demp = T)

# Density
returns.cvx.edp <- density(returns.cvx)
plot(returns.cvx.edp$x, returns.cvx.edp$y, xlab = "Log Returns", 
     ylab = "Density", main = "Empirical Density Plot: Chevron (CVX)", 
     sub = "Normal Distribution Overlay (blue)", type = "l", lwd = 2)
xfit <- seq(min(returns.cvx), max(returns.cvx), length = 100)
yfit <- dnorm(xfit, mean = mean(returns.cvx), sd = sd(returns.cvx))
lines(xfit, yfit, col = "blue", lwd = 2)

# Q-Q Plot
qqnorm(returns.cvx, main = "Normal Q-Q Plot: Chevron (CVX)")
qqline(returns.cvx)

# Kolmogorov-Smirnoff test
ks.test(returns.cvx, "pnorm", mean = mean(returns.cvx), sd = sd(returns.cvx))

# Shapiro-Wilk test
shapiro.test(returns.cvx)

#==============================================================================
# Q1F
#==============================================================================

# Use the Box-Jenkins methodology to perform univariate time series model
#   fitting to each of the series. Include details of each step of the process,
#   and support your final model selection for each series.

# Notes on choosing a model:
#   - Choose MA or AR based on ACF and PACF patterns
#   - If ACF exponentially decays and PACF cuts off, then fit an AR model
#   - If ACF cuts off and PACF exponentialy decays, then fit a MA model
#   - Or try all (AR, MA, and ARMA) models and compare AIC or other criteria

#------------------------------------------------------------------------------
# VLO
#------------------------------------------------------------------------------

#--------------------------------------
# Model Identification
#--------------------------------------

# Apply ADF test - check for unit-root nonstationarity
# If p-value > alpha, then fail to reject H0: that series is unit-root
#   nonstationary (i.e. a unit-root is likely present); suggests differencing
#   is required
adf.test(returns.vlo, alternative = "stationary")
ndiffs(returns.vlo)
nsdiffs(returns.vlo)

# Are there serial correlations in the log return?
# ACF plot of series for checking serial correlations in series
# ACF plot of abs(series) for checking dependence in series
par(mfcol = c(2, 1))
acf(returns.vlo, 25, xlim = c(1, 25), ylim = c(-0.2, 0.2))
acf(abs(returns.vlo), 25, xlim = c(1, 25), ylim = c(-0.2, 0.2))
par(mfcol = c(1, 1))

# Ljung-Box test
Box.test(returns.vlo, lag = 10, type = "Ljung")

#------------------
# Model Order
#------------------

# Examine ACF & PACF - first lag removed
# Suggests ARMA(6, 6)
par(mfcol = c(2, 1))
acf(returns.vlo, 25, xlim = c(1, 25), ylim = c(-0.1, 0.1))
pacf(returns.vlo, 25, ylim = c(-0.1, 0.1))
par(mfcol = c(1, 1))

# Examine EACF table
# Suggests ARMA(0, 0)
library(TSA, warn.conflicts = F, quietly = T)
eacf(returns.vlo, ar.max = 15, ma.max = 15)
detach("package:TSA", unload = T)

# Use auto.arima()
# Suggests ARMA(2, 2) based on AIC
auto.arima(returns.vlo, stationary = T, trace = T, approx = F, allowmean = F)

#--------------------------------------
# Parameter Estimation
#--------------------------------------

#------------------
# Model 1
#------------------

# Multiple issues with model fit - NaN values in S.E.
returns.vlo.m1 <- Arima(returns.vlo, order = c(6, 0, 6), include.mean = F)
returns.vlo.m1

# Multiple issues with model fit - NaN values in S.E.
returns.vlo.m1 <- Arima(returns.vlo, order = c(6, 0, 5), include.mean = F)
returns.vlo.m1

# Multiple issues with model fit - NaN values in S.E.
returns.vlo.m1 <- Arima(returns.vlo, order = c(5, 0, 6), include.mean = F)
returns.vlo.m1

# So far so good - no NaN values in S.E.
returns.vlo.m1 <- Arima(returns.vlo, order = c(5, 0, 5), include.mean = F)
returns.vlo.m1

# Removing coefficients: t-ratio
# Test the t-ratio of each, remove if abs(x) < 1.96

# Calculate standard error
returns.vlo.m1.se <- sqrt(diag(vcov(returns.vlo.m1))); returns.vlo.m1.se

# Now calculate the t-ratio
returns.vlo.m1.tratio <- abs(returns.vlo.m1$coef / returns.vlo.m1.se)
returns.vlo.m1.tratio

# Fix coefficients to zero
# **Model Breaks**
returns.vlo.m1 <- Arima(returns.vlo, order = c(5, 0, 5), include.mean = F,
                        fixed = c(0, 0, NA, NA, NA, 0, 0, NA, NA, NA),
                        transform.pars = F)
returns.vlo.m1

# Multiple issues with model fit - NaN values in S.E.
returns.vlo.m1 <- Arima(returns.vlo, order = c(5, 0, 4), include.mean = F)
returns.vlo.m1

# Multiple issues with model fit - NaN values in S.E.
returns.vlo.m1 <- Arima(returns.vlo, order = c(4, 0, 5), include.mean = F)
returns.vlo.m1

# So far so good - no NaN values in S.E.
returns.vlo.m1 <- Arima(returns.vlo, order = c(4, 0, 4), include.mean = F)
returns.vlo.m1

# Removing coefficients: t-ratio
# Test the t-ratio of each, remove if abs(x) < 1.96

# Calculate standard error
returns.vlo.m1.se <- sqrt(diag(vcov(returns.vlo.m1))); returns.vlo.m1.se

# Now calculate the t-ratio
returns.vlo.m1.tratio <- abs(returns.vlo.m1$coef / returns.vlo.m1.se)
returns.vlo.m1.tratio

# Fix coefficients to zero
# **IT'S ALIVE**
returns.vlo.m1 <- Arima(returns.vlo, order = c(4, 0, 4), include.mean = F,
                        fixed = c(NA, 0, NA, NA, NA, 0, NA, NA),
                        transform.pars = F)
returns.vlo.m1

#------------------
# Model 2
#------------------

returns.vlo.m2 <- Arima(returns.vlo, order = c(0, 0, 0), include.mean = F)
returns.vlo.m2

#------------------
# Model 3
#------------------

returns.vlo.m3 <- Arima(returns.vlo, order = c(2, 0, 2), include.mean = F)
returns.vlo.m3

# Removing coefficients: t-ratio
# Test the t-ratio of each, remove if abs(x) < 1.96

# Calculate standard error
returns.vlo.m3.se <- sqrt(diag(vcov(returns.vlo.m3))); returns.vlo.m3.se

# Now calculate the t-ratio
returns.vlo.m3.tratio <- abs(returns.vlo.m3$coef / returns.vlo.m3.se)
returns.vlo.m3.tratio

#--------------------------------------
# Diagnostic Checking
#--------------------------------------

#------------------
# Model 1
#------------------

# Summary stats
summary(returns.vlo.m1)

# TS diagnostics
tsdiag(returns.vlo.m1, gof = 25, col = "blue")

# Pormanteau test or Ljung-Box test & plot
# Set fitdf = p + q from arima(order = (p, d, q))
Box.test(residuals(returns.vlo.m1), lag = 10, fitdf = 8, type = "Ljung")
Box.test(residuals(returns.vlo.m1), lag = 20, fitdf = 8, type = "Ljung")

# ACF & PACF of residuals - first lag removed
par(mfcol = c(2, 1))
acf(residuals(returns.vlo.m1), 25, xlim = c(1, 25), ylim = c(-0.2, 0.2))
pacf(residuals(returns.vlo.m1), 25, ylim = c(-0.2, 0.2))
par(mfcol = c(1, 1))

# Q-Q plot of residuals
qqnorm(residuals(returns.vlo.m1), main = "Normal Q-Q Plot of Residuals
       Valero Energy (VLO) - Model 1")
qqline(residuals(returns.vlo.m1))

#------------------
# Model 2
#------------------

# Summary stats
summary(returns.vlo.m2)

# TS diagnostics
tsdiag(returns.vlo.m2, gof = 25, col = "blue")

# Pormanteau test or Ljung-Box test & plot
# Set fitdf = p + q from arima(order = (p, d, q))
Box.test(residuals(returns.vlo.m2), lag = 10, fitdf = 0, type = "Ljung")
Box.test(residuals(returns.vlo.m2), lag = 20, fitdf = 0, type = "Ljung")

# ACF & PACF of residuals - first lag removed
par(mfcol = c(2, 1))
acf(residuals(returns.vlo.m2), 25, xlim = c(1, 25), ylim = c(-0.2, 0.2))
pacf(residuals(returns.vlo.m2), 25, ylim = c(-0.2, 0.2))
par(mfcol = c(1, 1))

# Q-Q plot of residuals
qqnorm(residuals(returns.vlo.m2), main = "Normal Q-Q Plot of Residuals
       Valero Energy (VLO) - Model 2")
qqline(residuals(returns.vlo.m2))

#------------------
# Model 3
#------------------

# Summary stats
summary(returns.vlo.m3)

# TS diagnostics
tsdiag(returns.vlo.m3, gof = 25, col = "blue")

# Pormanteau test or Ljung-Box test & plot
# Set fitdf = p + q from arima(order = (p, d, q))
Box.test(residuals(returns.vlo.m3), lag = 10, fitdf = 4, type = "Ljung")
Box.test(residuals(returns.vlo.m3), lag = 20, fitdf = 4, type = "Ljung")

# ACF & PACF of residuals - first lag removed
par(mfcol = c(2, 1))
acf(residuals(returns.vlo.m3), 25, xlim = c(1, 25), ylim = c(-0.2, 0.2))
pacf(residuals(returns.vlo.m3), 25, ylim = c(-0.2, 0.2))
par(mfcol = c(1, 1))

# Q-Q plot of residuals
qqnorm(residuals(returns.vlo.m3), main = "Normal Q-Q Plot of Residuals
       Valero Energy (VLO) - Model 3")
qqline(residuals(returns.vlo.m3))

#------------------------------------------------------------------------------
# CVX
#------------------------------------------------------------------------------

#--------------------------------------
# Model Identification
#--------------------------------------

# Apply ADF test - check for unit-root nonstationarity
# If p-value > alpha, then fail to reject H0: that series is unit-root
#   nonstationary (i.e. a unit-root is likely present); suggests differencing
#   is required
adf.test(returns.cvx, alternative = "stationary")
ndiffs(returns.cvx)
nsdiffs(returns.cvx)

# Are there serial correlations in the log return?
# ACF plot of series for checking serial correlations in series
# ACF plot of abs(series) for checking dependence in series
par(mfcol = c(2, 1))
acf(returns.cvx, 25, xlim = c(1, 25), ylim = c(-0.2, 0.2))
acf(abs(returns.cvx), 25, xlim = c(1, 25), ylim = c(-0.2, 0.2))
par(mfcol = c(1, 1))

# Ljung-Box test
Box.test(returns.cvx, lag = 10, type = "Ljung")

#------------------
# Model Order
#------------------

# Examine ACF & PACF - first lag removed
# Suggests ARMA(3, 3) or ARMA(5, 5)
par(mfcol = c(2, 1))
acf(returns.cvx, 25, xlim = c(1, 25), ylim = c(-0.1, 0.1))
pacf(returns.cvx, 25, ylim = c(-0.1, 0.1))
par(mfcol = c(1, 1))

# Examine EACF table
# Suggests ARMA(2, 2)
library(TSA, warn.conflicts = F, quietly = T)
eacf(returns.cvx, ar.max = 15, ma.max = 15)
detach("package:TSA", unload = T)

# Use auto.arima()
# Suggests ARMA(3, 2) based on AIC
auto.arima(returns.cvx, stationary = T, trace = T, approx = F, allowmean = F)

#--------------------------------------
# Parameter Estimation
#--------------------------------------

#------------------
# Model 1
#------------------

returns.cvx.m1 <- Arima(returns.cvx, order = c(5, 0, 5), include.mean = F)
returns.cvx.m1

# Removing coefficients: t-ratio
# Test the t-ratio of each, remove if abs(x) < 1.96

# Calculate standard error
returns.cvx.m1.se <- sqrt(diag(vcov(returns.cvx.m1))); returns.cvx.m1.se

# Now calculate the t-ratio
returns.cvx.m1.tratio <- abs(returns.cvx.m1$coef / returns.cvx.m1.se)
returns.cvx.m1.tratio

# Fix coefficients to zero
returns.cvx.m1 <- Arima(returns.cvx, order = c(5, 0, 5), include.mean = F,
                        fixed = c(NA, 0, 0, 0, NA, NA, 0, 0, NA, 0),
                        transform.pars = F)
returns.cvx.m1

#------------------
# Model 2
#------------------

# Multiple issues with model fit - NaN values in S.E.
returns.cvx.m2 <- Arima(returns.cvx, order = c(2, 0, 2), include.mean = F)
returns.cvx.m2

# Try next suggested order from EACF() - ARMA(1, 1)
returns.cvx.m2 <- Arima(returns.cvx, order = c(1, 0, 1), include.mean = F)
returns.cvx.m2

# Removing coefficients: t-ratio
# Test the t-ratio of each, remove if abs(x) < 1.96

# Calculate standard error
returns.cvx.m2.se <- sqrt(diag(vcov(returns.cvx.m2))); returns.cvx.m2.se

# Now calculate the t-ratio
returns.cvx.m2.tratio <- abs(returns.cvx.m2$coef / returns.cvx.m2.se)
returns.cvx.m2.tratio

# Fix coefficients to zero
returns.cvx.m2 <- Arima(returns.cvx, order = c(1, 0, 1), include.mean = F,
                        fixed = c(0, 0), transform.pars = F)
returns.cvx.m2

# Identical to an ARIMA(0, 0, 0) model
returns.cvx.m2 <- Arima(returns.cvx, order = c(0, 0, 0), include.mean = F)
returns.cvx.m2

#------------------
# Model 3
#------------------

returns.cvx.m3 <- Arima(returns.cvx, order = c(3, 0, 2), include.mean = F)
returns.cvx.m3

# Removing coefficients: t-ratio
# Test the t-ratio of each, remove if abs(x) < 1.96

# Calculate standard error
returns.cvx.m3.se <- sqrt(diag(vcov(returns.cvx.m3))); returns.vlo.m3.se

# Now calculate the t-ratio
returns.cvx.m3.tratio <- abs(returns.cvx.m3$coef / returns.cvx.m3.se)
returns.cvx.m3.tratio

# Fix coefficients to zero
returns.cvx.m3 <- Arima(returns.cvx, order = c(3, 0, 2), include.mean = F,
                        fixed = c(0, NA, NA, 0, NA), transform.pars = F)
returns.cvx.m3

#--------------------------------------
# Diagnostic Checking
#--------------------------------------

#------------------
# Model 1
#------------------

# Summary stats
summary(returns.cvx.m1)

# TS diagnostics
tsdiag(returns.cvx.m1, gof = 25, col = "blue")

# Pormanteau test or Ljung-Box test & plot
# Set fitdf = p + q from arima(order = (p, d, q))
Box.test(residuals(returns.cvx.m1), lag = 10, fitdf = 10, type = "Ljung")
Box.test(residuals(returns.cvx.m1), lag = 20, fitdf = 10, type = "Ljung")

# ACF & PACF of residuals - first lag removed
par(mfcol = c(2, 1))
acf(residuals(returns.cvx.m1), 25, xlim = c(1, 25), ylim = c(-0.2, 0.2))
pacf(residuals(returns.cvx.m1), 25, ylim = c(-0.2, 0.2))
par(mfcol = c(1, 1))

# Q-Q plot of residuals
qqnorm(residuals(returns.cvx.m1), main = "Normal Q-Q Plot of Residuals
       Chevron (CVX) - Model 1")
qqline(residuals(returns.cvx.m1))

#------------------
# Model 2
#------------------

# Summary stats
summary(returns.cvx.m2)

# TS diagnostics
tsdiag(returns.cvx.m2, gof = 25, col = "blue")

# Pormanteau test or Ljung-Box test & plot
# Set fitdf = p + q from arima(order = (p, d, q))
Box.test(residuals(returns.cvx.m2), lag = 10, fitdf = 0, type = "Ljung")
Box.test(residuals(returns.cvx.m2), lag = 20, fitdf = 0, type = "Ljung")

# ACF & PACF of residuals - first lag removed
par(mfcol = c(2, 1))
acf(residuals(returns.cvx.m2), 25, xlim = c(1, 25), ylim = c(-0.2, 0.2))
pacf(residuals(returns.cvx.m2), 25, ylim = c(-0.2, 0.2))
par(mfcol = c(1, 1))

# Q-Q plot of residuals
qqnorm(residuals(returns.cvx.m2), main = "Normal Q-Q Plot of Residuals
       Chevron (CVX) - Model 2")
qqline(residuals(returns.cvx.m2))

#------------------
# Model 3
#------------------

# Summary stats
summary(returns.cvx.m3)

# TS diagnostics
tsdiag(returns.cvx.m3, gof = 25, col = "blue")

# Pormanteau test or Ljung-Box test & plot
# Set fitdf = p + q from arima(order = (p, d, q))
Box.test(residuals(returns.cvx.m3), lag = 10, fitdf = 5, type = "Ljung")
Box.test(residuals(returns.cvx.m3), lag = 20, fitdf = 5, type = "Ljung")

# ACF & PACF of residuals - first lag removed
par(mfcol = c(2, 1))
acf(residuals(returns.cvx.m3), 25, xlim = c(1, 25), ylim = c(-0.2, 0.2))
pacf(residuals(returns.cvx.m3), 25, ylim = c(-0.2, 0.2))
par(mfcol = c(1, 1))

# Q-Q plot of residuals
qqnorm(residuals(returns.cvx.m3), main = "Normal Q-Q Plot of Residuals
       Chevron (CVX) - Model 3")
qqline(residuals(returns.cvx.m3))

#==============================================================================
# Q1G
#==============================================================================

# Using the model you selected in Q1F, compute forecasts for the daily returns
#   for January 2016 (19 trading days) as well as 95% confidence intervals for
#   the forecast.

#--------------------------------------
# VLO - Model 3
#--------------------------------------

#------------------
# Forecast
#------------------
returns.vlo.m3.fc <- predict(returns.vlo.m3, n.ahead = 19)
returns.vlo.m3.fc

#------------------
# 95% C.I.
#------------------

# Lower Confidence Level
returns.vlo.m3.fc.lcl <- returns.vlo.m3.fc$pred + (qnorm(0.025, lower.tail = T)
                                                   * returns.vlo.m3.fc$se)
returns.vlo.m3.fc.lcl

# Upper Confidence Level
returns.vlo.m3.fc.ucl <- returns.vlo.m3.fc$pred + (qnorm(0.025, lower.tail = F)
                                                   * returns.vlo.m3.fc$se)
returns.vlo.m3.fc.ucl

#------------------
# Plots
#------------------

returns.vlo.m3.fc.min <- min(returns.vlo.m3.fc.ucl, returns.vlo.m3.fc.lcl)
returns.vlo.m3.fc.max <- max(returns.vlo.m3.fc.ucl, returns.vlo.m3.fc.lcl)
plot(returns.vlo.m3.fc$pred, main = "Valero Energy (VLO)
     January Forecast (19 Trading Days)",
     sub = "95% Confidence Intervals (Red)",
     ylim = c(returns.vlo.m3.fc.min, returns.vlo.m3.fc.max))
lines(returns.vlo.m3.fc.lcl, col = "red")
lines(returns.vlo.m3.fc.ucl, col = "red")

#--------------------------------------
# CVX - Model 3
#--------------------------------------

#------------------
# Forecast
#------------------
returns.cvx.m3.fc <- predict(returns.cvx.m3, n.ahead = 19)
returns.cvx.m3.fc

#------------------
# 95% C.I.
#------------------

# Lower Confidence Level
returns.cvx.m3.fc.lcl <- returns.cvx.m3.fc$pred + (qnorm(0.025, lower.tail = T)
                                                   * returns.cvx.m3.fc$se)
returns.cvx.m3.fc.lcl

# Upper Confidence Level
returns.cvx.m3.fc.ucl <- returns.cvx.m3.fc$pred + (qnorm(0.025, lower.tail = F)
                                                   * returns.cvx.m3.fc$se)
returns.cvx.m3.fc.ucl

#------------------
# Plots
#------------------

returns.cvx.m3.fc.min <- min(returns.cvx.m3.fc.ucl, returns.cvx.m3.fc.lcl)
returns.cvx.m3.fc.max <- max(returns.cvx.m3.fc.ucl, returns.cvx.m3.fc.lcl)
plot(returns.cvx.m3.fc$pred, main = "Chevron (CVX)
     January Forecast (19 Trading Days)",
     sub = "95% Confidence Intervals (Red)",
     ylim = c(returns.cvx.m3.fc.min, returns.cvx.m3.fc.max))
lines(returns.cvx.m3.fc.lcl, col = "red")
lines(returns.cvx.m3.fc.ucl, col = "red")

#==============================================================================
# Q1H
#==============================================================================

# Are there ARCH effects in the log return series? Why or why not?

#--------------------------------------
# VLO - Model 3
#--------------------------------------

#------------------
# Ljung-Box test
#------------------
# Use squared series
# H0: first m lags of ACF of squared series = 0
# If we reject H0:, series shows strong ARCH effects

# Test at lag 10 - reject H0:
Box.test(residuals(returns.vlo.m3)^2, lag = 10, type = "Ljung")

#------------------
# Examining ACF & PACF
#------------------
# Use squared series

# If autocorrelations > critical value lines, then:
#   1. Conclude serial correlations
#   2. Conclude ARCH effects

# ACF & PACF of squared residuals - first lag removed
par(mfcol = c(2, 1))
acf(residuals(returns.vlo.m3)^2, 25, xlim = c(1, 25), ylim = c(-0.2, 0.2))
pacf(residuals(returns.vlo.m3)^2, 25, ylim = c(-0.2, 0.2))
par(mfcol = c(1, 1))

#--------------------------------------
# CVX - Model 3
#--------------------------------------

#------------------
# Ljung-Box test
#------------------
# Use squared series
# H0: first m lags of ACF of squared series = 0
# If we reject H0:, series shows strong ARCH effects

# Test at lag 10 - reject H0:
Box.test(residuals(returns.cvx.m3)^2, lag = 10, type = "Ljung")

#------------------
# Examining ACF & PACF
#------------------
# Use squared series

# If autocorrelations > critical value lines, then:
#   1. Conclude serial correlations
#   2. Conclude ARCH effects

# ACF & PACF of squared residuals - first lag removed
par(mfcol = c(2, 1))
acf(residuals(returns.cvx.m3)^2, 25, xlim = c(1, 25), ylim = c(-0.2, 0.2))
pacf(residuals(returns.cvx.m3)^2, 25, ylim = c(-0.2, 0.2))
par(mfcol = c(1, 1))

#==============================================================================
# Q1I
#==============================================================================

# Fit a Gaussian ARMA-GARCH model to each of the log return series. Obtain the
#   normal Q-Q plot of the standardized residuals, and write down the fitted
#   model. Is the model adequate? Why or why not?

#--------------------------------------
# VLO
#--------------------------------------

#------------------
# Specification
#------------------
returns.vlo.ag.nm.spec <- ugarchspec(variance.model = list(garchOrder = c(1, 1),
                                    model = "sGARCH"), 
                                    mean.model = list(armaOrder = c(2, 2),
                                    include.mean = F),
                                    distribution.model = "norm")

#------------------
# Model
#------------------
returns.vlo.ag.nm.m1 <- ugarchfit(returns.vlo.ag.nm.spec, data = returns$VLO)

#------------------
# Model Adequacy
#------------------

# Examine model fit
# Includes Ljung-Box results for:
#   Standardized residuals - adequacy of model mean equation
#   Standardized residuals squared - adequacy of model variance equation
returns.vlo.ag.nm.m1

# Examine residuals for normality assumption
# Assign standardized residuals
returns.vlo.ag.nm.m1.res <- residuals(returns.vlo.ag.nm.m1, standardize = T)

# Q-Q Plot
plot(returns.vlo.ag.nm.m1, which = 9)

# ACF & PACF 
#   Standardized residuals - adequacy of model mean equation
#   Standardized residuals squared - adequacy of model variance equation
# First lag removed
par(mfcol = c(2, 2))
acf(returns.vlo.ag.nm.m1.res, 25, xlim = c(1, 25), ylim = c(-0.1, 0.1))
pacf(returns.vlo.ag.nm.m1.res, 25, ylim = c(-0.1, 0.1))
acf(returns.vlo.ag.nm.m1.res^2, 25, xlim = c(1, 25), ylim = c(-0.1, 0.1))
pacf(returns.vlo.ag.nm.m1.res^2, 25, ylim = c(-0.1, 0.1))
par(mfcol = c(1, 1))

#--------------------------------------
# CVX
#--------------------------------------

#------------------
# Specification
#------------------
returns.cvx.ag.nm.spec <- ugarchspec(variance.model = list(garchOrder = c(1, 1),
                                     model = "sGARCH"), 
                                     mean.model = list(armaOrder = c(3, 2),
                                     include.mean = F),
                                     fixed.pars = list(ar1 = 0, ma1 = 0),
                                     distribution.model = "norm")

#------------------
# Model
#------------------

returns.cvx.ag.nm.m1 <- ugarchfit(returns.cvx.ag.nm.spec, data = returns$CVX)

#------------------
# Model Adequacy
#------------------

# Examine model fit
# Includes Ljung-Box results for:
#   Standardized residuals - adequacy of model mean equation
#   Standardized residuals squared - adequacy of model variance equation
returns.cvx.ag.nm.m1

# Examine residuals for normality assumption
# Assign standardized residuals
returns.cvx.ag.nm.m1.res <- residuals(returns.cvx.ag.nm.m1, standardize = T)

# Q-Q Plot
plot(returns.cvx.ag.nm.m1, which = 9)

# ACF & PACF 
#   Standardized residuals - adequacy of model mean equation
#   Standardized residuals squared - adequacy of model variance equation
# First lag removed
par(mfcol = c(2, 2))
acf(returns.cvx.ag.nm.m1.res, 25, xlim = c(1, 25), ylim = c(-0.1, 0.1))
pacf(returns.cvx.ag.nm.m1.res, 25, ylim = c(-0.1, 0.1))
acf(returns.cvx.ag.nm.m1.res^2, 25, xlim = c(1, 25), ylim = c(-0.1, 0.1))
pacf(returns.cvx.ag.nm.m1.res^2, 25, ylim = c(-0.1, 0.1))
par(mfcol = c(1, 1))

#==============================================================================
# Q1J
#==============================================================================

# Build an ARMA-GARCH model with Student-t innovations for the log return
#   series. Perform model checking and write down the fitted model. Is this
#   model better or worse than Q1I?

#--------------------------------------
# VLO
#--------------------------------------

#------------------
# Specification
#------------------
returns.vlo.ag.st.spec <- ugarchspec(variance.model = list(garchOrder = c(1, 1),
                                    model = "sGARCH"),
                                    mean.model = list(armaOrder = c(2, 2),
                                    include.mean = F),
                                    distribution.model = "std")

#------------------
# Model
#------------------
returns.vlo.ag.st.m1 <- ugarchfit(returns.vlo.ag.st.spec, data = returns$VLO)

#------------------
# Model Adequacy
#------------------

# Examine model fit
# Includes Ljung-Box results for:
#   Standardized residuals - adequacy of model mean equation
#   Standardized residuals squared - adequacy of model variance equation
returns.vlo.ag.st.m1

# Examine residuals for normality assumption
# Assign standardized residuals
returns.vlo.ag.st.m1.res <- residuals(returns.vlo.ag.st.m1, standardize = T)

# Q-Q Plot
plot(returns.vlo.ag.st.m1, which = 9)

# ACF & PACF 
#   Standardized residuals - adequacy of model mean equation
#   Standardized residuals squared - adequacy of model variance equation
# First lag removed
par(mfcol = c(2, 2))
acf(returns.vlo.ag.st.m1.res, 25, xlim = c(1, 25), ylim = c(-0.1, 0.1))
pacf(returns.vlo.ag.st.m1.res, 25, ylim = c(-0.1, 0.1))
acf(returns.vlo.ag.st.m1.res^2, 25, xlim = c(1, 25), ylim = c(-0.1, 0.1))
pacf(returns.vlo.ag.st.m1.res^2, 25, ylim = c(-0.1, 0.1))
par(mfcol = c(1, 1))

#--------------------------------------
# CVX
#--------------------------------------

#------------------
# Specification
#------------------
returns.cvx.ag.st.spec <- ugarchspec(variance.model = list(garchOrder = c(1, 1),
                                     model = "sGARCH"), 
                                     mean.model = list(armaOrder = c(3, 2),
                                     include.mean = F),
                                     fixed.pars = list(ar1 = 0, ma1 = 0),
                                     distribution.model = "std")
#------------------
# Model
#------------------
returns.cvx.ag.st.m1 <- ugarchfit(returns.cvx.ag.st.spec, data = returns$CVX)

#------------------
# Model Adequacy
#------------------

# Examine model fit
# Includes Ljung-Box results for:
#   Standardized residuals - adequacy of model mean equation
#   Standardized residuals squared - adequacy of model variance equation
returns.cvx.ag.st.m1

# Examine residuals for normality assumption
# Assign standardized residuals
returns.cvx.ag.st.m1.res <- residuals(returns.cvx.ag.st.m1, standardize = T)

# Q-Q Plot
plot(returns.cvx.ag.st.m1, which = 9)

# ACF & PACF 
#   Standardized residuals - adequacy of model mean equation
#   Standardized residuals squared - adequacy of model variance equation
# First lag removed
par(mfcol = c(2, 2))
acf(returns.cvx.ag.st.m1.res, 25, xlim = c(1, 25), ylim = c(-0.1, 0.1))
pacf(returns.cvx.ag.st.m1.res, 25, ylim = c(-0.1, 0.1))
acf(returns.cvx.ag.st.m1.res^2, 25, xlim = c(1, 25), ylim = c(-0.1, 0.1))
pacf(returns.cvx.ag.st.m1.res^2, 25, ylim = c(-0.1, 0.1))
par(mfcol = c(1, 1))

#==============================================================================
# Q1K
#==============================================================================

# Obtain the 1-step ahead mean and volatility forecasts using the fitted
#   ARMA-GARCH model with Student-t innovations with 95% confidence intervals
#   for January 2016 (19 trading days).

#--------------------------------------
# VLO
#--------------------------------------

#------------------
# Model
#------------------
returns.vlo.ag.st.m2 <- ugarchfit(returns.vlo.ag.st.spec, 
                                  data = returns.full$VLO, out.sample = 19)

#------------------
# Forecast
#------------------
returns.vlo.ag.st.m2.fc <- ugarchforecast(returns.vlo.ag.st.m2, data = NULL,
                                          n.ahead = 1, n.roll = 19)

# Forecast returns
fitted(returns.vlo.ag.st.m2.fc)

# Forecast standard deviation
sigma(returns.vlo.ag.st.m2.fc)

# Plot forecast returns
plot(returns.vlo.ag.st.m2.fc, which = 2)

# Plot forecast standard deviation
plot(returns.vlo.ag.st.m2.fc, which = 4)

#--------------------------------------
# CVX
#--------------------------------------

#------------------
# Model
#------------------
returns.cvx.ag.st.m2 <- ugarchfit(returns.cvx.ag.st.spec, 
                                  data = returns.full$CVX, out.sample = 19)

#------------------
# Forecast
#------------------
returns.cvx.ag.st.m2.fc <- ugarchforecast(returns.cvx.ag.st.m2, data = NULL,
                                          n.ahead = 1, n.roll = 19)

# Forecast returns
fitted(returns.cvx.ag.st.m2.fc)

# Forecast standard deviation
sigma(returns.cvx.ag.st.m2.fc)

# Plot forecast returns
plot(returns.cvx.ag.st.m2.fc, which = 2)

# Plot forecast standard deviation
plot(returns.cvx.ag.st.m2.fc, which = 4)

#==============================================================================
# Q1L
#==============================================================================

# Is there significant cross-correlation in the log returns for the two stocks?

# Obtain the lags of the sample cross-correlation matricies of series
# Using level = T will output values and simplified notation
# ACFs are on primary diagonal and CCFs are on off diagonal
ccm(returns, lags = 10, level = T)

# Test the following hypotheses using a 5% significance level:
#   H0: p1 = ... = pn = 0
#   Ha: pi != 0 for some i, where {1, ..., n}
mq(returns, lag = 10)

#==============================================================================
# Q1M
#==============================================================================

# Using a 30-day moving window, compute and plot rolling covariances and
#   correlations. Briefly comment on what you see.

#--------------------------------------
# Compute and plot rolling correlations and covariances
#--------------------------------------

# Specify correlation & covariance functions
cor.fun <- function(x){
    cor(x)[1,2]
}

cov.fun <- function(x){
    cov(x)[1,2]
}

# Window: 30 days
returns.roll.cov <- rollapply(as.zoo(returns), FUN = cov.fun, width = 30,
                              by.column = F, align = "right")
returns.roll.cor <- rollapply(as.zoo(returns), FUN = cor.fun, width = 30,
                              by.column = F, align = "right")

# Plots
plot(returns.roll.cov, main = "VLO & CVX: 30-day Rolling Covariances",
     ylab = "Covariance", lwd = 2, col = "blue")
grid()
abline(h = cov(returns)[1,2], lwd = 2, col = "red")

plot(returns.roll.cor, main = "VLO & CVX: 30-day Rolling Correlations",
     ylab = "Correlation", lwd = 2, col = "blue")
grid()
abline(h = cor(returns)[1,2], lwd = 2, col = "red")

#--------------------------------------
# Compute and plot EWMA correlations and covariances
# Use lambda = 0.94
#--------------------------------------

# EWMA covariance
returns.cov.ewma <- covEWMA(as.data.frame(returns), lambda = 0.94)

# EWMA conditional covariance
returns.cond.cov <- returns.cov.ewma[, 2, 1]

# EWMA conditional correlation
t <- length(returns.cov.ewma[, 1, 1])
returns.cond.cor <- rep(0, t)
for (i in 1:t) {
    returns.cond.cor[i]<- cov2cor(returns.cov.ewma[i, , ])[1, 2]
}

# Plots
plot(x = time(as.zoo(returns)), y = returns.cond.cov, type = "l", xlab = "Time", 
     ylab = "Covariance", lwd = 2, col = "blue",
     main = "VLO & CVX: EWMA Covariance")
grid()
abline(h = cov(returns)[1, 2], lwd = 2, col = "red")

plot(x = time(as.zoo(returns)), y = returns.cond.cor, type = "l", xlab = "Time",
     ylab = "Correlation", lwd = 2, col = "blue",
     main = "VLO & CVX: EWMA Correlation")
grid()
abline(h = cor(returns)[1, 2], lwd = 2, col = "red")

#==============================================================================
# Q1N
#==============================================================================

# Let r_t = (r_VLO, t, r_CVX, t)^T. Using the dccfit() function from the
#   {rmgarch} package, estimate the normal-DCC(1,1) model. Briefly comment on
#   the estimated coefficients and the fit of the model.

#------------------
# Specification
#------------------
returns.dcc.st.spec <- dccspec(uspec = multispec(c(returns.cvx.ag.st.spec, 
                                                   returns.vlo.ag.st.spec)),
                               dccOrder = c(1, 1), distribution = "mvt")

#------------------
# Model
#------------------
returns.dcc.st.m1 <- dccfit(returns.dcc.st.spec, data = returns)

#------------------
# Model Adequacy
#------------------

# Examine model fit
# Includes Ljung-Box results for:
#   Standardized residuals - adequacy of model mean equation
#   Standardized residuals squared - adequacy of model variance equation
returns.dcc.st.m1

# Plot of standardized residuals
plot(returns.dcc.st.m1@mfit$stdresid[,1], type = "l",
     main = "Valero Energy (VLO)
     DCC Fit - Standardized Residuals")
plot(returns.dcc.st.m1@mfit$stdresid[,2], type = "l",
     main = "Chevron (CVX)
     DCC Fit - Standardized Residuals")

# ACF & PACF 
#   Standardized residuals - adequacy of model mean equation
#   Standardized residuals squared - adequacy of model variance equation
# First lag removed
par(mfcol = c(2, 2))
acf(returns.dcc.st.m1@mfit$stdresid, 25, xlim = c(1, 25), ylim = c(-0.1, 0.1))
pacf(returns.dcc.st.m1@mfit$stdresid, 25, ylim = c(-0.1, 0.1))
acf(returns.dcc.st.m1@mfit$stdresid^2, 25, xlim = c(1, 25), ylim = c(-0.1, 0.1))
pacf(returns.dcc.st.m1@mfit$stdresid^2, 25, ylim = c(-0.1, 0.1))
par(mfcol = c(1, 1))

# Obtain the lags of the sample cross-correlation matricies of series
# Using level = T will output values and simplified notation
# ACFs are on primary diagonal and CCFs are on off diagonal
ccm(returns.dcc.st.m1@mfit$stdresid, level = T, output = T, lags = 10)

# Test the following hypotheses using a 5% significance level:
#   H0: p1 = ... = pn = 0
#   Ha: pi != 0 for some i, where {1, ..., n}
mq(returns.dcc.st.m1@mfit$stdresid, lags = 10)

#==============================================================================
# Q1O
#==============================================================================

# Plot the estimated in-sample conditional covariances and correlations.
#   Compare the EWMA and rolling estimates.

# Conditional Covariances
plot(returns.dcc.st.m1, which = 3)

# Conditional Correlations
plot(returns.dcc.st.m1, which = 4)

#==============================================================================
# Q1P
#==============================================================================

# Using the estimated DCC(1,1) model, compute (using dccforecast() function)
#   and plot 1-step ahead forecasts of conditional covariance and correlation
#   for January 2016 (19 trading days).

#------------------
# Model
#------------------
returns.dcc.st.m2 <- dccfit(returns.dcc.st.spec, data = returns.full, 
                            out.sample = 19)

#------------------
# Forecast
#------------------
returns.dcc.st.m2.fc <- dccforecast(returns.dcc.st.m2, n.ahead = 1, n.roll = 19)

# Forecast returns
fitted(returns.dcc.st.m2.fc)

# Forecast standard deviation
sigma(returns.dcc.st.m2.fc)

# Plot forecast returns
plot(returns.dcc.st.m2.fc, which = 1)

# Plot forecast standard deviation
plot(returns.dcc.st.m2.fc, which = 2)

# Plot condiational covariance forecast
plot(returns.dcc.st.m2.fc, which = 3)

# Plot conditional correlation forecast
plot(returns.dcc.st.m2.fc, which = 4)

#------------------
# Alternative plot 1
#------------------
corsfit <- c()
for (i in 1:length(returns.dcc.st.m2@mfit$R)) {
    corsfit <- c(corsfit, returns.dcc.st.m2@mfit$R[[i]][1,2])
}
corsforecast <- returns.dcc.st.m2.fc@mforecast$R[[1]][seq(2, 76, 4)]
corsfit <- data.frame(time = 1:1257, corsfit)
corsforecast <- data.frame(time = 1258:1276, corsforecast)
plot(corsfit, type = "l")
lines(corsforecast, col = "red", type = "l", lwd = 4)

#------------------
# Alternative plot 2
#------------------
# Conditional Covariances
returns.dcc.st.m2.fc.cov <- rcov(returns.dcc.st.m2.fc)
returns.dcc.st.m2.fc.cov <- returns.dcc.st.m2.fc.cov[[1]]
ts.plot(returns.dcc.st.m2.fc.cov[1, 2, ],
        main = "VLO & CVX: DCC Conditional Covariance Forecast", 
        ylab = "Correlation", xlab = "Time (Trading Days)")

# Conditional Correlations
returns.dcc.st.m2.fc.cor <- rcor(returns.dcc.st.m2.fc)
returns.dcc.st.m2.fc.cor <- returns.dcc.st.m2.fc.cor[[1]]
ts.plot(returns.dcc.st.m2.fc.cor[1, 2, ], 
        main = "VLO & CVX: DCC Conditional Correlation Forecast", 
        ylab = "Correlation", xlab = "Time (Trading Days)")

#==============================================================================
# Q1Q
#==============================================================================

# Compare your mean and volatility forecast from Q1P with Q1G and Q1K.

# Narrative in report.

#==============================================================================
# Q1R
#==============================================================================

# Compare each of the forecasts in Q1G, Q1K, and Q1P to the actual log returns.
#   Note that you will need to download January 2016 data to do this 
#   comparison. Do the confidence intervals include the actual values? Which is
#   the best model?

# Q1K
fpm(returns.vlo.ag.st.m2.fc)
fpm(returns.cvx.ag.st.m2.fc)

###############################################################################
# FIN
###############################################################################