###############################################################################
# 413_Assignment_06.R
# Last updated: 2016-02-13 by MJG
###############################################################################

# Clear workspace
rm(list=ls())

# Load packages
library(fpp)
library(fBasics)
library(fGarch)
library(fUnitRoots)

# Source functions
setwd("C:/Users/Michael/Dropbox/MSPA/413-DL/Code/Functions")
source("413_Function_backtest.R")
source("413_Function_Igarch.R")
source("413_Function_garchM.R")
source("413_Function_Tgarch11.R")

# Set working directory
setwd("C:/Users/Michael/Dropbox/MSPA/413-DL/Data Sets")

#==============================================================================
# Problem 1
#==============================================================================

# Read data
da <- read.table("d-msft3dx0113.txt", header = T)

# Assign values
ms <- da$msft

# Transform to log returns
ms.log <- log(ms + 1)

# Explore summary stats
basicStats(ms.log)

# Convert to time series
ts.ms.log <- ts(ms.log, start = c(2001, 1, 3), frequency = 252)

# Plot the time series
plot(ts.ms.log, xlab = "Year", ylab = "Returns", 
     main = "Daily Returns of Microsoft
     Natual Log - Continuously Compounded")

# ACF & PACF
par(mfcol = c(2, 1))
acf(ms.log)
pacf(ms.log)
par(mfcol = c(1, 1))

# ACF & PACF - first differenced
par(mfcol = c(2, 1))
acf(diff(ms.log))
pacf(diff(ms.log))
par(mfcol = c(1, 1))

#======================================
# Q1A
#======================================

# Is the expected log return zero?
t.test(ms.log)

# Are there serial correlations in the log return?
# ACF plot of series for checking serial correlations in series
# ACF plot of abs(series) for checking dependence in series
par(mfcol = c(2, 1))
acf(ms.log, 25, xlim = c(1, 25), ylim = c(-0.2, 0.4))
acf(abs(ms.log), 25, xlim = c(1, 25), ylim = c(-0.2, 0.4))
par(mfcol = c(1, 1))

# Ljung-Box test
Box.test(ms.log, lag = 10, type = "Ljung")

#======================================
# Q1B
#======================================

# Build a mean equation for log returns; write down the fitted model
ms.log.m1 <- arima(ms.log, order = c(0, 0, 2), include.mean = F)

#--------------------------------------
# Model Adequacy
#--------------------------------------

# Comments on Pormanteau test or Ljung-Box test:
# Large p-value suggests residuals are white noise (uncorrelated)
# If the result is significant (p-value < alpha), reject null hypothesis of 
#   model adequacy
# If there is no correlation in your residuals, then you have sufficiently 
#   modeled everything
# If both results suggest model adequacy, use AIC or BIC as the next criterion
#   for model selection; these also consider parsimony

# Summary stats
summary(ms.log.m1)
tsdiag(ms.log.m1, gof = 25)

# Pormanteau test or Ljung-Box test & plot
# Set fitdf = p + q from arima(order = (p, d, q))
# Monthly series so test lags at 12 and 24
Box.test(residuals(ms.log.m1), lag = 10, fitdf = 2, type = "Ljung")
# Plot; start at value of fitdf()
plot(sapply(2:100, function(i) Box.test(residuals(ms.log.m1), lag = i, 
                                        fitdf = 2)$p.value), type = "l")

# ACF & PACF of residuals - first lag removed
par(mfcol = c(2, 1))
acf(residuals(ms.log.m1), 25, xlim = c(1, 25), ylim = c(-0.2, 0.2))
pacf(residuals(ms.log.m1), 25, ylim = c(-0.2, 0.2))
par(mfcol = c(1, 1))

# Removing coefficients: t-ratio
# Test the t-ratio of each, remove if abs(x) < 1.96

# Compute the standard error (se)
ms.log.m1.se <- sqrt(diag(vcov(ms.log.m1))); ms.log.m1.se

# Now calculate the t-ratio
# Use 1.96 as cutoff - 5% significance level
# Below 1.96 = not significantly statistically different from zero at 95%
#   confidence level, should be removed or fixed to zero
ms.log.m1.tratio <- abs(ms.log.m1$coef / ms.log.m1.se); ms.log.m1.tratio

#--------------------------------------
# Testing for ARCH effects
#--------------------------------------

#------------------
# Ljung-Box test
#------------------
# Use squared series
# H0: first m lags of ACF of squared series = 0
# If we reject H0:, series shows strong ARCH effects

# Test at lag 10 - reject H0:
Box.test(residuals(ms.log.m1)^2, lag = 10, type = "Ljung")

#------------------
# Lagrange multiplier test of Engle
#------------------
# Note: requires sourcing
# Note: two issues with the function as sourced:
#   1. Function will subtract mean from series, even if not necessary
#   2. Function will not work if NA is in first row

# Modified version below:
"archTestMod" <- function(rtn, m = 10){
    # Perform Lagrange Multiplier Test for ARCH effect of a time series
    # rtn: time series
    # m: selected AR order
    y <- rtn^2  
    T <- length(rtn)
    atsq <- y[(m+1):T]
    x <- matrix(0,(T-m),m)
    for (i in 1:m){
        x[,i] <- y[(m+1-i):(T-i)]
    }
    md <- lm(atsq~x)
    summary(md)
}

# Do not use squared series - function handles this
# If we reject H0:, series shows strong ARCH effects

# Test at lag 10 - reject H0:
archTestMod(residuals(ms.log.m1), 10)

#------------------
# Examining ACF & PACF
#------------------
# Use squared series

# If autocorrelations > critical value lines, then:
#   1. Conclude serial correlations
#   2. Conclude ARCH effects

# ACF & PACF of squared residuals - first lag removed
par(mfcol = c(2, 1))
acf(residuals(ms.log.m1)^2, 25, xlim = c(1, 25), ylim = c(-0.2, 0.2))
pacf(residuals(ms.log.m1)^2, 25, ylim = c(-0.2, 0.2))
par(mfcol = c(1, 1))

#======================================
# Q1C
#======================================

#--------------------------------------
# Fit a Gaussian ARMA-GARCH model to the log return series
# Perform model checking and write down the fitted series
#--------------------------------------
ms.log.m2 <- garchFit(~arma(0, 2) + garch(1, 1), data = ms.log, trace = F,
                      include.mean = F)

#--------------------------------------
# Model adequacy
#--------------------------------------
# Summary stats
# Includes Ljung-Box results for:
#   Standardized residuals - adequacy of model mean equation
#   Standardized residuals squared - adequacy of model variance equation
summary(ms.log.m2)

# Examine residuals for normality assumption
# Assign standardized residuals
ms.log.m2.res <- residuals(ms.log.m2, standardize = T)

# Q-Q Plot
qqnorm(ms.log.m2.res); qqline(ms.log.m2.res)

# Shapiro test of normality - H0: iid normal
shapiro.test(ms.log.m2.res)

# ACF & PACF 
#   Standardized residuals - adequacy of model mean equation
#   Standardized residuals squared - adequacy of model variance equation
# First lag removed
par(mfcol = c(2, 2))
acf(ms.log.m2.res, 25, xlim = c(1, 25), ylim = c(-0.1, 0.1))
pacf(ms.log.m2.res, 25, ylim = c(-0.1, 0.1))
acf(ms.log.m2.res^2, 25, xlim = c(1, 25), ylim = c(-0.1, 0.1))
pacf(ms.log.m2.res^2, 25, ylim = c(-0.1, 0.1))
par(mfcol = c(1, 1))

# Note: residuals do not appear to satisfy normality assumption

#--------------------------------------
# Fit a Gaussian ARMA-GARCH model to the log return series
# Perform model checking and write down the fitted series
#--------------------------------------
ms.log.m3 <- garchFit(~arma(0, 1) + garch(1, 1), data = ms.log, trace = F,
                      include.mean = F)

#--------------------------------------
# Model adequacy
#--------------------------------------
# Summary stats
# Includes Ljung-Box results for:
#   Standardized residuals - validity of mean equation
#   Standardized residuals squared - validity of volatility equation
summary(ms.log.m3)

# Examine residuals for normality assumption
# Assign standardized residuals
ms.log.m3.res <- residuals(ms.log.m3, standardize = T)

# Q-Q Plot
qqnorm(ms.log.m3.res); qqline(ms.log.m3.res)

# Shapiro test of normality - H0: iid normal
shapiro.test(ms.log.m3.res)

# ACF & PACF 
#   Standardized residuals - adequacy of model mean equation
#   Standardized residuals squared - adequacy of model variance equation
# First lag removed
par(mfcol = c(2, 2))
acf(ms.log.m3.res, 25, xlim = c(1, 25), ylim = c(-0.1, 0.1))
pacf(ms.log.m3.res, 25, ylim = c(-0.1, 0.1))
acf(ms.log.m3.res^2, 25, xlim = c(1, 25), ylim = c(-0.1, 0.1))
pacf(ms.log.m3.res^2, 25, ylim = c(-0.1, 0.1))
par(mfcol = c(1, 1))

# Note: residuals do not appear to satisfy normality assumption

#======================================
# Q1D 
#======================================

#--------------------------------------
# Fit an ARMA-GARCH model with Student-t innovations for the log returns
# Perform model checking and write down the fitted model
#--------------------------------------
ms.log.m4 <- garchFit(~arma(0, 1) + garch(1, 1), data = ms.log, trace = F,
                      cond.dist = "std", include.mean = F)

#--------------------------------------
# Model adequacy
#--------------------------------------
# Summary stats
# Includes Ljung-Box results for:
#   Standardized residuals - adequacy of model mean equation
#   Standardized residuals squared - adequacy of model variance equation
summary(ms.log.m4)

# Examine residuals for normality assumption
# Assign standardized residuals
ms.log.m4.res <- residuals(ms.log.m4, standardize = T)

# Q-Q Plot
qqnorm(ms.log.m4.res); qqline(ms.log.m4.res)

# Shapiro test of normality - H0: iid normal
shapiro.test(ms.log.m4.res)

# ACF & PACF 
#   Standardized residuals - adequacy of model mean equation
#   Standardized residuals squared - adequacy of model variance equation
# First lag removed
par(mfcol = c(2, 2))
acf(ms.log.m4.res, 25, xlim = c(1, 25), ylim = c(-0.1, 0.1))
pacf(ms.log.m4.res, 25, ylim = c(-0.1, 0.1))
acf(ms.log.m4.res^2, 25, xlim = c(1, 25), ylim = c(-0.1, 0.1))
pacf(ms.log.m4.res^2, 25, ylim = c(-0.1, 0.1))
par(mfcol = c(1, 1))

# Note: residuals do not appear to satisfy normality assumption

#======================================
# Q1E
#======================================

# Obtain the 1-step to 5-step ahead mean and volatility forecasts using the
#   fitted ARMA-GARCH model with Student-t innovations.

# Note: standardDeviation value is volatility forecast
ms.log.m4.predict <- predict(ms.log.m4, n.ahead = 5); ms.log.m4.predict

#==============================================================================
# Problem 2
#==============================================================================

# Continue using the daily log returns of Microsoft from Problem 1

#======================================
# Q2A
#======================================

# Fit an IGARCH(1,1) model to the daily log returns of Microsoft
# Write down the fitted model
# Note: model checking in Q2D
ms.log.m5 <- Igarch(ms.log)
names(ms.log.m5)

#======================================
# Q2B
#======================================

# Define standardized residuals
ms.log.m5.sigma <- ms.log.m5$volatility
ms.log.m5.res <- (ms.log / ms.log.m5.sigma)

# Are there any serial correlations in the standardized residuals?
# ACF
#   Standardized residuals - adequacy of model mean equation
# First lag removed
acf(ms.log.m5.res, 25, xlim = c(1, 25), ylim = c(-0.1, 0.1))

# Ljung-Box test
Box.test(ms.log.m5.res, lag = 10, type = "Ljung")

# Q-Q Plot
qqnorm(ms.log.m5.res); qqline(ms.log.m5.res)

# Shapiro test of normality - H0: iid normal
shapiro.test(ms.log.m5.res)

#======================================
# Q2C
#======================================

# Are there any serial correlations in the standardized residuals?
# ACF
#   Standardized residuals squared - adequacy of model variance equation
# First lag removed
acf(ms.log.m5.res^2, 25, xlim = c(1, 25), ylim = c(-0.1, 0.1))

# Ljung-Box test
Box.test(ms.log.m5.res^2, lag = 10, type = "Ljung")

# Q-Q Plot
qqnorm(ms.log.m5.res^2); qqline(ms.log.m5.res^2)

# Shapiro test of normality - H0: iid normal
shapiro.test(ms.log.m5.res^2)

#======================================
# Q2D
#======================================

# If the model appears adequate, obtain the 1-step to 4-step ahead forecasts
#   for the log return series

# Get length for last observation to use in 1-step ahead forecast
length(ms.log)

# Get value of beta coefficient for sigma^2(t-1)
# 0.9730667
ms.log.m5$par

# Now compute 1-step ahead
# Note: no value of alpha(0) included in equation; this is a special case of
#   IGARCH, where 1-step to N-step ahead forecasts are identical
ms.log.m5.predict <- ((1 - 0.9730667) * ms.log[3268]^2) 
                        + (0.9730667 * ms.log.m5.sigma[3268]^2)
# Take square root to get standard deviation (volatility forecast)
sqrt(ms.log.m5.predict)

#==============================================================================
# Problem 3
#==============================================================================

# Read data
da <- read.table("m-ba3dx6113.txt", header = T)

# Assign values
ba <- da$ba

# Transform to log returns
ba.log <- log(ba + 1)

# Explore summary stats
basicStats(ba.log)

# Convert to time series
ts.ba.log <- ts(ba.log, start = c(1961, 1), frequency = 12)

# Plot the time series
plot(ts.ba.log, xlab = "Year", ylab = "Returns", 
     main = "Monthly Returns of Boeing
     Natual Log - Continuously Compounded")

# ACF & PACF
par(mfcol = c(2, 1))
acf(ba.log)
pacf(ba.log)
par(mfcol = c(1, 1))

# ACF & PACF - first differenced
par(mfcol = c(2, 1))
acf(diff(ba.log))
pacf(diff(ba.log))
par(mfcol = c(1, 1))

#======================================
# Q3A
#======================================

# Is the expected log return zero?
t.test(ba.log)

# Are there serial correlations in the log return?
# ACF plot of series for checking serial correlations in series
# ACF plot of abs(series) for checking dependence in series
par(mfcol = c(2, 1))
acf(ba.log, 25, xlim = c(1, 25), ylim = c(-0.2, 0.4))
acf(abs(ba.log), 25, xlim = c(1, 25), ylim = c(-0.2, 0.4))
par(mfcol = c(1, 1))

# Ljung-Box test
Box.test(ba.log, lag = 12, type = "Ljung")

#--------------------------------------
# Testing for ARCH effects
#--------------------------------------

# Specify mean equation
# Since mean is significantly different from zero, subtract it
ba.log.res <- (ba.log - mean(ba.log))

#------------------
# Ljung-Box test
#------------------
# Use squared series
# H0: first m lags of ACF of squared series = 0
# If we reject H0:, series shows strong ARCH effects

# Test at lag 12 - reject H0:
Box.test(ba.log.res^2, lag = 12, type = "Ljung")

#------------------
# Examining ACF & PACF
#------------------
# Use squared series

# If autocorrelations > critical value lines, then:
#   1. Conclude serial correlations
#   2. Conclude ARCH effects

# ACF & PACF of squared residuals - first lag removed
par(mfcol = c(2, 1))
acf(ba.log.res^2, 25, xlim = c(1, 25), ylim = c(-0.2, 0.2))
pacf(ba.log.res^2, 25, ylim = c(-0.2, 0.2))
par(mfcol = c(1, 1))

#======================================
# Q3B
#======================================

# Build a GARCH model with Gaussian innovations for the log return series
# Perform model checking and write down the fitted model
ba.log.m1 <- garchFit(~garch(1, 1), data = ba.log, trace = F)

#--------------------------------------
# Model adequacy
#--------------------------------------
# Summary stats
# Includes Ljung-Box results for:
#   Standardized residuals - adequacy of model mean equation
#   Standardized residuals squared - adequacy of model variance equation
summary(ba.log.m1)

# Examine residuals for normality assumption
# Assign standardized residuals
ba.log.m1.res <- residuals(ba.log.m1, standardize = T)

# Q-Q Plot
qqnorm(ba.log.m1.res); qqline(ba.log.m1.res)

# Shapiro test of normality - H0: iid normal
shapiro.test(ba.log.m1.res)

# ACF & PACF 
#   Standardized residuals - adequacy of model mean equation
#   Standardized residuals squared - adequacy of model variance equation
# First lag removed
par(mfcol = c(2, 2))
acf(ba.log.m1.res, 25, xlim = c(1, 25), ylim = c(-0.1, 0.1))
pacf(ba.log.m1.res, 25, ylim = c(-0.1, 0.1))
acf(ba.log.m1.res^2, 25, xlim = c(1, 25), ylim = c(-0.1, 0.1))
pacf(ba.log.m1.res^2, 25, ylim = c(-0.1, 0.1))
par(mfcol = c(1, 1))

# Note: residuals do not appear to satisfy normality assumption

#======================================
# Q3C & Q3D
#======================================

# Fit a GARCH model with skew-Student-t innovations to the monthly log returns
# Perform model checking and write down the fitted model
ba.log.m2 <- garchFit(~garch(1, 1), data = ba.log, trace = F, 
                      cond.dist = "sstd")

#--------------------------------------
# Model adequacy
#--------------------------------------
# Summary stats
# Includes Ljung-Box results for:
#   Standardized residuals - adequacy of model mean equation
#   Standardized residuals squared - adequacy of model variance equation
summary(ba.log.m2)

# Examine residuals for normality assumption
# Assign standardized residuals
ba.log.m2.res <- residuals(ba.log.m2, standardize = T)

# Q-Q Plot
qqnorm(ba.log.m2.res); qqline(ba.log.m2.res)

# Shapiro test of normality - H0: iid normal
shapiro.test(ba.log.m2.res)

# ACF & PACF 
#   Standardized residuals - adequacy of model mean equation
#   Standardized residuals squared - adequacy of model variance equation
# First lag removed
par(mfcol = c(2, 2))
acf(ba.log.m2.res, 25, xlim = c(1, 25), ylim = c(-0.1, 0.1))
pacf(ba.log.m2.res, 25, ylim = c(-0.1, 0.1))
acf(ba.log.m2.res^2, 25, xlim = c(1, 25), ylim = c(-0.1, 0.1))
pacf(ba.log.m2.res^2, 25, ylim = c(-0.1, 0.1))
par(mfcol = c(1, 1))

# Note: residuals do not appear to satisfy normality assumption

# Based on the fitted model, is the monthly log returns of Boeing skewed?
ba.log.m2.tratio <- ((0.88820 - 1) / (0.05998)); ba.log.m2.tratio
ba.log.m2.pv <- 2*pnorm(ba.log.m2.tratio); ba.log.m2.pv

#======================================
# Q3E
#======================================

# Fit a GARCH-M model to the monthly log returns
# Write down the fitted model
# Is the risk premium statistically significant?
ba.log.m3 <- garchM(ba.log)
ba.log.m3.pv <- 2*pnorm(0.65823, lower.tail = F); ba.log.m3.pv

#======================================
# Q3F
#======================================

# Fit a TGARCH(1,1) model to the monthly log returns
# Write down the fitted model
# Is the leverage effect statistically significant?
ba.log.m4 <- Tgarch11(ba.log)

# Use 1-sided t-test based on H0:
# H0: gamma <= 0
# Ha: gamma > 0
ba.log.m4.tratio <- 2.54059
ba.log.m4.pv <- pnorm(ba.log.m4.tratio, lower.tail = F); ba.log.m4.pv

###############################################################################
# FIN
###############################################################################