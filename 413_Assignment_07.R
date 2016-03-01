###############################################################################
# 413_Assignment_07.R
# Last updated: 2016-02-19 by MJG
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
da <- read.table("m-ba3dx6113.txt", header = T)

# Assign values
spx <- da$sprtrn

# Transform to log returns
spx.log <- log(spx + 1)

# Explore summary stats
basicStats(spx.log)

# Produce t-ratio of sample skewness
# Result suggests series is negatively skewed
spx.log.skew.tratio <- skewness(spx.log)/sqrt(6/length(spx.log))
spx.log.skew.tratio

# Convert to time series
ts.spx.log <- ts(spx.log, start = c(1961, 1), frequency = 12)

# Plot the time series
plot(ts.spx.log, xlab = "Year", ylab = "Returns", 
     main = "Monthly Returns of S&P 500 Index
     Natual Log - Continuously Compounded")

# ACF & PACF
par(mfcol = c(2, 1))
acf(spx.log)
pacf(spx.log)
par(mfcol = c(1, 1))

#======================================
# Q1A
#======================================

# Fit a model to the log return series
# Perform model checking and write down the fitted series

# Are there serial correlations in the log return?
# ACF plot of series for checking serial correlations in series
# ACF plot of abs(series) for checking dependence in series
par(mfcol = c(2, 1))
acf(spx.log, 25, xlim = c(1, 25), ylim = c(-0.2, 0.4))
acf(abs(spx.log), 25, xlim = c(1, 25), ylim = c(-0.2, 0.4))
par(mfcol = c(1, 1))

# Ljung-Box test
# If the result is significant (p-value < alpha), reject null hypothesis of 
#   no serial correlations
Box.test(spx.log, lag = 12, type = "Ljung")
Box.test(abs(spx.log), lag = 12, type = "Ljung")

#--------------------------------------
# Testing for ARCH effects
#--------------------------------------

# Is the mean (expected return) significantly different from zero?
t.test(spx.log)

# Reject H0: based on p-value, subtract sample mean from mean equation when
#   testing for ARCH effects
# These are the residuals of the mean equation
spx.log.arch <- (spx.log - mean(spx.log))

#------------------
# Examining ACF & PACF
#------------------
# Use squared series of residuals

# If autocorrelations > critical value lines, then:
#   1. Conclude serial correlations in residuals
#   2. Conclude ARCH effects

# ACF & PACF of squared residuals - first lag removed
par(mfcol = c(2, 1))
acf(spx.log.arch^2, 25, xlim = c(1, 25), ylim = c(-0.2, 0.2))
pacf(spx.log.arch^2, 25, ylim = c(-0.2, 0.2))
par(mfcol = c(1, 1))

#------------------
# Ljung-Box test
#------------------
# Use squared series of residuals to check for conditional heteroscedasticity
# H0: first m lags of ACF of squared series = 0
# If we reject H0:, series shows strong ARCH effects

# Test at lag 12 - reject H0:
Box.test(spx.log.arch^2, lag = 12, type = "Ljung")

#--------------------------------------
# Model Building
#--------------------------------------

# Use full data set (not residuals)

#------------------
# Model 1: GARCH(1,1) | Normal Distribution
#------------------

spx.log.m1 <- garchFit(~garch(1, 1), data = spx.log, trace = F)

# Model adequacy

# Summary stats
# Includes Ljung-Box results for:
#   Standardized residuals - adequacy of model mean equation
#   Standardized residuals squared - adequacy of model variance equation
summary(spx.log.m1)

# Examine residuals for normality assumption
# Assign standardized residuals
spx.log.m1.res <- residuals(spx.log.m1, standardize = T)

# Q-Q Plot
qqnorm(spx.log.m1.res); qqline(spx.log.m1.res)

# Shapiro test of normality - H0: iid normal
shapiro.test(spx.log.m1.res)

# ACF & PACF 
#   Standardized residuals - adequacy of model mean equation
#   Standardized residuals squared - adequacy of model variance equation
# First lag removed
par(mfcol = c(2, 2))
acf(spx.log.m1.res, 25, xlim = c(1, 25), ylim = c(-0.1, 0.1))
pacf(spx.log.m1.res, 25, ylim = c(-0.1, 0.1))
acf(spx.log.m1.res^2, 25, xlim = c(1, 25), ylim = c(-0.1, 0.1))
pacf(spx.log.m1.res^2, 25, ylim = c(-0.1, 0.1))
par(mfcol = c(1, 1))

# Note: residuals do not appear to satisfy normality assumption

#------------------
# Model 2: GARCH(1,1) | Student-t Innovation
#------------------

spx.log.m2 <- garchFit(~garch(1, 1), data = spx.log, trace = F, 
                       cond.dist = "std")

# Model adequacy

# Summary stats
# Includes Ljung-Box results for:
#   Standardized residuals - adequacy of model mean equation
#   Standardized residuals squared - adequacy of model variance equation
summary(spx.log.m2)

# Examine residuals for normality assumption
# Assign standardized residuals
spx.log.m2.res <- residuals(spx.log.m2, standardize = T)

# Q-Q Plot
qqnorm(spx.log.m2.res); qqline(spx.log.m2.res)

# Shapiro test of normality - H0: iid normal
shapiro.test(spx.log.m2.res)

# ACF & PACF 
#   Standardized residuals - adequacy of model mean equation
#   Standardized residuals squared - adequacy of model variance equation
# First lag removed
par(mfcol = c(2, 2))
acf(spx.log.m2.res, 25, xlim = c(1, 25), ylim = c(-0.1, 0.1))
pacf(spx.log.m2.res, 25, ylim = c(-0.1, 0.1))
acf(spx.log.m2.res^2, 25, xlim = c(1, 25), ylim = c(-0.1, 0.1))
pacf(spx.log.m2.res^2, 25, ylim = c(-0.1, 0.1))
par(mfcol = c(1, 1))

# Note: residuals do not appear to satisfy normality assumption

#------------------
# Model 3: GARCH(1,1) | Skewed Student-t Innovation
#------------------

spx.log.m3 <- garchFit(~garch(1, 1), data = spx.log, trace = F, 
                       cond.dist = "sstd")

# Testing for skewness
# If the result is significant (p-value < alpha), reject null hypothesis of 
#   no skewness
# Reject H0: based on p-value, this model appears most appropriate for data
spx.log.m3
spx.log.m3.skew.tratio <- (0.7736 - 1)/(0.04639); spx.log.m3.skew.tratio
spx.log.m3.skew.pv <- 2*pnorm(spx.log.m3.skew.tratio); spx.log.m3.skew.pv

# Model adequacy

# Summary stats
# Includes Ljung-Box results for:
#   Standardized residuals - adequacy of model mean equation
#   Standardized residuals squared - adequacy of model variance equation
summary(spx.log.m3)

# Examine residuals for normality assumption
# Assign standardized residuals
spx.log.m3.res <- residuals(spx.log.m3, standardize = T)

# Q-Q Plot
qqnorm(spx.log.m3.res); qqline(spx.log.m3.res)

# Shapiro test of normality - H0: iid normal
shapiro.test(spx.log.m3.res)

# ACF & PACF 
#   Standardized residuals - adequacy of model mean equation
#   Standardized residuals squared - adequacy of model variance equation
# First lag removed
par(mfcol = c(2, 2))
acf(spx.log.m3.res, 25, xlim = c(1, 25), ylim = c(-0.1, 0.1))
pacf(spx.log.m3.res, 25, ylim = c(-0.1, 0.1))
acf(spx.log.m3.res^2, 25, xlim = c(1, 25), ylim = c(-0.1, 0.1))
pacf(spx.log.m3.res^2, 25, ylim = c(-0.1, 0.1))
par(mfcol = c(1, 1))

# Note: residuals do not appear to satisfy normality assumption

#======================================
# Q1B
#======================================

# Obtain the 1-step to 5-step ahead mean and volatility forecasts using the
#   fitted ARMA-GARCH model with Student-t innovations.

# Note: standardDeviation value is volatility forecast
spx.log.m3.predict <- predict(spx.log.m3, n.ahead = 5); spx.log.m3.predict

#======================================
# Q1C
#======================================

# Fit a GJR model (using APARCH) to the monthly log returns
# Write down the fitted model
# Is the leverage effect statistically significant?
spx.log.m4 <- garchFit(~aparch(1, 1), data = spx.log, delta = 2, 
                       include.delta = F, trace = F,cond.dist="sstd")

# Model adequacy

# Summary stats
# Includes Ljung-Box results for:
#   Standardized residuals - adequacy of model mean equation
#   Standardized residuals squared - adequacy of model variance equation
summary(spx.log.m4)

# Use 1-sided t-test based on H0:
# H0: gamma <= 0
# Ha: gamma > 0
spx.log.m4.tratio <- 0.978
spx.log.m4.pv <- pnorm(spx.log.m4.tratio, lower.tail = F); spx.log.m4.pv

# Examine residuals for normality assumption
# Assign standardized residuals
spx.log.m4.res <- residuals(spx.log.m4, standardize = T)

# Q-Q Plot
qqnorm(spx.log.m4.res); qqline(spx.log.m4.res)

# Shapiro test of normality - H0: iid normal
shapiro.test(spx.log.m4.res)

# ACF & PACF 
#   Standardized residuals - adequacy of model mean equation
#   Standardized residuals squared - adequacy of model variance equation
# First lag removed
par(mfcol = c(2, 2))
acf(spx.log.m4.res, 25, xlim = c(1, 25), ylim = c(-0.1, 0.1))
pacf(spx.log.m4.res, 25, ylim = c(-0.1, 0.1))
acf(spx.log.m4.res^2, 25, xlim = c(1, 25), ylim = c(-0.1, 0.1))
pacf(spx.log.m4.res^2, 25, ylim = c(-0.1, 0.1))
par(mfcol = c(1, 1))

# Note: residuals do not appear to satisfy normality assumption

#==============================================================================
# Problem 2
#==============================================================================

# Read data
da <- read.table("d-fxjpus0514.txt", header = F)

# Assign Values
fx.log <- log(da$V1 + 1)

# Explore summary stats
basicStats(fx.log)

# Produce t-ratio of sample skewness
# Result suggests series is negatively skewed
fx.log.skew.tratio <- skewness(fx.log)/sqrt(6/length(fx.log))
fx.log.skew.tratio

# Plot the time series
plot(fx.log, xlab = "Year", ylab = "Returns", 
     main = "JPY-USD Exchange Rate
     Natual Log - Continuously Compounded", type = 'l')

# ACF & PACF
par(mfcol = c(2, 1))
acf(fx.log)
pacf(fx.log)
par(mfcol = c(1, 1))

#======================================
# Q2A
#======================================

# Fit a GARCH model to the daily log returns
# Write down the fitted model

# Are there serial correlations in the log return?
# ACF plot of series for checking serial correlations in series
# ACF plot of abs(series) for checking dependence in series
par(mfcol = c(2, 1))
acf(fx.log, 25, xlim = c(1, 25), ylim = c(-0.2, 0.4))
acf(abs(fx.log), 25, xlim = c(1, 25), ylim = c(-0.2, 0.4))
par(mfcol = c(1, 1))

# Ljung-Box test
# If the result is significant (p-value < alpha), reject null hypothesis of 
#   no serial correlations
Box.test(fx.log, lag = 10, type = "Ljung")
Box.test(abs(fx.log), lag = 10, type = "Ljung")

#--------------------------------------
# Testing for ARCH effects
#--------------------------------------

# Is the mean (expected return) significantly different from zero?
# Fail to reject H0: based on p-value; mean is not significantly different
#   from zero
# Therefore, mean equation is simply return series
t.test(fx.log)

#------------------
# Examining ACF & PACF
#------------------
# Use squared series of residuals

# If autocorrelations > critical value lines, then:
#   1. Conclude serial correlations in residuals
#   2. Conclude ARCH effects

# ACF & PACF of squared residuals - first lag removed
par(mfcol = c(2, 1))
acf(fx.log^2, 25, xlim = c(1, 25), ylim = c(-0.2, 0.2))
pacf(fx.log^2, 25, ylim = c(-0.2, 0.2))
par(mfcol = c(1, 1))

#------------------
# Ljung-Box test
#------------------
# Use squared series of residuals to check for conditional heteroscedasticity
# H0: first m lags of ACF of squared series = 0
# If we reject H0:, series shows strong ARCH effects

# Test at lag 10 - reject H0:
Box.test(fx.log^2, lag = 10, type = "Ljung")

#--------------------------------------
# Model Building
#--------------------------------------

# Use full data set (not residuals)

#------------------
# Model 1: APARCH(1,1) | Student-t Innovation | leverage = F
# Note: same as GARCH(1,1) with leverage = T
#------------------

fx.log.m1 <- garchFit(~aparch(1, 1), data = fx.log, include.mean = F, delta = 2,
                      include.delta = F, trace = F, cond.dist = "std")

# Model adequacy

# Summary stats
# Includes Ljung-Box results for:
#   Standardized residuals - adequacy of model mean equation
#   Standardized residuals squared - adequacy of model variance equation
summary(fx.log.m1)

# Use 1-sided t-test based on H0:
# H0: gamma <= 0
# Ha: gamma > 0
fx.log.m1.tratio <- 1.934
fx.log.m1.pv <- pnorm(fx.log.m1.tratio, lower.tail = F); fx.log.m1.pv

# Examine residuals for normality assumption
# Assign standardized residuals
fx.log.m1.res <- residuals(fx.log.m1, standardize = T)

# Q-Q Plot
qqnorm(fx.log.m1.res); qqline(fx.log.m1.res)

# Shapiro test of normality - H0: iid normal
shapiro.test(fx.log.m1.res)

# ACF & PACF 
#   Standardized residuals - adequacy of model mean equation
#   Standardized residuals squared - adequacy of model variance equation
# First lag removed
par(mfcol = c(2, 2))
acf(fx.log.m1.res, 25, xlim = c(1, 25), ylim = c(-0.1, 0.1))
pacf(fx.log.m1.res, 25, ylim = c(-0.1, 0.1))
acf(fx.log.m1.res^2, 25, xlim = c(1, 25), ylim = c(-0.1, 0.1))
pacf(fx.log.m1.res^2, 25, ylim = c(-0.1, 0.1))
par(mfcol = c(1, 1))

# Note: residuals do not appear to satisfy normality assumption

#======================================
# Q2B
#======================================

# Fit a volatility model with leverage effect to percentage log return series
fx.log.pc <- (fx.log*100)

#------------------
# Model 1: GARCH(1,1) | Student-t Innovation | leverage = T
# Note: same as APARCH(1,1) with leverage = F
#------------------

fx.log.pc.m1 <- garchFit(~garch(1, 1), data = fx.log.pc, trace = F, 
                         cond.dist = "std", leverage = T, include.mean = F)

# Model adequacy

# Summary stats
# Includes Ljung-Box results for:
#   Standardized residuals - adequacy of model mean equation
#   Standardized residuals squared - adequacy of model variance equation
summary(fx.log.pc.m1)

# Use 1-sided t-test based on H0:
# H0: gamma <= 0
# Ha: gamma > 0
fx.log.pc.m1.tratio <- 1.934
fx.log.pc.m1.pv <- pnorm(fx.log.pc.m1.tratio, lower.tail = F); fx.log.pc.m1.pv

# Examine residuals for normality assumption
# Assign standardized residuals
fx.log.pc.m1.res <- residuals(fx.log.pc.m1, standardize = T)

# Q-Q Plot
qqnorm(fx.log.pc.m1.res); qqline(fx.log.pc.m1.res)

# Shapiro test of normality - H0: iid normal
shapiro.test(fx.log.pc.m1.res)

# ACF & PACF 
#   Standardized residuals - adequacy of model mean equation
#   Standardized residuals squared - adequacy of model variance equation
# First lag removed
par(mfcol = c(2, 2))
acf(fx.log.pc.m1.res, 25, xlim = c(1, 25), ylim = c(-0.1, 0.1))
pacf(fx.log.pc.m1.res, 25, ylim = c(-0.1, 0.1))
acf(fx.log.pc.m1.res^2, 25, xlim = c(1, 25), ylim = c(-0.1, 0.1))
pacf(fx.log.pc.m1.res^2, 25, ylim = c(-0.1, 0.1))
par(mfcol = c(1, 1))

# Note: residuals do not appear to satisfy normality assumption

###############################################################################
# FIN
###############################################################################