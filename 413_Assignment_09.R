###############################################################################
# 413_Assignment_09.R
# Last updated: 2016-03-08 by MJG
###############################################################################

# Clear workspace
rm(list=ls())

# Load packages
library(fBasics)
library(fGarch)
library(MTS)
library(quantmod)
library(rmgarch)
library(rugarch)

# Source functions
setwd("C:/Users/Michael/Dropbox/MSPA/413-DL/Code/Functions")
source("413_Function_covEWMA.R")

#==============================================================================
# Problem 1
#==============================================================================

# Download daily adjusted closing prices on Microsoft and the S&P 500 over the
#   period 2000-01-03 to 2012-04-10, and compute continuously compounded returns

# Download price series
getSymbols("MSFT", from = "2000-01-03", to = "2012-04-10")
getSymbols("^GSPC", from = "2000-01-03", to = "2012-04-10")

# Use adjusted prices, merge series, compute continuously compounded returns
# Can also use periodReturn() but that uses log() not log1p(); see end of code
#   for alternate approach
# Drop first observation (NA)
# Rename columns
returns <- diff(log1p(merge(MSFT$MSFT.Adjusted, GSPC$GSPC.Adjusted)))
returns <- returns[-1]
colnames(returns) <- c("MSFT", "SPX")

# Explore summary stats
basicStats(returns)

# Scatterplot of returns
plot(coredata(returns$SPX), coredata(returns$MSFT), xlab = "SPX", ylab = "MSFT",
     type = "p", pch = 16, lwd = 2, col = "blue",
     main = "Scatterplot of Adjusted Daily Closing Log Returns
     2000-01-04 to 2012-04-10")
abline(h = 0, v = 0)

#=====================================
# EDA on MSFT
#=====================================

#-------------------------------------
# Plots
#-------------------------------------

# Prices
plot(MSFT$MSFT.Adjusted, main = "Adjusted Daily Closing Prices: Microsoft
     2000-01-03 to 2012-04-10")

# Returns
plot(returns$MSFT, main = "Adjusted Daily Closing Log Returns: Microsoft
     2000-01-04 to 2012-04-10")

#-------------------------------------
# Check for serial correlations
#-------------------------------------

# Are there serial correlations in the log return?
# ACF plot of series for checking serial correlations in series
# ACF plot of abs(series) for checking dependence in series
par(mfcol = c(2, 1))
acf(returns$MSFT, 25, xlim = c(1, 25), ylim = c(-0.2, 0.4))
acf(abs(returns$MSFT), 25, xlim = c(1, 25), ylim = c(-0.2, 0.4))
par(mfcol = c(1, 1))

# Ljung-Box test
# If the result is significant (p-value < alpha), reject null hypothesis of 
#   no serial correlations
Box.test(returns$MSFT, lag = 10, type = "Ljung")
Box.test(abs(returns$MSFT), lag = 10, type = "Ljung")

#-------------------------------------
# Test for ARCH effects
#-------------------------------------

# Is the mean (expected return) significantly different from zero?
t.test(returns$MSFT)

# Fail to reject H0: based on p-value, do not subtract sample mean from mean
#   equation when testing for ARCH effects

#------------------
# Examining ACF & PACF
#------------------
# Use squared series of residuals

# If autocorrelations > critical value lines, then:
#   1. Conclude serial correlations in residuals
#   2. Conclude ARCH effects

# ACF & PACF of squared residuals - first lag removed
par(mfcol = c(2, 1))
acf(returns$MSFT^2, 25, xlim = c(1, 25), ylim = c(-0.2, 0.2))
pacf(returns$MSFT^2, 25, ylim = c(-0.2, 0.2))
par(mfcol = c(1, 1))

#------------------
# Ljung-Box test
#------------------
# Use squared series of residuals to check for conditional heteroscedasticity
# H0: first m lags of ACF of squared series = 0
# If we reject H0:, series shows strong ARCH effects

# Test at lag 10 - reject H0:
Box.test(returns$MSFT^2, lag = 10, type = "Ljung")

#=====================================
# EDA on SPX
#=====================================

#-------------------------------------
# Plots
#-------------------------------------

# Prices
plot(GSPC$GSPC.Adjusted, main = "Adjusted Daily Closing Prices: S&P 500
     2000-01-03 to 2012-04-10")

# Returns
plot(returns$SPX, main = "Adjusted Daily Closing Log Returns: S&P 500
     2000-01-04 to 2012-04-10")

#-------------------------------------
# Check for serial correlations
#-------------------------------------

# Are there serial correlations in the log return?
# ACF plot of series for checking serial correlations in series
# ACF plot of abs(series) for checking dependence in series
par(mfcol = c(2, 1))
acf(returns$SPX, 25, xlim = c(1, 25), ylim = c(-0.2, 0.4))
acf(abs(returns$SPX), 25, xlim = c(1, 25), ylim = c(-0.2, 0.4))
par(mfcol = c(1, 1))

# Ljung-Box test
# If the result is significant (p-value < alpha), reject null hypothesis of 
#   no serial correlations
Box.test(returns$SPX, lag = 10, type = "Ljung")
Box.test(abs(returns$SPX), lag = 10, type = "Ljung")

#-------------------------------------
# Test for ARCH effects
#-------------------------------------

# Is the mean (expected return) significantly different from zero?
t.test(returns$SPX)

# Fail to reject H0: based on p-value, do not subtract sample mean from mean
#   equation when testing for ARCH effects

#------------------
# Examining ACF & PACF
#------------------
# Use squared series of residuals

# If autocorrelations > critical value lines, then:
#   1. Conclude serial correlations in residuals
#   2. Conclude ARCH effects

# ACF & PACF of squared residuals - first lag removed
par(mfcol = c(2, 1))
acf(returns$SPX^2, 25, xlim = c(1, 25), ylim = c(-0.2, 0.4))
pacf(returns$SPX^2, 25, ylim = c(-0.2, 0.4))
par(mfcol = c(1, 1))

#------------------
# Ljung-Box test
#------------------
# Use squared series of residuals to check for conditional heteroscedasticity
# H0: first m lags of ACF of squared series = 0
# If we reject H0:, series shows strong ARCH effects

# Test at lag 10 - reject H0:
Box.test(returns$SPX^2, lag = 10, type = "Ljung")

#======================================
# Q1A
#======================================

# Estimate an ARCH(5) model for each of the series. What is the sum of the ARCH
#   coefficients?

#------------------
# MSFT
#------------------

# Build model
msft.log.m1 <- garchFit(~1 + garch(5, 0), data = returns$MSFT, trace = F)

# Summary stats
# Includes Ljung-Box results for:
#   Standardized residuals - adequacy of model mean equation
#   Standardized residuals squared - adequacy of model variance equation
summary(msft.log.m1)

# Examine residuals for normality assumption
# Assign standardized residuals
msft.log.m1.res <- residuals(msft.log.m1, standardize = T)

# Q-Q Plot
qqnorm(msft.log.m1.res); qqline(msft.log.m1.res)

# Shapiro test of normality - H0: iid normal
shapiro.test(msft.log.m1.res)

# ACF & PACF 
#   Standardized residuals - adequacy of model mean equation
#   Standardized residuals squared - adequacy of model variance equation
# First lag removed
par(mfcol = c(2, 2))
acf(msft.log.m1.res, 25, xlim = c(1, 25), ylim = c(-0.1, 0.1))
pacf(msft.log.m1.res, 25, ylim = c(-0.1, 0.1))
acf(msft.log.m1.res^2, 25, xlim = c(1, 25), ylim = c(-0.1, 0.1))
pacf(msft.log.m1.res^2, 25, ylim = c(-0.1, 0.1))
par(mfcol = c(1, 1))

# Note: residuals do not appear to satisfy normality assumption

#------------------
# SPX
#------------------

# Build model
spx.log.m1 <- garchFit(~1 + garch(5, 0), data = returns$SPX, trace = F)

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

#======================================
# Q1B
#======================================

# Estimate a GARCH(1,1) model for each of the series. What is the sum of the 
#   ARCH and GARCH coefficients?

#------------------
# MSFT
#------------------

# Build model
msft.log.m2 <- garchFit(~garch(1, 1), data = returns$MSFT, trace = F)

# Summary stats
# Includes Ljung-Box results for:
#   Standardized residuals - adequacy of model mean equation
#   Standardized residuals squared - adequacy of model variance equation
summary(msft.log.m2)

# Examine residuals for normality assumption
# Assign standardized residuals
msft.log.m2.res <- residuals(msft.log.m2, standardize = T)

# Q-Q Plot
qqnorm(msft.log.m2.res); qqline(msft.log.m2.res)

# Shapiro test of normality - H0: iid normal
shapiro.test(msft.log.m2.res)

# ACF & PACF 
#   Standardized residuals - adequacy of model mean equation
#   Standardized residuals squared - adequacy of model variance equation
# First lag removed
par(mfcol = c(2, 2))
acf(msft.log.m2.res, 25, xlim = c(1, 25), ylim = c(-0.1, 0.1))
pacf(msft.log.m2.res, 25, ylim = c(-0.1, 0.1))
acf(msft.log.m2.res^2, 25, xlim = c(1, 25), ylim = c(-0.1, 0.1))
pacf(msft.log.m2.res^2, 25, ylim = c(-0.1, 0.1))
par(mfcol = c(1, 1))

# Note: residuals do not appear to satisfy normality assumption

#------------------
# SPX
#------------------

# Build model
spx.log.m2 <- garchFit(~garch(1, 1), data = returns$SPX, trace = F)

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

#======================================
# Q1C
#======================================

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

# Windows: 20 days = trading month; 252 days = trading year
returns.roll.cov <- rollapply(as.zoo(returns), FUN = cov.fun, width = 20,
                     by.column = F, align = "right")
returns.roll.cor <- rollapply(as.zoo(returns), FUN = cor.fun, width = 20,
                     by.column = F, align = "right")

# Plots
plot(returns.roll.cov, main = "MSFT & SPX: 20-day Rolling Covariances",
     ylab = "Covariance", lwd = 2, col = "blue")
grid()
abline(h = cov(returns)[1,2], lwd = 2, col = "red")

plot(returns.roll.cor, main = "MSFT & SPX: 20-day Rolling Correlations",
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
     main = "MSFT & SPX: EWMA Covariance")
grid()
abline(h = cov(returns)[1, 2], lwd = 2, col = "red")

plot(x = time(as.zoo(returns)), y = returns.cond.cor, type = "l", xlab = "Time",
     ylab = "Correlation", lwd = 2, col = "blue",
     main = "MSFT & SPX: EWMA Correlation")
grid()
abline(h = cor(returns)[1, 2], lwd = 2, col = "red")

#======================================
# Q1D
#======================================

# Use dccfit() from {rmgarch}, and estimate the normal-DCC(1,1) model
# Comment on the estimated coefficients and the model fit

# Specification of DCC model for each series - univariate normal GARCH(1,1)
spec.garch11 <- ugarchspec(variance.model = list(garchOrder = c(1, 1),
                                                 model = "sGARCH"),
                           mean.model = list(armaOrder = c(0, 0)),
                           distribution.model = "norm")

# Multivariate specification of DCC specification for conditional correlations
spec.garch11.dcc <- dccspec(uspec = multispec(replicate(2, spec.garch11)),
                            dccOrder = c(1, 1), distribution = "mvnorm")

# Validate specification of DCC model
spec.garch11.dcc

# Build model
returns.dcc.m1 <- dccfit(spec = spec.garch11.dcc, data = returns)

# Explore available parameters and results
slotNames(returns.dcc.m1)
names(returns.dcc.m1@mfit)
names(returns.dcc.m1@model)

# View DCC GARCH(1,1) fit
returns.dcc.m1

# Q-Q Plot
qqnorm(returns.dcc.m1@mfit$stdresid); qqline(returns.dcc.m1@mfit$stdresid)

# ACF & PACF 
#   Standardized residuals - adequacy of model mean equation
#   Standardized residuals squared - adequacy of model variance equation
# First lag removed
par(mfcol = c(2, 2))
acf(returns.dcc.m1@mfit$stdresid, 25, xlim = c(1, 25), ylim = c(-0.1, 0.1))
pacf(returns.dcc.m1@mfit$stdresid, 25, ylim = c(-0.1, 0.1))
acf(returns.dcc.m1@mfit$stdresid^2, 25, xlim = c(1, 25), ylim = c(-0.1, 0.1))
pacf(returns.dcc.m1@mfit$stdresid^2, 25, ylim = c(-0.1, 0.1))
par(mfcol = c(1, 1))

# Obtain the lags of the sample cross-correlation matricies of series
# Using level = T will output values and simplified notation
# ACFs are on primary diagonal and CCFs are on off diagonal
ccm(returns.dcc.m1@mfit$stdresid, level = T, output = T)

# Test the following hypotheses using a 5% significance level:
#   H0: p1 = ... = pn = 0
#   Ha: pi != 0 for some i, where {1, ..., n}
mq(returns.dcc.m1@mfit$stdresid)

#======================================
# Q1E
#======================================

# Plot the estimated in-sample conditional covariances and correlations of the
#   fitted model; compare the EWMA and rolling estimates
# Note: par(mfrow()) will not work here, as that only works for {base} graphics

#------------------
# Method 1
#------------------
# Conditional Covariances
plot(returns.dcc.m1, which = 3)

# Conditional Correlations
plot(returns.dcc.m1, which = 4)

#------------------
# Method 2
#------------------
# Conditional Covariances
ts.plot(rcov(returns.dcc.m1)[1, 2, ])

# Conditional Correlations
ts.plot(rcor(returns.dcc.m1)[1, 2, ])

#======================================
# Q1F
#======================================

# Compute and plot the first 100 h-step ahead forecasts of the conditional
#   covariance and correlation of the fitted model

# Build model
returns.dcc.m1.fc <- dccforecast(fit = returns.dcc.m1, n.ahead = 100)

# Explore available parameters and results
slotNames(returns.dcc.m1.fc)
names(returns.dcc.m1.fc@mforecast)
names(returns.dcc.m1.fc@model)

# View Forecast DCC GARCH(1,1) fit
returns.dcc.m1.fc

#------------------
# Method 1
# Note: second part fails, see Method 2 below
#------------------
# Conditional Covariances
plot(returns.dcc.m1.fc, which = 3)

# Conditional Correlations
plot(returns.dcc.m1.fc, which = 4)

#------------------
# Method 2
#------------------
# Conditional Covariances
returns.dcc.m1.fc.cov <- rcov(returns.dcc.m1.fc)
returns.dcc.m1.fc.cov <- returns.dcc.m1.fc.cov[[1]]
ts.plot(returns.dcc.m1.fc.cov[1, 2, ],
        main = "SPX & MSFT: DCC Conditional Covariance Forecast", 
        ylab = "Correlation", xlab = "Time (Trading Days)")

# Conditional Correlations
returns.dcc.m1.fc.cor <- rcor(returns.dcc.m1.fc)
returns.dcc.m1.fc.cor <- returns.dcc.m1.fc.cor[[1]]
ts.plot(returns.dcc.m1.fc.cor[1, 2, ], 
        main = "SPX & MSFT: DCC Conditional Correlation Forecast", 
        ylab = "Correlation", xlab = "Time (Trading Days)")

###############################################################################
# FIN
###############################################################################

#==============================================================================
# Alternate Approach for Downloading Data
#==============================================================================

# Download price series
getSymbols("MSFT", from = "2000-01-03", to = "2012-04-10")
getSymbols("^GSPC", from = "2000-01-03", to = "2012-04-10")

#--------------------------------------
# First Approach
#--------------------------------------

# Merge series and calculate returns
returns <- round(diff(log(merge(MSFT$MSFT.Adjusted, GSPC$GSPC.Adjusted))),
                 digits = 6)
returns <- returns[-1]

#--------------------------------------
# Second Approach
#--------------------------------------
msft.test <- round(periodReturn(MSFT$MSFT.Adjusted, period = "daily", 
                                  type = 'log'), digits = 6)
spx.test <- round(periodReturn(GSPC$GSPC.Adjusted, period = "daily",
                                 type = "log"), digits = 6)
returns.test <- merge(msft.test[-1], spx.test[-1])
returns == returns.test
