###############################################################################
# 413_Assignment_01.R
# Last updated: 2016-01-13 by MJG
###############################################################################

# Clear workspace
rm(list=ls())

# Set working directory
setwd("C:/Users/mgilbert/Desktop/Personal/School/MSPA/413-DL/Data Sets")

# Load packages
library(fBasics)
library(fpp)

#==============================================================================
# Problem 1
#==============================================================================

# Read data
returns <- read.table("d-nflx3dx0913.txt", header = T)

# Explore summary stats, head, tail
summary(returns)
head(returns)
tail(returns)

# Function to table results
tableBS <- function(ts) {
    bs <- as.data.frame(sapply(ts, basicStats))
    colnames(bs) <- colnames(ts)
    rownames(bs) <- rownames(basicStats(ts[, 1]))
    bs
}

#======================================
# Q1A
#======================================

# Get basic stats on each of the four tickers
tableBS(returns[, c(3:6)])

#======================================
# Q1B
#======================================

# Transform simple returns to log returns, then repeat
# A log return is the same as a continuously compounded return
returnsLog <- log(returns[, 3:6]+1)

# Get basic stats on log returns on each of the four tickers
tableBS(returnsLog[, c(1:4)])

#======================================
# Q1C
#======================================

# Test the null hypothesis that the mean of the log returns of NFLX is 0
t.test(returnsLog$nflx)

#======================================
# Q1D
#======================================

# Obtain the empirical density plot of the daily log returns of NFLX stock
# and the S&P composite index

# Set to 1x2
par(mfcol = c(1, 2))

# NFLX
edpNFLX <- density(returnsLog$nflx)
plot(edpNFLX$x, edpNFLX$y, xlab = "Returns", ylab = "Density",
     main = "Netflix", sub = "Normal Distribution Overlay (blue)", 
     type = "l", lwd = 2)
xfit <- seq(min(returnsLog$nflx), max(returnsLog$nflx), length = 100)
yfit <- dnorm(xfit, mean = mean(returnsLog$nflx), sd = sd(returnsLog$nflx))
lines(xfit, yfit, col = "blue", lwd = 2)

# SP
edpSP <- density(returnsLog$sprtrn)
plot(edpSP$x, edpSP$y, xlab = "Returns", ylab = "Density",
     main = "S&P Composite", sub = "Normal Distribution Overlay (blue)",
     type = "l", lwd = 2)
xfit <- seq(min(returnsLog$sprtrn), max(returnsLog$sprtrn), length = 100)
yfit <- dnorm(xfit, mean = mean(returnsLog$sprtrn), sd = sd(returnsLog$sprtrn))
lines(xfit, yfit, col = "blue", lwd = 2)

# Set to 1x1
par(mfcol = c(1, 1))

#==============================================================================
# Problem 2
#==============================================================================

# Read data
returns <- read.table("m-ge3dx8113.txt", header = T)

# Explore summary stats, head, tail
summary(returns)
head(returns)
tail(returns)

# Transform simple returns to log returns, then repeat
# A log return is the same as a continuously compounded return
returnsLog <- log(returns[, 3:6]+1)

# GE
ge <- returnsLog$ge

#======================================
# Q2A
#======================================

# Two ways of getting to 95% confidence interval:
basicStats(ge)
t.test(ge)

#======================================
# Q2B
#======================================

# Test H0: M3 = 0 vs. Ha: M3 != 0; where M3 = skewness of return
# Here we are testing the symmetry of geM3 with respect to the mean
# This is the third moment (M3)
geM3 <- skewness(ge)/sqrt(6/length(ge)); geM3

# Compute p-value for skewness
# pnorm computes the value to the left
# 1-pnorm computes value to the right
pv <- 2*(1-pnorm(abs(geM3))); pv

#======================================
# Q2C
#======================================

# Test H0: K = 3 vs. Ha: K != 3, where K = kurtosis of return
# Here we are testing the tail behavior of geK with respect to the mean
# This is the fourth moment (M4)
geM4 <- kurtosis(ge, method = "excess")/sqrt(24/length(ge)); geM4

# Compute p-value for kurtosis
pv <- 2*(1-pnorm(abs(geM4))); pv

#==============================================================================
# Problem 3
#==============================================================================

# Load data
ts.visitors <- visitors

# Explore summary stats, head, and tail
summary(ts.visitors)
head(ts.visitors)
tail(ts.visitors)

#======================================
# Q3A
#======================================

# Plot the time-series
plot(ts.visitors, ylab = "Visitors", xlab = "Year", 
     main = "Time-Series of Monthly Australian Short-Term Overseas Visitors
     May 1985 - April 2005",
     type = "o")

#======================================
# Q3B & Q3C
#======================================

# Forecast the next two years using Holt-Winters' multiplicative method

#==================
# Multiplicative
#==================

fit1 <- hw(ts.visitors, h = 24, seasonal = "multiplicative")

# Plot the data
plot(fit1, ylab = "Visitors", xlab = "Year", 
     main = "Time-Series of Monthly Australian Short-Term Overseas Visitors
     May 1985 - April 2005",
     type = "o")
# Use line below to show the fit tracing the data
# lines(fitted(fit), col = "red", lty = 2)
lines(fit1$mean, type = "o", col = "red")
legend("topleft", lty = 1, pch = 1, col = 1:2,
       c("Historic Data", "Forecast: Holt-Winters' Multiplicative"))

# Plot the components
statesFit1 <- cbind(fit1$model$states[,1:3])
colnames(statesFit1) <- c("level", "slope", "seasonal")
plot(statesFit1, xlab = "Year")

# Print the components to console
fit1$model$state[,1:3]

# Print historic fitted values to console
fitted(fit1)

# Print forecast values to console (lo 80, hi 80, lo 95, hi 95)
fit1

# Print forecast mean values to console
fit1$mean

#==================
# Additive
#==================

fit2 <- hw(ts.visitors, h = 24, seasonal = "additive")

# Plot the data
plot(fit2, ylab = "Visitors", xlab = "Year", 
     main = "Time-Series of Monthly Australian Short-Term Overseas Visitors
     May 1985 - April 2005",
     type = "o")
# Use line below to show the fit tracing the data
# lines(fitted(fit), col = "red", lty = 2)
lines(fit2$mean, type = "o", col = "red")
legend("topleft", lty = 1, pch = 1, col = 1:2,
       c("Historic Data", "Forecast: Holt-Winters' Additive"))

# Plot the components
statesFit2 <- cbind(fit2$model$states[,1:3])
colnames(statesFit2) <- c("level", "slope", "seasonal")
plot(statesFit2, xlab = "Year")

# Print the components to console
fit2$model$state[,1:3]

# Print historic fitted values to console
fitted(fit2)

# Print forecast mean values to console
fit2$mean

#==================
# Combined Plot
#==================

# Set to 1x2
par(mfcol = c(1, 2))

# Plot
statesFit1 <- cbind(fit1$model$states[,1:3])
colnames(statesFit1) <- c("level", "slope", "seasonal")
plot(statesFit1, main = "Multiplicative Seasonality", xlab = "Year")

statesFit2 <- cbind(fit2$model$states[,1:3])
colnames(statesFit2) <- c("level", "slope", "seasonal")
plot(statesFit2, main = "Additive Seasonality", xlab = "Year")

# Set to 1x1
par(mfcol = c(1, 1))

#======================================
# Q3D
#======================================

#==================
# Multiplicative: Exponential and Damped
#==================

# Multiplicative Exponential
fit3 <- hw(ts.visitors, h = 24, seasonal = "multiplicative", exponential = T)

# Multiplicative Damped
fit4 <- hw(ts.visitors, h = 24, seasonal = "multiplicative", damped = T)

# Multiplicative Exponential & Damped
fit5 <- hw(ts.visitors, h = 24, seasonal = "multiplicative",
           exponential = T, damped = T)

# Plot the data
# Legend resized using 'cex = 0.5' option
plot(fit1, ylab = "Visitors", xlab = "Year", 
     main = "Time-Series of Monthly Australian Short-Term Overseas Visitors
     May 1985 - April 2005",
     plot.conf = F, type = "o")
lines(fit1$mean, type = "o", col = 2)
lines(fit3$mean, type = "o", col = 3)
lines(fit4$mean, type = "o", col = 5)
lines(fit5$mean, type = "o", col = 6)
legend("topleft", lty = 1, pch = 1, col = 1:6,
       c("Historic Data", "Holt-Winters' Multiplicative",
         "Holt-Winters' Multiplicative Exponential",
         "Holt-Winters' Multiplicative Damped",
         "Holt-Winters' Multiplicative Exponential & Damped"),
       cex = 0.5)

#======================================
# Q3E
#======================================

# Compare RMSE of the one-step forecasts from the various methods
accuracy(fit2)   # HW Additive
accuracy(fit1)   # HW Multiplicative
accuracy(fit3)   # HW Multiplicative Exponential
accuracy(fit4)   # HW Multiplicative Damped
accuracy(fit5)   # HW Multiplicative Exponential Damped

#======================================
# Q3F & Q3G
#======================================

# Fitting additional models

# Load data
ts.visitors <- visitors

# Multiplicative Holt Winters' method
Q3F1 <- hw(ts.visitors, h = 24, seasonal = "multiplicative")
plot(Q3F1)                              # Forecast plotted
hist(residuals(Q3F1), nclass = 20)      # Residuals plotted (histogram)
plot(residuals(Q3F1))                   # Residuals plotted (line chart)
accuracy(Q3F1)
Q3F1$model

# ETS model
Q3F2 <- forecast(ets(ts.visitors), h = 24, opt.crit = "mse")
plot(Q3F2)                              # Forecast plotted
hist(residuals(Q3F2), nclass = 20)      # Residuals plotted (histogram)
plot(residuals(Q3F2))                   # Residuals plotted (line chart)
accuracy(Q3F2)
Q3F2$model

# Additive ETS model applied to a Box-Cox transformed series
Q3F3 <- forecast(ets(ts.visitors, lambda = T, model = "AAA"), h = 24)
plot(Q3F3)                              # Forecast plotted
hist(residuals(Q3F3), nclass = 20)      # Residuals plotted (histogram)
plot(residuals(Q3F3))                   # Residuals plotted (line chart)
accuracy(Q3F3)
Q3F3$model

# Seasonal naive method applied to a Box-Cox transformed series
Q3F4 <- forecast(snaive(ts.visitors, lambda = T), h = 24)
plot(Q3F4)                              # Forecast plotted
hist(residuals(Q3F4), nclass = 20)      # Residuals plotted (histogram)
plot(residuals(Q3F4))                   # Residuals plotted (line chart)
accuracy(Q3F4)
Q3F4$model

# STL decomposition applied to a Box-Cox transformed data, followed by an
# ETS model applied to the seasonally adjusted (transformed) data
Q3F5 <- forecast(stlm(ts.visitors, method = "ets", lambda = T), h = 24)
plot(Q3F5)                              # Forecast plotted
hist(residuals(Q3F5), nclass = 20)      # Residuals plotted (histogram)
plot(residuals(Q3F5))                   # Residuals plotted (line chart)
accuracy(Q3F5)
Q3F5$model

###############################################################################
# FIN
###############################################################################
