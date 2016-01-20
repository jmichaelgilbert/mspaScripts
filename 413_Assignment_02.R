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
library(quantmod)

#==============================================================================
# Problem 1
#==============================================================================

# Read data
returns <- read.table("m-ge3dx8113.txt", header = T)

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
returnsLog <- log(returns[,3:6]+1)

# Get basic stats on log returns on each of the four tickers
tableBS(returnsLog[, c(1:4)])

#======================================
# Q1C
#======================================

# Test the null hypothesis that the mean of the log returns of GE is 0
t.test(returnsLog$ge)

#======================================
# Q1D
#======================================

# Obtain the empirical density plot of the daily log returns of GE stock
# and the S&P composite index

# Set to 1x2
par(mfcol = c(1, 2))

# GE
edpGE <- density(returnsLog$ge)
plot(edpGE$x, edpGE$y, xlab = "Log Returns", ylab = "Density",
     main = "General Electric", sub = "Normal Distribution Overlay (blue)", 
     type = "l", lwd = 2)
xfit <- seq(min(returnsLog$ge), max(returnsLog$ge), length = 100)
yfit <- dnorm(xfit, mean = mean(returnsLog$ge), sd = sd(returnsLog$ge))
lines(xfit, yfit, col = "blue", lwd = 2)

# SP
edpSP <- density(returnsLog$sprtrn)
plot(edpSP$x, edpSP$y, xlab = "Log Returns", ylab = "Density",
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
returns <- read.table("d-nflx3dx0913.txt", header = T)

# Explore summary stats, head, tail
summary(returns)
head(returns)
tail(returns)

# Transform simple returns to log returns, then repeat
# A log return is the same as a continuously compounded return
returnsLog <- log(returns[,3:6]+1)

# NFLX
nflx <- returnsLog$nflx

#======================================
# Q2A
#======================================

# Test H0: M3 = 0 vs. Ha: M3 != 0; where M3 = skewness of return
# Here we are testing the symmetry of nflxM3 with respect to the mean
# This is the third moment (M3)
nflxM3 <- skewness(nflx)/sqrt(6/length(nflx)); nflxM3

# Compute p-value for skewness
# pnorm computes the value to the left
# 1-pnorm computes value to the right
pv <- 2*(1-pnorm(abs(nflxM3))); pv

#======================================
# Q2B
#======================================

# Test H0: K = 3 vs. Ha: K != 3, where K = kurtosis of return
# Here we are testing the tail behavior of nflxM4 with respect to the mean
# This is the fourth moment (M4)
nflxM4 <- kurtosis(nflx, method = "excess")/sqrt(24/length(nflx)); nflxM4

# Compute p-value for kurtosis
pv <- 2*(1-pnorm(abs(nflxM4))); pv

#======================================
# Q2C
#======================================

# Two ways of getting to 95% confidence interval:
basicStats(nflx)
t.test(nflx)

#==============================================================================
# Problem 3
#==============================================================================

# Load data
ts.cars <- ukcars

# Explore summary stats, head, and tail
summary(ts.cars)
head(ts.cars)
tail(ts.cars)

#======================================
# Q3A
#======================================

# Plot the time-series
plot(ts.cars, ylab = "Cars Produced (000s)", xlab = "Year", 
     main = "Quarterly UK Passenger Car Production (000s) 
     1977Q1 - 2005Q1",
     type = "o")

#======================================
# Q3B
#======================================

# Decompose the series using STL
# robust = T = robust to outliers; occasional outliers will not affect
# estimates of the trend-cycle and seasonal components, but do affect remainder
carsDecomp <- stl(ts.cars, s.window = 4, robust = T)

# Print historic fitted values to console
carsDecomp

# Plot
plot(carsDecomp, main = "Quarterly UK Passenger Car Production (000s)")

# Obstain seasonally adjusted (deseasonalized) data; remove seasonal component
carsSA <- seasadj(carsDecomp)

#======================================
# Q3C
#======================================

# Forecast next two years using additive damped trend method applied to 
# the seasonally adjusted (deseasonalized) data

#======================================
    # Three approaches that all yield same results:

    # 1. Using stlf(), specify the original time series
carsFit1 <- stlf(ts.cars, s.window = 4, robust = T, h = 8, damped = T)

    # 2. Using forecast.stl(), specify the decomposed time series
carsFit2 <- forecast.stl(stl(ts.cars, s.window = 4, robust = T),
                         h = 8, damped = T)

    # 3. Same as (2) but not as redundant
carsFit3 <- forecast.stl(carsDecomp, h = 8, damped = T)
#======================================

# Using the first for simplicity:
carsFitP <- stlf(ts.cars, s.window = "periodic", robust = T, h = 8, damped = T)
carsFit4 <- stlf(ts.cars, s.window = 4, robust = T, h = 8, damped = T)

# Check results
accuracy(carsFitP); carsFitP$model
accuracy(carsFit4); carsFit4$model

# Model with s.window = 4 is more accurate. Suggests seasonal effect is not
# identical year-over-year.

#======================================
# Q3D
#======================================

# Forecast next two years using Holt's linear applied method to the seasonally
# adjusted (deseasonalized) data

# Need to specifically use the seasonally adjusted data here, as function
# will not take care of it; verify with carsFitH$model$x

carsFitH1 <- holt(carsSA)
carsFitH1$model$x == carsSA

carsFitH2 <- holt(ts.cars)
carsFitH2$model$x == carsSA

# Now do complete specification of model:
carsFitH <- holt(carsSA, seasonal = "additive", initial = "optimal", h = 8)

# Check results
accuracy(carsFitH); carsFitH$model

#======================================
# Q3E
#======================================

# Using ETS to select seasonal model ("ANA" is chosen)
carsFitETS <- forecast(ets(ts.cars, opt.crit = "mse"), robust = T, h = 8)

# Check results
accuracy(carsFitETS); carsFitETS$model

#======================================
# Q3F
#======================================

# Compare accuracy across models
accuracy(carsFit4); carsFit4$model          #Q3C
accuracy(carsFitH); carsFitH$model          #Q3D
accuracy(carsFitETS); carsFitETS$model      #Q3E

# Plot data
plot(carsFit4$residuals, ylab = "Residuals", xlab = "Year", 
     main = "Residuals from Various Forecast Methods", type = "o", col = 2)
lines(carsFitH$residuals, type = "o", col = 4)
lines(carsFitETS$residuals, type = "o", col = 3)
legend("bottom", lty = 1, pch = 1, col = c(2,4,3),
       c("Q3C: carsFit4", "Q3D: carsFitH", "Q3E: carsFitETS"))

#======================================
# Q3F
#======================================

# Compare forecasts from various methods. Which seems most reasonable?

plot(ts.cars, ylab = "Cars Produced (000s)", xlab = "Year", 
     main = "Quarterly UK Passenger Car Production (000s) 
     1977Q1 - 2005Q1",
     type = "o", xlim = c(1977, 2007))
lines(carsFit4$mean, type = "o", col = 2)
lines(carsFitH$mean, type = "o", col = 4)
lines(carsFitETS$mean, type = "o", col = 3)
legend("bottomright", lty = 1, pch = 1, col = c(2,4,3),
       c("Q3C: carsFit4", "Q3D: carsFitH", "Q3E: carsFitETS"))

###############################################################################
# FIN
###############################################################################
