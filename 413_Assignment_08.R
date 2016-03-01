###############################################################################
# 413_Assignment_08.R
# Last updated: 2016-02-23 by MJG
###############################################################################

# Clear workspace
rm(list=ls())

# Load packages
library(fBasics)
library(MTS)

# Set working directory
setwd("C:/Users/Michael/Dropbox/MSPA/413-DL/Data Sets")

# Load data
data("mts-examples", package = "MTS")
rm(ibmspko, tenstocks)

#==============================================================================
# Problem 1
#==============================================================================

# Read data
da <- read.table("q-fdebt.txt", header = T)

# Transform to log returns
# Note: use log1p(x) instead of log(x+1)
qd.log <- data.frame(da[, 1:2], apply(da[, 3:4], 2, log1p))

# Assign first differenced series
qd.log.diff <- data.frame(qd.log[-1, 1:2], diffM(qd.log[, 3:4]))

# Explore summary stats
basicStats(qd.log.diff[, 3:4])

#======================================
# Q1A
#======================================

# Plot the time series

# Convert to time series
ts.qd.log.diff <- ts(qd.log.diff, start = c(1970, 4), frequency = 4)

# Plot the time series
# HBFIN
plot(ts.qd.log.diff[, 3], xlab = "Year", ylab = "Debt Growth - QoQ", 
     main = "Quarterly U.S. Federal Debt Growth (NSA, Natural Log)
     Held by Foreign & International Investors")

# Plot the time series
# HBFRBN
plot(ts.qd.log.diff[, 4], xlab = "Year", ylab = "Debt Growth - QoQ", 
     main = "Quarterly U.S. Federal Debt Growth (NSA, Natural Log)
     Held by Federal Reserve Banks")

#======================================
# Q1B
#======================================

# Obtain the first 5 lags of the sample cross-correlation matricies of series
# Using level = T will output values and simplified notation
# In simplified notation, 
ccm(qd.log.diff[, 3:4], lags = 5, level = T, output = T)

#======================================
# Q1C
#======================================

# Test the following hypotheses using a 5% significance level:
#   H0: p1 = ... = p10 = 0
#   Ha: pi != 0 for some i, where {1, ..., 10}
mq(qd.log.diff[, 3:4], lag = 10)

#==============================================================================
# Problem 2
#==============================================================================

#--------------------------------------
# EDA
#--------------------------------------

# Explore summary stats, head, tail
basicStats(qgdp)
head(qgdp)
tail(qgdp)

# Transform to log returns
# Note: use log1p(x) instead of log(x+1)
qgdp.log <- data.frame(qgdp[, 1:2], apply(qgdp[, 3:5], 2, log1p))

# Assign first differenced series
qgdp.log.diff <- data.frame(qgdp.log[-1, 1:2], diffM(qgdp.log[, 3:5]))

# Assign percentage (growth) series
qgdp.log.diff.pc <- data.frame(qgdp.log.diff[, 1:2], (qgdp.log.diff[, 3:5]*100))

# Explore summary stats
basicStats(qgdp.log.diff.pc[, 3:5])

# Apply t.test on mean
# Reject H0: true mean not significantly different from zero
# Therefore, mean does not need to subtracted from series (include.mean = F)
apply(qgdp.log.diff.pc[, 3:5], 2, t.test)

#--------------------------------------
# Plot the time series
#--------------------------------------

# Convert to time series
ts.qgdp.log.diff.pc <- ts(qgdp.log.diff.pc, start = c(1980, 4), frequency = 4)

# Plot the time series
# United Kingdom
plot(ts.qgdp.log.diff.pc[, 3], xlab = "Year", ylab = "Percentage Growth Rates", 
     main = "Percentage Growth Rates of Quarterly Real GDP
     United Kingdom, 1980-2011")

# Plot the time series
# Canada
plot(ts.qgdp.log.diff.pc[, 4], xlab = "Year", ylab = "Percentage Growth Rates", 
     main = "Percentage Growth Rates of Quarterly Real GDP
     Canada, 1980-2011")

# Plot the time series
# United States
plot(ts.qgdp.log.diff.pc[, 5], xlab = "Year", ylab = "Percentage Growth Rates", 
     main = "Percentage Growth Rates of Quarterly Real GDP
     United States, 1980-2011")

#--------------------------------------
# Determine Model Order
#--------------------------------------

# Obtain the lags of the sample cross-correlation matricies of series
# Using level = T will output values and simplified notation
# ACFs are on primary diagonal and CCFs are on off diagonal
ccm(qgdp.log.diff.pc[, 3:5], level = T, output = T)

# Test the following hypotheses using a 5% significance level:
#   H0: p1 = ... = pn = 0
#   Ha: pi != 0 for some i, where {1, ..., n}
# Suggests VMA(4)
mq(qgdp.log.diff.pc[, 3:5])

# Use VMAorder to determine model order
# Suggests VMA(2)
VMAorder(qgdp.log.diff.pc[, 3:5])

#--------------------------------------
# Fit Models
#--------------------------------------

# Apply t-test to check if H0: mean is not significantly different from zero
# Reject H0: in all series; include mean in modeled series
apply(qgdp.log.diff.pc[, 3:5], 2, t.test)

#------------------
# Model VAR(4)
#------------------

# Build Model
qgdp.m1 <- VAR(qgdp.log.diff.pc[, 3:5], p = 4, output = T)

# Fix threshold (remove significant coefficients at the 5% level)
qgdp.m1.fixed <- refVAR(qgdp.m1, thres = 1.96)

# Summary stats of residuals
MTSdiag(qgdp.m1.fixed)

#------------------
# Model VAR(2)
#------------------

qgdp.m2 <- VAR(qgdp.log.diff.pc[, 3:5], p = 2, output = T)

# Fix threshold (remove significant coefficients at the 5% level)
qgdp.m2.fixed <- refVAR(qgdp.m2, thres = 1.96)

# Summary stats of residuals
MTSdiag(qgdp.m2.fixed)

###############################################################################
# FIN
###############################################################################
