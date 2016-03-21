###############################################################################
# 413_IntroTS_Chapter04
# Last updated: 2016-02-13 by MJG
###############################################################################

# Clear the workspace
rm(list=ls())

# Load packages
library(quantmod)
library(fpp)

# Set working directory
#   Home
setwd("C:/Users/Michael/Dropbox/Textbooks/IntroTS/Data/ch04data")

# Read data
da <- read.table("m-intcsp7309.txt", header = T)

intc <- log(da$intc+1)

# Get basic stats on monthly log returns of INTC
basicStats(intc)

#--------------------------------------
# Test for ARCH effects
#--------------------------------------

# Test the null hypothesis that mean of monthly log returns of INTC is 0
# H0: mu = 0 at alpha of 0.05
t.test(intc)

# Reject H0, subtract mean when testing for ARCH effects
y <- (intc - mean(intc))

# Pormanteau test at lags 12 and 24 (for monthly returns)
# H0: first m lags of ACF of squared series = 0
# If we reject H0, series shows strong ARCH effects

# Test at lag 12 - reject H0 at lag 12
Box.test(y^2, lag = 12, type = 'Ljung')

# Test at lag 24 - reject H0 at lag 24
Box.test(y^2, lag = 24, type = 'Ljung')

# ARCH Test (note: requires sourcing) at lags 12 and 24 (for monthly returns)
# H0: series contains ARCH effects

# Test at lag 12 - reject H0 at lag 12
archTest(y, 12)

# Test at lag 24 - reject H0 at lag 24
archTest(y, 24)

# ACF & PACF first lag removed
par(mfcol = c(2, 1))
acf(y^2, 25, xlim = c(1, 25), ylim = c(-0.4, 0.4))
pacf(y^2, 25, ylim = c(-0.4, 0.4))
par(mfcol = c(1, 1))

#==============================================================================
# Following example on page 179
#==============================================================================

par(mfcol = c(2, 1))
acf(intc, 25, xlim = c(1, 25), ylim = c(-0.4, 0.4))
acf(abs(intc), 25, ylim = c(-0.4, 0.4))
par(mfcol = c(1, 1))

#==============================================================================
# Following example on page 194
#==============================================================================

m1 <- garchFit(~1 + garch(3, 0), data = intc, trace = F)
summary(m1)

m2 <- garchFit(~1 + garch(1, 0), data = intc, trace = F)
summary(m2)

resi <- residuals(m2, standardize = T)

acf(resi, lag = 20)
pacf(resi^2, lag = 20)

Box.test(resi, lag = 10, type = "Ljung")
Box.test(resi, lag = 20, type = "Ljung")

Box.test(resi^2, lag = 10, type = "Ljung")
Box.test(resi^2, lag = 20, type = "Ljung")

#==============================================================================
# Following example on page 214
#==============================================================================

y <- intc*100
intc.m1 <- garchM(y)
