###############################################################################
# W01_rTutorials
# Last updated: 2016-01-06 by MJG
###############################################################################

# Clear the workspace
rm(list=ls())

###############################################################################
# quantmod
###############################################################################

library(quantmod)

#==============================================================================
# AAPL Equity Data from Yahoo!
#==============================================================================

# Download AAPL equity data, assign object
getSymbols("AAPL")

# Explore dimensions, head, and tail
dim(AAPL)
head(AAPL)
tail(AAPL)

# Plot AAPL
chartSeries(AAPL, theme = "white")
chartSeries(AAPL)

# Download AAPL equity data, assign object, specify date ranges
getSymbols("AAPL", from = "2005-01-02", to = "2010-12-31")

# Explore dimensions, head, and tail
dim(AAPL)
head(AAPL)
tail(AAPL)

#==============================================================================
# Unemployment Rate from FRED
#==============================================================================

# Download monthly unemployment rate data from FRED (U-3)
getSymbols("UNRATE", src = "FRED")

# Explore dimensions, head, and tail
dim(UNRATE)
head(UNRATE)
tail(UNRATE)

# Plot UNRATE
chartSeries(UNRATE, theme = "white")

#==============================================================================
# INTC Equity Data from Google
#==============================================================================

# Download INTC equity data, assign object
getSymbols("INTC", src = "google")

# Explore dimensions, head, and tail
dim(INTC)
head(INTC)
tail(INTC)

#==============================================================================
# 10-year Treasury Note Data from Yahoo!
#==============================================================================

# Download TNX data, assign object
getSymbols("^TNX")

# Explore dimensions, head, and tail
dim(TNX)
head(TNX)
tail(TNX)

# Plot TNX, no volume
chartSeries(TNX, theme = "white", TA = NULL)

###############################################################################
# fBasics
###############################################################################

# Set working directory
setwd("C:/Users/mgilbert/Desktop/Personal/School/Textbooks/IntroTS")

library(fBasics)

#==============================================================================
# Demo commands
#==============================================================================

# Assign X a value of 10 and display it
x <- 10; x

# Basic operation: addition
1+2

# Basic operation: division
10/2

# Loading .TXT data
da <- read.table('d-ibm-0110.txt', header = T)

# Explore dimensions, head, and tail
dim(da)
head(da)
tail(da)

# Loading .CSV data
da <- read.csv("d-vix0411.csv", header = T)

# Explore dimensions, head, and tail
dim(da)
head(da)
tail(da)

###############################################################################
# quantmod
###############################################################################

library(quantmod)

#==============================================================================
# AAPL Equity Data from Yahoo!
#==============================================================================

# Download AAPL equity data, assign object, specify date ranges
getSymbols("AAPL", from = "2007-01-03", to = "2011-12-02")

# Compute log returns and plot
AAPL.rtn <- diff(log(AAPL$AAPL.Adjusted))
chartSeries(AAPL.rtn, theme = "white")

#==============================================================================
# 10-year Treasury Note Data from Yahoo!
#==============================================================================

# Download TNX data, assign object, specify date ranges
getSymbols("^TNX", from = "2007-01-03", to = "2011-12-02")

# Compute difference and plot
TNX.rtn <- diff(TNX$TNX.Adjusted)
chartSeries(TNX.rtn, theme = "white")

#==============================================================================
# USD:EUR Exchange Rate Data from FRED
#==============================================================================

# Download USD:EUR exchange rate data, assign object, specify date ranges
getSymbols("DEXUSEU", src = "FRED")

# Explore dimensions, head, and tail
dim(DEXUSEU)
head(DEXUSEU)
tail(DEXUSEU)

# Compute log returns and plot
USEU.rtn <- diff(log(DEXUSEU$DEXUSEU))
chartSeries(DEXUSEU, theme = "white")
chartSeries(USEU.rtn, theme = "white")

###############################################################################
# fBasics
# Chapter 1 - IntroTS (p25)
###############################################################################

setwd("C:/Users/mgilbert/Desktop/Personal/School/Textbooks/IntroTS/Data/ch01data")

library(fBasics)

# Read the .TXT file
da = read.table("d-mmm-0111.txt", header = T)

# View the head
head(da)

# Obtain 3M simple returns
mmm = da[,2]

# Compute summary statistics
# LCL = lower end of 95% CI; UCL = upper end of 95% CI
basicStats(mmm)

# Commands for individual moments
mean(mmm)
var(mmm)
stdev(mmm)

# One sample t-test; testing mean return = 0
t.test(mmm)

# Additional metrics
s3 <- skewness(mmm)                     # Skewness
T <- length(mmm); T                     # Sample size
t3 <- s3 / sqrt(6/T); t3                # Skewness test

pp <- 2 * (1 - pnorm(t3)); pp           # Compute p-value

# Kurtosis test
s4 <- kurtosis(mmm)
t4 <- s4 / sqrt(24/T); t4               # Value is huge; reject the null

# JB-test
normalTest(mmm, method = "jb")
# p-value is less than significance level of 0.05, H0 is rejcted (p24)
# Normality is rejected, confirmed by plot of EDF w/ normal overlay (p28)

###############################################################################
# FIN
###############################################################################
