###############################################################################
# 413_Log_Example
# Last updated: 2016-03-08 by MJG
###############################################################################

# Clear the workspace
rm(list=ls())

# Load packages
library(fpp)
library(PerformanceAnalytics)
library(quantmod)

#==============================================================================
# Natural Log (Continuously Compounded) Returns
#==============================================================================

# Download data
getSymbols("ATVI")

# Plot price series
chartSeries(ATVI, theme = "white")

#--------------------------------------
# Log returns
#--------------------------------------
# Note: periodReturn() default is to use closing prices, adjusted prices take
#   dividend payments and stock splits into account

# Assign data
atvi.log.1 <- round(periodReturn(ATVI, period = "daily", type = "log"), 
                    digits = 6)
atvi.log.2 <- round(diff(log(ATVI$ATVI.Close)), digits = 6)
atvi.log.3 <- round(CalculateReturns(ATVI$ATVI.Close, method = "log"), 
                    digits = 6)

# Test equality
atvi.log.1[-1] == atvi.log.2[-1]
atvi.log.1[-1] == atvi.log.3[-1]
atvi.log.2[-1] == atvi.log.3[-1]

#--------------------------------------
# Arithmetic Returns
#--------------------------------------
# Note: periodReturn() default is to use closing prices, adjusted prices take
#   dividend payments and stock splits into account

# Assign data
atvi.art.1 <- round(periodReturn(ATVI, period = "daily", type = "arithmetic"), 
                    digits = 6)
atvi.art.2 <- round(Delt(ATVI$ATVI.Close), digits = 6)
atvi.art.3 <- round(CalculateReturns(ATVI$ATVI.Close, method = "discrete"), 
                    digits = 6)

# Test equality
atvi.art.1[-1] == atvi.art.2[-1]
atvi.art.1[-1] == atvi.art.3[-1]
atvi.art.2[-1] == atvi.art.3[-1]
