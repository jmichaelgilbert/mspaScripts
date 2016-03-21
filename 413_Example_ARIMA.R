###############################################################################
# 413_ARIMA_Diff_Example.R
# Last updated: 2016-02-06 by MJG
###############################################################################

# Clear workspace
rm(list=ls())

# Set working directory
#setwd("C:/Users/Michael/Dropbox/MSPA/413-DL/Data Sets")
setwd("C:/Users/michael.gilbert/Dropbox/MSPA/413-DL/Data Sets")

# Load packages
library(fpp)
library(fBasics)
library(fUnitRoots)

# Read data
da <- read.table("q-earn-msft.txt", header = T)

# Assign values
ms <- da$value

# Create log returns
ms.log <- log(ms)

# Test Model 1
ms.log.test.m1 <- arima(diff(ms.log), order = c(1, 0, 1), include.mean = F)
ms.log.test.m1

# Test Model 2
ms.log.test.m2 <- arima(ms.log, order = c(1, 1, 1), include.mean = T)
ms.log.test.m2

# Compare
summary(ms.log.test.m1) == summary(ms.log.test.m2)
ms.log.test.m1$residuals == ms.log.test.m2$residuals
ms.log.test.m1$var.coef == ms.log.test.m2$var.coef
