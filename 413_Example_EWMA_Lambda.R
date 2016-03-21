###############################################################################
# 413_Assignment_09_EWMA_Lambda.R
# Last updated: 2016-03-07 by MJG
###############################################################################

# Clear workspace
rm(list=ls())

# Load packages
library(quantmod)
library(rmgarch)

#======================================
# Get Data
#======================================

# Download daily adjusted closing prices on Microsoft and the S&P 500 over the
#   period 2000-01-03 to 2012-04-10, and compute continuously compounded returns

# Download price series
getSymbols("MSFT", from = "2000-01-03", to = "2012-04-10")
getSymbols("^GSPC", from = "2000-01-03", to = "2012-04-10")

# Use adjusted prices, merge series, and compute continuously compounded returns
# Can also use periodReturn() but that uses log() not log1p(); see end of code
#   for alternate approach
# Drop first observation (NA)
# Rename columns
returns <- diff(log1p(merge(MSFT$MSFT.Adjusted, GSPC$GSPC.Adjusted)))
returns <- returns[-1]
colnames(returns) <- c("MSFT", "SPX")

#======================================
# Estimate Lambda
#======================================

# Specify the EWMA model
# Note: including mean here based on results of t.test on each series,
#   fail to reject H0: expected mean is not significantly different from zero
ewma.spec.lambda <- ugarchspec(variance.model = list(model = "iGARCH"),
                               mean.model = list(armaOrder = c(0, 0),
                                                 include.mean = T),
                               fixed.pars = list(omega = 0))

ewma.spec.lambda.multi <- multispec(replicate(2, ewma.spec.lambda))

# Fit the EWMA model
returns.m1 <- ugarchfit(ewma.spec.lambda, returns[,1])
returns.m2 <- ugarchfit(ewma.spec.lambda, returns[,2])
returns.m3 <- ugarchfit(ewma.spec.lambda, returns)
returns.m4 <- multifit(ewma.spec.lambda.multi, returns)

# Values of Lambda
coef(returns.m1)[4]         # Lambda = beta1 = 0.9698727
coef(returns.m2)[4]         # Lambda = beta1 = 0.9321161
coef(returns.m3)[4]         # Lambda = beta1 = 0.9526944
coef(returns.m4)[4, 1:2]    # Lambda = beta1 = (0.9698727, 0.9321161)

#======================================
# Calculate EWMA Covariances & Correlations
#======================================

lambda <- coef(returns.m4)[4, 1:2]
cov.ewma <- covEWMA(as.data.frame(returns), lambda = lambda)

# Conditional Variance
returns.cond.cov <- cov.ewma[, 2, 1]

# Conditional Correlation
t <- length(cov.ewma[, 1, 1])
returns.cond.cor <- rep(0, t)
for (i in 1:t) {
    returns.cond.cor[i] <- cov2cor(cov.ewma[i, , ])[1, 2]
}

# Plots
par(mfrow = c(2, 1))
plot(x = time(as.zoo(returns)), y = returns.cond.cov,
     type = "l", xlab = "Time", ylab = "Covariance", lwd = 2, col = "blue",
     main = "EWMA Covariance between MSFT and S&P500");
grid()
abline(h = cov(returns)[1, 2], lwd = 2, col = "red")
plot(x = time(as.zoo(returns)), y = returns.cond.cor,
     type = "l", xlab = "Time", ylab = "Correlation", lwd = 2, col = "blue",
     main = "EWMA Correlation between MSFT and S&P500");
grid()
abline(h = cor(returns)[1, 2], lwd = 2, col = "red")
par(mfrow=c(1,1))

