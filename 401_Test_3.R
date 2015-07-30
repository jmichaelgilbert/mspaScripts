# Test 3: Predict 401-DL
# Last updated: 2015-05-16 by MJG

# Clear workspace
rm(list=ls())

# Q1: Use the degree of confidence and sample data to construct a confidence
# interval for the population of proportion p.
# N = 56, x = 30; 95% confidence

n <- 56
x <- 30
zValue <- qnorm(1 - 0.05 / 2) #Resulting from 95% confidence
pHat <- x/n
moe <- zValue * sqrt((pHat * ((1 - pHat)/n)))
ciLower <- pHat - moe
ciUpper <- pHat + moe
print(round(c(ciLower, pHat, ciUpper), 3))

# Q2: Use the given data to find the minimum sample size required to estimate
# the population proportion.
# MOE: 0.005; confidence level: 96%; p and q unknown. Use z = 2.05

moe <- 0.005
n = (0.5) * (0.5) * ((2.05 / moe)**2)
round(n, 2)

# Note: using the confidence level, we could solve more "accurately" by
# calculating actual zValue, instead of using 2.05:

zValue <- qnorm(1 - 0.04 / 2)
moe <- 0.005
pHat <- 0.50
qHat <- 0.50
n = (pHat) * (qHat) * ((zValue / moe)**2)
n

# Q3: Use the given data to find the minimum sample size requried to estimate
# the population proportion.
# MOE: 0.04; confidence level: 95%; from a prior study, p is estimated by the
# decimal equivalent of 60%

moe <- 0.04
zValue <- qnorm(1 - 0.05 / 2)
pHat <- 0.60
qHat <- (1 - pHat)
n <- (pHat) * (qHat) * ((zValue / moe)**2)
round(n, 0)

# Q4: Use the given degree of confidence and sample data to construct a 
# confidence interval for the population mean mu. Assume that the population
# has a normal distribution.
# n = 10, xbar = 8.1, s = 4.8, confidence level: 95%

zValue <- qnorm(1 - 0.05 / 2)
n <- 10
# xBar = sample mean
xBar <- 8.1
# s = sample standard deviation
s <- 4.8
# SE = standard error estimate
SE <- s/sqrt(n)

# use 0.975 because alpha/2 = 0.05/2 = 0.025 at each tail
moe <- qt(0.975, df = n - 1) * SE
ciLower <- xBar - moe
ciUpper <- xBar + moe
print(c(ciLower, xBar, ciUpper))

# Q5: Use the information to find the minimum sample size required to estimate
# an unknown population mean mu.
# MOE = 135, confidence level: 95%, sigma = 500
# More info see: http://bit.ly/1PNbPRf

moe <- 135
zValue <- qnorm(1 - 0.05 / 2)
sigma <- 500
n <- ((zValue**2 * sigma**2) / (moe**2))
round(n, 0)

# Q6: Solve the problem. A 99% confidence interval (in inches) for the mean
# height of the population is 65.7 < mu < 67.3. This result is based on a 
# sample of size 144. Construct the 95% confidence interval. (Hint: you will
# first need to find the sample mean and sample standard deviation.)

n <- 144
zScore <- qnorm(1 - 0.01 / 2)
ciLower <- 65.7
ciUpper <- 67.3
xBar <- mean(c(ciLower, ciUpper))
s <- sqrt(144) * (ciUpper - xBar) / zScore
SE <- s/sqrt(n)

# Now we can find the CI @ 95%:
moe <- qt(0.975, df = n-1) * SE
ciLower <- xBar - moe
ciUpper <- xBar + moe
print(round(c(ciLower, xBar, ciUpper), 1))

# Q7: Use the given degree of confidence and sample data to find a confidence
# interval for the population standard deviation. Assume that the population 
# has a normal distribution. Round the confidence interval limits to the same 
# number of decimal places as the sample standard deviation and pick the 
# closest answer.
# Weights of men: confidence level: 90%, n = 14, xbar = 161.5, s = 13.7

n <- 14
xBar <- 161.5
s <- 13.7
SE <- s/sqrt(n)

# Find critical values:
X2Right <- qchisq(0.95, df = n-1)
X2Left <- qchisq(0.05, df = n-1)
# Now construct CI:
ciLower <- sqrt(((n - 1) * s**2)/X2Right)
ciUpper <- sqrt(((n - 1) * s**2)/X2Left)
print(round(c(ciLower, s, ciUpper), 1))
