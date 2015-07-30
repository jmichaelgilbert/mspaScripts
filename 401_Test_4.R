# Test 4: Predict 401-DL
# Last updated: 2015-05-21 by MJG

# Clear workspace
rm(list=ls())

# Assume independent random samples are available from two populations giving
# information about population proportions. Use the following information fpr
# the next two problems. For the first sample assume n1 = 100 and x1 = 42. For 
# the second sample assume n2 = 100 and x2 = 45. Round your answer to the 
# nearest thousandth.

# Q1: use the given sample sizes and numbers to find the pooled estimate of
# pbar.

n1 <- 100
x1 <- 42
n2 <- 100
x2 <- 45

pHat1 <- x1/n1 
pHat2 <- x2/n2
pHat <- (x1 + x2)/(n1 + n2)
round(pHat, 4)

# Q2: Test the null hypothesis that the proportion for population 2 is greater
# than the proportion for population 1. Pick the correct z and p-value.

# H0 = pHat2 > pHat1
# H0 determines one sided test (since not set to equal)

# Determine test statistic:
zScore <- (pHat2 - pHat1)/(sqrt((pHat*(1-pHat))*((1/n1)+(1/n2))))
round(zScore, 3)
# Now look up zScore in table: 0.428 = 0.664
# Since we are doing greater than, we subtract 1, as z-score is to LEFT and we
# need to look to RIGHT. Thus: 1 - 0.664 = 0.336

# Q3: Two types of flares are tested and their burning times are recorded. The
# summary statistics are given below.

# Brand X: n = 35, mu = 19.4 minutes, sd = 1.4 minutes
# Brand Y: n = 40, mu = 15.1 minutes, sd = 1.3 minutes

# Construct a 95% confidence interval for the differences between the mean
# burning time of the brand X flare and the mean burning time of the brand Y
# flare.

x.n <- 35
x.mu <- 19.4
x.sd <- 1.4

y.n <- 40
y.mu <- 15.1
y.sd <- 1.3

# Determine standard error
SE <- sqrt((x.sd**2/x.n)+(y.sd**2/y.n))

# Determine degrees of freedom
df <- (x.n - 1)+(y.n - 1)

# Determine t level for 95% CI @ DF
t <- qt((1-0.05/2), df)

ci.lower <- (x.mu - y.mu) - (t * SE)
ci.upper <- (x.mu - y.mu) + (t * SE)
print(round(c(ci.lower, ci.upper), 1))

# Q4: Construct a confidence interval for mu-d, the differences d for the
# population of paired data. Assume that the population of paired differences
# is normally distributed.

# If d = 3.125, sd = 2.911, and n = 8, determine a 95 percent CI for mu-d

d <- 3.125
sd <- 2.911
n <- 8

SE <- (sd / sqrt(n))
df <- (n - 1)
t <- qt(1-0.05/2, df)

ci.lower <- (d) - (t * SE)
ci.upper <- (d) + (t * SE)

print(round(c(ci.lower, ci.upper), 3))

# Q5: Find the value of the linear correlation coefficient r.
# x = 47.0, 46.6, 27.4, 33.2, 40.9
# y = 8, 10, 10, 5, 10

x <- c(47.0, 46.6, 27.4, 33.2, 40.9)
y <- c(8, 10, 10, 5, 10)

cor(x, y)

# Q6: Identify the value of the test statistic
# See printout for solution!

# Q7: A manager at a bank is interested in the standard deviation of the
# waiting times when a single waiting line is used and when individual lines
# are used. He wishes to test the claim that the population standard deviation
# when using multiple lines are used, is greater than the population standard
# deviation for waiting times when a single line is used. Find the p-value
# for a test of this claim given the following sample data. You won't be able
# to find the exact p-value, but will be able to give a range of possible
# values.

# Sample 1 (multiple waiting lines): n1 = 13, s1 = 2.1 minutes
# Sample 2 (single waiting line): n2 = 16, s2 = 1.1 minutes

# I have absolutely no clue how to answer this.

# Q8: Fill in the missing entries in the following partially completed one-way
# ANOVA table.
# See printout for solution! 