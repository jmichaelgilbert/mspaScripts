# Lesson 06
# Author: MJG
# Last updated: 2015-04-25

# Q1: Assume the purchases of shoppers in a store have been studied for a period
# of time and it is determined the daily purchases by individual shoppers are
# normally distributed with a mean of $81.14, and a standard deviation of $20.71.
# Find the following probabilities using R.

# Q1A: What is the probability that a randomly chosen shopper spends less than
# $75.00?

# lower.tail = T means probabilities are P[X <= x]
# lower.tail = F means probabilities are P[X >= x]
Q1A <- pnorm(75, mean = 81.14, sd = 20.71, lower.tail = T)
round(Q1A, 4)

# Q1B: What proportion of shoppers spend more than $100.00?
Q1B <- pnorm(100, mean = 81.14, sd = 20.71, lower.tail = F)
round(Q1B, 4)

# Q1C: What proportion of shoppers spend between $50.00 and $80.00?
Q1C50 <- pnorm(50, mean = 81.14, sd = 20.71, lower.tail = T)
Q1C80 <- pnorm(80, mean = 81.14, sd = 20.71, lower.tail = F)
Q1C <- (1 - (Q1C50 + Q1C80))
round(Q1C, 4)

# Q2: Assume that the shopper's purchases are normally distributed with a mean
# of $97.11 and a standard deviation of $39.46. Find the following scores using
# R.

# Q2A: What weight is the 90th percentile of the shoppers' purchases? That is,
# find the score P90 that separates the bottom 90.0% of shoppers' purchases from
# the top 10%.

# Use qnorm() for percentile calculations (quantile function)
# Use lower.tail = T, because we're looking at bottom 90.0%
Q2A <- qnorm(0.90, mean = 97.11, sd = 39.46, lower.tail = T)
round(Q2A, 4)

# Q2B: What is the median shoppers' purchase? (Find the score P50 that separates
# the bottom 50.0% of shoppers' from the top 50.0%.) What is important about
# this number?

# For median we can use lower.tail = T or lower.tail = F (test below)
Q2B <- qnorm(0.50, mean = 97.11, sd = 39.46, lower.tail = T)
round(Q2B, 4)

Q2B_Test <- qnorm(0.50, mean = 97.11, sd = 39.46, lower.tail = F)
result <- Q2B - Q2B_Test
result

# Q3: Generate a sample of size 50 from a normal distribution with a mean of 100
# and a standard deviation of 4. What is the mean and standard error of the
# mean for the sample? Generate a second sample of size 50 from the same normal
# population. What is the mean and standard error of the mean for this second
# sample? Now, repeat this process generating a sample size of 5,000. Calculate
# the mean and standard error fo the mean for this third sample and compare to
# the previous samples. What do you observe?

# SE contained in describe() in library(psych)
library(psych)

N50A <- rnorm(50, mean = 100, sd = 4)
N50B <- rnorm(50, mean = 100, sd = 4)
N5000 <- rnorm(5000, mean = 100, sd = 4)
describe(N50A)
describe(N50B)
describe(N5000)

# Q4: Assume a biased coin when flipped will generate heads one third of the
# time. Estimate the probability of getting at least 250 heads out of 600 flips
# using the normal distribution approximation. Compare to the exact probability
# using the binomial distribution.

# Normal approximation to binomial: z = (x - n * p) / sqrt(n * p * (1 - p))
n <- 600
p <- 1/3
x <- 250
z <- (x - n * p) / sqrt(n * p * (1 - p))

# lower.tail = F as "at least 250 heads" implies upper half
result_Q4A <- pnorm(z, mean = 0, sd = 1, lower.tail = F)
round(result_Q4A, 10)

# Now use the built-in function
result_Q4B <- pbinom(q = x, size = n, prob = p, lower.tail = F)
round(result_Q4B, 10)

# Calulate the differences
Q4 <- result_Q4B - result_Q4A
Q4

# Q5: Use the uniform distribution over 0 to 1. Generate three separate simple
# random samples of size n = 25, n = 100, and n = 400. Plot histograms for each
# and comment on what you observe.

# First, we'll set it so we can fit all three histograms in one pane;
# oma takes the form [c(bottom, left, top, right)] for size of outer margins
# in lines of text
par(mfrow = c(1,3), oma = c(0, 0, 2, 0))

# Uniform distribution can be dunif, punif, runif, qunif
# We use runif as it generates random deviates
hist(runif(25, min = 0, max = 1), col = "beige", main = "")
hist(runif(100, min = 0, max = 1), col = "beige", main = "")
hist(runif(400, min = 0, max = 1), col = "beige", main = "")

# mtext will inject main from above
mtext("Histograms of Uniform Distribution (n = 25, 100, and 400)", side = 3,
      outer = T, line = -1)

# Now reset par() to normal
par(mfrow = c(1,1))

# Q6: Use salaries.csv. The data contain CEO age and salaries for 60 small 
# business firms. Construct QQ plots and historgrams. Is the distribution of
# ages a normal distribution? Explain your answer.

# Read the salaries.csv sheet
salary <- read.csv("salaries.csv", sep = ",")

# Do some EDA on the data, check for NAs
str(salary$AGE)
describe(salary$AGE)
is.na(salary$AGE)

par(mfrow = c(1,2))

hist(salary$AGE,
     col = "beige",
     main = "Histogram of CEO Ages",
     xlab = "Ages")

plot(density(salary$AGE), main = "Density of CEO Ages")

par(mfrow = c(1,1))

# Now QQ plots
# datax = T means plot data values on x-axis
# probs = numeric length of two, representing probabilities - corresponding
# quantile pairs define the line drawn
qqnorm(salary$AGE,
       main = "Age QQ",
       xlab = "Normal Quantiles",
       ylab = "Age Quantiles",
       datax = T)
qqline(salary$AGE,
       datax = T,
       distribution = qnorm,
       probs = c(0.25, 0.75),
       qtype = 7)
shapiro.test(salary$AGE)
