# Solution for Lesson_8_Exercises_Using_R

# Exercises

# 1) Assume a random sample of size 100 is drawn from a normal distribution for 
# which the mean and variance are unknown. Assume the sample mean is 50 and the
# standard deviation of the sample is 2. Test the hypothesis that the true mean
# is 56, and also test the hypothesis that the true mean is 40. Report p-values
# and comment on the results.

# Two-Tailed Test of Population Mean with Unknown Variance
# http://www.r-tutor.com/elementary-statistics/hypothesis-testing/two-tailed-test-population-mean-unknown-variance

samp.mean <- 50 	# sample mean
test.mean1 <- 56 	# mean value to test
test.mean2 <- 40	# alternate mean value to test
samp.sd <- 2	# sample standard deviation
n <- 100		# sample size

t <- (samp.mean - test.mean1)/(samp.sd/sqrt(n)) # test statistic
t #[1] -30

p.value <- 2 * pt(-abs(t), df=n-1) 	# we use -abs(t) because pt()
						# returns the probability that
						# the actual value is less than the
						# supplied t
p.value # [1] 1.70085e-51


# And, for test.mean2 (40)
t <- (samp.mean - test.mean2)/(samp.sd/sqrt(n)) # test statistic
t #[1] 50

p.value <- 2 * pt(-abs(t), df=n-1)
p.value # [1] 4.595366e-72


# To review... we return to the hot dogs data
# Data Set: hot_dogs.csv
# Reference: Original source: Consumer Reports, June 1986, pp. 366-367.
# Description: Results of a laboratory analysis of calories and sodium 
# content of major hot dog brands. Researchers for Consumer Reports 
# analyzed three types of hot dog: beef, poultry, and meat 
# (mostly pork and beef, but up to 15% poultry meat). 

# Read in the data, create data frame, examine its structure.
hotdogs <- read.csv(file.path("c:/Rdata/","hot_dogs.csv"))
print(str(hotdogs))

# Look at the data... a little EDA.
plot(hotdogs)

# summary statistics
print(summary(hotdogs))

# Again, we see that some of the questions
# concern subsets of the data by hotdog type. To respond to the
# questions, we will create three subset data frames.
beef <- subset(hotdogs, subset = (Type == "Beef"))
meat <- subset(hotdogs, subset = (Type == "Meat"))
poultry <- subset(hotdogs, subset = (Type == "Poultry"))

# Look at summary statistics for these data frames.
print(summary(beef))
print(summary(meat))
print(summary(poultry))

# ------------------------
# 1) Use hot_dogs.csv data and hypothesis tests to determine which type of
# hot dog has average calories less than 140 with 95% confidence. Present
# boxplots of calories by type of hot dog

# First, we'll identify the 95% confidence intervals, per Type
with(beef, t.test(Calories)$conf.int)
with(meat, t.test(Calories)$conf.int)
with(poultry, t.test(Calories)$conf.int)

# Second, we'll extract just the lower bound of the 95% CI
with(beef, t.test(Calories)$conf.int[1])
with(meat, t.test(Calories)$conf.int[1])
with(poultry, t.test(Calories)$conf.int[1])

# Third, we'll add a logical comparison to our statement (" < 140")
with(beef, as.numeric(t.test(Calories)$conf.int)[1] < 140)
with(meat, as.numeric(t.test(Calories)$conf.int)[1] < 140)
with(poultry, as.numeric(t.test(Calories)$conf.int)[1] < 140) 

with(hotdogs, boxplot(Calories ~ Type, main = "Calories, by hotdog type",
	ylab = "Calories"))

# Note that only poultry hot dogs meet this test.


# ------------------------
# 2) Using hot_dogs.csv data and hypothesis tests at the 95% confidence level,
# determine which type of hot dog has an average Sodium level different from
# 425 milligrams (i.e. not equal to).

# This looks like a set of two-tailed t-tests for means. Let the null hypothesis
# be H0: mu = 425, and we will perform a t-test to get the p-value for each
# type of hot dog.

# First we refer to the R documentation for the t.test function.
# t.test(x, y = NULL,
#       alternative = c("two.sided", "less", "greater"),
#       mu = 0, paired = FALSE, var.equal = FALSE,
#       conf.level = 0.95, ...)
with(beef, t.test(Sodium, alternative = "two.sided", mu = 425))
with(meat, t.test(Sodium, alternative = "two.sided", mu = 425))
with(poultry, t.test(Sodium, alternative = "two.sided", mu = 425))  
# None of the three p-values is less than 0.05, so we do not reject the null hypothesis.

# Note. When running many classical tests, we often adjust the critical values
# for the tests so that we avoid making type-one errors. Here no tests were
# statistically significant, so we do not need to adjust critical values.

# ------------------------
# 3) Using the hot_dogs.csv data and hypothesis tests, determine if the variance 
# in Sodium values for beef hot dogs is different from 6000 with 95% confidence.
# Here we will use the appropriate chi-square test. Refer to R documentation:
# dchisq(x, df, ncp = 0, log = FALSE)
# pchisq(q, df, ncp = 0, lower.tail = TRUE, log.p = FALSE)
# qchisq(p, df, ncp = 0, lower.tail = TRUE, log.p = FALSE)
# rchisq(n, df, ncp = 0)
# x, q	vector of quantiles.
# p	vector of probabilities.
# n	 number of observations. 
# df degrees of freedom (non-negative, but can be non-integer).
# ncp non-centrality parameter (non-negative).
# log, log.p logical; if TRUE, probabilities p are given as log(p).
# lower.tail	logical; if TRUE (default), probabilities are P[X ≤ x], 
#    otherwise, P[X > x].
# We could build on the confidence interval function we developed in lesson 7:
var.conf.int = function(x, conf.level = 0.95) {
    df <- length(x) - 1
    chilower <- qchisq((1 - conf.level)/2, df, lower.tail = TRUE)
    chiupper <- qchisq((1 - conf.level)/2, df, lower.tail = FALSE)
    v <- var(x)
    c(df * v/chiupper, df * v/chilower)
    }
with(beef, var.conf.int(Sodium))

# If this logical is TRUE, then we reject the null hypothesis that mu = 6000:
with(beef, (6000 < var.conf.int(Sodium)[1]) || (6000 > var.conf.int(Sodium)[2]))

# ------------------------
# 4) A coin is flipped 100 times. If it is unbiased the probability of a heads should
# equal the probability of a tails. At the 95% confidence level, test the null
# hypothesis the coin is unbiased versus the alternative that it is biased 
# if 43 heads are obtained. Test the same hypothesis if 63 heads are obtained. 
# Use one-sided hypothesis tests.
# Looks like another binomial problem. Refer to R documentation for prop.test().
# prop.test(x, n, p = NULL,
#           alternative = c("two.sided", "less", "greater"),
#           conf.level = 0.95, correct = TRUE)
# x: a vector of counts of successes, a one-dimensional table with two entries, or a two-dimensional 
#     table (or matrix) with 2 columns, giving the counts of successes and failures, respectively.
# n:	a vector of counts of trials; ignored if x is a matrix or a table.
# p:	a vector of probabilities of success. The length of p must be the same as the number of groups 
#     specified by x, and its elements must be greater than 0 and less than 1.
# alternative:  character string specifying the alternative hypothesis, 
# must be one of "two.sided" (default), "greater" or "less". You can specify just the initial letter.
# conf.level: confidence level of the returned confidence interval. 
# Must be a single number between 0 and 1. 
# Only used when testing the null that a single proportion equals a given value, or that two proportions 
# are equal; ignored otherwise.
# correct: a logical indicating whether Yates' continuity correction should be applied where possible.
prop.test(x = 43, n = 100, alternative = "less")  # see p-value 0.0968 > 0.05 (do not reject null hypothesis)

prop.test(x = 63, n = 100, alternative = "greater")  # see p-value 0.00621 < 0.05 (reject null hypothesis)

# 5) salaries.csv contains data derived from a November 8, 1993 article in Forbes titled 
# "America's Best Small Companies". The file gives the CEO age and salary for 60 small 
# business firms. Use these data to test the hypothesis at 95% confidence that 
# at least 50% of the CEOs are 45 years old or older. Also test the hypothesis 
# at 95% confidence that at least 50% of the CEOs earn less than $500,000 per year. 
# Use one-sided hypothesis tests.
# Being by reading in the data, creating a data frame, and checking its structure.
salaries <- read.csv(file.path("c:/Rdata/","salaries.csv")) 
print(str(salaries))
print(summary(salaries))  # note that salaries are expressed in thousands of dollars

# To test the hypothesis about age, we must count the number <= 45 years old.
# Then prop.test will be used.

age <- salaries$AGE >= 45
count <- sum(age)
total <- length(age)
prop.test(x = count, n = total, alternative = "greater")

# The p-value is less than 0.05 so reject the null hypothesis.

# Now we must count the number earning less than $500,000 per year.
# Then prop.test will be used. 

salary <- salaries$SAL < 500
count <- sum(salary)
total <- length(salary)
prop.test(x= count, n = total, alternative = "greater")

# The null hypothesis must be rejected based on the small p-value.

