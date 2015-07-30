# Lesson 09
# Author: MJG
# Last updated: 2015-05-09

# Clear workspace
rm(list=ls())

# Q1: Assume a random sample of size 100 is drawn from a normal distribution for
# which the mean and variance are unknown. Assume the sample mean is 50 and the
# standard deviation of the sample is 2. Test the hypothesis that the true mean
# is 56, and also test the hypothesis that the true mean is 40. Report p-values
# and comment on the results.

# Two-tailed Test of the Population Mean with Unknown Variance

# Parameters
sampleMean <- 50
trueMean1 <- 56
trueMean2 <- 40
sampleSD <- 2
n <- 100

# Math for trueMean1
testStatistic <- (sampleMean - trueMean1)/(sampleSD/sqrt(n))
round(testStatistic, 4)

# P-Value note: we use -abs(testStatistic) because pt() returns the probability
# that the actual value is less than the supplied testStatistic
pValue <- 2 * pt(-abs(testStatistic), df = n-1)
round(pValue, 4)

# Now for trueMean2
testStatistic <- (sampleMean - trueMean2)/(sampleSD/sqrt(n))
round(testStatistic, 4)

# P-Value, see note from above
pValue <- 2 * pt(-abs(testStatistic), df = n-1)
round(pValue, 4)

# Q2: Use the hot_dogs.csv data and hypothesis tests to determine which type of
# hot dog has average calories less than 140 with 95% confidence. Present
# boxplots of calories by type of hot dog.

hotdogs <- read.csv("hot_dogs.csv")
str(hotdogs)

# Do a little EDA
summary(hotdogs)
summary(hotdogs$Type)
summary(hotdogs$Calories)
summary(hotdogs$Sodium)
plot(hotdogs)

# Now let's create some subsets to explore by Type
beef <- subset(hotdogs, subset = (Type == "Beef"))
meat <- subset(hotdogs, subset = (Type == "Meat"))
poultry <- subset(hotdogs, subset = (Type == "Poultry"))

# Now a little EDA for these
summary(beef)
summary(meat)
summary(poultry)

# Ok, finally on to our question! First up: determine the confidence intervals
# per Type for Calories:
with(beef, t.test(Calories)$conf.int)
with(meat, t.test(Calories)$conf.int)
with(poultry, t.test(Calories)$conf.int)

# Rad! Now our original question asked "less than 140" so we want to extract
# the lower result of our confidence interval:
with(beef, t.test(Calories)$conf.int[1])
with(meat, t.test(Calories)$conf.int[1])
with(poultry, t.test(Calories)$conf.int[1])

# And finally, add a logical comparison to see which (if any) are less than 140:
with(beef, as.numeric(t.test(Calories)$conf.int[1])[1] < 140)
with(meat, as.numeric(t.test(Calories)$conf.int[1])[1] < 140)
with(poultry, as.numeric(t.test(Calories)$conf.int[1])[1] < 140)

# And our plot:
with(hotdogs, boxplot(Calories ~ Type,
                      main = "Calories, by hotdog type",
                      col = "beige",
                      ylab = "Calories"))

# Q3: Using hot_dogs.csv and hypothesis tests at the 95% confidence level,
# determine which type of hot dog has an average Sodium level different from
# 425 milligrams (that is, not equal to 425 mg).

# This looks like a set of two-tailed t-tests for means. Let the null hypothesis
# be H0: mu = 425, and we will perform a t-test to get the p-value for each
# type of hot dog. (Be sure to review ?t.test)
with(beef, t.test(Sodium, alternative = "two.sided", mu = 425))
with(meat, t.test(Sodium, alternative = "two.sided", mu = 425))
with(poultry, t.test(Sodium, alternative = "two.sided", mu = 425))

# None of the p-values are < 0.05, so we do not reject H0.

# Note: when running many classical tests, we often adjust the critical values
# for the tests so that we avoid making type-one errors. Here no tests were
# statistically significant, so we do not need to adjust critical values.

# Q4: Using the hot_dogs.csv data and hypothesis tests, determine if the
# variance in Sodium values for beef hot dogs is different from 6000 with 95%
# confidence.

# Note: make sure we use the appropriate chi-square test.
# And build on the confidence interval function we developed in Lesson 07:
var.conf.int = function(x, conf.level = 0.95){
        df <- length(x) - 1
        chiLower <- qchisq((1 - conf.level)/2, df, lower.tail = T)
        chiUpper <- qchisq((1 - conf.level)/2, df, lower.tail = F)
        v <- var(x)
        c(df * v/chiUpper, df * v/chiLower)
}

with(beef, var.conf.int(Sodium))

# If this logical is TRUE, then we reject H0 that mu = 6000:
with(beef, (6000 < var.conf.int(Sodium)[1]) || (6000 > var.conf.int(Sodium)[2]))

# Result = Reject H0

# Q5: A coin is flipped 100 times. If it is unbiased the probability of a heads
# should equal the probability of a tails. At the 95% confidence level, test
# the null hypothesis the coin is unbiased versus the alternative that it is
# biased if 43 heads are obtained. Test the same hypothesis if 63 heads are
# obtained. Use one-sided hypothesis tests.

# Looks like another binomial problem. Research prop.test()
# Pay attention to "alternative = X", that helps determine which to use.
# For "conf.level = X", only used when testing the null (H0) that a single
# proportion equals a given value, OR that two proportions are equal; ignored
# otherwise.

# IMPORTANT: So, our confidence level is 95%, which means 1 - 0.95 = 0.05 
# (alpha). That's the p-level we're looking for to determine whether or not we 
# reject H0. That's for a single-tailed test. For two-tailed, we need to divide
# alpha by two to get the p-value in one tail.

# We use "less" because 43 < 50
prop.test(x = 43, n = 100, alternative = "less")
# Our resulting p-value is 0.0968, which is > 0.05, so DO NOT reject H0

# We use "greater" because 63 > 50
prop.test(x = 63, n = 100, alternative = "greater")
# Our resulting p-value is 0.00621, which is < 0.05, so DO reject H0

# Q6: salaries.csv contains data derived from a November 8, 1993 article in
# Forbes titled "America's Best Small Companies". The file gives the CEO age
# and salary for 60 small business firms. Use these data to test the hypothesis
# at 95% confidence that at least 50% of the CEOs are 45 years old or older.
# Also test the hypothesis at 95% confidence that at least 50% of the CEOs earn
# less than $500,000 per year. Use one-sided hypothesis tests.

# Let's get the data in and do some EDA
salaries <- read.csv("salaries.csv")
str(salaries)
summary(salaries)

# To test the hypothesis about age, we must count the number >= 45 years old.
# Then use prop.test.
age <- salaries$AGE >= 45
count <- sum(age)
total <- length(age)
prop.test(x = count, n = total, alternative = "greater")
# Our resulting p-value is way less than 0.05, so DO reject H0.

# To test the hypothesis about salary, we must count the number < 500.
# Then use prop.test.
sal <- salaries$SAL < 500
count <- sum(sal)
total <- length(sal)
prop.test(x = count, n = total, alternative = "greater")
# Our resulting p-value is less than 0.05, so DO reject H0.
