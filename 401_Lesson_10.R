# Lesson 10
# Author: MJG
# Last updated: 2015-05-11

# Clear workspace
rm(list=ls())

# Q1: A double-blind clinical trial of a new drug for back pain was designed
# using control and treatment groups. Volunteers were fully informed and
# assigned at random to each group. Neither the volunteers nor the doctor knew
# when the new drug or a placebo was being administered. When 100 volunteers in
# each group had been treated and evaluated, the results revealed an 85% success
# rate for the new drug and a 65% success rate for the control group. At the
# 95% confidence level, is there a statistically significant difference between
# the two reported rates? Use a one-sided test. Report a confidence interval
# for the difference.

# Create a matrix:
drugMatrix <- matrix(c(85, 65, 15, 35), nrow = 2, ncol = 2, byrow = F,
                     dimnames = list(c("New Drug", "Control"),
                                     c("Success", "Fail")))
prop.test(drugMatrix, alternative = "greater", conf.level = 0.95)
# p-value = 0.0009589 < 0.05,
# so reject null hypothesis as result is statistically significant

# Q2: Two baseball players had their career records compared. In 267 times at
# bat, one player hit 85 home runs. In 248 times at bat, the other player hit
# 89 home runs. Assume the number of home runs follows a binomial distribution.
# Is there a statistically significant difference with 95% confidence between
# the home run averages for these two baseball players?

baseballMatrix <- matrix(c(85, (267-85), 89, (248-89)), nrow = 2,
                         ncol = 2, byrow = T,
                         dimnames = list(c("Player A", "Player B"),
                                         c("HR", "Other")))
baseballMatrix
prop.test(baseballMatrix, alternative = "two.sided", conf.level = 0.95)
# p-value = 0.3799 > 0.05
# so do not reject null hypothesis as result is not statistically significant

# Q3: Using the home_prices.csv data, compare mean selling prices between homes
# located in the northeast sector of the city versus the remaining homes. Also,
# compare the mean selling prices between homes with a corner lot and those
# located elsewhere. Use two-sample t-tests for the hypothesis tests at the 95%
# confidence level. Report confidence intervals for each.

homePrices <- read.csv("home_prices.csv")
str(homePrices)
# Assume NBR = YES is fro the northeast sector of the city
summary(homePrices)
# Now look at stats across the two sectors
with(homePrices, by(PRICE, NBR, summary))
with(homePrices, by(PRICE, CORNER, summary))

# Ok, now on to the hypothesis testing

# First up: NE homes
priceNE <- subset(homePrices, subset = (NBR == "YES"))$PRICE
priceOTHER <- subset(homePrices, subset = (NBR == "NO"))$PRICE
t.test(priceNE, priceOTHER, alternative = "two.sided", conf.level = 0.95)
# p-value = 0.1134 > 0.05
# so do not reject null hypothesis as result is not statistically significant
# or, prices in NE are not statistically different from prices of other homes

# Second up: Corner homes
priceCORNER <- subset(homePrices, subset = (CORNER == "YES"))$PRICE
priceNONCORNER <- subset(homePrices, subset = (CORNER == "NO"))$PRICE
t.test(priceCORNER, priceNONCORNER, alternative = "two.sided", conf.level = 0.95)
# p-value = 0.6695 > 0.05
# so do not reject null hypothesis as result is not statistically significant
# or, prices for corner homes are not statistically different from prices of
# non-corner homes

# Q4: The nsalary.csv data are derived from data collected by the Department of
# Social Services of the State of New Mexico. The data have been adapted for
# this problem. Present a boxplot comparing RURAL and non-RURAL salaries. Using
# these data, compare mean salary levels between RURAL and non-RURAL locations.
# Use a two-sample t-test at the 95% confidence level. Report your results.

# Bring the data in, check it out
nsalary <- read.csv("nsalary.csv")
str(nsalary)
summary(nsalary)
# Stats broken out by RURAL: Yes & No
with(nsalary, by(NSAL, RURAL, summary))

# Create boxplot
with(nsalary, boxplot(NSAL ~ RURAL, 
                      main = "Salary, Rural",
                      ylab ="Salary",
                      col = "beige"))
# We definitely see some differences.

salaryRural <- subset(nsalary, subset = (RURAL == "YES"))$NSAL
salaryNonRural <- subset(nsalary, subset = (RURAL == "NO"))$NSAL
t.test(salaryRural, salaryNonRural, alternative = "two.sided", conf.int = 0.95)
# p-value = 8.504e-06 < 0.05
# so do reject null hypothesis as result is statistically significant

# Q5: Tires.csv contains data published by R.D. Stichler, G.G. Richey, and J.
# Mandrel, "Measurment of Treadware of Commercial Tires, Rubber Age, 73:2
# (May 1953). Treadwear measures of each tire was subject to measurement by
# two methods, the first based on weight loss and the second based on groove
# wear. Use a paired t-test at the 95% confidence level to test for a difference
# between the two methods. Report your results using a confidence interval.

# Bring data in, check it out
tires <- read.csv("tires.csv")
str(tires)
summary(tires)

# Scatterplot
with(tires, plot(WGT, GRO, las = 1,
                 xlim = c(min(WGT, GRO), max(WGT, GRO)),
                 ylim = c(min(WGT, GRO), max(WGT, GRO))))
segments(10, 10, 45, 45, col = "darkred")
title("Comparing Measures of Tire Wear")
# Note that all but one of the WGT measures is larger than the corresponding
# GRO measure.

# Now paired t-test.
with(tires, t.test(WGT, GRO, alternative = "two.sided", 
                   paired = T, conf.int = 0.95))
# p-value = 4.614e-05 < 0.05
# so do reject null hypothesis as result is statistically significant
# thus, there are statistically significant differences between the two
# measures of tire wear