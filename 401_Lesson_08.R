# Lesson 08
# Author: MJG
# Last updated: 2015-04-27

# Clear workspace
rm(list=ls())

# Q1: Assume a random sample of size 100 is drawn from a normal distribution
# with variance 1. The average value of the sample is 50. FInd a 95% confidence
# interval for the mean.

confInt <- function(n, mu, sd){
        moe <- qnorm(1 - (0.05 / 2)) * (sd / sqrt(n))
        c (mu - moe, mu + moe)
}

confInt(100, 50, sqrt(1))

# Q2: Assume the standard deviation for a normal distribution is equal to 100
# units. Also assume we want to estimate the unknown mean with a 95% confidence
# interval of total width 8 units. Calculate the sample size required.

zScore <- qnorm(0.025, mean = 0, sd = 1, lower.tail = F)
sampleSize <- (zScore * 100.0 / 4.0)**2
round(sampleSize)

# Q3: A random sample of 1600 registered voters are contacted and asked a variety
# of questions. For one question, 60% of the voters expressed approval and 40%
# disapproval. Calculate a 95% confidence interval for the proportion expressing
# approval.

# Note: See R Lesson 8 Solutions, very detailed explanation on the function
propTestObject <- prop.test(x = 1600 * 0.6,
                            n = 1600,
                            alternative = "two.sided",
                            conf.level = 0.95)
# Print the structure
print(str(propTestObject))
# Extract the confidence interval results
as.numeric(propTestObject$conf.int)

# Q4: A random sample of consumers are presented with two beverages in random
# order and asked which they prefer most. All of the consumers expressed a
# preference. One beverage was preferred 85% of the time. Use this number to
# determine how large a sample of consumers would be needed to generate a 95%
# confidence interval with an overall width just less than 2% (i.e. from 84% to
# 86%)? Note: Calculations taken from Triola page 333.

# Since P is known
p <- 0.85
zScore <- qnorm(0.025, mean = 0, sd = 1, lower.tail = FALSE)
sampleSize <- (zScore**2) * p * (1-p) / (0.01)**2
round(sampleSize)

# If P is unknown
sampleSize <- (zScore**2) * (0.25) / (0.01)**2
round(sampleSize)

# Q5: Create boxplots and find 95% confidence intervals for the mean amount of
# calories in each type of hot dog: beef, meat, and poultry. Construct 99%
# one-sided lower confidence intervals for the mean amount of calories in each
# type of hot dog: beef, meat, and poultry.

# Import our data and examine the structure
hotdogs <- read.csv("hot_dogs.csv")
print(str(hotdogs))

# Do a little EDA
plot(hotdogs)
print(summary(hotdogs))

# Create dataframes for hotdogs by type
beef <- subset(hotdogs, subset = (Type == "Beef"))
meat <- subset(hotdogs, subset = (Type == "Meat"))
poultry <- subset(hotdogs, subset = (Type == "Poultry"))

# Now look at summary statistics for hotdogs by type
print(summary(beef))
print(summary(meat))
print(summary(poultry))

# Use with for "data" or data frames in R
with(hotdogs, boxplot(Calories ~ Type,
                      main = "Calories, by hotdog type",
                      ylab = "Calories",
                      col = "beige"))

# Now calculate our 95% confidence intervals for mean amount of calories by type
with(beef, t.test(Calories)$conf.int)
with(meat, t.test(Calories)$conf.int)
with(poultry, t.test(Calories)$conf.int)

# And 99% one-sided lower confidence intervals for mean amount of calories by
# type. Since it's one-sided and lower, alternative = less. One-sided and higher
# would get alternative = greater.
t.test(beef$Calories, alternative = "less", conf.level = 0.95)
t.test(meat$Calories, alternative = "less", conf.level = 0.95)
t.test(poultry$Calories, alternative = "less", conf.level = 0.95)

# Find a 95% confidence interval for the variance in the amount of calories
# found for each type of hotdog: beef, meat, and poultry.

# Have to do a user-defined function. Note: this is a chi-squared test, so we
# use qchisq().

var.conf.int = function(x, conf.level = 0.95){
        df <- length(x) - 1
        chilower <- qchisq((1 - conf.level)/2, df, lower.tail = T)
        chiupper <- qchisq((1 - conf.level)/2, df, lower.tail = F)
        v <- var(x)
        c(df * v/chiupper, df * v/chilower)
}

with(beef, var.conf.int(Calories))
with(meat, var.conf.int(Calories))
with(poultry, var.conf.int(Calories))
