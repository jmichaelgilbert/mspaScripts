# Lesson 11
# Author: MJG
# Last updated: 2015-05-20

# Clear workspace
rm(list=ls())

# Q1: Use the tableware.csv data to test the hypothesis that the mean RATE for
# the five levels of TYPE are equal. Test the hypothesis using a 0.05 
# signifcance level. Produce and plot means and confidence intervals for each
# level of TYPE. (You may use the example given in Section 16.1.1 of Lander as
# a reference and guide. Try the analysis two different ways. Use -1 in the 
# model to suppress the intercept, and alternatively without using -1. Compare
# results. Load the ggplot2() and plyr() packages.)

tableware <- read.csv("tableware.csv")
str(tableware)

RATE_anova <- aov(RATE ~ TYPE -1, data = tableware)
RATE_lm <- lm(RATE ~ TYPE -1, data = tableware)

summary(RATE_anova)
summary(RATE_lm)

# ddply(), package: plyr, can hasten generation of our pet TYPE confidence
# intervals and let us output a handy table
library(plyr)
RATEbyType <- ddply(tableware, "TYPE", summarize,
                    RATE.mean = mean(RATE),
                    RATE.sd = sd(RATE),
                    Length = NROW(RATE),
                    tfrace = qt(p = 0.975, df = Length - 1),
                    Lower = RATE.mean - tfrac * RATE.sd / sqrt(Length),
                    Upper = RATE.mean + tfrac * RATE.sd / sqrt(Length))
RATEbyType

# Ok rad! We can no use Lower and Upper CI values in a plot:
library(ggplot2)
ggplot(RATEbyType,
       aes(x = RATE.mean, y = TYPE))+
        geom_point()+
        geom_errorbarh(aes(xmin = Lower, xmax = Upper),
                       height = 0.3)+
        ggtitle("Average Rate by Day")

# This shows the results without a -1. The means are not different from each
# other.

RATE_anova <- aov(RATE ~ TYPE, data = tableware)
RATE_lm <- lm(RATE ~ TYPE, data = tableware)

summary(RATE_anova)
summary(RATE_lm)

# Q2: Use the tableware.csv data to test the hypothesis that the mean PRICE for
# the five levels of TYPE are equal. Test the hypothesis using a 0.05 
# significance level. Print out 95% confidence intervals for each level of TYPE.

# Alternate way to code the problem:
my_price_model <- {PRICE ~ TYPE}
my_price_model_fit <- lm(my_price_model, data = tableware)

summary(my_price_model_fit)
anova(my_price_model_fit)

# Confidence intervals for the coefficients in the regression model.
confint(my_price_model_fit, level = 0.95)

# Q3: Use the hot_dogs.csv data. Perform a one-way ANOVA by Type on Calories 
# and also Sodium. Use Tukey's Honest Significant Different Test if the F-test
# is signifcant. Generate boxplots.

hotdogs <- read.csv("hot_dogs.csv")
str(hotdogs)

# Now boxplots:
with(hotdogs, boxplot(Calories ~ Type, 
                      main = "Calories",
                      col = "Beige"))
with(hotdogs, boxplot(Sodium ~ Type,
                      main = "Sodium",
                      col = "Beige"))

# Perform one-way AOV, Calories
calories.anova <- aov(Calories ~ Type, data = hotdogs)
summary(calories.anova)

# Perform one-way AOV, Sodium
sodium.anova <- aov(Sodium ~ Type, data = hotdogs)
summary(sodium.anova)

# Perform Tukey's Honest Significance Test
TukeyHSD(calories.anova, conf.level = 0.95)
TukeyHSD(sodium.anova, conf.level = 0.95)

