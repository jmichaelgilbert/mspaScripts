# Lesson 12
# Author: MJG
# Last updated: 2015-05-23

# Clear workspace
rm(list=ls())

# Questions 1 through 4 use newspapers.csv
# Questions 5 through 7 use tableware.csv

#------------------------------------------------------------------------------

# Bring in CSV
newspapers <- read.csv("newspapers.csv")
str(newspapers)
summary(newspapers)

# Q1: Plot Sunday circulation vs. Daily circulation. Does the scatter plot
# suggest a linear relationship between the two variables? Calculate the Pearson
# product moment correlation coefficient between Sunday and Daily circulation.

# Could use "plot(Daily, Sunday)" but reverses axes:
with(newspapers, plot(Sunday ~ Daily))
# Smoothed line looks straight:
with(newspapers, scatter.smooth(Sunday ~ Daily))
# Strong positive correlation:
with(newspapers, print(cor(Sunday, Daily)))

# Q2: Fit a regression line with Sunday circulation as the dependent variable.
# Plot the regression line with the circulation data (Use Lander pages 212 and
# 213 for reference.) Comment on the quality of the fit. What percent of the
# total variation in Sunday circulation is accounted for by the regression line?

# We use LM for linear model:
newsModel <- lm(Sunday ~ Daily, data = newspapers)
newsModel
summary(newsModel)

# Now plot:
plot(newspapers$Sunday ~ newspapers$Daily,
     main = "Sunday vs. Daily Circulation")
# abline adds straight line through the plot:
abline(newsModel)

# Set graph to show 2x2
par(mfrow=c(2,2))
plot(newsModel)
# Reset graph to show 1x1
par(mfrow=c(1,1))

# Multiple R-squared:
summary(newsModel)
# So, 0.9181 = proportion of Sunday circulation variance accounted for.

# Q3: Obtain 95% confidence intervals for the coefficients in the regression
# model. Use confint().

confint(newsModel, level = 0.95)

# Q4: Determine a 95% prediction interval to predict Sunday circulation for all
# available values of Daily circulation. Use predict(model, 
# interval = "prediction", level = 0.95). Then, define a new data frame using
# Daily = 500 and Sunday = NA. Predict an interval for Sunday circulation.

# This predicts and provides upper and lower bounds (intervals) by observation
predict(newsModel, interval = "prediction", level = 0.95)

# Now define a new data frame. Daily = 500, Sunday = NA:
# Note: var names MUST match the parent DF, here "newsModel"
# If names do not match, will throw an error.
# Could rename, but just do it right the first time.
Daily <- 500
Sunday <- NA
newsDataFrame <- data.frame(Daily, Sunday)

predict(newsModel, newdata = newsDataFrame, 
        interval = "prediction", level = 0.95)

#------------------------------------------------------------------------------

# Clear workspace:
rm(list=ls())

# Bring in CSV
tableware <- read.csv("tableware.csv")
str(tableware)
summary(tableware)

# Q5: Regress PRICE as a dependent variable against TIME. Comment on the
# quality of the fit. Is a simple linear regression model adequate or is
# something mor needed?

# First, let's do a few plots:
with(tableware, plot(PRICE ~ TIME))
# Smoothed line looks straight:
with(tableware, scatter.smooth(PRICE ~ TIME))
# Strong positive correlation:
with(tableware, print(cor(TIME, PRICE)))

tableModel <- lm(PRICE ~ TIME, data = tableware)
tableModel

# Multiple R-squared:
summary(tableModel)
# So, 0.8508 = proportion of PRICE variance accounted for by the model.
# Could do better by adding exploratory variables, but good start.

# Q6: ANOVA can be accomplished using a regression model. Regress PRICE against
# the variables BOWL, CASS, DISH, and TRAY as they are presented in the data
# file. What do the coefficients represent in this regression model? How is the
# effect of plate accounted for?

# WBI stands for "with binary indicators":
tableModelWBI <- {PRICE ~ BOWL + CASS + DISH + TRAY}
tableModelWBIFit <- lm(tableModelWBI, data = tableware)
print(tableModelWBIFit)
print(summary(tableModelWBIFit))
# The estimated coefficients represent incremental costs associated with the
# types of tableware. The type plate is represented by all zeroes for the
# indicator variables included in the model with binary indicators.

tableIndex <- tableware$TYPE == "plate"
mean(tableware[tableIndex, 8])

# BUT, there is a better way to fit a model of this form using R because the
# tableware data frame has the factor variable type. This factor variable can
# be used to create constrasts. If we like binary indicator constrasts, we can
# ask for treatment constrasts.

options(contrasts = c("contr.treatment", "contr.poly"))
tableModelFactor <- {PRICE ~ TYPE}
tableModelFactorFit <- lm(tableModelFactor, data = tableware)
print(tableModelFactorFit)
print(summary(tableModelFactorFit))
print(anova(tableModelFactorFit))

# Note on R-squared (both multiple and adjusted): values are identical between
# tableModelWBIFit and tableModelFactorFit:
print(summary(tableModelWBIFit))
print(summary(tableModelFactorFit))

# Q7: Plot PRICE versus DIAM and calculate the Pearson product moment
# correlation coefficient. Include DIAM in the regression model in (6). Compare
# results between the two models. DIAM is referred to as a covariate. Does its
# inclusion improve upon the fit of the first model without DIAM?

plot(tableware$DIAM, tableware$PRICE, pch = 21,
     bg = c("darkred", "blue4", "plum", "bisque4", "slategray4")
     [unclass(tableware$TYPE)])
legend("bottomright", legend = c("bowl", "cass", "dish", "plate", "tray"),
       col = c("darkred", "blue4", "plum", "bisque4", "slategray4"),
       pch = 20, bty = "n")
with(tableware, print(cor(DIAM, PRICE)))

# Now, first fit PRICE as a function of TYPE:
priceType <- {PRICE ~ TYPE}
priceTypeFit <- lm(priceType, data = tableware)
summary(priceTypeFit)
anova(priceTypeFit)

# Now, expand to include DIAM:
biggerModel <- {PRICE ~ DIAM + TYPE}
biggerModelFit <- lm(biggerModel, data = tableware)
summary(biggerModelFit)
anova(biggerModelFit)
# After reading ANOVA table, both variables (DIAM, TYPE) are significant

# Additional graphics can be examined for the fitted model itself.
# These are diagnostic graphics:
par(mfrow=c(2,2))
plot(biggerModelFit)
par(mfrow=c(1,1))

# Note how everything we do in R involves objects and functions. Fitted models
# are objects. And by giving these fitted models names, we make it easy to use
# all kinds of functions with these models. By naming a fitted linear model
# "my_biggest_model_fit" (for example), we can easily obtain a summary table
# with regression coefficients, an analysis of variance for effects, and
# diagnostic graphics. We can also obtain confidence intervals, predictions,
# and prediction intervals. R is an object-oriented programming environment
# that helps us to do data science.