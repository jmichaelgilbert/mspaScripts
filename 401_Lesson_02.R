# Lesson 02
# Author: MJG
# Last updated: 2015-04-08

# Clear workspace
rm(list=ls())

# Bring in data
home_prices <- read.csv("C:\\Users\\mgilbert\\Desktop\\Personal\\School\\MSPA\\401-DL\\R Lessons\\Lesson 2\\home_prices.csv")

# Exercises

# Part 1:
# a. Construct a histogram for PRICE. Describe the distribution shape.
hist(home_prices$PRICE, col = "beige")

# b. Construct a histogram for TAX. Describe the distribution shape.
hist(home_prices$TAX, col = "beige")

# c. Construct a scatterplot displaying TAX versus PRICE. Is there a relationship?
# Format is plot(x-axis, y-axis)
plot(home_prices$PRICE, home_prices$TAX,
     main = "Home Prices Relative to Taxes",
     xlab = "Taxes",
     ylab = "Prices",
     pch = 20,
     col = "grey")

# d. Construct a stem-and-leaf plot for TAX using stem().
# d. To aid in the interpretation of the plot, define X = TAX/100.
# d. Round X to one digit using round() and plot stem(X)
stem(round(home_prices$TAX/100, digits = 1))

# e. Use the par() and mfrow() or mfcol() functions to construct a window with
# e. two rows and one column showing the histograms for PRICE and TAX.
par(mfrow=c(1,2))
hist(home_prices$TAX, 
     col = "beige", 
     main = "Histogram of Taxes",
     xlab = "Taxes")
hist(home_prices$PRICE, 
     col = "beige",
     main = "Histogram of Prices",
     xlab = "Prices")
par(mfrow=c(1,2))

# Part 2:
# a. Construct a histogram for PRICE starting the first class at 1300 ($hundreds)
# a. with a class width of 600 ($hundreds).

# b. Construct a histogram for TAX starting the first class at $500
# b. with a class width of $500.