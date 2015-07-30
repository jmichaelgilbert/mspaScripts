# Data Analysis Assignment 1
# Last updated: 2015-04-21 by MJG

rm(list=ls())

# Part 1

library(moments)

# Set the seed for repeatability of random number generation
set.seed(123)

# Generate standard normal random variables using the function rnorm()
normal_x <- rnorm(10000, mean = 0, sd = 1)

# Check summary statistics
summary(normal_x)
skewness(normal_x)
kurtosis(normal_x)

str(normal_x)

# Plot histogram with density function overlay
hist(normal_x, prob = T, ylim = c(0.0,0.5), col = "beige")
lines(density(normal_x), lwd = 2)

# Generate a second vector:
normal_w <- rnorm(10000, mean = 0, sd = 1)

# Now sort and match the ordered sets of data
normal_x <- sort(normal_x)
normal_w <- sort(normal_w)
# Graph on scatterplot
plot(normal_x, normal_w, main = "Scatterplot of two normal random variables")

# QQ is a scatterplot of two sets of data, and values of quantiles are
# plotted against each other. If distributions are the same, the plot is a 
# straight line.

qqnorm(normal_x)
qqline(normal_x)

# Using plot.ecdf() function, use to compare to our large sample
normal_w <- rnorm(50, mean = 0, sd = 1)
plot.ecdf(normal_x,
          xlab = "Standard Normal Variable x",
          main = "Comparison to Standard Normal")
plot.ecdf(normal_w, col = "blue", pch = 2, add = T)
abline(v = 0.0, lty = 2, col = "red")
legend("topleft", 
       legend = c("normal", "sample"),
       col = c("black", "blue"), 
       pch = c(19,2))


# Part 2
# EDA using Coca-Cola Data

# Start by clearing our workspace
rm(list=ls())

library(moments)
coke <- read.csv("Coke.csv", sep = " ")
str(coke)

# Now summary stats for variable Fill in Coke:
summary(coke$Fill)
sd(coke$Fill)
stem(coke$Fill)
boxplot(coke$Fill, col = "beige", notch = T)
100 * sd(coke$Fill) / mean(coke$Fill)

# Trim the mean:
mean(coke$Fill, trim = 0.2)
mean(coke$Fill)

# Look at distribution of data:
hist(coke$Fill, prob = T, ylim = c(0.0,1.5), col = "beige")
lines(density(coke$Fill), lwd = 2, col = "darkred")

# Look at skewness and kurtosis:
skewness(coke$Fill)
kurtosis(coke$Fill)

library(psych)
describe(coke$Fill)

# QQ plot of filled coke cans
qqnorm(coke$Fill)
qqline(coke$Fill)

# Now, let's compare our coke fill volume data vs. standard normal using
# empirical distribution functions. To do this, we standardize the data to a
# mean of zero, and standard deviation of one.

# Create distribution
mu <- mean(coke$Fill)
std <- sd(coke$Fill)
Fill <- (coke$Fill - mu) / std
# Alternate method (one liner)
Fill2 <- (coke$Fill - mean(coke$Fill)) / sd(coke$Fill)
# Verify methods are the same
Fill - Fill2
# Do some basic graphs
hist(Fill2, col = "beige")
boxplot(Fill2, col = "beige", notch = T)

normal <- rnorm(10000, mean = 0, sd = 1)
hist(normal, col = "beige")
boxplot(normal, notch = T, col = "beige")

# Let's plot the data to compare:
plot.ecdf(normal, 
          xlab = "Standard Normal Variable x", 
          main = "Comparison to Standard Normal")
plot.ecdf(Fill2, 
          col = "blue", 
          pch = 2, 
          add = T)
abline(v = 0.0, lty = 2, col = "red")
legend("topleft",
       legend = c("normal", "sample"),
       col = c("black", "blue"),
       pch = c(19,2))

# Prepare a relative frequency table
# Begin by defining cell boundaries, then defining cell midpoints.
cells <- seq(from = 339, to = 341.2, by = 0.2)
center <- seq(from = 339.1, to = 341.1, by = 0.2)

Fill_Volume <- coke$Fill
# cut() will place each fill volume into the associated cell
Fill_Volume <- cut(Fill_Volume, cells, include.lowest = T, right = F)
# table() followed by prop.table() will calculate proportions in each cell
Fill_Volume <- prop.table(table(Fill_Volume))
# Include the cell center in the data frame
Fill_Volume <- data.frame(Fill_Volume, center)
# Print out the data frame and compare to the stem-and-leaf plot
Fill_Volume

# Superimpose histogram using established breaks from cells
# First, establish the count in each cell
count <- Fill_Volume$Freq * length(coke$Fill)
Fill_Volume <- data.frame(Fill_Volume, count)

# Plot the frequency (count) for each cell with overlay
hist(coke$Fill,
     breaks = cells,
     main = "Frequency in Each Cell",
     right = F)
lines(Fill_Volume$center,
      Fill_Volume$count,
      type = "b",
      col = "red")

# Calculate the mean and standard deviation from the grouped data and compare
mean <- sum(Fill_Volume$Freq * Fill_Volume$center)
mean
delta2 <- (Fill_Volume$center - mean)**2
std <- sqrt(sum(delta2 * Fill_Volume$Freq))
std

# Add an index variable to the data frame so that a scatter plot can be made
index <- seq(1,50)
sample <- data.frame(coke, index)
plot(sample$index,
     sample$Fill,
     ylim = c(335, 345),
     main = "Fill versus Index")
abline(h = mean(sample$Fill))

coke$Fill <- sort(coke$Fill)
coke$Fill[48] # 95th Percentile Value
# Easier way for 95th percentile:
quantile(coke$Fill, 0.95)
