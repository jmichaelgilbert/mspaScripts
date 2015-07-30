# Data Analysis Assignment 1 (for Quiz)
# Last updated: 2015-04-22 by MJG

# Clear our workspace and load libraries
rm(list=ls())
library(moments)

# Import data
soap <- read.csv("soap_sales.csv", sep = ",")

# 1: Execute summary(), stem(), hist(), and boxplot()
summary(soap$sales)
stem(soap$sales)
hist(soap$sales,
     main = "Soap Sales",
     xlab = "Dollars",
     col = "beige")
boxplot(soap$sales,
        main = "Soap Sales",
        xlab = "Dollars",
        col = "beige",
        notch = T,
        horizontal = T)

# Bplot in Rlab package
bplot(soap$sales,
      main = "Soap Sales",
      xlab = "Dollars",
      col = "beige",
      horizontal = T,
      outlier = T)

# Percent variance:
100 * (sd(soap$sales) / mean(soap$sales))

# 2: Calculate and compare the 20% trimmed mean to the mean from summary()
salesMean <- round(mean(soap$sales), 2)
salesMeanTrimmed <- round(mean(soap$sales, trim = 0.2), 2)
diffMean <- salesMean - salesMeanTrimmed

# 3: Calculate the skewness and kurtosis, plot qqnorm(), qqline(), and
# plot.ecdf(). Compare the results of Parts 1 and 2.
skewness(soap$sales)
kurtosis(soap$sales)

qqnorm(soap$sales,
       pch = 21, 
       col = "black", 
       bg = "beige",
       main = "Normal Q-Q Plot of Soap Sales",
       ylab = "Sample Quantiles ($)")
qqline(soap$sales,
       pch = 21,
       col = "black",
       bg = "beige",
       lwd = 1)

# For ECDF we want to compare to a random normal sample, so we do that first
# then add in our soap$sales data. But first let's look at it now:

plot.ecdf(soap$sales,
          pch = 21,
          col = "black")
legend("topleft",
       legend = "Sample",
       col = "black",
       pch = 21)

# Calculate 95th percentile
round(quantile(soap$sales, 0.95), 1)

# 4: Construct a six-cell relative frequency table with lower cell boundary
# starting at 10 and cell width of 5. Calculate the mean and standard deviation
# from the grouped data.

# Prepare the relative frequency table
cells <- seq(from = 10, to = 40, by = 5)
center <- seq(from = 12.5, to = 37.5, by = 5)

freqSales <- soap$sales
freqSales <- cut(freqSales, cells, include.lowest = T, right = F)
freqSales <- prop.table(table(freqSales))
freqSales <- data.frame(freqSales, center)
freqSales$Freq
freqSalesMean <- sum(freqSales$Freq * freqSales$center)
freqSalesMean

delta2 <- (freqSales$center - freqSalesMean)**2
std <- sqrt(sum(delta2 * freqSales$Freq))
std

# 5: Using the results from (4), plot a histogram of the relative frequencies
# with overlay as shown in Part 2. Compare this plot with the stem-and-leaf
# plot.



# 6: Plot sales as a function on week similarly to the index plot shown in Part
# 2.

