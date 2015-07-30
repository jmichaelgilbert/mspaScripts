# Lesson 01
# Author: MJG
# Last updated: 2015-04-08

home_prices <- read.csv("C:\\Users\\mgilbert\\Desktop\\Personal\\School\\MSPA\\401-DL\\R Lessons\\Lesson 1\\home_prices.csv")

# Exercises

# a. What are the measurement levels of each of the eight variables?
str(home_prices)
head(home_prices)
tail(home_prices)
summary(home_prices)

# b. Should any variable have its values changed to better reflect its true nature?
# NBR & CORNER could be changed to logic

# c. For the variable PRICE, select a simple random sample of size 12 from the file
SRS <- sample(home_prices$PRICE, 12)
print (SRS)
print (mean(SRS))

# d. For the variable PRICE, select a systemic sample of twelve observations.
# d. Start with the seventh observation and pick every 10th observation thereafter.
SS <- home_prices$PRICE[seq(from = 7, to = length(home_prices$PRICE), by = 10)]
print (SS)
print (mean(SS))

# e. Examine the printed values and mean values obstained from the two sampling /
# e. procedures. Do you see a difference?
print (summary(SRS))
print (summary(SS))

# f. Create boxplots for SRS and SS using boxplot(). How do the two samples compare?
boxplot(SRS,
        col = "beige",
        horizontal = T,
        main = "Random Sample of Home Prices\nn = 12",
        xlab = "Home Prices in Thousands")

boxplot(SS,
        col = "beige",
        horizontal = T,
        main = "Systemic Sample of Home Prices\nEvery 10th Observation from 7 through 117",
        xlab = "Home Prices in Thousands")

# Extra: create histogram
hist(SRS, col = "beige")
hist(SS, col = "beige")
