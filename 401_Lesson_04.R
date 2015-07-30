# Lesson 04
# Author: MJG
# Last updated: 2015-04-18

# Clear workspace
rm(list=ls())

# Bring in data
shoppers <- read.csv("C:\\Users\\mgilbert\\Desktop\\Personal\\School\\MSPA\\401-DL\\R Lessons\\Lesson 4\\shoppers.csv")
str(shoppers)
# Rows = number of shoppers
nrow(shoppers)

# Exercises
# 1: Assume the fifty shoppers exit the store individually in random order.

# A: If one shopper is picked at random, what's the probability of picking
# a shopper who spent $40 or more dollars? What's the probability of picking
# a shopper who spent less than $10?

# How many did spend more than $40?
table(shoppers$Spending >= 40)
# The [2] is for TRUE, so we're really taking 8/50
p_event_1A1 <- table(shoppers$Spending >= 40)[2] / nrow(shoppers)
round(p_event_1A1, 4)

p_event_1A2 <- table(shoppers$Spending <= 10)[2] / nrow(shoppers)
round(p_event_1A2, 4)

# HINT: B through E: assume sampling without replacement

# B: If two shoppers are picked at random, what is the probability the pair
# will include a shopper who spent $40 or more dollars and one who spent
# less than $10?

n <- nrow(shoppers)
n1 <- sum(shoppers$Spending < 10)
n2 <- sum(shoppers$Spending >= 40)
p_event_1B <- (n1*n2)/(n*(n-1)/2)
round(p_event_1B, 4)

# C: If two shoppers are picked at random, what's the probability the pair
# will include two shoppers who spent no less than $10 and no more than $40?

m <- sum((shoppers$Spending >= 10) & (shoppers$Spending <= 40))
m
n <- nrow(shoppers)
n
p_event_1C <- (m*(m-1)) / (n*(n-1))
round(p_event_1C, 4)

# D: If four shoppers are picked at random, what's the probability one shopper
# will have spent less than $10, one shopper will have spent $40 or more 
# dollars, and two shoppers will have spent no less than $10 and no more than
# $40?

n <- nrow(shoppers)
n1 <- sum(shoppers$Spending < 10)
n2 <- sum(shoppers$Spending >= 40)
m <- sum((shoppers$Spending >= 10) & (shoppers$Spending <=40))
successful_combinations <- (n1*n2*m*(m-1))/2
total_combinations <- (n*(n-1)*(n-2)*(n-3)) / (4*3*2*1)
p_event_1D <- successful_combinations / total_combinations
round(p_event_1D, 4)

# E: If we know a randomly picked shopper has spent more than $30, what's the
# probability that shopper has spent more than $40?

# First, subset the data:
shoppers_GT30 <- subset(shoppers, subset = Spending > 30)
shoppers_GT30
# Now, compute from resulting rows:
# Sum shoppers spending >$40 / total resulting rows
round(sum((shoppers_GT30$Spending > 40) == TRUE) / nrow(shoppers_GT30), 4)

# 2: Use R to answer the following questions:

# A: Draw 100 samples with replacement of size 22 from the 365 integers. Count
# the number of samples in which one or more of the numbers sampled is
# duplicated. Divide by 100 to estimate the probability of such duplication
# occuring. (If 22 people are selected at random, what's the probability of 
# two or more matching birthdays?)

# We set the seed for reproducibility:
set.seed(1234)
count_duplicates <- 0
for (i in 1:100) {
        this_sample <- sample(1:365, size = 22, replace = T)
        if(length(this_sample) != length(unique(this_sample)))
                count_duplicates <- count_duplicates + 1
}
prob_event_2A1 <- count_duplicates/100
round(prob_event_2A1, 4)

# Now, we can do the second part in () with a shorter amount of code:
set.seed(1234)
prob_event_2A2 <- 
        mean(replicate(100,any(duplicated(sample(1:365, 22, replace=T)))))
round(prob_event_2A2, 4)

# And if we increase that to 10,000 samples...
set.seed(1234)
prob_event_2A3 <-
        mean(replicate(10000,any(duplicated(sample(1:365, 22, replace=T)))))
round(prob_event_2A3, 4)

# B: Suppose that 60% of marbles in a bag are black, and 40% are white. Generate
# a random sample of size 20 with replacement using uniform random numbers. For
# the numbers in each sample, if a random number of 0.6 or less, code it as a 1.
# If it is not 0.6 or less, code it as a zero. Add the twenty coded numbers. Do
# this 50 times and calculate the proporation of times the sum is 11 or greater.
# What have you estimated? Expand the number of trials to 10,000. The exact
# binomial estimated probability is 0.755 and the expectation is 12.

# Create a function that codes 0.6 or less as 1 (True), else 0
count <- function(N, p){
        x <- runif(n = N)
        count <- x <= p
        m <- sum(count)
}

# Now we'll reference count and pass sample size 20, and 0.6 as p
set.seed(1234)
result = NULL
for (i in 1:10000)
        +{result <- c(result, count(20 ,0.6))}

# Now we score these results and convert to probability
result <- (result >= 11)
sum(result) / 10000