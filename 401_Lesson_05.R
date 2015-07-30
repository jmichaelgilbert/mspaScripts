# Lesson 05
# Author: MJG
# Last updated: 2015-04-21

# Clear workspace
rm(list=ls())

# Q1: Suppose a gambler goes to the race track to bet on four races. There are
# six horses in each race. He picks one at random out of each race and bets on
# each of the four selections. Assuming a binomial distributions, answer the
# following questions.

# Q1A: The gambler wins all four races.
Q1A <- dbinom(x = 4, size = 4, prob = 1/6)
round(Q1A, 4)

# Q1B: The gambler loses all four races.
Q1B <- dbinom(x = 0, size = 4, prob = 1/6)
round(Q1B, 4)

# Q1C: The gambler wins exactly one race.
Q1C <- dbinom(x = 1, size = 4, prob = 1/6)
round(Q1C, 4)

# Q1D: The gambler wins at least one race.
# Two ways, the first uses input from earlier:
Q1D <- 1 - Q1B
round(Q1D, 4)
# Second way, longer:
Q1D <- 1 - dbinom(x = 0, size = 4, prob = 1/6)
round(Q1D, 4)

# Q2: A woman claims she can tell by taste if cream is added before a tea bag
# is placed in a tea cup containing hot water. An experiment is designed. A
# series of cups of tea will be prepared with m of them having the cream added
# prior to the tea bag and m of them with the cream added after the tea bag. 
# This gives a total of 2m cups of tea. The sequence of tea cups is presented
# in random order. If the woman cannot discriminate it will be expected on 
# average she would guess at random and be correct on half the tea cups. Answer
# the following questions assuming the number of successes follows a binomial
# distribution with probability equal to 0.5 and 2m trials. That is we are 
# working with a binomial distribution (n = 2m, p = 0.5). The p = 0.5 comes 
# from the fact that half of the cups have cream added first and the other half
# do not.

# Q2A: If the total number of binomial trials is 2n = 20, what is the
# probability the woman is correct more than 15 out of 20 times?

# Use the distribution function and look at the upper tail:
# Important to note:
# The argument lower.tail = TRUE gives probabilities of x being q or less.
# The argument lower.tail = FALSE gives probabilities of x being greater than q.
Q2A <- pbinom(q = 15, size = 20, prob = 0.5, lower.tail = FALSE)
round(Q2A, 4)

# Q2B: To reduce the amount of labor, how small can the total number of
# binomial trials be while keeping the probability of 2n consecutive successes
# at or below 0.05? (We use 2n = the number of trials since half the cream first
# and half after the tea bag.)

# Use a while loop to achieve this:
target_p <- 0.05
current_p <- 1.0
n <- 0

while (current_p > target_p){
        # Since we increase by 2n per above
        n <- n + 2
        # Since we are using consecutive hits >= to n-1
        current_p <- pbinom(q = n-1, size = n, prob = 0.5, lower.tail = FALSE)
        # Print the results after each iteration
        cat("\n Number of Consecutive Cups Correctly Identified:", n, 
            "p_value: ", round(current_p, 4))
}

# Loop has finished so we have our answer:
cat("\n\nLady Tasting Tea Solution: ", n, "Consecutive Cups Correctly Labeled",
    "\n p-value: ", round(current_p, 4), "<= 0.05 critical value.")

# Q3: An emergency room has 4.6 serious accidents to handle on average each
# night. Using the Poisson distribution, calculate the distribution of accidents
# per night. (In other words, what is the probability of 0, 1, 2, ... accidents
# per night?) Plot the result.

# Use a for loop:
for (x in 0:20)
        cat("\n x: ", x, "prob: ", round(dpois(x, lambda = 4.6), 4))

# We can plot this as well:
x <- 0:20
prob_x <- dpois(x, lambda = 4.6)
plot(x, prob_x, last = 1, type = "h") # type = "h" is a trigger for histogram
title("Poisson Probabilities (lamda = 4.6)")

# Q4: A production process occasionally produces at random a defective product
# at a rate of 0.001. If these products are packaged 100 at a time in a box and
# sold, answer the following questions and compare your answers. Plot the
# distributions for each type of variable over the range of 0, 1, 2, 3, and 4.
# What do you conclude?

# Q4A: Using the binomial distribution, calculate the box has 0 defects.
Q4A <- dbinom(x = 0, size = 100, prob = 0.001)
round(Q4A, 4)

# Q4B: Using the Poisson distribution, calculate the box has 0 defects.
Q4B <- dpois(x = 0, lambda = (100 * 0.001))
round(Q4B, 4)

# Q4C: Plot and compare
x <- 0:4
prob_x <- dpois(x, lambda = (100 * 0.001))
plot(x, prob_x, las = 1, type ="h")
title("Poisson Probabilities (lambda = 0.1")

c <- c(0, 1, 2, 3, 4)
prob <- dbinom(x = c, size = 100, prob = 0.001)
plot(c, prob, las = 1, type = "h")
title("Binomial Probabilities (n = 100, p = 0.001)")
