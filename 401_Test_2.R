# Test 2: Predict 401-DL
# Last updated: 2015-04-26 by MJG

# Clear workspace
rm(list=ls())

# Question 1: In a homicide case 4 different witnesses picked the same man from 
# a line up. The line up contained 5 men. If the identifications were made by 
# random guesses, find the probability that all 4 witnesses would pick the same 
# person.

# Be careful here, and watch what the question is asking. It's not that all
# four witnesses pick the same person, it's that three of them pick the same
# person as the first. That changes it THREE, not FOUR.

# Easy way
Q1 <- dbinom(x = 4, size = 4, prob = 1/5)
round(Q1, 4)

# Two alternatives
Q1 <- (5) * (1/5)^4
Q1 <- (1/5)^3

# Question 2: A IRS auditor randomly selects 3 tax returns from 49 returns of 
# which 7 contain errors. What is the probability that she selects none of those
# containing errors? Round to four decimal places.
Q2 <- dhyper(x = 0, m = 7, n = 42, k = 3)
round(Q2, 4)

# Question 3: 3.8% of a population are infected with a certain disease. There 
# is a test for the disease, however the test is not completely accurate. 93.9%
# of those who have the disease test positive. However 4.1% of those who do not 
# have the disease also test positive (false positives). A person is randomly 
# selected and tested for the disease.
#
# What is the probability that the person has the disease given that the test 
# result is positive?
d <- 0.038
dNot <- (1 - d)

dHaveTrue <- 0.939
dHaveFalse <- (1 - dHaveTrue)

dNotTrue <- 0.041                                     #False Positive
dNotFalse <- (1 - dNotTrue)

Q3 <- (d * dHaveTrue) / ((dNot * dNotTrue) + (d * dHaveTrue))
round(Q3, 4)

# Question 4: NOTE: Solved with z-score table

# Question 5: A study of the amount of time it takes a mechanic to rebuild the 
# transmission for a 2005 Chevrolet Cavalier shows that the mean is 8.4 hours 
# and the standard deviation is 1.8 hours. If 40 mechanics are randomly 
# selected, find the probability that their mean rebuild time exceeds 8.7 hours.

# One mean z-test, page 242 in Basic Statistics
omzt <- function(x, n, mu, sd){
        z <- (x - mu) / (sd / sqrt(n))
        round(z, 4)
}

omzt(8.7, 40, 8.4, 1.8)

# Lookup z-score based on Q5
z <- 0.8531
1 - z

# Question 6: NOTE: Solved 

# Question 7: In one region, the September energy consumption levels for 
# single-family homes are found to be normally distributed with a mean of 1050 
# kWh and a standard deviation of 218 kWh. Find P45, the 45th percentile of the 
# distribution.

Q7 <- qnorm(0.45, mean = 1050, sd = 218, lower.tail = T)
round(Q7, 4)