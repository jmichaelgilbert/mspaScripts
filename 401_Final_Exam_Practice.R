# 401-DL Practice Final Exam
# Last Updated: 2015-06-06 by MJG

# Clear workspace:
rm(list=ls())

# Load libraries:
library(gtools)

# -----------------------------------------------------------------------------
# `Q1`
# -----------------------------------------------------------------------------

# Suppose that a class of 30 students is assigned to write an essay.

# Suppose 4 essays are randomly chosen to appear on the class bulletin board.
# How many different groups of 4 are posssible?

Q1a <- combinations(30, 4)
nrow(Q1a)

# Suppose 4 essays are randomly chosen for awards of $10, $7, $5, and $3. How
# many different groups of 4 are possible?

# First up we have 4 possible groups ($dollars) and 4 possible essays. Order
# matters, so it's a permuatation:
Q1b1 <- permutations(4, 4)
nrow(Q1b1)

# Which gives us 24. Then we multiply 24 by the original number from Q1a:
Q1b2 <- nrow(Q1a) * nrow(Q1b1)
Q1b2

# -----------------------------------------------------------------------------
# `Q2`
# -----------------------------------------------------------------------------

# Use Bayes' theorem to find the indicated probability. Use the results 
# summarized in the table.

# Create a table:
Q2table <- matrix(c(8, 17, 18, 13, 7, 37), ncol = 2, byrow = T)
colnames(Q2table) <- c("Approve of mayor", "Do not approve of mayor")
rownames(Q2table) <- c("Republican", "Democrat", "Independent")
Q2table

# Verify work for sum() argument.
Q2 <- Q2table[2,1] / sum(Q2table[1:3])
round(Q2, 4)

# -----------------------------------------------------------------------------
# `Q3`
# -----------------------------------------------------------------------------

# A police department reports that the probabilities that 0, 1, 2, and 3
# burglaries will be reported in a given day are 0.46, 0.41, 0.09, and 0.04
# respectively. Find the standard deviation for the probability distribution.
# Round answer to the nearest hundredth.

e <- c(0, 1, 2, 3)
p <- c(0.46, 0.41, 0.09, 0.04)
mu <- sum(e*p)

var <- sum(((e-mu)**2)*(p))
sd <- sqrt(var)
Q3 <- sd
round(Q3, 2)

# -----------------------------------------------------------------------------
# `Q4`
# -----------------------------------------------------------------------------

# Assume that the weight loss for the first month of a diet program varies
# between 6 pounds and 12 pounds, and is spread evenly over the range of
# possibilities, so that there is a uniform distribution. Find the probability
# of the given range of pounds lost:

# Lost between 8.5 and 10 pounds.

# Uniform distribution = all are equally likely, so likelihood for each:
pdf <- (1 / (12 - 6))
# Range of the loss:
loss <- (10 - 8.5)
# Likelihood of falling in range of loss:
Q4 <- (pdf * loss); Q4

# -----------------------------------------------------------------------------
# `Q5`
# -----------------------------------------------------------------------------

# Find the indicated z score. The graph depicts the standard normal distribution
# with mean 0 and standard deviation 1. Shaded area is 0.0901.

# Use qnorm to find z score:
z <- qnorm(0.0901); z

# -----------------------------------------------------------------------------
# `Q6`
# -----------------------------------------------------------------------------

# True or false: In a hypothesis test, an increase in alpha will cause a
# decrease in the power of the test provided the sample size is kept fixed.

# Increasing alpha generally increases the power of the test.
# Increasing sample size increases power. Alternatively, if we hold the power
#       constant, and we decrease alpha, we need a larger sample size.
# Larger alpha gives a smaller confidence level.

# Examples:
Q5a <- pwr.t.test(d = (0-5)/10,
                  n = 35,
                  sig.level = 0.01,
                  type = "paired",
                  alternative = "two.sided")
Q5a

Q5b <- pwr.t.test(d = (0-5)/10,
                  n = 35,
                  sig.level = 0.05,
                  type = "paired",
                  alternative = "two.sided")
Q5b

# -----------------------------------------------------------------------------
# `Q7`
# -----------------------------------------------------------------------------

# True or false: In a hypothesis test regarding a population mean, the 
# probability of a Type II error, beta, depends on the true value of the
# population mean.

# Alpha is the probability of rejecting the hypothesis tested when that
#       hypothesis is true - the Type I error (false positive).
# Beta is the probability of accepting the hypothesis tested when the 
#       alternative hypothesis is true - the Type II error (false negative).
# Power is the probability of rejecting the hypothesis tested when the 
#       alternative hypothesis is true.

# -----------------------------------------------------------------------------
# `Q8`
# -----------------------------------------------------------------------------

# A cereal company claims that the mean weight of the cereal in its packets is
# 14 oz. Identify the Type I error for the test.

# A Type I error is a false positive. An example of this is positively tested
#       for cancer, when you really do not have cancer. By definition, a Type I 
#       error involves the rejection of a null hypothesis that is actually true.
# A Type II error is a false negative. By definition, a Type II error involves
#       failing to reject a null hypothesis that is actually false.
# In this example, the null hypothesis (H0) is:
#       The mean weight of the cereal in its packets is 14 oz. 

# The only choice that allows this is (C): Reject the claim that the mean weight
#       is 14 oz when it is actually greater than 14 oz.

# -----------------------------------------------------------------------------
# `Q9`
# -----------------------------------------------------------------------------

# Suppose that you perform a hypothesis test regarding a population mean, and
# the evidence does not warrant rejection of the null hypothesis. When
# formulating the conclusion to the test, why is the phrase "fail to reject the
# null hypothesis" more accurate than the phrase "accept the null hypothesis"?

# We use the phrase "fail to reject the null hypothesis" because there is still
# a chance the null hypothesis is false. That size of that chance depends on
# the value of alpha that we set during the test. Our failure to reject the null
# hypothesis is only true for the assumptions and parameters we specify during
# the test, and really means that based on those, we do not find sufficient
# evidence to reject the null hypothesis. That does not entail the null
# hypothesis is true, just that we do not have sufficient evidence to reject it.

# -----------------------------------------------------------------------------
# `Q10`
# -----------------------------------------------------------------------------

# Scores on a test are normally distributed with a mean of 68.2 and a standard
# deviation of 10.4. Estimate the probability that among 75 randomly selected
# students, at least 20 of them score greater than 78.

# Score greater than means lower.tail is FALSE
p <- pnorm(78, mean = 68.2, sd = 10.4, lower.tail = F); p
p <- round(p, 4)
n <- 75
q <- 1-p
np <- n*p
nq <- n*(q)
# Both np and nq >5 so can use continuity correction.
var <- n*p*q
sd <- sqrt(var)
# Conintuity correction brings 20 to 20.5
stu <- 20.5
z <- (stu-np)/sd; z
# Look up 1.98 in z-table
0.5 - 0.4893

# -----------------------------------------------------------------------------
# `Q11`
# -----------------------------------------------------------------------------

# According to a recent poll, 53% of Americans would vote for the incumbent
# president. If a random sample of 100 people results in 45% who would vote
# for the incumbent, test the claim that the actual percentage is 53%. Use a
# 0.10 significance level.

# H0: p = 0.53
# H1: p != 0.53
# Two sided test since it's !=

p <- 0.53
phat <- 45/100
z <- ((phat - p)/(sqrt(p*(1-p)/100))); z
# z score of -1.60 = p-value of 0.0548

# Find critical values:
alpha <- 0.10
zcrit <- qnorm(1-alpha/2)

ifelse(abs(z)>zcrit,"Reject null.","Fail to reject null.")

# -----------------------------------------------------------------------------
# `Q12`
# -----------------------------------------------------------------------------

# Find the value of the linear correlation coefficient:

x <- c(47.0, 46.6, 27.4, 33.2, 40.9)
y <- c(8, 10, 10, 5, 10)
cor(x, y)

# -----------------------------------------------------------------------------
# `Q13`
# -----------------------------------------------------------------------------

# What is the relationship between the linear correlation coefficient and the
# usefulness of the regression equation for making predections?

# The linear regression equation is appropriate for prediction only when there
# is a significant linear correlation between two variables. The strength of
# the linear relationship (as measured by the linear correlation coefficient)
# indicates the usefulness of the regression equation for making predictions.

# -----------------------------------------------------------------------------
# `Q14`
# -----------------------------------------------------------------------------

# Describe the standard error of estimate, se. How do smaller values of se
# relate to the dispersion of data points about the line determined by the
# linear regression equation? What does it mean when se is 0?

# The standard error of estimate, se, is a measure of the distances between the
# observed sample y values, and the predicted values yhat. Smaller values of se
# indicate that the actual values of y will be closer to the regression line,
# whereas larger values of se indicate a greater dispersion of the y values
# about the regression line. When the standard error estimate is 0, the y
# values lie on the regression line.

# -----------------------------------------------------------------------------
# `Q15`
# -----------------------------------------------------------------------------

# Use the given sample data to test the claim that p1 > p2. Use a significance
# level of 0.01.

# H0: p1 > p2

# Sample 1
n1 <- 85
x1 <- 38

# Sample 2
n2 <- 90
x2 <- 23

pHat1 <- x1/n1
pHat2 <- x2/n2
pHat <- (x1 + x2)/(n1 + n2)
round(pHat, 4)

# Critical value:
cv <- qnorm(1-alpha)

zScore <- (pHat1 - pHat2)/(sqrt((pHat*(1-pHat))*((1/n1)+(1/n2))))
# Now look up zScore in the table: 2.66 = 0.9961

# Since our zScore is greater than our critical value, we reject the null.

# To find the p-value:
# Since we are doing greater than, we subtract 1, as z-score is to the LEFT and
# we need to look to the right.
pval <- 1 - 0.9961; pval

# -----------------------------------------------------------------------------
# `Q16`
# -----------------------------------------------------------------------------

# Two types of flares are tested and their burning times (in minutes) are
# recorded. The summarys statistics are given below.

# H0: u1 = u2
# Ha: u1 != u2

# Brand X
x.n <- 35
x.mu <- 19.4
x.sd <- 1.4

# Brand Y
y.n <- 40
y.mu <- 15.1
y.sd <- 0.8

df <- (x.n - 1) + (y.n - 1)

alpha <- 0.05
cval <- qt((1-alpha/2), df)

# -----------------------------------------------------------------------------
# `Q17`
# -----------------------------------------------------------------------------

# Construct the indicated confidence interval for the difference between
# population proportions p1 - p2. Assume that the samples are independent and
# that they have been randomly selected.

# Construct a 90% confidence interval.
alpha <- 0.10
z <- qnorm(1-alpha/2)

n1 <- 50
x1 <- 15
p1 <- x1/n1

n2 <- 60
x2 <- 23
p2 <- x2/n2

se <- sqrt((p1*(1-p1)/n1)+(p2*(1-p2)/n2))
moe <- z*se
ciLower <- (p1-p2) - moe
ciUpper <- (p1-p2) + moe
print(c(ciLower, ciUpper))

# -----------------------------------------------------------------------------
# `Q18`
# -----------------------------------------------------------------------------

# Use the given data to find the equation of the regression line. Round the
# final values to three significant digits, if necessary.

x <- c(6, 8, 20, 28, 36)
y <- c(2, 4, 13, 20, 30)

lm(y~x)


