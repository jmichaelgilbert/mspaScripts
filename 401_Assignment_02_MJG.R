# Data Analysis Assignment 2
# Last updated: 2015-05-10 by MJG

# Clear workspace
rm(list=ls())

hospital <- read.csv("Hospital.csv")

# Determine structure of data:
str(hospital)

# Generate table with margins, convert data to factors (equivalent to
# generating nominal variables for table construction).
control <- factor(hospital$Control)
region <- factor(hospital$Geog..Region)
control_region <- table(control, region)

# Check structure of table:
str(control_region)
control_region

# Add marginal totals and rename (table frequencies can be indexed by row, col).
m_c_r <- addmargins(control_region)
m_c_r

# (ROW, COL)
# Black: Page 140 Problem 2:
# Use the hospital database. Construct a cross-tabulation table for region and
# for type of control. You should have a 7x4 table. Using this table, answer
# the following questions. (Refer to Chapter 1 for category members.) 

# What is the probability that a randomly selected hospital is in the Midwest
# if the hospital is known to be for-profit?
m_c_r[3,3]/m_c_r[3,8]

# If the hospital is known to be in the South, what is the probability that it
# is a government, non-federal hospital?
m_c_r[1,1]/m_c_r[5,1]

# What is the probability that a hospital is in the Rocky Mountain region or a
# not-for-profit, non-government hospital?
(m_c_r[5,5]+m_c_r[2,8]-m_c_r[2,5])/m_c_r[5,8]

# What is the probability that a hospital is a for-profit hospital located in
# California?
m_c_r[3,6]/m_c_r[5,8]

# Black: Chapter 5 page 180 problem 2:
# Use the hospital database. 

# Create a factor out of Service and form a table
service <- factor(hospital$Service, labels = c("medical", "psychiatric"))
service <- table(service)
service <- addmargins(service)
service

# What is the breakdown between hospitals that are general medical hospitals
# and those that are psychiatric hospitals in this database of 2000 hospitals?
# (Hint: In Service, 1 = general medical and 2 = psychiatric.)
service[1:1] # medical
service[2:2] # psychiatric

# Using these figures and the hypergeometric distribution, determine the 
# probability of randomly selecting 16 hospitals from the database and getting
# exactly 9 that are psychiatric hospitals.
dhyper(x = 9, m = 32, n = 168, k = 16, log = F)

# Now, determine the number of hospitals in this database that are for-profit
# (Hint: In Control, 3 = for-profit.)
length(which(hospital$Control == 3))

# From this number, calculate p, the proportion of hospitals that are
# for-profit.
length(which(hospital$Control == 3)) / length(hospital$Control)

# Using this value of p and the binomial distribution, determine the probability
# of randomly selecting 30 hospitals and getting exactly 10 that are for-profit.
dbinom(x = 10, size = 30, prob = 0.225, log = F)

# Black: Chapter 6 page 220 problem 3:
# Use the hospital database.

# It can be determined that some hospitals admit around 50 patients per day.
# Suppose we select a hospital that admits 50 patients per day. Assuming that
# admittance only occurs within a 12-hour time period each day, and that
# admittance is Poisson distributed, what is the value of lambda per hour for
# this hospital?

lambda <- 50/12

# What is the interarrival time for admittance based on this figure?

arrival <- 1/lambda
arrival

# Suppose a person was just admitted to the hospital. What is the probability
# that it would be more than 30 minutes before the next person was admitted?

pexp(q = 3/6, rate = lambda, lower.tail = F, log.p = F)

# What is the probability that there would be less than 10 minutes before the
# next person was admitted?

pexp(q = 1/6, rate = lambda, lower.tail = T, log.p = F)

# Black: Chapter 7 problem 3 page 254:
# Use the hospital database.

# Determine the proportion of hospitals that are under the control of
# nongovernment not-for-profit organizations (Control = 2). Assume that this
# proportion represents the entire population for all hospitals.

p <- m_c_r[2,8]/m_c_r[5,8]

# If you randomly selected 500 hospitals from across the United States, what is
# the probability that 45% or more are under the control of nongovernment 
# not-for-profit organizations? 

# Notice we subtract 1 from q!!
# Exact binomial probability:
pbinom(q = ((0.45*500)-1), size = 500, prob = p, lower.tail = F)
# Or:
1 - pbinom(q = ((0.45*500)-1), size = 500, prob = p, lower.tail = T)

# This is a data check (apparently)
x <- c(seq(1, 224, by = 1))
1 - sum(dbinom(x = x, size = 500, prob = p, log = F))

# Continuity correction:
z <- (0.45 - p)/(sqrt(p*(1-p)/500))
pnorm(z, mean = 0, sd = 1, lower.tail = F, log.p = F)

# If you randomly selected 100 hospitals, what is the probability that less
# than 40% are under the control of nongovernment not-for-profit organizations?

# We also subtract 1 from Q here, because while lower.tail = T, we pair it with
# less than, which is equivalent to lower.tail = F, and then pairing it with
# greater than. See example below.

# Exact binomial probability:
pbinom(q = ((0.40*100)-1), size = 100, prob = p, lower.tail = T)
# And now we do the inverse, "unadjusted", which gives the same result:
pbinom(q = 0.60*100, size = 100, prob = 1-p, lower.tail = F)

# Continuity correction:
z <- ((0.395)-p)/(sqrt(p*(1-p)/100))
pnorm(z, mean = 0, sd = 1, lower.tail = T, log.p = F)

# REASONS WHY WE SUBTRACT 1 FROM Q:
# When working with discrete random variables, pawesome with lower.tail = FALSE
# fnds the probability of scoring above q. What this means is that if you are
# trying to find the probability of throwing at least 6 free-throws (out of 10
# total free throws), then the q that will give you the correct probability is
# NOT 6 ... but 5! How annoying!

# See also PDF saved in 401-DL folder "Samples and Distributions", page 5