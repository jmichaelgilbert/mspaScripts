# 410-DL Variance Example
# Last updated: 2015-06-22 by MJG

# Clear workspace:
rm(list=ls())

# -----------------------------------------------------------------------------
# Long Version:
# -----------------------------------------------------------------------------

# Set seed for reproducibility:
set.seed(1234)

# Generate 10,000 random variables:
sample <- rnorm(10000, mean = 0, sd = 1)
# Print the variance:
sampleVar <- var(sample); sampleVar

# Increase scale of sample by 10:
sampleBig <- sample*10
# Now check variance:
sampleBigVar <- var(sampleBig); sampleBigVar

# Does increasing scale by 10, increase variance by a factor of 100?
ifelse(sampleBigVar==sampleVar*100, T, F)

# Does changing the constant (location) affect the variance?
ifelse(var(sample)==(var(sample+10)), T, F)

# -----------------------------------------------------------------------------
# Compact Version:
# -----------------------------------------------------------------------------
sample <- rnorm(10000, 0, 1)
# Does increasing scale by 10, increase variance by a factor of 100?
ifelse((var(sample*10))==(var(sample)*100), T, F)
# Does changing the constant (location) affect the variance?
ifelse((var(sample))==(var(sample+10)), T, F)

# Both statements evaluate to true.
