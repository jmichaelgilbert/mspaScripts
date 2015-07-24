###############################################################################
# 410-DL | Quiz #3
# Last updated: 2015-07-23 by MJG
###############################################################################

# Q2: Compute AIC for the linear regression model:
# y = b0 + b1*X1 + b2*X2 + b3*X3
# Regression model was fitted on a sample of 250 observations and yielded a
# likelihood value of 0.18.

# What do we know? We know the model has an intercept. So the model parameters
# equal the number of predictor variables + 1:
# p = k + 1, where p = parameters and k = predictor variables
# AIC formula is given in LRA, p 336:
# AIC = (-2)*ln(L)+(2*p), where L = likelihood function for the model

q2n <- 250
q2k <- 3
q2p <- q2k + 1
q2lhd <- 0.18

q2 <- ((-2)*(log(0.18)))+(2*(q2p)); q2

# Q3: Compute BIC for the linear regression model:
# y = b0 + b1*X1 + b2*X2 + b3*X3
# Regression model was fitted on a sample of 250 observations and yielded a
# likelihood value of 0.18.

# What do we know? We know the model has an intercept. So the model parameters
# equal the number of predictor variables + 1:
# p = k + 1, where p = parameters and k = predictor variables
# BIC formula is given in LRA, p 336:
# BIC = (-2)*ln(L)+(p*ln(n)), where L = likelihood function for the model

q3n <- 250
q3k <- 3
q3p <- q2k + 1
q3lhd <- 0.18

q3 <- ((-2)*(log(0.18)))+(q3p*(log(q3n))); q3




