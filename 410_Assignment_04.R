###############################################################################
# 410-DL | Assignment #4
# Last updated: 2015-07-17 by MJG
###############################################################################

# Q3: Compute the t-statistic for Beta1 in Model 1
tStat <- 2.1860/0.4104; tStat

# Q4: Compute the R-Sqared for Model 1
m1n <- 72
m1k <- 4
m1p <- (m1k+1)
m1SSR <- 2126.0090
m1SSE <- 630.3595
m1SST <- 2756.3685

rSquared <- m1SSR/m1SST; rSquared

# Verify calculation:
ifelse((m1SSR/m1SST)==(1-(m1SSE/m1SST)),TRUE,FALSE)

# Q5: Compute the adjusted R-Squared for Model 1
rSquaredAdjusted <- (1-((m1SSE/(m1n-m1p))/(m1SST/(m1n-1))))
print(rSquaredAdjusted)

(m1SSE/(m1n-m1p))
(m1SST/(m1n-1))

(9.4083/38.8220)
1 - 0.2423

# Q7: Compute the F-statistic for the Overall F-test
m1F0 <- ((m1SSR/(m1k))/(m1SSE/(m1n-m1p))); m1F0

(m1SSR/(m1k))
(m1SSE/(m1n-m1p))

(531.5023/9.4083)

# Q10: Compute the F-statistic for the nested F-test using Model 1 & Model 2

nestF0 <- ((630.3595-572.6091)/(7-5))/((572.6091)/(72-7))
nestF0

28.8752/8.8093

# Q11: Compute the AIC values for Model 1 & Model 2

m1AIC <- (72)*(log(630.35953/72))+(2*(5)); m1AIC

m2AIC <- (72)*(log(572.60911/72))+(2*(7)); m2AIC

# Q12: Compute the BIC values for Model 1 & Model 2

m1BIC <- (72)*(log(630.35953/72))+(2*(5+2)*((72*9.40835)/(630.35953)))-(2*((72*9.40835)/(630.35953))**2)
m1BIC

m2BIC <- (72)*(log(572.60911/72))+(2*(7+2)*((72*8.80937)/(572.60911)))-(2*((72*8.80937)/(572.60911))**2)
m2BIC

# Q13: Compute Mallow's Cp values for Model 1 & Model 2

m1CP <- ((630.35953/9.40835)+(2*5)-72); m1CP

m2CP <- ((572.60911/8.80937)+(2*7)-72); m2CP
