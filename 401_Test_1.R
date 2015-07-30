# Quiz 1: Predict 401-DL
# Authored by MJG
library(psych)

# Question 2
# Find Median
Q2 <- read.csv("C:\\Users\\mgilbert\\Desktop\\Personal\\School\\MSPA\\401-DL\\Quizzes\\Quiz1_Q2.csv", header=FALSE)
# Comes in as Integer, we want Numeric:
Q2Num <- sapply(Q2$V1[1:20], as.numeric)
median(Q2Num)
# Can also use describe from psych library, defaults trim to 0.1
describe(Q2Num)

# Question 3
# Find Trimmed Mean
# Insheet variables, header = False
Q3 <- read.csv("C:\\Users\\mgilbert\\Desktop\\Personal\\School\\MSPA\\401-DL\\Quizzes\\Quiz1_Q3.csv", header=FALSE)
# Comes in as Integer, we want Numeric:
Q3Num <- sapply(Q3$V1[1:20], as.numeric)
# Now trim it:
mean(Q3Num, trim=0.10)
# Can also use describe from psych library, defaults trim to 0.1
describe(Q3Num)

# Question 5
# Create Coefficient Variation Function
CV <- function(mean, sd){
      (sd/mean)*100
      }
# Specify our variables
Q5 <- c(159, 150, 186, 105, 197, 130, 172, 121, 116, 125)
CV(mean = mean(Q5), sd = sd(Q5))

# Question 6
# Done in Excel (JesuS Saves)

# Question 7
Q7 <- read.csv("C:\\Users\\mgilbert\\Desktop\\Personal\\School\\MSPA\\401-DL\\Quizzes\\Quiz1_Q7.csv")
describe(Q7)
