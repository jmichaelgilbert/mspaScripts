# Original code by Chad R. Bhatti on 2015-08-18
# Last updated: 2015-08-24 by MJG
# building_prices_pca.R

###############################################################################
# Data Loadings
###############################################################################

# Use the building prices data from 'Regression Analysis By Example' by 
#   Chatterjee and Hadi

# User will define location of file
my.path <- 'C:\\Users\\mgilbert\\Desktop\\Personal\\School\\MSPA\\410-DL\\Lecture Video Attachments\\'
my.file <- paste(my.path, 'W06_building_prices.txt', sep = '')
my.data <- read.table(my.file, header = TRUE)

# Check the file read by viewing the data
str(my.data)

# Create a matrix of predictor variables, i.e. the X matrix
# Note that we drop the response variable (Y) and two discrete variables
# This drops X2 and X9 (discrete, not continuous), and Y (response variable)
X <- as.matrix(my.data[, -c(2, 9, 10)]); X

###############################################################################
# Think of this as our 'Toy Matrix' (A, b, c) - this is an example for our
#   matrix standardization.
# Need to standardize X so that PCA does not focus on the scale of the variables
# Let's check how we can structure these computations in R
A <- matrix(1:4, nrow = 2, ncol = 2, byrow = TRUE); A
b <- matrix(rep(1:2, 2), nrow = 2, byrow = TRUE); b
c <- matrix(rep(c(2, 4), 2), nrow = 2, byrow = TRUE); c
(A-b)/c
###############################################################################

# Now compute the quantities that we need and standardize the X data to S data
# Create a matrix of means and a matrix of standard deviations
mu.x <- matrix(rep(apply(X, FUN = mean, MARGIN = 2), dim(X)[1]), 
               nrow = dim(X)[1], byrow = TRUE); mu.x
sd.x <- matrix(rep(apply(X, FUN = sd, MARGIN = 2), dim(X)[1]), 
               nrow = dim(X)[1], byrow = TRUE); sd.x
# Look at the matrices. 
# Do they have the correct structure to perform elementwise array computations?
#   Yes!

# Now standardize the X matrix
# Remember: there's a correlation matrix and a covariance matrix, and there's a
#   relationship between them.
# The correlation matrix of X, is a form of a standardized matrix (S)
# If a matrix is standardized, then the covariance matrix and correlation 
#   matrix will be the same. The covariance matrix of the standardized matrix
#   will be the same as the correlation matrix of the unstandardized matrix.
#   Which is one of the reasons we standardize the matrix (to make them equal)!
S <- ((X-mu.x)/sd.x)
# Verify mean 0 and standard deviation 1:
summary(S)
apply(S, FUN = sd, MARGIN = 2)
# Looks like our data is mean 0 with standard deviation 1

###############################################################################
# Principal Components
###############################################################################

# Are we ready to compute the principal components?  What matrix do we need? 
#   X? S? Something else?
# We need t(S)%*%S; note the '%*%' = matrix multiplication in R
# Need a square matrix for PCA/SVD (t = transpose)

# Check out the matrix S - is it square? What about X? Neither are square.
dim(S); dim(X)

# IMPORTANT:
# Remember t(X)%*%X is the unstandardized covariance matrix
# t(S)%*%S is the covariance matrix of the standardized variables, which is 
#   equivalent to the correlation matrix of X
# Check this by using these R commands:
cor(X)
cov(S)
# They look pretty similar, but what happens in the below?
cor(X)-cov(S)
# Small differences (e-16, e-17) are OK; that's known as 'floating point noise'

# Moving forward - again, t = transpose, so transpose(S)*(S):
SS <- t(S)%*%S
dim(SS)

# Use help(eigen) to learn more about the function eigen()
# eigen() performs a spectral decomposition
# Use only.values = FALSE because we want to get the eigenvectors
SS.eigen <- eigen(SS, only.values = FALSE); SS.eigen

# Make a scree plot by hand - first we get the scree values, effectively just
#   a pie chart of the percent the $values represent across their aggregation
scree.values <- SS.eigen$values/sum(SS.eigen$values); scree.values
# Make plot
plot(scree.values, type = 'l', lwd = 2, col = 'red')
points(scree.values, col = 'red', cex = 2)
title('Scree Plot of Eigenvalues')
# How many principal components should we retain?
# We would probably select five principal components - WHY?

# Total Variance Explained
total.var <- cumsum(SS.eigen$values)/sum(SS.eigen$values); total.var
# Five principal components will explain 96.4% of the variation in the 
#   predictor variables
# Make plot
plot(total.var, type = 'l', lwd = 2, col = 'blue')
points(total.var, col = 'blue', cex = 2)
title('Total Variance Explained')

# Pull out the eigenvectors
V <- SS.eigen$vectors; V
# Note that this is already a matrix
is.matrix(V)

# Eigenvectors are 'unit vectors', i.e. they are length 1 in euclidean distance
# Do we know what euclidean distance is?
# Good news - euclidan distance is how we typically think about distance
sqrt(t(V[, 1])%*%V[, 1])
sqrt(t(V[, 3])%*%V[, 3])

# Compute the principal components in matrix format - Z
# We're multiplying our standardized matrix (S) by our eigenvector matrix (V)
Z <- S%*%V; Z

# R has a built in function to compute principal components
# This function princomp() uses the eigen() function just like we just performed
# help(princomp) will explain in more detail
R.X <- princomp(X); R.X
R.S <- princomp(S); R.S

# We have a difference here - R does not standardize the data for you! You need
#   to use the COR option
# Type help(princomp) on the R command line to learn more about the princomp()
#   function
cor.option <- princomp(X, cor = TRUE); cor.option

cbind(Z[, 1], R.S$scores[, 1], abs(Z[, 1]) - abs(R.S$scores[, 1]))
# These results agree as we should expect

cbind(Z[, 1], cor.option$scores[, 1], abs(Z[, 1]) - abs(cor.option$scores[, 1]))
# These results do not agree as closely as we would like

# Note that there is something going on here - what?
# The cov(S) and the cor(X) are the same matrix, but we are not getting the 
#   same result
cbind(R.S$scores[, 1], cor.option$scores[, 1], abs(R.S$scores[, 1]) 
      - abs(cor.option$scores[, 1]))
cbind(R.S$scores[, 2], cor.option$scores[, 2], abs(R.S$scores[, 2]) 
      - abs(cor.option$scores[, 2]))
cbind(R.S$scores[, 3], cor.option$scores[, 3], abs(R.S$scores[, 3]) 
      - abs(cor.option$scores[, 3]))
cbind(R.S$scores[, 4], cor.option$scores[, 4], abs(R.S$scores[, 4]) 
      - abs(cor.option$scores[, 4]))
cbind(R.S$scores[, 5], cor.option$scores[, 5], abs(R.S$scores[, 5]) 
      - abs(cor.option$scores[, 5]))

cbind(Z[, 1], R.S$scores[, 1], abs(Z[, 1]) - abs(R.S$scores[, 1]))
cbind(Z[, 2], R.S$scores[, 2], abs(Z[, 2]) - abs(R.S$scores[, 2]))
cbind(Z[, 3], R.S$scores[, 3], abs(Z[, 3]) - abs(R.S$scores[, 3]))
cbind(Z[, 4], R.S$scores[, 4], abs(Z[, 4]) - abs(R.S$scores[, 4]))
cbind(Z[, 5], R.S$scores[, 5], abs(Z[, 5]) - abs(R.S$scores[, 5]))
cbind(Z[, 6], R.S$scores[, 6], abs(Z[, 6]) - abs(R.S$scores[, 6]))
