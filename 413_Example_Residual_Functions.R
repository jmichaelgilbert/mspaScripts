# Functions for Residual Analysis

# Testing number of residuals +/- 3 standard deviations from mean
res <- function(data, alpha) {
    n <- (length(data)-1)
    std_pos <- (mean(data)+(sd(data)*3))
    std_neg <- (mean(data)-(sd(data)*3))
    count <- round(alpha*(n-1), digits = 0)
    
    ifelse(count < sum(data > std_pos, data < std_neg),
           "Looking good!", 
           "Re-assess viability!")
}

# Testing the skewness and kurtosis of residuals
sk <- function(data, alpha) {
    
    # Skewness
    s <- skewness(data)/sqrt(6/length(data))
    s_pv <- 2*(1-pnorm(abs(s)))
    print(paste("Residuals: p-value of skewness:", 
                round(s_pv[1], digits = 5)))
    print(ifelse(s_pv < alpha, 
                 "Reject H0: skewness = 0 at specified alpha.",
                 "Fail to reject H0: skewness = 0 at specified alpha."))
    
    # Kurtosis
    k <- kurtosis(data)/sqrt(24/length(data))
    k_pv <- 2*(1-pnorm(abs(k)))
    print(paste("Residuals: p-value of excess kurtosis:", 
                round(k_pv[1], digits = 5)))
    print(ifelse(k_pv < alpha, 
                 "Reject H0: excess kurtosis = 0 at specified alpha.",
                 "Fail to reject H0: excess kurtosis = 0 at specified alpha."))
}


