## Boostrap CI Methods from percentile family ##

source("BCaHelperFunctions.r")

GetPercentileBootstrapCI <- function(data, num.resamples = 999, alpha = 0.05) {
  # Calculate bootstrap confidence interval using the percentile method
  # Args: data - data sample to be used for resampling
  #       num.resamples - number of bootstrap resamples to perform, constant at 999
  #       alpha - the size of the confidence interval to be calculated is 1 - alpha
  #               given as a decimal number, constant at 0.05
  # Returns:ci - the calculated confidence interval

  sample.size <- length(data)
  resamples <- matrix(NA, nrow = sample.size, ncol = num.resamples + 1)
  resamples[1:sample.size, 1:num.resamples] <- replicate(num.resamples, sample(data, sample.size, replace = TRUE))
  resamples[, num.resamples + 1] <- data
  means <- apply(resamples, 2, mean)
  
  ci <- as.numeric(quantile(means, probs = c(alpha/2, 1-alpha/2)))
  return(ci)
}

GetBCaBootstrapCI <- function(data, num.resamples = 999, alpha = 0.05) {
  # Calculate bootstrap confidence interval using the BCa method
  # Args: data - data sample to be used for resampling
  #       num.resamples - number of bootstrap resamples to perform, constant at 999
  #       alpha - the size of the confidence interval to be calculated is 1 - alpha
  #               given as a decimal number, constant at 0.05
  # Returns:ci - the calculated confidence interval
  
  sample.size <- length(data)
  resamples <- replicate(num.resamples, sample(data, sample.size, replace = TRUE))
  means <- apply(resamples, 2, mean)
  sample.mean <- mean(data)
  
  zhat0 <- get.zhat0(sample.mean, means)
  ahat <- get.ahat(data)
  
  alpha.half <- alpha/2
  alpha1 <- pnorm(zhat0 + (zhat0 + qnorm(alpha.half))/(1 - ahat * (zhat0 + qnorm(alpha.half))))
  alpha2 <- pnorm(zhat0 + (zhat0 + qnorm(1 - alpha.half))/(1 - ahat * (zhat0 + qnorm(1 - alpha.half))))
  
  means <- c(means, sample.mean)
  ci <- quantile(means, probs = c(alpha1, alpha2))
  return(ci)  
}