## Boostrap CI Methods from pivotal family ##

GetNonStudPivotalBootstrapCI <- function(data, num.resamples = 999, alpha = 0.05) {
  # Calculate bootstrap confidence interval using the non-studentized pivotal method
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
  sample.mean <- mean(data)
  
  w.star <- means - sample.mean
  w.ci <- quantile(w.star, probs = c(alpha/2, 1 - alpha/2))
  ci <- c(sample.mean - w.ci[2], sample.mean - w.ci[1])
  return(ci)
}

GetStudPivotalBootstrapCI <- function(data, num.resamples = 999, alpha = 0.05) {
  # Calculate bootstrap confidence interval using the studentized pivotal method
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
  
  GetStandardErrorOfMean <- function(data) {
    return(sqrt(var(data)/length(data)))
  }
  
  st.errors <- apply(resamples, 2, GetStandardErrorOfMean)
  sample.mean <- mean(data)
  sample.st.error <- GetStandardErrorOfMean(data)
  
  t.star <- (means - sample.mean)/st.errors
  t.ci <- quantile(t.star, probs = c(alpha/2, 1 - alpha/2))
  ci <- c(sample.mean - sample.st.error * t.ci[2], sample.mean - sample.st.error * t.ci[1])
  
  return(ci)
}