## Methods to generate sample data and evalaute a bootstrap method ##

source("PercentileMethods.R")
source("PivotalMethods.R")

EvaluateBootstrapCI <- function(input.data, mean, GET.CI.FUNC = GetPercentileBootstrapCI, num.resamples = 999, alpha = 0.05) {
  # Calculate the inclusion percentage of the true mean calculated from the given bootstrap method on the given data
  # Args: input.data - matrix of input data in which each column represents one set of input data
  #       mean - the true mean of the underlying distribution
  #       num.resamples - number of bootstrap resamples to perform, constant at 999
  #       alpha - the size of the confidence interval to be calculated is 1 - alpha
  #               given as a decimal number, constant at 0.05
  # Returns: inclusion percentage of the true mean using the given bootstrap method
  num.sim <- dim(input.data)[2]
  ci <- apply(input.data, 2, GET.CI.FUNC, num.resamples = num.resamples, alpha = alpha)
  
  IsInCi <- function(ci, mean) {
    val <- ifelse(mean >= ci[1] && mean <= ci[2], 1, 0)
    return(val)
  }
  
  mean.in.ci <- apply(ci, 2, IsInCi, mean)
  return(round(100 * sum(mean.in.ci)/num.sim, digits = 2))
}

GenerateEmpiricalInputData <- function(num.sim = 1000, data.source, sample.size = 10) {
  # Generate the given number of data samples by resampling with replacement
  # of the given size from the given data source.
  # Args: num.sim - number of data samples to generate, constant at 1000
  #       data.source - the pool of data to resample from that represents the empirical distribution
  #       sample.size - the size of the sample to generate
  # Returns: data - a matrix of size sample.size * num.sim in which each column represents one generated sample
  #          mean - the true mean of the underlying empirical distribution
  mean <- mean(data.source)
  size <- length(data.source)
  
  data <- matrix(NA, sample.size, num.sim)
  data <- replicate(num.sim - 1, sample(data.source, sample.size, replace = TRUE))
  return(list(data = data, mean = mean)) 
}