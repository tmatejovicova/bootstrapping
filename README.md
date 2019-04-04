# Performance of Bootstrap for Obtaining 95% Confidence Intervals on the Mean
Author: Tatiana Matejovicova  
University of St Andrews

## Abstract
In this study performance of different bootstrapping methods when extracting the 95% confidence interval is evaluated. Two versions of methods from each of pivotal and percentile method families were considered for symmetric and non-symmetric distributions. Minimum sample size for percentile method to perform reasonably well was determined. We show the difference between the performance of the standard versions of both methods for different sample sizes and how this is improved when using the advanced versions. Finally the accuracy of the more advanced methods from the two families are compared and we conclude that the pivotal method performs better especially for small sample sizes.

See report.pdf or for more information, analysis and conclusions.

## Code
Code is organised in the following files.
- SimulationStudy.R - Running all the experiments
- PercentileMethods.R - Helper functions for percentile methods
- PivotalMethods.R - Helper functions for pivotal methods
- EvaluateBootstrapCI.R - Methods to evaluate the bootstrapping result

## Note
See report.pdf for more information, analysis and conclusions.
