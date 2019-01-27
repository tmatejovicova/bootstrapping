## MT4113 Assignment 4 ##
## Coding style follows Google's R Style Guide https://google.github.io/styleguide/Rguide.xml ##
## I confirm that the attached is my own work, except where clearly indicated in the text. ##

## A script to perform the simulation study ##

source("EvaluateBootstrapCI.R")

# Ensure results are the same everytime the study is run
set.seed(798654327)

# Empirical distributions to sample from
NORMAL.DATA.SOURCE <- rnorm(n = 1000, mean = 0, sd = 1)
EXP.DATA.SOURCE <- rexp(n = 1000, rate = 1)

# Stable variable constants to be used in all simulations
NUM.RESAMPLES <- 999 # The 1000th resample is the data itself
NUM.SIM <- 1000
ALPHA <- 0.05

## Simulation 1: Sample size and Percentile method performance ##
## Larger sample sizes ##
sample.sizes <- seq(5, 100, 5)
incl.sym <- numeric(20)
incl.non.sym <- numeric(20)
for (i in 1:20) {
  # Symmetric distribution
  input <- GenerateEmpiricalInputData(num.sim = NUM.SIM, data.source = NORMAL.DATA.SOURCE, sample.size = sample.sizes[i])
  incl.sym[i] <- EvaluateBootstrapCI(input$data, input$mean, GET.CI.FUNC = GetPercentileBootstrapCI, num.resamples = NUM.RESAMPLES, alpha = ALPHA)
  # Non-symmetric distribution
  input <- GenerateEmpiricalInputData(num.sim = NUM.SIM, data.source = EXP.DATA.SOURCE, sample.size = sample.sizes[i])
  incl.non.sym[i] <- EvaluateBootstrapCI(input$data, input$mean, GET.CI.FUNC = GetPercentileBootstrapCI, num.resamples = NUM.RESAMPLES, alpha = ALPHA)
}
sim1.1 <- data.frame(sample.sizes, incl.sym, incl.non.sym)
write.csv(sim1.1, file = "output/sim1.1")
# Figure 1 - Percentile method for larger sample sizes
with(sim1.1, plot(sample.sizes, incl.sym, xlim = c(5,100), xaxt = "n", ylim = c(84, 100), yaxt = "n", main = "Percentile method for larger sample sizes", xlab = "Sample size", ylab = "Inclusion percentage [%]", col = "blue", pch = 16))
with(sim1.1, points(sample.sizes, incl.non.sym, col = "red", pch = 16))
axis(1, at = seq(5, 100, 5))
axis(2, at = seq(84, 100, 1))
abline(h = 95)
abline(h = 97, lty = 2)
abline(h = 93, lty = 2)
legend("bottomright", legend = c("Symmetric", "Non-symmetric"), col = c("blue", "red"), pch = c(16, 16))
dev.copy(png, "plots/fig1.png")
dev.off()

## Smaller sample sizes ##
sample.sizes <- seq(4, 42, 2)
incl.sym <- numeric(20)
incl.non.sym <- numeric(20)
for (i in 1:20) {
  # Symmetric distribution
  input <- GenerateEmpiricalInputData(num.sim = NUM.SIM, data.source = NORMAL.DATA.SOURCE, sample.size = sample.sizes[i])
  incl.sym[i] <- EvaluateBootstrapCI(input$data, input$mean, GET.CI.FUNC = GetPercentileBootstrapCI, num.resamples = NUM.RESAMPLES, alpha = ALPHA)
  # Non-symmetric distribution
  input <- GenerateEmpiricalInputData(num.sim = NUM.SIM, data.source = EXP.DATA.SOURCE, sample.size = sample.sizes[i])
  incl.non.sym[i] <- EvaluateBootstrapCI(input$data, input$mean, GET.CI.FUNC = GetPercentileBootstrapCI, num.resamples = NUM.RESAMPLES, alpha = ALPHA)
}
sim1.2 <- data.frame(sample.sizes, incl.sym, incl.non.sym)
write.csv(sim1.2, file = "output/sim1.2")
# Figure 2 - Percentile method for smaller sample sizes
with(sim1.2, plot(sample.sizes, incl.sym, xlim = c(4,42), xaxt = "n", ylim = c(70, 100), yaxt = "n", main = "Percentile method for smaller sample sizes", xlab = "Sample size", ylab = "Inclusion percentage [%]", col = "blue", pch = 16))
with(sim1.2, points(sample.sizes, incl.non.sym, col = "red", pch = 16))
axis(1, at = seq(4, 42, 2))
axis(2, at = seq(45, 100, 5))
abline(h = 95)
abline(h = 100, lty = 2)
abline(h = 90, lty = 2)
legend("bottomright", legend = c("Symmetric", "Non-symmetric"), col = c("blue", "red"), pch = c(16, 16))
dev.copy(png, "plots/fig2.png")
dev.off()
# Figure 3 - T-test for difference in percentile method accuracy with symmetric and non-symmetric distribution
with(sim1.2, t.test(incl.sym, incl.non.sym, paired = TRUE))
# Figure 4 - Symmetric and non-symmetric distribution for percentile method
with(sim1.2, plot(sample.sizes, incl.sym - incl.non.sym, main = "Symmetric and non-symmetric distribution for percentile method", xlab = "Sample size", ylab = "Inclusion percentage symmetric - non-symmetric [%]"))
dev.copy(png, "plots/fig4.png")
dev.off()

## Simulation 2: Comparison of Percentile Method and BCa Method
sample.sizes <- seq(4,42,2)
incl.sym.perc <- numeric(20)
incl.sym.BCa <- numeric(20)
incl.non.sym.perc <- numeric(20)
incl.non.sym.BCa <- numeric(20)
for (i in 1:20) {
  # Symmetric distribution
  input <- GenerateEmpiricalInputData(num.sim = NUM.SIM, data.source = NORMAL.DATA.SOURCE, sample.size = sample.sizes[i])
  incl.sym.perc[i] <- EvaluateBootstrapCI(input$data, input$mean, GET.CI.FUNC = GetPercentileBootstrapCI, num.resamples = NUM.RESAMPLES, alpha = ALPHA)
  incl.sym.BCa[i] <- EvaluateBootstrapCI(input$data, input$mean, GET.CI.FUNC = GetBCaBootstrapCI, num.resamples = NUM.RESAMPLES, alpha = ALPHA)
  # Non-symmetric distribution
  input <- GenerateEmpiricalInputData(num.sim = NUM.SIM, data.source = EXP.DATA.SOURCE, sample.size = sample.sizes[i])
  incl.non.sym.perc[i] <- EvaluateBootstrapCI(input$data, input$mean, GET.CI.FUNC = GetPercentileBootstrapCI, num.resamples = NUM.RESAMPLES, alpha = ALPHA)
  incl.non.sym.BCa[i] <- EvaluateBootstrapCI(input$data, input$mean, GET.CI.FUNC = GetBCaBootstrapCI, num.resamples = NUM.RESAMPLES, alpha = ALPHA)
}
sim2 <- data.frame(sample.sizes, incl.sym.perc, incl.sym.BCa, incl.non.sym.perc, incl.non.sym.BCa)
sim2["sym.diff"]<-sim2$incl.sym.BCa - sim2$incl.sym.perc
sim2["non.sym.diff"]<-sim2$incl.non.sym.BCa - sim2$incl.non.sym.perc
write.csv(sim2, file = "output/sim2")
# Figure 5 - Percentile method and BCa method
with(sim2, plot(sample.sizes, sym.diff, ylim = c(-2, 3), main = "Percentile method and BCa method", xlab = "Sample size", ylab = "Inclusion percentage BCa - percentile [%]", col = "blue", pch = 16))
with(sim2, points(sample.sizes, non.sym.diff, col = "red", pch = 16))
abline(h = 0, lty = 2)
legend("bottomright", legend = c("Symmetric", "Non-symmetric"), col = c("blue", "red"), pch = c(16, 16))
dev.copy(png, "plots/fig5.png")
dev.off()
# Figure 6 - T-test for difference in accuracy between BCa and percentile method for symmetric and non-symmetric distribution 
with(sim2, t.test(non.sym.diff, sym.diff, paired = TRUE))

## Simulation 3: Comparison of non-studentized and studentized pivotal method
sample.sizes <- seq(4,42,2)
incl.sym.non <- numeric(20)
incl.sym.stud <- numeric(20)
incl.non.sym.non <- numeric(20)
incl.non.sym.stud <- numeric(20)
for (i in 1:20) {
  # Symmetric distribution
  input <- GenerateEmpiricalInputData(num.sim = NUM.SIM, data.source = NORMAL.DATA.SOURCE, sample.size = sample.sizes[i])
  incl.sym.non[i] <- EvaluateBootstrapCI(input$data, input$mean, GET.CI.FUNC = GetNonStudPivotalBootstrapCI, num.resamples = NUM.RESAMPLES, alpha = ALPHA)
  incl.sym.stud[i] <- EvaluateBootstrapCI(input$data, input$mean, GET.CI.FUNC = GetStudPivotalBootstrapCI, num.resamples = NUM.RESAMPLES, alpha = ALPHA)
  # Non-symmetric distribution
  input <- GenerateEmpiricalInputData(num.sim = NUM.SIM, data.source = EXP.DATA.SOURCE, sample.size = sample.sizes[i])
  incl.non.sym.non[i] <- EvaluateBootstrapCI(input$data, input$mean, GET.CI.FUNC = GetNonStudPivotalBootstrapCI, num.resamples = NUM.RESAMPLES, alpha = ALPHA)
  incl.non.sym.stud[i] <- EvaluateBootstrapCI(input$data, input$mean, GET.CI.FUNC = GetStudPivotalBootstrapCI, num.resamples = NUM.RESAMPLES, alpha = ALPHA)
}
sim3 <- data.frame(sample.sizes, incl.sym.non, incl.sym.stud, incl.non.sym.non, incl.non.sym.stud)
write.csv(sim3, file = "output/sim3")
# Figure 7 - Pivotal methods with symmetric distribution
with(sim3, plot(sample.sizes, incl.sym.non, ylim = c(70, 100), main = "Pivotal methods with symmetric distribution", xlab = "Sample size", ylab = "Inclusion percentage [%]", col = "blue", pch = 16))
with(sim3, points(sample.sizes, incl.sym.stud, col = "red", pch = 16))
with(sim3, points(sample.sizes, incl.non.sym.non, col = "blue", pch = 1))
with(sim3, points(sample.sizes, incl.non.sym.stud, col = "red", pch = 1))
abline(h = 95, lty = 2)
legend("bottomright", legend = c("non-stud sym", "non-stud non-sym", "stud sym", "stud non-sym"), col = c("blue", "blue", "red", "red"), pch = c(16, 1, 16, 1))
dev.copy(png, "plots/fig7.png")
dev.off()

## Simulation 4: Comparison of studentized pivotal and BCa method
sample.sizes <- seq(4,42,2)
incl.sym.bca <- numeric(20)
incl.sym.stud <- numeric(20)
incl.non.sym.bca <- numeric(20)
incl.non.sym.stud <- numeric(20)
for (i in 1:20) {
  # Symmetric distribution
  input <- GenerateEmpiricalInputData(num.sim = NUM.SIM, data.source = NORMAL.DATA.SOURCE, sample.size = sample.sizes[i])
  incl.sym.bca[i] <- EvaluateBootstrapCI(input$data, input$mean, GET.CI.FUNC = GetBCaBootstrapCI, num.resamples = NUM.RESAMPLES, alpha = ALPHA)
  incl.sym.stud[i] <- EvaluateBootstrapCI(input$data, input$mean, GET.CI.FUNC = GetStudPivotalBootstrapCI, num.resamples = NUM.RESAMPLES, alpha = ALPHA)
  # Non-symmetric distribution
  input <- GenerateEmpiricalInputData(num.sim = NUM.SIM, data.source = EXP.DATA.SOURCE, sample.size = sample.sizes[i])
  incl.non.sym.bca[i] <- EvaluateBootstrapCI(input$data, input$mean, GET.CI.FUNC = GetBCaBootstrapCI, num.resamples = NUM.RESAMPLES, alpha = ALPHA)
  incl.non.sym.stud[i] <- EvaluateBootstrapCI(input$data, input$mean, GET.CI.FUNC = GetStudPivotalBootstrapCI, num.resamples = NUM.RESAMPLES, alpha = ALPHA)
}
sim4 <- data.frame(sample.sizes, incl.sym.bca, incl.sym.stud, incl.non.sym.bca, incl.non.sym.stud)
write.csv(sim4, file = "output/sim4")
# Figure 8 - Bootstrap methods with symmetric distribution
with(sim4, plot(sample.sizes, incl.sym.bca, ylim = c(70, 100), main = "Bootstrap methods with symmetric distribution", xlab = "Sample size", ylab = "Inclusion percentage [%]", col = "blue", pch = 16))
with(sim4, points(sample.sizes, incl.non.sym.bca, col = "blue", pch = 1))
with(sim4, points(sample.sizes, incl.sym.stud, col = "red", pch = 16))
with(sim4, points(sample.sizes, incl.non.sym.stud, col = "red", pch = 1))
abline(h = 95, lty = 2)
legend("bottomright", legend = c("BCa sym", "BCa non-sym", "Stud pivotal sym", "Stud pivotal non-sym"), col = c("blue", "blue", "red", "red"), pch = c(16, 1, 16, 1))
dev.copy(png, "plots/fig8.png")
dev.off()