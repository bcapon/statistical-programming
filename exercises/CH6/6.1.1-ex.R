data(morley)
morley$Speed
n <- length(morley$Speed)
bs <- matrix(sample(morley$Speed, 1000 * n, replace = TRUE), n, 1000)
mean <- colMeans(bs)
quantile(mean, c(0.025, 0.975)) # bs percentile confidence interval
