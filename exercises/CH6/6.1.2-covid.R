# Gives approx distribution for covid time from infection to onset
n <- 10000
d <- rlnorm(n, 1.63, .5) + rgamma(n, shape = 4, scale = 4)
hist(d, breaks = 0:100)