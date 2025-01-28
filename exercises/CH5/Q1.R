# DIDNT GET THIS ONE AT FIRST AS WASNT FAMILIAR WITH * OPERATOR BETWEEN VECTORS AND MATRICES

set.seed(5); n <- 1000
w <- runif(n)
A <- matrix(runif(n*n),n,n)

system.time(B_1 <- diag(w) %*% A)
system.time(B_2 <- w * A)
range(B_2 - B_1)
