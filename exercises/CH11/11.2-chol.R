n <- 2000
A <- crossprod(matrix(runif(n*n),n,n))
b <- runif(n)
system.time(c0 <- solve(A,b))
system.time({R <- chol(A); c1 <- backsolve(R, forwardsolve(t(R),b))})
range(c0-c1)