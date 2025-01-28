# Solution 1
p <-1000
A <- matrix(1,n,n)
A[lower.tri(A)] <- 0
A

# Solution 2
p <- 1000
A <- matrix(1,n,n)
col <- col(A); row <- row(A)
(A[row > col] <- 0)
A

# Given Solution
system.time(matrix(rep(rep(1:0,p),rep(c(0,p),p) + rep(1:p,each=2)*rep(c(1,-1),p)),p,p))

# Q9
B <- matrix(runif(n*n),n,n)
B * A # Multiply this way to get upper triangular matrix
B
