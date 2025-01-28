set.seed(0);n<-100; A <-matrix(runif(n*n),n,n)
R <- qr.R(qr(A))
det(R)
prod(abs(diag(R)))
determinant(A,log=FALSE)

# Now multiply by a 1000:
A <-A*1000
R <- qr.R(qr(A))
prod(abs(diag(R))) ## overflow
determinant(A,log=FALSE) ## overflow

sum(log(abs(diag(R)))) ## fine
determinant(A) ## fine