rmvn <- function(n,m,S) {
  ## generate n multivariate normal vectors from MVN(m,S).
  ## Let p=length(m). Start with chol decomp R^TR=S, and
  ## p vector of N(0,1) random variables Z. R^TZ have cov
  ## matrix S, by preceding result. So R^TZ + m gives
  ## desired random vector.
  R <- chol(S)  ## Cholesky factor of cov matrix M
  p <- length(m)
  if (p!=nrow(S)&&p!=1) stop("m and S dimensions do not match")
  Z <- matrix(rnorm(n*p),p,n) ## n standard normal p-vectors
  t(R)%*%Z+m  ## n MVN(m,S) vectors
} ## rmvn

## check it is working as intended
n <- 3
S <- crossprod(matrix(runif(n*n),n,n))
m <- runif(n)
x <- rmvn(10000,m,S) 
rowMeans(x);m ## check mean 
cov(t(x));S   ## check covariance