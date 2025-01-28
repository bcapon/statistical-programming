nll <- function(X,y,beta){
  eta <- X %*% beta
  mu <- exp(eta)
  return(-sum(dpois(y, mu, log = TRUE)))
}