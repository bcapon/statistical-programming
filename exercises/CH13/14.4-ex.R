## Section 14.4

nll <- function(theta,t,y) {
  ## -ve log likelihood for AIDS model y_i ~ Poi(alpha*exp(beta*t_i))
  ## theta = (alpha,beta)  
  mu <- theta[1] * exp(theta[2] * t) ## mu = E(y)
  -sum(dpois(y,mu,log=TRUE)) ## the negative log likelihood
} ## nll

gll <- function(theta,t,y) {
  ## grad of -ve log lik of Poisson AIDS early epidemic model
  alpha <- theta[1];beta <- theta[2] ## enhances readability
  ebt <- exp(beta*t) ## avoid computing twice
  -c(sum(y)/alpha - sum(ebt),     ## -dl/dalpha
     sum(y*t) - alpha*sum(t*ebt)) ## -dl/dbeta
} ## gll

hll <- function(theta,t,y) {
  ## Hessian of -ve log lik of Poisson AIDS early epidemic model
  alpha <- theta[1];beta <- theta[2] ## enhances readability
  ebt <- exp(beta*t) ## avoid computing twice
  H <- matrix(0,2,2) ## matrix for Hessian of -ve ll
  H[1,1] <- sum(y)/alpha^2
  H[2,2] <-  alpha*sum(t^2*ebt)
  H[1,2] <- H[2,1] <- sum(t*ebt)
  H
} ## hll


nll2 <- function(theta,t,y) {
  ## wrapper function for nll and its grad and Hessian,
  ## suitable for optimization by nlm 
  z <- nll(theta,t,y) ## the objective
  attr(z,"gradient") <- gll(theta,t,y)
  attr(z,"hessian") <- hll(theta,t,y)
  z
} ## nll2

t80 <- 1:13 ## years since 1980
y <- c(12,14,33,50,67,74,123,141,165,204,253,246,240) ## AIDS cases

th0 <- c(1,1)

fit <- nlm(nll2,th0,y=y,t=t80)
fit$estimate
plot(t80+1980,y,xlab="year",ylab="AIDS cases")
mu <- fit$estimate[1]*exp(fit$estimate[2]*t80)
attributes(fit)
lines(t80+1980,mu)

nll2 <- function(theta,t,y) {
  alpha <- theta[1];beta <- theta[2] ## enhances readability
  ebt <- exp(beta*t) ## avoid computing twice
  sum.y <- sum(y)
  sum.tebt <- sum(t*ebt)
  
  mu <- alpha * ebt
  z <- -sum(dpois(y,mu,log=TRUE)) ## the negative log likelihood

  g <- -c(sum.y/alpha - sum(ebt), sum(y*t) - alpha*sum.tebt) ## -dl/dbeta
  
  H <- matrix(0,2,2) ## matrix for Hessian of -ve ll
  H[1,1] <- sum.y/alpha^2
  H[2,2] <-  alpha*sum(t^2*ebt)
  H[1,2] <- H[2,1] <- sum.tebt
  
  attr(z,"gradient") <- g
  attr(z,"hessian") <- H
  z
} ## nll2

fit <- nlm(nll3,th0,y=y,t=t80)
fit$estimate

nll3 <- function(theta,t,y) {
  ## -ve log likelihood for AIDS model y_i ~ Poi(alpha*exp(beta*t_i))
  ## theta = (log(alpha),beta)
  alpha <- exp(theta[1]) ## so theta[1] unconstrained, but alpha > 0
  beta <- theta[2]
  mu <- alpha * exp(beta * t) ## mu = E(y)
  -sum(dpois(y,mu,log=TRUE)) ## the negative log likelihood
}

fit <- nlm(nll3,th0,y=y,t=t80, hessian = TRUE)
attributes(fit)
fit$hessian
