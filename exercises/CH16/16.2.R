ns <- 20000 ## samples
theta <- matrix(0,3,ns)
theta_p <- theta[,1] <- c(48,.23,0) ## guess initial by data inspection

ll <- sum(dpois(y,theta_p[1]*exp(theta_p[2]*t+theta_p[3]*t^2),log=TRUE))
rsd <- c(.01,.005)
u <- runif(ns); accept <- 0
for (i in 2:ns) {
  theta_p <- theta[,i-1] + c(sample(-4:4,1),rnorm(2)*rsd)
  llp <- sum(dpois(y,theta_p[1]*exp(theta_p[2]*t+theta_p[3]*t^2),log=TRUE))
  if (u[i]<exp(llp-ll)*(theta_p[1]>0)) {
    accept <- accept + 1
    theta[,i] <- theta_p
    ll <- llp
  } else {
    theta[,i] <- theta[,i-1]
  }
}
accept/ns

plot(theta[1,], type = "l")
