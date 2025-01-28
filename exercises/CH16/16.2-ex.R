## 16.2 Belgian AIDS data
y <- c(12,14,33,50,67,74,123,141,165,204,253,246,240)
t <- -6:6 ## year zero is 1987

## MH sampler for model Y ~ Poi(N_0 exp(r t))

ns <- 10 ## samples
th <- matrix(0,3,ns)
thp <- th[,1] <- c(48,.23,0) ## guess initial by data inspection
ll <- sum(dpois(y,thp[1]*exp(thp[2]*t+thp[3]*t^2),log=TRUE))
rsd <- c(.01,.005)
u <- runif(ns); accept <- 0
for (i in 2:ns) {
  thp <- th[,i-1] + c(sample(-4:4,1),rnorm(2)*rsd)
  llp <- sum(dpois(y,thp[1]*exp(thp[2]*t+thp[3]*t^2),log=TRUE))
  MHR <- exp(llp-ll)
  print(MHR)
  if (u[i]<exp(llp-ll)*(thp[1]>0)) {
    accept <- accept + 1
    th[,i] <- thp
    ll <- llp
  } else th[,i] <- th[,i-1]
}
accept/ns