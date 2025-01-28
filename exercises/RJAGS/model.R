data{
  for(i in 1:250){
    p[i] <- 1/250
  }
}

model{
  for(i in 1:13){
    mu[i] <- theta[1]*exp(theta[2]*t + theta[3]*t**2)
    y[i] <- dpois(mu[i])
  }
  theta[1] <- dcat(p)
  theta[2] <- dunif(.01, 1)
  theta[3] <- dunif(-1, 1)
} 