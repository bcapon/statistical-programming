nlli <- deriv( ## what to differentiate...
  expression(-y*(log(alpha)+beta*t)+alpha*exp(beta*t)+lgamma(y+1)),
  c("alpha","beta"), ## differentiate w.r.t. these
  function.arg=c("alpha","beta","t","y"),## return function - with these args
  hessian=TRUE) ## compute the Hessian as well as the gradient
t80 <- 1:13 ## years since 1980
y <- c(12,14,33,50,67,74,123,141,165,204,253,246,240) ## AIDS cases
nlli(1,1,t80,y)