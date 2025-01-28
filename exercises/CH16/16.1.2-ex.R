n <- 100; x <- runif(n); mu <- .2+x/2+2*x^2;
n.reps <- 1; bt <- c(.2,.5,2)
cp0 <- cp <- rep(0,3)
for (r in 1:n.reps) { 
  y <- rpois(n,mu)
  dat <- data.frame(x=x,y=y)
  b <- lm(y~x+I(x^2),data=dat)
  bc <- summary(b)$coefficients
  print(bc)
  ci0 <- rbind(bc[,1]-1.96*bc[,2],bc[,1]+1.96*bc[,2])
  cp0 <- cp0 + as.numeric(ci0[1,]<= bt&ci0[2,]>=bt)
  nb <- 1000; bs <- matrix(0,3,nb)
  for (i in 1:nb) {
    bs[,i] <- coef(lm(y~x+I(x^2),data=dat[sample(1:n,n,replace=TRUE),]))
  }
  ci <- apply(bs,1,quantile,probs=c(.025,.975))
  cp <- cp + as.numeric(ci[1,]<= bt&ci[2,]>=bt)
}
cp/n.reps;cp0/n.reps