n <- 100;p <- rep(NA,n); p[1] <- 0.1
for (i in 2:n) p[i] <- 3.7 * p[i-1] * (1 - p[i-1])
plot(1:n,p,type="l",xlab="i")

# use break to break out of loop
