ecdf <- function(x,h=1) {
  ## function to plot empirical cdf of sample x
  noise <- rnorm(100*length(x), 0, 1)
  x <- noise + x
  print(x)
  
  n <- length(x)
  p <- 1:n/n ## create cumulative probability vector
  xs <- sort(x) ## sort the x values
  ## plot the x, p points
  p1 <- x1 <- rep(NA,3*n+2) ## create p1 and x1 with NAs everywhere
  p1[(1:n)*3+2] <- p1[(1:n)*3+1] <- p ## fill in the step heights(5,8,11,...)
  x1[1:n*3+1] <- xs ## step start
  x1[1:n*3+2] <- c(xs[-1],2*xs[n]-xs[1]) ## step ends
  p1[1:2] <- 0 ## initial prob is zero
  x1[1:2] <- c(2*xs[1]-xs[n],xs[1]) ## x co-ords for zero prob
  
  #print(x1)
  #print(p1)
  plot(xs,p,pch=".",cex=.5,xlab="x",ylab=expression(P(X<=x)))
  lines(x1,p1)
} ## ecdf

## test it
set.seed(8);
x1 <- rgamma(50,shape=3,scale=2) ## test data
ecdf(x1)
x0 <- seq(0,25,length=200)
lines(x0,pgamma(x0,shape=3,scale=2))
