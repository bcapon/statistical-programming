ad <- function(x,diff = c(1,1)) { 
  ## create class "ad" object. diff[1] is number of grads
  ## diff[2] is element to set to 1.
  grad <- rep(0,diff[1]) 
  if (diff[2]>0 && diff[2]<=diff[1]) grad[diff[2]] <- 1
  attr(x,"grad") <- grad  
  class(x) <- "ad"
  x
}

sin.ad <- function(a) {
  grad.a <- attr(a,"grad")
  a <- as.numeric(a) ## avoid infinite recursion!
  d <- sin(a)
  attr(d,"grad") <- cos(a) * grad.a
  class(d) <- "ad"
  d
}

"*.ad" <- function(a,b) { ## ad multiplication
  grad.a <- attr(a,"grad")
  grad.b <- attr(b,"grad")
  a <- as.numeric(a)
  b <- as.numeric(b)
  d <- a*b
  attr(d,"grad") <- a * grad.b + b * grad.a ## chain rule
  class(d) <- "ad"
  d
}

"/.ad" <- function(a,b) {
  grad.a <- attr(a,"grad")
  grad.b <- attr(b,"grad")
  a <- as.numeric(a)
  b <- as.numeric(b)
  d <- a/b
  attr(d,"grad") <- grad.a/b - a * grad.b / b^2
  class(d) <- "ad"
  d
}

"+.ad" <- function(a,b) {
  grad.a <- attr(a,"grad")
  grad.b <- attr(b,"grad")
  a <- as.numeric(a)
  b <- as.numeric(b)
  d <- a + b
  attr(d,"grad")  <- grad.a + grad.b 
  class(d) <- "ad"
  d
}

exp.ad <- function(a) {
  grad.a <- attr(a,"grad")
  a <- as.numeric(a)
  d <- exp(a)
  attr(d,"grad") <- d * grad.a
  class(d) <- "ad"
  d
}
## regular...
x1 <- 1; x2 <- 2; x3 <- pi/2
(x1*x2*sin(x3)+ exp(x1*x2))/x3

## AD...
x1 <- ad(1,c(3,1));x2 <- ad(2,c(3,2))
x3 <- ad(pi/2,c(3,3))
a <- (x1*x2*sin(x3)+ exp(x1*x2))/x3; a