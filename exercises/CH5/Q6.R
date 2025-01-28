f <- function(x){
  return(log(x) - (1/x))
}

f.zeroed <- function(x0,x1){
  x_t <- 1 # Initialisation
  while(abs(f(x_t)) > 10**-10){
    x_t <- (x0 + x1) / 2 # Could also use mean here
    print(f(x_t))
    if(f(x_t) > 0){
      x1 <- x_t
    }else{
      x0 <- x_t
    }
  }
}
f.zeroed(.1,10)