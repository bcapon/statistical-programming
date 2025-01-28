f1 <- function(x) return(log(x) - (1/x))
g1 <- function(x,a,b) return(x**b - exp(a*x) + 2)
f.zeroed <- function(interval, f,...){
  x_t <- 1 # Initialisation
  
  x0 <- interval[1]; x1 <- interval[2]
  f_x0 <- f(x0,...); f_x1 <- f(x1,...)
  cat("f_x0:", f_x0, "f_x1", f_x1)
  if(f_x0 * f_x1 > 0) stop("Interval not different sign")
  up <- sign(f(x1,...))
  while(abs(f(x_t,...)) > 10**-10){
    x_t <- (x0 + x1) / 2 # Could also use mean here
    print(f(x_t,...))
    if(up * f(x_t,...) > 0){
      x1 <- x_t
    }else{
      x0 <- x_t
    }
  }
  return(x_t)
}
f.zeroed(c(0,10), f1)
f.zeroed(c(-100,0), g1, a = 2, b = 3)