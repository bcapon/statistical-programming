rb <- function(th,k=2) { ## Rosenbrock function
  v <- k*(th[2]-th[1]^2)^2 + (1-th[1])^2
  g <- c(-2*(1-th[1])-k*4*th[1]*(th[2]-th[1]^2),k*2*(th[2]-th[1]^2))
  h <- matrix(0,2,2)
  h[1,1] <- 2-k*2*(2*(th[2]-th[1]^2) - 4*th[1]^2)
  h[2,2] <- 2*k;h[1,2] <- h[2,1] <- -4*k*th[1]
  list(v=v,g=g,H=h)
}

th <- c(-1,2)
for(i in 1:100){
  r <- rb(th)
  ev <- eigen(r$H)
  ev$values <- abs(ev$values)
  lambda <- ev$values
  U <- ev$vectors
  del <- -drop(U %*% (crossprod(U, r$g) / lambda))
  for (j in 1:30) {
    r0 <- rb(th+del)
    if (r0$v<=r$v) {
      th <- th + del
      r <- r0
      break
    } else del <- del/2
  }
  if (r0$v>r$v) {
    cat("Step failure"); break
  }
  if (max(abs(r$g))<1e-8) break
}
th; r$g