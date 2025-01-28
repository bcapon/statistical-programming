mat.fun <- function(A, fun = I,...){
  ea <- eigen(A, symmetric = TRUE)
  print(fun(ea$values))
  return(ea$vectors %*% (fun(ea$values,...) * t(ea$vectors)))
}
n <- 3
B <- crossprod(matrix(runif(n*n),n,n))
#range(mat.fun(B) - B)
mat.fun(B, function(x,a=1,b=1) b/a*x, a = 2)

# ... argument can be used to alter default values in function