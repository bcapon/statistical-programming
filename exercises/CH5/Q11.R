# Square matrix with increasing rows and cols
set.seed(4);M<-list();for (i in 1:10) M[[i]] <- matrix(runif((i+1)^2)-.5,i+1,i+1)
#M

# Q1
names(M) <- paste("M", 2:11, sep = "")
M

# Q2, Could have used sapply here as it returns a vector as default
typeof(unlist(lapply(M, function(x) norm(x, type = "F")))) # Unlist to obtain vector

# Q3
lapply(M, svd)

# Q4
A <- M[[10]] # or M[["M11"]]
alpha <- function(A){
  return(prod(apply(abs(A), 2, max)))
}
alpha(A)

# Q5
sapply(M, alpha)