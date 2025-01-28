p = 5
A <- matrix(rep(1, p*p),p,p) # Could also use 1 for first arg as recycling is used.
for(i in 1:p){
  for(j in 1:p){
    if(i > j){
      A[i,j] <- 0
    }
  }
}
A