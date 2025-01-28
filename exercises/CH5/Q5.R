x <- rnorm(20); z <- rnorm(20); y <- rnorm(20)
#for(i in 1:length(x)){ # Slower
 # if(z[i] < 1 | y[i]/z[i] < 0){
  #  x[i] <- x[i]**2
  #}
#}

ii <- which(z < 1 | y/z < 0) 
ii            
x[ii] <- x[ii]**2
#ii <- z!=0 & (z<1 | y/z <0) another way, could just boolean index