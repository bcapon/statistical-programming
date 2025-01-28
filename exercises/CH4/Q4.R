new_mean <- function(y, n_sds = 2){
  mu <- mean(y)
  sigma <- sd(y)
  remove_indices <- which(abs(y - mu) > n_sds * sigma )
  y_new <- y[-remove_indices]
  length(y_new)
  print(mean(y_new))
  hist(y_new)
  
}

set.seed(0);y <-rt(100,df=4)
new_mean(y,3)