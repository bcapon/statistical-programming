set.seed(0);y <-rt(100,df=4)
hist(y)

edited_mean <- function(y, k = 2){
  mu <- mean(y)
  sigma <- sd(y)
  remove_indices <- which(abs(y - mu) < k * sigma)
  new_y <- y[-remove_indices]
  length(new_y)
  mean(new_y)
}
edited_mean(y)