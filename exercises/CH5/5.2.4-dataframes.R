# Data frames are matrices with column names and different types
dat <- data.frame(y = c(.3,.7,1.2),x = 1:3,fac = factor(c("a","b","a")))
dim(dat) ## like a matrix
dat$fac ## and like a list!
dat