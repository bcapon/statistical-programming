a <- array(1:24, c(2,3,4))
a
apply(a,1,sum) # 1 indicates rows
apply(a,c(1,3),prod) # Applies to each row of each layer

# tapply can be applied to groups
x <- 1:10
fac1 <- factor(c(1,3,2,3,1,2,3,3,2,1))
tapply(x,fac1,sum)
fac2 <- factor(rep(c("a", "b"), each =5 )) # aaaaabbbbb... not ababababab...
tapply(x, list(fac1,fac2), sum)

# lapply can simplifythe list version
b <- list(a=1:5,b=c(4,5,7),c=-10:10)
unlist(lapply(b, function(x) sort(x)[2])) # returns second smallest entry