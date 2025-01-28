x = list(M = matrix(runif(3*3),3,3), b = list("foo bar", c(5:10), v = c(-3, 5, -1)))

y = list(x['M'], x[[2]]['v'])
y

x = list(x[[1]], x[[2]][-3])
names(x) = c('A','blist')
x