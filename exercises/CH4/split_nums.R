x <- c(10,2,7,89,43,1) ## an example vector to try out
# x[-1]      #returns all but first indice rather than last in Python
ii <- which(x%/%10 > 0) ## indices of the double digits in x?
ii
xs <- rep(0,length(ii)+length(x)) ## vector to store the single digits
xs
ii + 1
iis <- ii + (1:length(ii)) - 1 ## where should 10s digits go in xs?
iis
xs[iis] <- x[ii]%/%10 ## insert 10s digits
xs[-iis] <- x%%10 ## insert the rest (units)
xs