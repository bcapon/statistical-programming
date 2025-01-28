x <- c("10","2","7","89","43","1") ## example vector
ii <- which(nchar(x)>1) ## which elements of x are double digit?
ii
xs <- rep("",length(ii)+length(x)) ## vector to store the single digits
xs
iis <- ii+1:length(ii) ## where should second digit go in xs?
xs[iis] <- substr(x[ii],2,2) ## insert 2nd digits
xs[-iis] <- substr(x,1,1) ## insert 1st digits
xs