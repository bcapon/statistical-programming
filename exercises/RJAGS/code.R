library("rjags")
y <- c(12,14,33,50,67,74,123,141,165,204,253,246,240)
t <- -6:6 ## year zero is 1987
setwd("C:/Users/BCapo/Documents/GitHub/extended-statistical-programming/RJAGS") ## EDIT: where is JAGS file?
jaid <- jags.model("model.jags",data=list(t=t,y=y))
sam <- coda.samples(jaid,c("theta","mu"),n.iter=20000)
effectiveSize(sam)
sam[[1]]
autocorr.plot(sam)
#plot(sam[,1:4]) ## etc

mum <- as.matrix(sam)[,1:13]
apply(mum,2,function(x) median(x))
