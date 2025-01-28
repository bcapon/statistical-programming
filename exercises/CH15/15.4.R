library("ggplot2")
par(mfrow=c(1,3),mar=c(5,5,1,1))
boxplot(len~supp,data=ToothGrowth,cex.lab=1.5)
boxplot(len~dose,data=ToothGrowth,cex.lab=1.5)
boxplot(len~dose+supp,data=ToothGrowth,cex.lab=1.5)

ToothGrowth$comb <- with(ToothGrowth,paste(supp,dose))
ggplot(ToothGrowth,aes(y=len,group=comb)) + geom_boxplot()