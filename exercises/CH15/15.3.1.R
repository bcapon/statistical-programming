library("ggplot2")
d <- c(3362,428,290,369,978,1723,2471,3731,5607,7926,12934,20688,29905,40456,53579,
       84784,106448,136269,150325,178684) ## all deaths
cd <- c(3,1,3,10,26,65,133,257,438,740,1418,2587,4250,6337,8528,13388,18622,25220,
        27718,30247) ## Covid-19 deaths
names(cd) <- names(d) <- c("0","1-4","5-9","10-14","15-19","20-24","25-29","30-34",
                           "35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79",
                           "80-84","85-89","90+") ## ONS age classe
par(mfrow=c(1,2))
barplot(d,las=2,main="All deaths Mar 20-Aug 21")
barplot(cd,ylim=c(0,max(d)),las=2,main="Covid deaths Mar 20-Aug 21")

par(mfrow=c(1,2))
D <- rbind(cd,d-cd) ## create matrix of counts to plot
rownames(D) <- c("covid","not covid")
barplot(D,las=2,main="All deaths Mar 20-Aug 21",legend.text=TRUE,
        args.legend=list(x=7,y=150000)) ## where to put legend on plot
barplot(D[,1:10],las=2,main="Age 0-44 deaths Mar 20-Aug 21")

n <- length(d)
age <- factor(1:n);levels(age) <- names(d) ## age class as a factor
ggdat <- data.frame(age=c(age,age),deaths=c(cd,d-cd),
                    type=c(rep("covid",n),rep("not covid",n))) ## tidy data frame
ggplot(ggdat,aes(x=age,y=deaths,fill=type)) + geom_col() +
  labs(title="All UK deaths Mar 2020 - Aug 2021")