library(ggplot2)
head(mpg)
mpg$transmisssion <- rep("manual", nrow(mpg))
mpg$transmission[grep("auto",mpg$trans)] <- "automatic"
#par(mfrow = c(1,2), mar = c(4,4,1,1))
#plot(mpg$displ,mpg$hwy,xlab="displacement",ylab="highway mpg",
#     pch=19,col=(mpg$transmission=="automatic")+1)
#plot(mpg$displ,mpg$hwy,xlab="displacement",ylab="highway mpg",type="n") # empty plot
#rect(0,0,8,50,col="lightgrey") # add a grey rectangle over plot area
#abline(v=2:7,h=seq(10,45,by=5),col="white") # add grid lines (see also ?grid)
#points(mpg$displ,mpg$hwy,col=(mpg$transmission=="automatic")+1,pch=19,cex=.5) # data

# ggplot2 version
a <- ggplot(mpg,aes(x=displ,y=hwy)) + geom_point(aes(colour=transmission)) +
  geom_smooth(method="gam",formula=y~s(x)) ## add a smooth as it’s easy!
a <- a + coord_cartesian(xlim=c(0,8),ylim=c(0,50))
a <- a + scale_x_continuous(limits=c(1,7),name="displacement (litres)")
a ## ’print’ plot object ’a’ - that is plot it!
#ggsave("mpg-gg.eps", width = 5, height = 3)