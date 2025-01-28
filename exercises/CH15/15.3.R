n <- nrow(faithful);set.seed(8)
erupt <- faithful$eruption + rnorm(n*1000,sd=.2) ## simulate much bigger sample
hist(erupt,breaks=100,freq=FALSE) ## plot i
plot(density(faithful$eruptions,adjust=0.6),type="l",xlab="eruption time (mins)",
     main="kernel density estimate")

# ggplot2 version
ggplot(faithful,aes(x=eruptions,after_stat(density))) + ## data, base mappings etc
  geom_histogram(bins=20,col="green",fill="purple") + ## garish histogram
  geom_density(adjust=.5,col="red") ## added kernel density estimate