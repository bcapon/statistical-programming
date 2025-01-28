data("PlantGrowth")
linear_model <- lm(PlantGrowth$weight ~ PlantGrowth$group)
summary(linear_model)
#plot(linear_model)


PlantGrowth1 <- PlantGrowth
PlantGrowth1$group <- factor(PlantGrowth1$group,levels=c("trt1","trt2","ctrl"))
summary(lm(PlantGrowth1$weight~PlantGrowth1$group))
##... yes