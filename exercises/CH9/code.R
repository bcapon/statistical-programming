library("ggplot2")
mpg1 <- data.frame(mpg)
mpg1$trans <- factor(gsub("\\(.*\\)", "", mpg1$trans))

head(model.matrix(cty~trans+displ,mpg1))

ml <- lm(cty ~ trans + displ, data = mpg1)
ml
par(mfrow=c(2,2))
plot(ml) # residuals don't appear to be indep with zero mean and const variance

plot(mpg1$displ, residuals(ml)) # appears ast least quadratic

ml2 <- lm(cty~trans+displ+I(displ^2), data = mpg1)
summary(ml2)
# Not much evidence for trans, lets do an anova test for with+without:
ml3 <- lm(cty~displ+I(displ^2), data = mpg1)
anova(ml3,lm(cty~(trans+displ+I(displ^2))^2, data = mpg1))
# => no evidence for trans BUT:
summary(lm(cty~trans, mpg1))
# This implies very strong evidence so what has happened?