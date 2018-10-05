#Dados Monitores x Ocupação

#x <- c(76, 93, 86, 98, 90, 75, 15, 15, 7, 88)
#y <- c(32, 46, 48, 37, 38, 29, 15, 13, 17, 29)


x <- c(98, 90, 88, 75, 15, 15, 7)
y <- c(37.2, 37.8, 29.3, 28.5, 14.5, 13.4, 13.7)
scatter.smooth(x, y, main="Dist ~ Speed")  # scatterplot
cor(x, y)
linearMod <- lm(y ~ x)
print(linearMod)
summary(linearMod)


plot(x,y,col = "blue",main = "Titulo",
     abline(lm(x~y)),cex = 1.3,pch = 16,
     xlab = "Ocupação",ylab = "Nº Monitores")


plot(y,x,col = "blue",main = "Titulo",
     abline(lm(y~x)),cex = 1.3,pch = 16,
     xlab = "Ocupação",ylab = "Nº Monitores")


relation <- lm(y~x)
print(summary(relation))

relation2 <- lm(y~x)
print(summary(relation2))

cor(x, y)

spreadLevelPlot(relation)

# load data
data(longley)
# fit model
fit <- lm(Employed~., longley)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, longley)
# summarize accuracy
mse <- mean((longley$Employed - predictions)^2)
print(mse)




# load data
data(longley)
# fit model
base <- lm(Employed~., longley)
# summarize the fit
summary(base)
# perform step-wise feature selection
fit <- step(base)
# summarize the selected model
summary(fit)
# make predictions
predictions <- predict(fit, longley)
# summarize accuracy
mse <- mean((longley$Employed - predictions)^2)
print(mse)








library(KernSmooth)
fhat <- bkde(x)
plot (fhat, xlab="x", ylab="Density function")

fhat <- bkde(y)
plot (fhat, xlab="y", ylab="Density function")



library(tidyverse)
library(broom)
theme_set(theme_classic())
model.diag.metrics <- augment(relation)
head(model.diag.metrics)

ggplot(model.diag.metrics, aes(x, y)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = x, yend = .fitted), color = "red", size = 0.3)





x <- c(98, 90, 88, 75, 15, 15, 7)
y <- c(37.2, 37.8, 29.3, 28.5, 14.5, 13.4, 13.7)
mod1 <- lm(y ~ x)
plot(x, y, xlim=c(min(x)-5, max(x)+5), ylim=c(min(y)-10, max(y)+10))
abline(mod1, lwd=2)


# calculate residuals and predicted values
res <- signif(residuals(mod1), 5)
pre <- predict(mod1) # plot distances between points and the regression line
segments(x, y, x, pre, col="red")

# add labels (res values) to points
library(calibrate)
textxy(x, y, res, cx=0.7)
