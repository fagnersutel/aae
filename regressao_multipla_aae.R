# Multiple Linear Regression Example 
tarifa <- c(1.5, 1.5, 2, 2, 2)
ocupacao <- c(29.3, 28.5, 14.57, 13.44, 16.8)
monitores <- c(88.0, 75.0, 30.0, 15.0, 7.0)
autuacoes <- c(6.6, 14.7, 9.3, 6.2, 23.6)
mydata <- cbind(monitores, autuacoes, ocupacao, tarifa)
mydata <- as.data.frame(mydata)
options(scipen=999)

cor(tarifa, ocupacao)
cor(monitores, ocupacao)
cor(autuacoes, ocupacao)
cor(tarifa + autuacoes + monitores, ocupacao)
cor(autuacoes + monitores, ocupacao)
cor(tarifa + monitores, ocupacao)
cor(tarifa + autuacoes, ocupacao)
par(mfrow=c(1,3))
fit1t <- lm(ocupacao ~ tarifa, data=mydata)
summary(fit1t) 
plot(ocupacao,tarifa,col = "blue",main = "Relação Tarifa vs % Ocupação na AAE",
     abline(lm(tarifa~ocupacao), col="red"),cex = 1.3,pch = 16,
     xlab = "% Ocupação",ylab = "Tarifa R$")

fit2t <- lm(ocupacao ~ autuacoes, data=mydata)
summary(fit2t) 
plot(ocupacao,autuacoes,col = "blue",main = "Relação Autuações vs % Ocupação na AAE",
     abline(lm(autuacoes~ocupacao), col="red"),cex = 1.3,pch = 16,
     xlab = "% Ocupação",ylab = "Nº Autuações * 1000")

fit3t <- lm(ocupacao ~ monitores, data=mydata)
summary(fit3t)
plot(ocupacao,monitores,col = "blue",main = "Relação Monitores vs % Ocupação na AAE",
     abline(lm(monitores~ocupacao), col="red"),cex = 1.3,pch = 16,
     xlab = "% Ocupação",ylab = "Nº Monitores")


monitoreslog <- log(monitores)
monitoreslog
ocupacaolog <- log(ocupacao)
ocupacaolog
autuacoeslog <- log(autuacoes)
autuacoeslog
tarifalog <- log(tarifa)
tarifalog
par(mfrow=c(1,3))
fitb <- lm(ocupacaolog ~ autuacoeslog)
summary(fitb) # show results
plot(ocupacaolog,autuacoeslog,col = "blue",main = "Relação Autuações vs Oucpação na AAE",
     abline(lm(autuacoeslog~ocupacaolog)),cex = 1.3,pch = 16,
     xlab = "Taxa de Ocupação (Log)",ylab = "Autuações (Log)")
cor(ocupacaolog, autuacoeslog)

fitc <- lm(ocupacao ~ monitores, data=mydata)
summary(fitc) # show results
plot(ocupacaolog,monitoreslog,col = "blue",main = "Relação nº Monitores vs % Ocupação na AAE",
     abline(lm(monitoreslog~ocupacaolog)),cex = 1.3,pch = 16,
     xlab = "Ocupação (Log)",ylab = "Monitores (Log)")



fit2 <- lm(ocupacao ~ monitores + autuacoes + tarifa, data=mydata)
summary(lm(ocupacao ~ monitores + autuacoes + tarifa, data=mydata)) # show results
shapiro.test(rstandard(lm(ocupacao ~ monitores + autuacoes + tarifa, data=mydata)))
anova(fit2)

fit3 <- lm(ocupacao ~ monitores + autuacoes, data=mydata)
summary(fit3) # show results
shapiro.test(rstandard(lm(ocupacao ~ monitores + autuacoes, data=mydata)))
anova(fit3)

fit4 <- lm(ocupacao ~ monitores + tarifa, data=mydata)
summary(fit4) # show results
shapiro.test(rstandard(lm(ocupacao ~ monitores + tarifa, data=mydata)))
anova(fit4)

# Other useful functions 
coefficients(fit2) # model coefficients
confint(fit2, level=0.95) # CIs for model parameters 
fitted(fit2) # predicted values
residuals(fit2) # residuals
anova(fit2) # anova table 
vcov(fit2) # covariance matrix for model parameters 
influence(fit2) # regression diagnostics





# Other useful functions 
coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters 
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table 
vcov(fit) # covariance matrix for model parameters 
influence(fit) # regression diagnostics


# diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit)


h<-hist(autuacoes, col="red", xlab="Numero de Autua??es", main="Histograma") 
xfit<-seq(min(autuacoes),max(autuacoes),length=40)
yfit<-dnorm(xfit,mean=mean(autuacoes),sd=sd(autuacoes))
yfit <- yfit*diff(h$mids[1:2])*length(autuacoes)
lines(xfit, yfit, col="blue", lwd=2)
lines(ocupacao, col="red", lwd=2)


h<-hist(monitores, col="red", xlab="Numero de Monitores", main="Histograma") 
xfit<-seq(min(monitores),max(monitores),length=40)
yfit<-dnorm(xfit,mean=mean(monitores),sd=sd(monitores))
yfit <- yfit*diff(h$mids[1:2])*length(monitores)
lines(xfit, yfit, col="blue", lwd=2)
lines(ocupacao, col="red", lwd=2)


monitoreslog <- log(monitores)
monitoreslog
ocupacaolog <- log(ocupacao)
ocupacaolog
autuacoeslog <- log(as.numeric(autuacoes))
autuacoeslog
tarifalog <- log(as.numeric(tarifa))
tarifalog

fit <- lm(ocupa??olog ~ monitores + autuacoeslog + monitores * autuacoeslog)
summary(fit) # show results

fit <- lm(ocupacao ~ monitores + autuacoes)
summary(fit) # show results


fit <- lm(ocupacao ~ monitores + autuacoes + tarifa)
summary(fit) # show results

fit <- lm(ocupacao ~ monitores +  tarifa)
summary(fit) # show results


fit <- lm(ocupa??olog ~ monitores + autuacoeslog + monitores * autuacoeslog)
summary(fit) # show results

mydata$tarifa <- NULL
head(mydata)
plot(mydata,  pch=16, col="blue", main="Matriz de Dispers?o")

#install.packages('rgl', dependencies = TRUE)
library(rgl)
require(rgl) 
require(car) 
tarifa <- as.numeric(tarifa)
colors <- c("#999999", "#E69F00", "#56B4E9")
scatter3d(ocupacao ~ monitores + autuacoes, radius=tarifa, point.col = tarifa, color=colors, data=mydata, xlab = NULL, zlab = NULL, ylab = NULL, axis.scales = FALSE)
scatter3d(ocupacao ~ monitores + autuacoes, radius=tarifa, data=mydata, xlab = NULL, zlab = NULL, ylab = NULL)
tarifa <- as.factor(tarifa)
scatter3d(ocupacao ~ monitores + autuacoes,  groups = tarifa, data=mydata)


scatter3d(ocupacao ~ monitores + autuacoes  | ocupacao, radius=tarifa, surface=FALSE, data=mydata)

mydata$tarifa <- NULL

par(mfrow=c(1,1))
#install.packages("scatterplot3d") # Install
library("scatterplot3d") # load
colors <- c("#999999", "#E69F00")
colors <- colors[as.numeric(tarifa)]
scatterplot3d(mydata, pch = 16, color=colors)
mydata <- cbind(tarifa, mydata)
head(mydata)

shapes = c(16, 17) 
shapes <- shapes[as.numeric(tarifa)]

colors <- c("red", "green")
colors <- colors[as.numeric(tarifa)]

s3d <- scatterplot3d(mydata, pch = shapes,  type = "h",color=colors, cex.symbols=2,
                     main = "Gráfico de Flutuações")
my.lm  <- with(mydata, lm(ocupacao ~ monitores + autuacoes))
my.lm2 <- lm(ocupacao ~ monitores + tarifa)
s3d$plane3d(my.lm, col='blue')
s3d$plane3d(my.lm2, col='red')






library(car)
ncvTest(lm(ocupacao ~ monitores))
ncvTest(lm(ocupacao ~ tarifa))
ncvTest(lm(ocupacao ~ autuacoes))
ncvTest(lm(ocupacao ~ monitores +  tarifa))
ncvTest(lm(ocupacao ~ monitores +  autuacoes))
ncvTest(lm(ocupacao ~ monitores +  tarifa + autuacoes))
lmtest::bptest(lm(ocupacao ~ monitores +  tarifa + autuacoes))


shapiro.test(rstandard(lm(ocupacao ~  tarifa)))
shapiro.test(rstandard(lm(ocupacao ~ autuacoes)))
shapiro.test(rstandard(lm(ocupacao ~ monitores)))
shapiro.test(rstandard(lm(ocupacao ~ monitores + autuacoes)))
shapiro.test(rstandard(lm(ocupacao ~ monitores + tarifa)))


shapiro.test(rstandard(lm(ocupacao ~ monitores + autuacoes + tarifa)))
shapiro.test(rstandard(lm(ocupacao ~ monitores + autuacoes))) 
shapiro.test(rstandard(lm(ocupacao ~ monitores + tarifa))) 



par(mfrow=c(1,3))
fit1t <- lm(ocupacaolog ~ tarifalog)
summary(fit1t) # show results
plot(ocupacaolog,tarifalog,col = "blue",main = "Relação Tarifa vs % Ocupação na AAE",
     abline(lm(tarifalog~ocupacaolog), col="red"),cex = 1.3,pch = 16,
     xlab = "% Ocupação (Log)",ylab = "Tarifa (Log)")

fit2t <- lm(ocupacaolog ~ autuacoeslog, data=mydata)
summary(fit2t) # show results
plot(ocupacaolog,autuacoeslog,col = "blue",main = "Relação Autuações vs % Ocupação na AAE",
     abline(lm(autuacoeslog~ocupacaolog), col="red"),cex = 1.3,pch = 16,
     xlab = "% Ocupação  (Log)",ylab = "Nº Autuações  (Log)")

fit3t <- lm(ocupacaolog ~ monitoreslog, data=mydata)
summary(fit3t) # show results
plot(ocupacaolog,monitoreslog,col = "blue",main = "Relação Monitores vs % Ocupação na AAE",
     abline(lm(monitoreslog~ocupacaolog), col="red"),cex = 1.3,pch = 16,
     xlab = "% Ocupação (Log)",ylab = "Nº Monitores (Log)")
