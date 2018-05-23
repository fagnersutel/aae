# Multiple Linear Regression Example 
tarifa <- c(1.5, 1.5, 2, 2, 2)
ocupacao <- c(29.3, 28.5, 14.57, 13.44, 16.8)
monitores <- c(88, 75, 30, 15, 7)
autuacoes <- c(6.6, 14.7, 9.3, 6.2, 23.6)
mydata <- cbind(monitores, autuacoes, ocupacao, tarifa)
mydata <- as.data.frame(mydata)
options(scipen=999)

cor(tarifa, ocupacao)
cor(monitores, ocupacao)
cor(autuacoes, ocupacao)
cor(tarifa + autuacoes + monitores, ocupacao)
cor(autuacoes + monitores, ocupacao)

fit1t <- lm(ocupacao ~ tarifa, data=mydata)
summary(fit1t) # show results
plot(ocupacao,tarifa,col = "blue",main = "Relação Tarifa vs Oucpação na AAE",
     abline(lm(tarifa~ocupacao)),cex = 1.3,pch = 16,
     xlab = "Taxa de Ocupação %",ylab = "Tarifa")

fita <- lm(ocupacao ~ autuacoes, data=mydata)
summary(fita) # show results
plot(ocupacao,autuacoes,col = "blue",main = "Relação Autuações vs Ocupação na AAE",
     abline(lm(autuacoes~ocupacao)),cex = 1.3,pch = 16,
     xlab = "Taxa de Ocupação %",ylab = "Autuações * 1000")
monitoreslog <- log(monitores)
monitores
ocupacaolog <- log(ocupacao)
ocupacaolog
autuacoeslog <- log(autuacoes)
autuacoeslog

fitb <- lm(ocupacaolog ~ autuacoeslog)
summary(fitb) # show results
plot(ocupacaolog,autuacoeslog,col = "blue",main = "Relação Autuações vs Oucpação na AAE",
     abline(lm(autuacoeslog~ocupacaolog)),cex = 1.3,pch = 16,
     xlab = "Taxa de Ocupação %",ylab = "Autuações * 1000")


fitc <- lm(ocupacao ~ monitores, data=mydata)
summary(fitc) # show results
plot(ocupacao,monitores,col = "blue",main = "Relação nº Monitores vs Ocupação na AAE",
     abline(lm(monitores~ocupacao)),cex = 1.3,pch = 16,
     xlab = "Taxa de Ocupação %",ylab = "Nº de Monitores")



fit2 <- lm(ocupacao ~ monitores + autuacoes + tarifa, data=mydata)
summary(fit2) # show results


fit3 <- lm(ocupacao ~ monitores + autuacoes, data=mydata)
summary(fit3) # show results

# Other useful functions 
coefficients(fit2) # model coefficients
confint(fit2, level=0.95) # CIs for model parameters 
fitted(fit2) # predicted values
residuals(fit2) # residuals
anova(fit2) # anova table 
vcov(fit2) # covariance matrix for model parameters 
influence(fit2) # regression diagnostics


fit3 <- lm(ocupacao ~ monitores + autuacoes + monitores * autuacoes, data=mydata)
summary(fit3) # show results

fit4 <- lm(ocupacao ~ monitores + autuacoes, data=mydata)
summary(fit4) # show results

fit5 <- lm(ocupacao ~ monitores + tarifa, data=mydata)
summary(fit5) # show results
anova(fit5)



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


h<-hist(autuacoes, col="red", xlab="Numero de Autuações", main="Histograma") 
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
monitores
ocupaçãolog <- log(ocupacao)
ocupaçãolog
autuacoeslog <- log(autuacoes)
autuacoeslog

fit <- lm(ocupaçãolog ~ monitores + autuacoeslog + monitores * autuacoeslog)
summary(fit) # show results

fit <- lm(ocupacao ~ monitores + autuacoes)
summary(fit) # show results


fit <- lm(ocupacao ~ monitores + autuacoes + tarifa)
summary(fit) # show results

fit <- lm(ocupacao ~ monitores +  tarifa)
summary(fit) # show results


fit <- lm(ocupaçãolog ~ monitores + autuacoeslog + monitores * autuacoeslog)
summary(fit) # show results

mydata$tarifa <- NULL
head(mydata)
plot(mydata,  pch=16, col="blue", main="Matriz de Dispersão")

#install.packages('rgl')
require(rgl) 
require(car) 
scatter3d(ocupacao ~ monitores + autuacoes, radius=tarifa, data=mydata)

scatter3d(ocupacao ~ monitores + autuacoes  | ocupacao, radius=tarifa, surface=FALSE, data=mydata)

