## if not available, run the following first:
## install.packages("apcluster")

library(apcluster)

## create four synthetic 2D clusters
cl1 <- cbind(rnorm(300, 0.3, 0.05), rnorm(300, 0.7, 0.04))
cl2 <- cbind(rnorm(300, 0.7, 0.04), rnorm(300, 0.4, .05))
cl3 <- cbind(rnorm(200, 0.50, 0.03), rnorm(200, 0.72, 0.03))
cl4 <- cbind(rnorm(250, 0.50, 0.03), rnorm(250, 0.42, 0.04))
x <- rbind(cl1, cl2, cl3, cl4)
dim(x)
x
## run apcluster() (you may replace the Euclidean distance by a different
## distance, e.g. driving distance, driving time)
apres <- apcluster(negDistMat(r=2), x, q=0)

## create new samples
xNew <- cbind(rnorm(10000, 0.3, 0.05), rnorm(100, 0.7, 0.04))

## auxiliary predict() function
predict.apcluster <- function(s, exemplars, newdata)
{
  simMat <- s(rbind(exemplars, newdata), sel=(1:nrow(newdata)) + nrow(exemplars))[1:nrow(exemplars), ]
  unname(apply(simMat, 2, which.max))
}

## assign new data samples to exemplars
resul <- predict.apcluster(negDistMat(r=2), x[apres@exemplars, ], xNew)
length(resul)
head(resul)
teste <- as.data.frame(resul)
dim(teste)
#View(resul)
final <- cbind(xNew, teste)
head(final,25)

## ... the result is a vector of indices to which exemplar/cluster each
## data sample is assigned