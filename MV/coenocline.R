
## a simulated coenocline according to a non-interacting Gaussian model of community structure
position <- 1:100
s <- 3
means <- runif(s, min=10, max=20)
sds <- length(position)*runif(s)
i<-1
error <- 10
abundances <- dnorm(position+runif(s, -error, error),
	means[i], sd=sds[i])
for(i in 2:s)
  abundances <- cbind(abundances, dnorm(position+runif(s, -error, error), means[i], sd=sds[i]))
matplot(position, abundances, type="l", lty="solid", xlab="gradient", lwd=2)



## shape of data swarm for two species
## and the data cloud for a pca of all species
layout(matrix(c(1,2), 1, 2))
plot(abundances[,1], abundances[,2], pch=21, bg=heat.colors(100))
pp <- princomp(abundances)
plot(pp$scores[,1:2], pch=21, bg=heat.colors(100))
library(rgl)
plot3d(abundances[,1:3], type="s", col=heat.colors(100))
plot3d(pp$scores[,1:3], type="s", col=heat.colors(100))

library(vegan)

mmm <- metaMDS(abundances, autotransform=F,
				distance="euclidean")
plot(mmm, type="n")
points(mmm, display="sites", pch=21, cex=2,
	bg=heat.colors(100))
				
				
				