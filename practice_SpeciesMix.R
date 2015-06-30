# this represents my attempt to play around with the SpeciesMix package, which may be a promising approach to modelling communities

library(SpeciesMix)

## Create some artificial data assuming a negative binomial distribution. Taken straight from example file in "SpeciesMix" package.
G <-4
S <- 50
theta <- matrix(c(-9,35,-32,0,0.7,0,-16,23,-8.2,-3,-0.6,0.8),4,3,byrow=TRUE)
dat <- data.frame(y=rep(1,200),x=runif(200,0,2.5),z=rnorm(200,10,2))
dat <- data.frame(dat,x.sq=dat$x^2)
dat1 <- artificial.data(y~1+x+x.sq,dat,theta,S)

## Analyze the data
fm4 <- SpeciesMix(obs~1+x+x.sq,dat1$pa,dat,G=4,em.prefit=TRUE,em.refit=1,est.var=TRUE)
fm4$logl
fm4$pi
fm4$aic
fm4$bic
fm4$tau


# cluster select
dat <- dat[,2:3]
clusters <- clusterSelect(obs~1+x,dat1$pa,dat,G=2:5,em.refit=2)

