## This script contains functions for evaluating the fits of models to data.

## load required libraries
library(ggplot2)
library(lme4)

## ggplot function for evaluating normality of random effects assumption of mixed-effect models.
ggQQ_ranef <- function(ranef) # argument: vector of random effects
{
  y <- quantile(ranef, c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  p <- ggplot(data.frame(ranef = ranef), aes(sample=ranef)) +
    stat_qq(alpha = 0.5) +
    geom_abline(slope = slope, intercept = int, color="blue")
  
  return(p)
} 

## function for evaluating overdispersion in generalized linear mixed effect models. Taken from glmm.wiki.dot (http://glmm.wikidot.com/faq)
overdisp_fun <- function(model) {
  ## number of variance parameters in 
  ##   an n-by-n variance-covariance matrix
  vpars <- function(m) {
    nrow(m)*(nrow(m)+1)/2
  }
  model.df <- sum(sapply(VarCorr(model),vpars))+length(fixef(model))
  rdf <- nrow(model.frame(model))-model.df
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

# calculate overdispersion in generalized linear models.
dfun <- function(object){
  with(object, sum((weights * residuals^2)[weights > 0])/df.residual)
}

# Curve plots that are meant to be plotted on top of meanvar.plots in the 'mvabund' package or any other mean-variance plots. 
poisson_curve <- function(from, to, add = TRUE, color = "blue", line.width = 3, line.type = 1){
  curve(expr = 1*x, # poisson assumes that mean = variance
        from = from, to = to, add = add, col = color, lwd = line.width, lty = line.type)
}

quasipoisson_curve <- function(from, to, quasi.scalar, add = TRUE, color = "red", line.width = 3, line.type = 1){
  curve(expr = quasi.scalar*x, # quaispoisson assumes that variance scales linearly with mean according to a constant.
        from = from, to = to, add = add, col = color, lwd = line.width, lty = line.type)
}

neg.binomial_curve <- function(from, to, theta.negbin, add = TRUE, color = "black", line.width = 3, line.type = 1){
  curve(expr = x + theta.negbin*x^2, # negative binomial assumes this mean-variance relationship (quadratic)
        from = from, to = to, add = add, col = color, lwd = line.width, lty = line.type)
}