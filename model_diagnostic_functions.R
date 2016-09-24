## This script contains functions for evaluating the fits of models to data.

## load required libraries
library(ggplot2)
library(lme4)

## Anova table function. ----
anova.table <- function(model, test = "F", type = 3, experiment){
  require(car)
  require(dplyr)
  require(broom)
  tmp <- Anova(model, test = test, type = type) %>%
    tidy() %>%
    mutate(response = all.vars(terms(model))[1],
           test = test,
           experiment = experiment)
  return(tmp)
}

## Calculate variance components of lmer or glmer model. ----
#Much of this function is taken from within 'sem.model.fits()' in 'piecewiseSEM' package.
var.table <- function(model, experiment){
  require(piecewiseSEM)
  require(merTools)
  require(dplyr)
  if (any(class(model) %in% c("lmerMod", "merModLmerTest"))) {
    varResid = attr(VarCorr(model), "sc")^2
  }
  if (any(class(model) == "glmerMod")) {
    #obs = names(unlist(lapply(ranef(model), nrow))[unlist(lapply(ranef(model), nrow)) == nrow(model@pp$X)])
    #if (length(obs) == 0) 
     # varDisp = 0
    #else {
     # varDisp = sum(sapply(VarCorr(model)[obs], function(Sigma) {
      #  X = model.matrix(model)
       # Z = X[, rownames(Sigma)]
        #sum(diag(Z %*% Sigma %*% t(Z)))/nrow(X)
      #}))
    #}
    if (summary(model)$family == "binomial") {
      if (summary(model)$link == "logit") 
        varDist = (pi^2)/3
      else if (summary(model)$link == "probit") 
        varDist = 1
      else {
        warning(paste("Model link '", summary(model)$link, 
                      "' is not yet supported for the ", summary(model)$family, 
                      "distribution"))
        varDist = NA
      }
    }
    else if (summary(model)$family == "poisson") {
      null.model = update(model, formula = paste(". ~ ", 
                                                 get.random.formula(model, "~1", modelList = NULL)))
      null.fixef = as.numeric(fixef(null.model))
      if (summary(model)$link == "log") 
        varDist = log(1 + 1/exp(null.fixef))
    }
    else if (summary(model)$link == "sqrt") 
      varDist = 0.25
    else {
      warning(paste("Model link '", summary(model)$link, 
                    "' is not yet supported for the ", summary(model)$family, 
                    "distribution"))
      varDist = NA
    }
    varResid = varDist #+ varDisp # making sure to manually included individual level random effect in models when necessary
  }
  #scope <- drop.scope(model)

  #fixed.R2 <- list()
  #for (i in seq_along(scope)) {
    #tt <- scope[i]
    #model.up <- update(model, as.formula(paste("~ . -", tt)))
    #fixed.R2[[i]] <- data.frame(Factors = tt, var = var(as.vector(fixef(model) %*% t(model@pp$X))) - var(as.vector(fixef(model.up) %*% t(model.up@pp$X)))) # calculate the change in total variance
  #}
  #varFixed <- plyr::ldply(fixed.R2)
  fixed.var <- list()
  for (i in seq_along(fixef(model))) {
    fixed.var[[i]] <- data.frame(Factors = names(fixef(model))[i],
                                 var = var(as.vector(fixef(model)[i] %*% t(model@pp$X)[i, ])))
  }
  varFixed <- plyr::ldply(fixed.var)
  #varFixed <- data.frame(var = var(as.vector(fixef(model) %*% t(model@pp$X))))
  #browser()
  varRand <- data.frame(var = REsdExtract(model)^2,
                        Factors = names(REsdExtract(model)))
  varOther <- data.frame(var = varResid,
                         Factors = "var_Resid")
  varALL <- bind_rows(varFixed, varRand, varOther) %>%
    mutate(var_percent = var/sum(var),
           response = all.vars(terms(model))[1],
           experiment = experiment)
  return(varALL)
}

## Use Kenward-Roger corrected F test to test the significance of each dropped predictor.
KRSumFun <- function(object, objectDrop, ...) {
  krnames <- c("ndf","ddf","Fstat","p.value","F.scaling")
  r <- if (missing(objectDrop)) {
    setNames(rep(NA,length(krnames)),krnames)
  } else {
    krtest <- KRmodcomp(object,objectDrop)
    unlist(krtest$stats[krnames])
  }
  attr(r,"method") <- c("Kenward-Roger via pbkrtest package")
  r
}

## Parametric bootstrap for work with drop1 function. Taken from ?help drop1.merMod
PBSumFun <- function(object, objectDrop, nsim, ...) {
  pbnames <- c("stat","p.value")
  r <- if (missing(objectDrop)) {
    setNames(rep(NA,length(pbnames)),pbnames)
  } else {
    pbtest <- PBmodcomp(object,objectDrop,nsim=nsim)
    unlist(pbtest$test[2,pbnames])
  }
  attr(r,"method") <- c("Parametric bootstrap via pbkrtest package")
  r
}

## calculate R2 for a predictor
deltaR2 <- function(base.model, reduced.model){
  fits <- sem.model.fits(
    list(base.model, reduced.model)
  )
  R2 <- fits[1,"Marginal"] - fits[2,"Marginal"]
  print(R2)
}

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