# table generating function
anova_table <- function(name, y, model){
  G = complete.data$Genotype
  counts = count(na.exclude(data.frame(y = y,G = G)), vars = "G")
  min_max_weightN = data.frame(min = min(aggregate(y ~ G, FUN = mean)[,2]), max = max(aggregate(y ~ G, FUN = mean,)[,2]), weightN = weighted.mean(counts[ ,2])) # necessary for heritability analysis which is essentially "repeatability"
  gen.var = (get(paste("model"))[1,3] - get(paste("model"))[2,3])/min_max_weightN$weightN
  geno.df = get(paste("model"))[1,1]
  resid.df = get(paste("model"))[2,1]
  F = round(get(paste("model"))[1,4],2) 
  P = round(get(paste("model"))[1,5],2)
  H2 = round(gen.var/(gen.var + get(paste("model"))[2,3]),2)
  c(name, round(min_max_weightN[,1],1), round(min_max_weightN[,2],1), geno.df, resid.df, F, P, H2)
}