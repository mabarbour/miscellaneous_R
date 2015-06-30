# copied from http://stackoverflow.com/questions/7680959/convert-type-of-multiple-columns-of-a-dataframe-at-once on January 9, 2014

convert.magic1 <- function(obj,types){
  out <- lapply(1:length(obj),FUN = function(i){FUN1 <- switch(types[i],character = as.character,numeric = as.numeric,factor = as.factor); FUN1(obj[,i])})
  names(out) <- colnames(obj)
  as.data.frame(out)
}