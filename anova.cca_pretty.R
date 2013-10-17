
anova.cca_pretty <- function(anova_object){

  # rename columns of output
  colnames(anova_object) <- c("df", "Eigenvalue", "F", "Number of Permutations", "P")
  
  # round decimal places for eigen value 
  anova_object$Eigenvalue <- round(anova_object$Eigenvalue,2)
  
  # round decimal places for F
  anova_object$F <- round(anova_object$F,2)
  
  # round decimal places for p-value
  anova_object$P <- round(anova_object$P,3)
  
  # calculate percent variance based on eigenvalues in column "Var" and rounds the decimal places
  anova_object$"Percent Variance" <- round((anova_object$Eigenvalue/sum(anova_object$Eigenvalue))*100,1)
  
  anova_object$"Percent Variance"[length(anova_object$Eigenvalue)] <- "" # replaces last percent variance value with a blank cell, because we are not interested in how much residual variance there is.
  
  # reorder dataframe
  anova_object_reorder <- anova_object[ ,c(1,2,6,3,4,5)] # place percent variance column to the right of eigenvalue 
  
  return(anova_object_reorder)
}