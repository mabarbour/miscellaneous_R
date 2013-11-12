# Calculates phenotypic and genetic correlations among trait variables and combines them into one large matrix.  User can specify whether the phenotypic or genetic correlations go on top.
# Author: Matt Barbour
# Date: October 14, 2013

# need to think about writing a method for ensuring the data columns correspond exactly to each other.
pheno_geno_correlations <- function(phenotypic_data, genotype_data){
  library(psych)
  library(gdata)
  library(vegan)
  library(ggplot2)
  library(scales)
  source('~/Documents/miscellaneous_R/ggplot_themes.R')
  
  x_corr_data <- corr.test(phenotypic_data)
  y_corr_data <- corr.test(genotype_data)
  
  x_corr_euclid_dist <- vegdist(x_corr_data$r,"euclidean")
  y_corr_euclid_dist <- vegdist(y_corr_data$r, "euclidean")
  
  print(mantel(x_corr_euclid_dist, y_corr_euclid_dist, method = "pearson", permutations = 10000))
  
  x_corr_data$r[upper.tri(x_corr_data$r)] <- y_corr_data$r[upper.tri(y_corr_data$r)] # keeps the phenotypic correlation matrix in the lower left triangle and the genotypic correlations in the upper right triangle (assuming x = phenotypic data and y = genotypic data)
  print(x_corr_data$r) 
}

heatmap_correlations <- function(plot_data){
  source('~/Documents/miscellaneous_R/ggplot_themes.R')
  # create heat map of correlation data set of users choice
  melt_plot <- melt(cor(plot_data, use="pairwise.complete.obs"))
  melt_plot$X1 <- factor(melt_plot$X1, levels=unique(melt_plot$X1), ordered=TRUE)
  melt_plot$X2 <- factor(melt_plot$X2, levels=unique(melt_plot$X2), ordered=TRUE)
  
  (plot_corr_plot <- qplot(data=melt_plot, x=X2, y=X1, fill=value, geom="tile") + scale_fill_gradientn(colours=c("blue","white", "red"), values = rescale(c(-1,0,1)), guide="colorbar", limits=c(-1,1), name = expression(paste("Pearson's ",italic(r),""))) + xlab("") + ylab("")+ theme_heatmap) # its weird that I have a value of 1 corresponding with "white" but that is not reflected in the figure legend.  The figure is representing what I want it to thoug
}


