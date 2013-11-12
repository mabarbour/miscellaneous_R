# Author: Matt Barbour


rankAbundancePlot <- function(community, title="", cex.increase=0){
  #library(rich)
  #library(ggplot2)
  #species_abundances <- as.data.frame(colSums(community))
  #ggplot(species_abundances, aes(x = factor(rownames(species_abundances)), y = species_abundances[ ,1])) + geom_bar(stat = "identity")
  barplot(sort(colSums(community), decreasing = FALSE), horiz=TRUE, main = title, las=1, cex.names=(1-0.01*dim(community)[2] + cex.increase), mgp=c(0,0,-1))
  # the code in cex.names scales the font size by the number of species in the dataset, but also allows the user to modify this with the cex.increase parameter (default = 0)
}

    

heatmap_community <- function(community, method = ""){
  require(vegan)
  require(ggplot2)
  
  source('~/Documents/miscellaneous_R/ggplot_themes.R')
  
  if(method == "rel abund"){
    min_cols <- apply(community, 2, min)
    min_cols[which(min_cols < 1)] <- 1 # all values with abundances less than 1 were changed to 1 to make comparable "fold" comparisons
    community_new <- sweep(community, 2, min_cols, FUN="/")
  } # relative abundance of species ACROSS sites
  
  if(method %in% c("total", "max", "frequency", "normalize", "range", "standardize", "pa", "chi.square", "hellinger", "log")){
    community_new <- decostand(community, method = method)
  }
  
  if(method == ""){
    community_new <- community
  }
  
  sites <- rownames(community_new)
  community_data_with_sites <- cbind.data.frame(sites, community_new)
  melt_plot <- melt(community_data_with_sites, id.vars = "sites")
  melt_plot$sites <- factor(melt_plot$sites, levels=names(sort(rowSums(community), decreasing=TRUE)), ordered=TRUE) # orders this factor based on total species abundances observed at sites.
  melt_plot$variable <- factor(melt_plot$variable, levels=names(sort(colSums(community), decreasing=TRUE)), ordered=TRUE)  # orders this factor based on the original abundances of species among sites
  
  (plot_abund_plot <- qplot(data=melt_plot, x=variable, y=sites, fill=value, geom="tile") + scale_fill_gradient(low="white", high="red") + xlab("") + ylab("")+ theme_heatmap) # its weird that I have a value of 1 corresponding with "white" but that is not reflected in the figure legend.  The figure is representing what I want it to thoug
}



  

    
