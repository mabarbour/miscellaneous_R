library(ggplot2)
library(grid)


theme_axisL_legend <- theme_bw() + theme(axis.text=element_text(colour="black",size=16), axis.title=element_text(size=18), axis.line=element_line(colour="black"), legend.key=element_blank(), panel.grid=element_blank(), panel.border=element_blank(),legend.title=element_text(face="bold", size=18),legend.text=element_text(face="italic", size=16),legend.justification=c(0,1), legend.position=c(0,1)) 

theme_heatmap <- theme_bw() + theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5), panel.border=element_blank())

theme_ordination_black_white <- theme_bw() + theme(axis.text=element_text(colour="black",size=16), legend.key=element_rect(colour="black"), panel.grid=element_blank(), panel.border=element_rect(colour="black"), axis.title.x=element_text(size=16, vjust=-0.5), axis.title.y=element_text(size=16, vjust=0.25), axis.ticks=element_blank()) # unable to add vertical and horizontal lines along origins.

theme_facet <- theme_bw() + theme(axis.text=element_text(colour="black",size=16), axis.title=element_text(size=18), legend.key=element_blank(), panel.grid=element_blank(), panel.border=element_rect(colour="black"),legend.text=element_text(face="italic", size=18), legend.title=element_blank(),legend.justification=c(0,1), legend.position=c(0,1), panel.margin=unit(2,"lines"), strip.background  = element_rect(colour="black",fil="white"), strip.text.x=element_text(size=16), strip.text.y=element_text(size=16))

# theme for trait-community plots for manuscript
#trait_community_plot_theme <- theme_bw() + theme(axis.text=element_text(colour="black",size=16), legend.key=element_rect(colour="black"), panel.grid=element_blank(), axis.title.x=element_text(size=16, vjust=-0.5), axis.title.y=element_text(size=16, face="bold", vjust=0.25), panel.border=element_blank(), axis.ticks=element_blank()) + theme(axis.line=element_line(color="black"))