### Code for base distribution of the R package for the examples in:
### Analysis of Variance with Unbalanced Data: An Update for Ecology & Evolution
### by Andy Hector, Stefanie von Felten and Bernhard Schmid

rm( list = ls( all = TRUE ) ) # clear workspace

## TABLE 1: HYPOTHETICAL EXAMPLE DATA
# reproduced from Shaw & Mitchell-Olds (1993) Ecology 74(6), pp. 1638-1645
 
# the data: 

height <- c( 50, 57, 91, 94, 102, 110, 57, 71, 85, 105, 120 )
initial.size <- c( rep( "small", 2 ), rep( "large", 4 ), rep( "small", 3 ), rep( "large", 2 )) 
treatment <- c( rep( "control", 6 ), rep( "removal", 5 ) ) 
Shaw.MitchellOlds <- data.frame( height, initial.size, treatment ) ; Shaw.MitchellOlds

# cell means:

tapply( height, list(initial.size, treatment), mean )

## TABLE 2: SEQUENTIAL SUMS OF SQUARES

# TABLE 2a: sequential analysis fitting treatment first: 
# reproduces published Shaw and Mitchell-Olds ANOVA table 2 top

anova( txs.model <- lm( height ~ treatment * initial.size , data = Shaw.MitchellOlds ) )

# TABLE 2b: sequential analysis fitting size first:

anova( sxt.model <- lm( height ~ initial.size * treatment, data = Shaw.MitchellOlds ) )

# total sums of squares: 

anova( sst <- lm( height ~ 1 , data = Shaw.MitchellOlds ) ) 
# 5640.5 

# correlation of explanatory variables

ts.model <- lm( height ~ treatment + initial.size , data = Shaw.MitchellOlds ) 
summary(ts.model,cor=T)
#main effects model, no interaction

### TABLE 3: ADJUSTED SUMS OF SQUARES WITH HIGHER-LEVEL TERMS OMITTED 

# TABLE 3a:

anova( txs.model<-lm( height ~ treatment * initial.size, data = Shaw.MitchellOlds ) ) # for interaction

# TABLE 3b:

anova( st.model<-lm( height ~ initial.size + treatment, data = Shaw.MitchellOlds ) ) # main effect treatment

# TABLE 3c:

anova( ts.model<-lm( height ~ treatment + initial.size, data = Shaw.MitchellOlds ) ) # main effect size

# TABLE 3d:
# composite table got from these three sequential models

adjusted.total <- 11.4 +590.2 + 4846.0 +  747.8  ; adjusted.total # [1] 6195.4


### TABLE 4: ADJUSTED SUMS OF SQUARES WITH HIGHER-LEVEL TERMS INCLUDED 

# type III tests by fitting the interaction as a third variable
# code interaction as separate variable

Shaw.MitchellOlds$size <- c( rep( 2, 2 ), rep( 1, 4 ), rep( 2, 3 ), rep( 1, 2 ) ) # numeric
Shaw.MitchellOlds$treat <- c( rep( 1, 6 ), rep( 2, 5 ) ) # numeric
Shaw.MitchellOlds$ts <- c( rep( 2, 2 ), rep( 1, 7 ), rep( 2, 2 ) ) 
Shaw.MitchellOlds

# TABLE 4a:

anova( tsi.model <- lm( height ~ treatment + initial.size + ts , data = Shaw.MitchellOlds ) ) # for interaction

#equivalent to:

anova( lm( height ~ treatment * initial.size, data = Shaw.MitchellOlds ) ) # for interaction

# TABLE 4b:

anova( ist.model <- lm( height ~ ts + initial.size + treatment , data = Shaw.MitchellOlds ) ) # main effect 

treatment

# TABLE 4c:

anova( its.model <- lm( height ~ ts + treatment + initial.size , data = Shaw.MitchellOlds ) ) # main effect size

# TABLE 4d:
# composite table got from these three sequential models

adj.total <- 747.8 + 597.2 + 4807.9 + 11.4 # [1] 6164.3

# correlation of explanatory variables

txs.model <- lm( height ~ treatment*initial.size , data = Shaw.MitchellOlds )
summary(txs.model,cor=T)


### THE EXAMPLE DATASET REVISITED
# Our favoured model: sequential type I tests fitting initial size first 

anova( sxt.model <- lm( height ~ initial.size * treatment, data = Shaw.MitchellOlds ) )

# initial size as a covariate 
# to adjust for differences in initial size before assessing the effect of the treatment 
# This analysis suggests that: 
# no interaction between initial size and treatment 
# That initial size accounts for most of the differences in final size; 
# There is a treatment effect that is close to significant (at the conventional 5 % level). 
# The alternative sequential model fitting treatment first... 
# ...does not adjust for differences in initial size 
# ...and misses the effect of treatment. 
# In this case, where there is no interaction, the type III sum of squares and the type II sum of squares produce an almost identical result to our a priori sequence.


# Sequential type I solution in Shaw and Mitchell-Olds Ecology (1993) 
# Sequential type I tests fitting treatment first: 

anova( txs.model <- lm( height ~ treatment*initial.size, data = Shaw.MitchellOlds) )



### Additional material
# model matrix with treatment contrasts
# treatment contrasts are default in R, but here we show how to set them for clarity
# the first contrast argument gives the function to be used with unordered factors,
# the second the function to be used with ordered factors.

options( contrasts = c( "contr.treatment", "contr.poly" ) ) 
anova( ts.model<-lm( height ~ treatment + initial.size, data = Shaw.MitchellOlds ) ) # main effect size
model.matrix(ts.model) #main effects model, no interaction
txs.model <- lm( height ~ treatment*initial.size , data = Shaw.MitchellOlds )
model.matrix(txs.model) #model with interaction

# model matrix with sum-to-zero contrasts
options( contrasts = c( "contr.sum", "contr.poly" ) ) # sum-to-zero contrasts 
txs.model <- lm( height ~ treatment*initial.size , data = Shaw.MitchellOlds )
model.matrix(txs.model)