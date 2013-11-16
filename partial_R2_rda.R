# Calculates partial R-squared for marginal effects of RDA and makes a new table with this additional data.
# x = anova.ccabymargin object

partial_R2_rda <- function(x) {
  x$partial_R2 <- with(x, Var/sum(Var))
  print(x)
}
