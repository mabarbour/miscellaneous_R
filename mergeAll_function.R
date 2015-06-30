######  Merge all of the datasets. I found (and adapted) this function from an R forum (https://stat.ethz.ch/pipermail/r-help/2012-January/301984.html). It appears to be written by R. Michael Weylandt (michael.weylandt@gmail.com)
mergeAll <- function(..., by = "Plant.Position", all = TRUE) {
  dotArgs <- list(...)
  dotNames <- lapply(dotArgs, names)
  repNames <- Reduce(intersect, dotNames)
  repNames <- repNames[repNames != by]
  for(i in seq_along(dotArgs)){
    wn <- which( (names(dotArgs[[i]]) %in% repNames) &
                   (names(dotArgs[[i]]) != by))
    names(dotArgs[[i]])[wn] <- paste(names(dotArgs[[i]])[wn],
                                     names(dotArgs)[[i]], sep = ".")
  }
  Reduce(function(x, y) merge(x, y, by = by, all = all), dotArgs)
}