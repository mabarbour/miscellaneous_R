autoplot.custom <- function (object, geom = c("point", "text"), 
                             layers = c("species", 
                                                        "sites", "biplot", "centroids"), arrows = TRUE, legend.position = "right", 
          ylab, xlab, const, pt.size = 1, arrow.size = 1, txt.size = 3, ...) 
{
  obj <- fortify(object, ...)
  LAYERS <- levels(obj$Score)
  dimlabels <- attr(obj, "dimlabels")
  geom <- match.arg(geom)
  point <- TRUE
  if (isTRUE(all.equal(geom, "text"))) 
    point <- FALSE
  obj <- obj[obj$Score %in% layers, , drop = FALSE]
  plt <- ggplot()
  take <- if (isTRUE(arrows)) {
    "sites"
  }
  else {
    c("species", "sites")
  }
  want <- obj$Score %in% take
  if (point) {
    plt <- plt + geom_point(data = obj[want, , drop = FALSE], 
                            aes(x = Dim1, y = Dim2, shape = Score, colour = Score), size = pt.size, show.legend = FALSE)
  }
  else {
    plt <- plt + geom_text(data = obj[want, , drop = FALSE], 
                           aes(x = Dim1, y = Dim2, label = Label, colour = Score), 
                           size = txt.size)
  }
  if (isTRUE(arrows)) {
    want <- obj$Score == "species"
    pdat <- obj[want, , drop = FALSE]
    col <- "steelblue"
    want <- obj[["Score"]] == "species"
    plt <- plt + geom_segment(data = pdat, aes(x = 0, y = 0, 
                                               xend = Dim1, yend = Dim2), arrow = arrow(length = unit(0.2, 
                                                                                                      "cm")), colour = col, size = arrow.size)
    pdat[, c("Dim1", "Dim2")] <- 1.1 * pdat[, c("Dim1", "Dim2"), 
                                            drop = FALSE]
    plt <- plt + geom_text(data = pdat, aes(x = Dim1, y = Dim2, 
                                            label = Label), size = txt.size, colour = col) # so text would match arrow color
  }
  if (all(c("biplot", "centroids") %in% LAYERS)) {
    want <- obj$Score == "biplot"
    tmp <- obj[want, ]
    obj <- obj[!want, ]
    bnam <- tmp[, "Label"]
    cnam <- obj[obj$Score == "centroids", "Label"]
    obj <- rbind(obj, tmp[!bnam %in% cnam, , drop = FALSE])
  }
  if (any(want <- obj$Score == "constraints")) {
    if (point) {
      plt <- plt + geom_point(data = obj[want, , drop = FALSE], 
                              aes(x = Dim1, y = Dim2))
    }
    else {
      plt <- plt + geom_text(data = obj[want, , drop = FALSE], 
                             aes(x = Dim1, y = Dim2, label = Label))
    }
  }
  if (any(want <- obj$Score == "biplot")) {
    if (length(layers) > 1) {
      mul <- ggvegan:::arrowMul(obj[want, c("Dim1", "Dim2"), drop = FALSE], # added ggvegan::: to call ggvegan's function
                      obj[!want, c("Dim1", "Dim2"), drop = FALSE])
      obj[want, c("Dim1", "Dim2")] <- mul * obj[want, c("Dim1", 
                                                        "Dim2")]
    }
    col <- "black"
    plt <- plt + geom_segment(data = obj[want, , drop = FALSE], 
                              aes(x = 0, y = 0, xend = Dim1, yend = Dim2), arrow = arrow(length = unit(0.2, 
                                                                                                       "cm")), colour = col, size = arrow.size)
    obj[want, c("Dim1", "Dim2")] <- 1.1 * obj[want, c("Dim1", 
                                                      "Dim2")]
    plt <- plt + geom_text(data = obj[want, , drop = FALSE], 
                           aes(x = Dim1, y = Dim2, label = Label), colour = col, size = txt.size) # added color here so it would match arrow color
  }
  if (any(want <- obj$Score == "centroids")) {
    plt <- plt + geom_text(data = obj[want, , drop = FALSE], 
                           aes(x = Dim1, y = Dim2, label = Label), colour = "grey", size = txt.size)
  }
  if (missing(xlab)) 
    xlab <- dimlabels[1]
  if (missing(ylab)) 
    ylab <- dimlabels[2]
  plt <- plt + xlab(xlab) + ylab(ylab)
  plt <- plt + coord_fixed(ratio = 1)
  plt <- plt + theme(legend.position = legend.position)
  plt
}
