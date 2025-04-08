quantify <- function(y, ry, x, quantify = TRUE) {
  if (!is.factor(y)) {
    return(list(ynum = y,
                labels = NULL,
                quant = NULL))
  }
  if (!quantify) {
    ynum <- as.integer(y)
    return(list(ynum = ynum,
                labels = levels(y),
                quant = 1L:length(levels(y))))
  }

  # replace (reduced set of) categories by optimal scaling
  yf <- factor(y[ry], exclude = NULL)
  yd <- model.matrix(~ 0 + yf)
  xd <- cbind(1, x[ry, , drop = FALSE])
  cca <- cancor(y = yd, x = xd, xcenter = FALSE, ycenter = FALSE)
  quant <- as.vector(cca$ycoef[, 2L])
  quant_expand <- quant[match(levels(y), levels(yf))]
  ynum <- quant_expand[match(as.character(y), levels(y))]
  return(list(ynum = ynum,
              labels = levels(y),
              quant = quant_expand))
}

unquantify <- function(ynum = NULL, quant = NULL, labels = NULL) {
  if (is.null(labels)) return(ynum)
  y <- factor(ynum, levels = quant, labels = labels)
  if (anyNA(levels(y))) {
    y <- droplevels(y, exclude = NA)
  }
  return(y)
}
