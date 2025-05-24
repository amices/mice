validate.arguments <- function(y, ry, x, wy, allow.x.NULL = FALSE,
                               allow.x.NA = FALSE) {
  # validate standard arguments of mice.impute functions
  if (!allow.x.NULL && is.null(x)) {
    stop("Cannot handle NULL value for `x`")
  }
  if (!allow.x.NA && anyNA(x)) {
    stop("Cannot handle NA in `x`")
  }
  if (!is.vector(ry)) {
    stop("`ry` is not a vector")
  }
}
