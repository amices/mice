get.dfcom <- function(model, dfcom = NULL) {
  # Input: a fitted model
  # residual degrees of freedom of model fitted on hypothetically complete data
  # assumed to be the same across imputations

  if (!is.null(dfcom)) {
    return(as.numeric(max(dfcom, 1)))
  }

  # first, try the standard df.residual() function
  dfcom <- tryCatch(stats::df.residual(model),
                    error = function(e) NULL)
  if (!is.null(dfcom)) return(as.numeric(dfcom))

  # coxph model: nevent - p
  if (inherits(model, "coxph")) {
    return(as.numeric(max(model$nevent - length(stats::coef(model)), 1)))
  }

  # other model: n - p
  nobs <- tryCatch(length(stats::residuals(model)),
                   error = function(e) NULL)
  if (!is.null(nobs)) {
    return(as.numeric(max(nobs - length(stats::coef(model)), 1)))
  }

  # nothing found
  Inf
}


get.glanced <- function(object) {
  if (!is.list(object)) stop("Argument 'object' not a list", call. = FALSE)
  object <- as.mira(object)

  glanced <- try(data.frame(summary(getfit(object), type = "glance")), silent = TRUE)
  if (inherits(glanced, "data.frame")) {
    # nobs is needed for pool.r.squared
    # broom <= 0.5.6 does not supply it
    if (!"nobs" %in% colnames(glanced)) {
      glanced$nobs <- length(stats::residuals(object$analyses[[1]]))
    }
  } else {
    glanced <- NULL
  }
  glanced
}
