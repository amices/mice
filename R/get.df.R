get.dfcom <- function(object, dfcom = NULL) {
  # residual degrees of freedom of model fitted on hypothetically complete data
  # assumed to be the same across imputations

  if (!is.null(dfcom)) {
    return(max(dfcom, 1L))
  }

  glanced <- get.glanced(object)

  # try to extract from df.residual
  if (!is.null(glanced)) {
    if ("df.residual" %in% colnames(glanced)) {
      return(glanced$df.residual[1L])
    }
  }

  # try n - p (or nevent - p for Cox model)
  if (!is.null(glanced)) {
    if ("nobs" %in% colnames(glanced)) {
      model <- getfit(object, 1L)
      if (inherits(model, "coxph")) {
        return(max(model$nevent - length(coef(model)), 1L))
      }
      return(max(glanced$nobs[1L] - length(coef(model)), 1L))
    }
  }

  # not found
  warning("Infinite sample size assumed.")
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
