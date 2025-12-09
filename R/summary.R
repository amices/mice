#' Summary of a \code{mira} object
#'
#' @rdname summary
#' @param object A \code{mira} object
#' @param type A length-1 character vector indicating the
#' type of summary. There are three choices: \code{type = "tidy"}
#' return the parameters estimates of each analyses as a data frame.
#' \code{type = "glance"} return the fit statistics of each analysis
#' as a data frame. \code{type = "summary"} returns a list of
#' length \code{m} with the analysis results. The default is
#' \code{"tidy"}.
#' @param dfcom Manually supplied degrees of freedom. For internal use by
#' \code{pool()}.
#' @param ... Other parameters passed down to \code{print()} and \code{summary()}
#' @return \code{NULL}
#' @seealso \code{\link{mira}}
#' @method summary mira
#' @export
summary.mira <- function(
  object,
  type = c("tidy", "glance", "summary"),
  dfcom = NULL,
  ...
) {
  type <- match.arg(type)
  fitlist <- getfit(object)
  if (type == "tidy") {
    # Try standard tidy() first, with fallback for lmer objects
    v <- tryCatch(
      {
        lapply(fitlist, tidy, effects = "fixed", parametric = TRUE, ...) %>%
          bind_rows()
      },
      error = function(e) {
        # Check if this is an lmerMod object without broom.mixed
        if (
          inherits(fitlist[[1]], "lmerMod") &&
            grepl("No.*tidy.*method", e$message, ignore.case = TRUE)
        ) {
          # Ensure lme4 is available (CRAN-compliant check)
          install.on.demand("lme4", ...)
          # Manual extraction for lmer objects using built-in methods
          lapply(fitlist, function(fit) {
            coefs <- lme4::fixef(fit)
            se <- sqrt(diag(as.matrix(stats::vcov(fit))))
            data.frame(
              term = names(coefs),
              estimate = as.numeric(coefs),
              std.error = as.numeric(se),
              stringsAsFactors = FALSE
            )
          }) %>%
            bind_rows()
        } else {
          # Re-throw the error if it's not an lmer issue
          stop(e)
        }
      }
    )
  }
  if (type == "glance") {
    v <- lapply(fitlist, glance, ...) %>% bind_rows()
  }
  # nobs is needed for pool.r.squared
  # not supplied by broom <= 0.5.6
  model <- getfit(object, 1L)
  if (!"nobs" %in% colnames(v)) {
    v$nobs <- tryCatch(length(stats::residuals(model)), error = function(e) {
      NULL
    })
  }

  # get df.residuals
  if (!is.null(dfcom)) {
    # overwrite df.residual by a user-specified value
    v$df.residual <- dfcom
  } else if (!"df.residuals" %in% colnames(v)) {
    # emergency call to df.residual
    v$df.residual <- get.dfcom(model)
  }

  if (type == "summary") {
    v <- lapply(fitlist, summary, ...)
  }
  v
}


#' Print a \code{mice.anova} object
#'
#' @rdname summary
#' @return \code{NULL}
#' @seealso \code{\link{mipo}}
#' @method summary mice.anova
#' @export
summary.mice.anova <- function(object, ...) {
  # handle objects from anova
  out <- object$out

  # handle objects from D1, D2 and D3
  if (is.null(out)) {
    out <- list(
      `1 ~~ 2` = list(
        result = object$result,
        dfcom = object$dfcom
      )
    )
  }

  test <- names(out)
  dfcom <- vapply(out, function(x) x$dfcom, numeric(1))
  results <- t(vapply(out, function(x) x$result, numeric(5)))
  rf <- data.frame(
    test = test,
    statistic = results[, 1],
    df1 = results[, 2],
    df2 = results[, 3],
    dfcom = dfcom,
    p.value = results[, 4],
    riv = results[, 5],
    row.names = NULL
  )

  formulas <- object$formulas
  ff <- data.frame(
    model = names(formulas),
    formula = as.character(formulas)
  )

  structure(
    list(
      models = ff,
      comparisons = rf,
      m = object$m,
      method = object$method,
      use = object$use
    ),
    class = c("mice.anova.summary", class(object))
  )
}
