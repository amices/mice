#' Conditional post-processing of imputations
#'
#' \code{ifdo()} replaces imputations that meet a condition. It is meant to
#' be used inside the \code{post} argument of \code{\link{mice}()}, where it
#' acts on the imputations of the current variable in the current iteration.
#'
#' Each condition is evaluated against the data as they are after the
#' imputation of the current variable, so the current variable holds its new
#' imputations. Expressions may refer to any variable in the data. For the
#' rows where condition \code{k} is \code{TRUE}, the imputations are replaced
#' by action \code{k}. An action can be a constant, or an expression that is
#' evaluated for those rows, such as \code{Ozone / 2}. All conditions are
#' evaluated before any replacement is made, so a later action does not
#' change which rows meet a later condition. If a row meets more than one
#' condition, the last action wins. Rows where a condition is \code{NA} are
#' left unchanged.
#'
#' \code{ifdo()} works wherever \code{mice()} applies \code{post}, that is,
#' for variables imputed by a univariate method. It does nothing useful
#' outside \code{post}.
#'
#' @param cond A condition, or several conditions combined with \code{c()},
#' e.g. \code{c(Ozone < 1, Ozone > 200)}. Conditions are expressions in terms
#' of the variables in the data.
#' @param action The replacement values, one for each condition, combined
#' with \code{c()} if there is more than one, e.g. \code{c(1, 200)}.
#' @return Called for its side effect: it updates the imputations of the
#' current variable. Returns \code{NULL} invisibly.
#' @seealso \code{\link{squeeze}}, \code{\link{make.post}}
#' @author Stef van Buuren, 2012, 2026
#' @references
#' Van Buuren, S. (2018). \emph{Flexible Imputation of Missing Data. Second
#' Edition.} Chapman & Hall/CRC. Section 6.4.6.
#' @examples
#' post <- make.post(airquality[, 1:2])
#' post["Ozone"] <- "ifdo(c(Ozone < 1, Ozone > 200), c(1, 200))"
#' imp <- mice(airquality[, 1:2],
#'   method = "norm.nob", m = 1, maxit = 1,
#'   seed = 1, post = post, print = FALSE
#' )
#' range(imp$imp$Ozone)
#'
#' # same result as
#' post["Ozone"] <- "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(1, 200))"
#' @export
ifdo <- function(cond, action) {
  env <- parent.frame()
  need <- c("imp", "data", "where", "j", "i")
  if (!all(vapply(need, exists, logical(1), envir = env, inherits = FALSE))) {
    stop(
      "ifdo() can only be used inside the `post` argument of mice()",
      call. = FALSE
    )
  }

  cond <- ifdo.args(substitute(cond))
  action <- ifdo.args(substitute(action))
  if (length(cond) != length(action)) {
    stop(
      "ifdo(): `cond` has ",
      length(cond),
      " condition(s), but `action` has ",
      length(action),
      " value(s)",
      call. = FALSE
    )
  }

  j <- get("j", envir = env)
  i <- get("i", envir = env)
  imp <- get("imp", envir = env)
  wy <- get("where", envir = env)[, j]
  d <- get("data", envir = env)[wy, , drop = FALSE]
  d[[j]] <- imp[[j]][, i]

  val <- imp[[j]][, i]
  for (k in seq_along(cond)) {
    idx <- eval(cond[[k]], d, env)
    if (!is.logical(idx) || length(idx) != nrow(d)) {
      stop(
        "ifdo(): condition `",
        deparse(cond[[k]]),
        "` does not give one TRUE/FALSE per imputed value",
        call. = FALSE
      )
    }
    idx[is.na(idx)] <- FALSE
    if (!any(idx)) {
      next
    }
    a <- eval(action[[k]], d[idx, , drop = FALSE], env)
    val[idx] <- rep_len(a, sum(idx))
  }

  imp[[j]][, i] <- val
  assign("imp", imp, envir = env)
  invisible(NULL)
}

# Split an unevaluated c(a, b, ...) call into list(a, b, ...)
ifdo.args <- function(expr) {
  if (is.call(expr) && identical(expr[[1L]], as.name("c"))) {
    return(as.list(expr)[-1L])
  }
  list(expr)
}

#' Appends specified break to the data
#'
#' A custom function to insert rows in long data with new pseudo-observations
#' that are being done on the specified break ages. There should be a
#' column called \code{first} in \code{data} with logical data that codes whether
#' the current row is the first for subject \code{id}. Furthermore,
#' the function assumes that columns \code{age}, \code{occ},
#' \code{hgt.z}, \code{wgt.z} and
#' \code{bmi.z} are available. This function is used on the \code{tbc}
#' data in FIMD chapter 9. Check that out to see it in action.
#' @aliases appendbreak
#' @param data A data frame in the long long format
#' @param brk A vector of break ages
#' @param warp.model A time warping model
#' @param id The subject identifier
#' @param typ Label to signal that this is a newly added observation
#' @return A long data frame with additional rows for the break ages
#' @export
appendbreak <- function(
  data,
  brk,
  warp.model = warp.model,
  id = NULL,
  typ = "pred"
) {
  k <- length(brk)
  app <- data[data$first, ]
  if (!is.null(id)) {
    idx <- app$id %in% id
    app <- app[idx, ]
  }
  nap <- nrow(app)

  ## update administrative variables
  app$first <- FALSE
  app$typ <- typ
  app$occ <- NA
  app <- app[rep.int(seq_len(nap), length(brk)), ]

  ## update age variables
  app$age <- rep(brk, each = nap)
  app$age2 <- predict(warp.model, newdata = app)
  X <- splines::bs(
    app$age,
    knots = brk,
    Boundary.knots = c(brk[1], brk[k] + 0.0001),
    degree = 1
  )
  X <- X[, -(k + 1)]
  app[, paste0("x", seq_len(ncol(X)))] <- X

  ## update outcome variable (set to missing)
  app[, c("hgt.z", "wgt.z", "bmi.z")] <- NA
  app <- rbind(data, app)
  app[order(app$id, app$age), ]
}

#' Extract broken stick estimates from a \code{lmer} object
#'
#' @param fit An object of class \code{lmer}
#' @return A matrix containing broken stick estimates
#' @author Stef van Buuren, 2012
#' @export
extractBS <- function(fit) {
  siz <- t(lme4::ranef(fit)[[1]]) + lme4::fixef(fit)
  matrix(siz, nrow = nrow(siz) * ncol(siz), ncol = 1)
}


## used by mice.impute.midastouch
bootfunc.plain <- function(n) {
  random <- sample.int(n, replace = TRUE)
  as.numeric(table(factor(random, levels = seq_len(n))))
}

minmax <- function(x, domin = TRUE, domax = TRUE) {
  maxx <- sqrt(.Machine$double.xmax)
  minx <- sqrt(.Machine$double.eps)
  if (domin) {
    x <- pmin(x, maxx)
  }
  if (domax) {
    x <- pmax(x, minx)
  }
  x
}

single2imputes <- function(single, mis) {
  nmis <- colSums(mis)
  vars <- names(single)[nmis > 0]
  z <- vector("list", length(vars))
  names(z) <- vars
  for (j in vars) {
    z[[j]] <- single[mis[, j], j]
  }
  z
}

syms <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(list())
  }
  if (!is.character(x)) {
    stop("Input must be a character vector.", call. = FALSE)
  }
  if (anyDuplicated(x)) {
    warning("Duplicate column names detected.")
  }
  lapply(x, as.name)
}
