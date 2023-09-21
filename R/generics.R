#' Combine R objects by rows and columns
#'
#' Functions `cbind()` and `rbind()` are defined in
#' the `mice` package in order to
#' enable dispatch to `cbind.mids()` and `rbind.mids()`
#' when one of the arguments is a `data.frame`.
#'
#' The standard `base::cbind()` and `base::rbind()`
#' always dispatch to
#' `base::cbind.data.frame()` or `base::rbind.data.frame()`
#' if one of the arguments is a
#' `data.frame`. The versions defined in the `mice`
#' package intercept the user command
#' and test whether the first argument has class `"mids"`. If so,
#' function calls `cbind.mids()`, respectively `rbind.mids()`. In
#' all other cases, the call is forwarded to standard functions in the
#' `base` package.
#'
#' @inheritDotParams base::cbind
#' @details
#' The `cbind.mids()` function combines two `mids` objects
#' columnwise into a single
#' object of class `mids`, or combines a single `mids` object with
#' a `vector`, `matrix`, `factor` or `data.frame`
#' columnwise into a `mids` object.
#'
#' If both arguments of `cbind.mids()` are `mids`-objects, the
#' `data` list components should have the same number of rows. Also, the
#' number of imputations (`m`) should be identical.
#' If the second argument is a `matrix`,
#' `factor` or `vector`, it is transformed into a
#' `data.frame`. The number of rows should match with the `data`
#' component of the first argument.
#'
#' The  `cbind.mids()` function renames any duplicated variable or block names by
#' appending `".1"`, `".2"` to duplicated names.
#'
#' The `rbind.mids()` function combines two `mids` objects rowwise into a single
#' `mids` object, or combines a `mids` object with a vector, matrix,
#' factor or data frame rowwise into a `mids` object.
#'
#' If both arguments of `rbind.mids()` are `mids` objects,
#' then `rbind.mids()` requires that both have the same number of multiple
#' imputations. In addition, their `data` components should match.
#'
#' If the second argument of `rbind.mids()` is not a `mids` object,
#' the columns of the arguments should match. The `where` matrix for the
#' second argument is set to `FALSE`, signalling that any missing values in
#' that argument were not imputed. The `ignore` vector for the second argument is
#' set to `FALSE`. Rows inherited from the second argument will therefore
#' influence the parameter estimation of the imputation model in any future
#' iterations.
#
#' @note
#' The `cbind.mids()` function constructs the elements of the new `mids` object as follows:
#' \tabular{ll}{
#' `data`     \tab Columnwise combination of the data in `x` and `y`\cr
#' `imp`      \tab Combines the imputed values from `x` and `y`\cr
#' `m`        \tab Taken from `x$m`\cr
#' `where`    \tab Columnwise combination of `x$where` and `y$where`\cr
#' `blocks`   \tab Combines `x$blocks` and `y$blocks`\cr
#' `call`     \tab Vector, `call[1]` creates `x`, `call[2]`
#' is call to `cbind.mids()`\cr
#' `nmis`     \tab Equals `c(x$nmis, y$nmis)`\cr
#' `method`   \tab Combines `x$method` and `y$method`\cr
#' `predictorMatrix` \tab Combination with zeroes on the off-diagonal blocks\cr
#' `visitSequence`   \tab Combined as `c(x$visitSequence, y$visitSequence)`\cr
#' `formulas`  \tab Combined as `c(x$formulas, y$formulas)`\cr
#' `post`      \tab Combined as `c(x$post, y$post)`\cr
#' `dots`     \tab Combined as `c(x$dots, y$dots)`\cr
#' `ignore`    \tab Taken from `x$ignore`\cr
#' `seed`            \tab Taken from `x$seed`\cr
#' `iteration`       \tab Taken from `x$iteration`\cr
#' `lastSeedValue`   \tab Taken from `x$lastSeedValue`\cr
#' `chainMean`       \tab Combined from `x$chainMean` and `y$chainMean`\cr
#' `chainVar`        \tab Combined from `x$chainVar` and `y$chainVar`\cr
#' `loggedEvents`    \tab Taken from `x$loggedEvents`\cr
#' `version`    \tab Current package version\cr
#' `date`       \tab Current date\cr
#' }
#'
#' The  `rbind.mids()` function constructs the elements of the new `mids` object as follows:
#' \tabular{ll}{
#' `data`     \tab Rowwise combination of the (incomplete) data in `x` and `y`\cr
#' `imp`      \tab Equals `rbind(x$imp[[j]], y$imp[[j]])` if `y` is `mids` object; otherwise
#' the data of `y` will be copied\cr
#' `m`        \tab Equals `x$m`\cr
#' `where`    \tab Rowwise combination of `where` arguments\cr
#' `blocks`   \tab Equals `x$blocks`\cr
#' `call`     \tab Vector, `call[1]` creates `x`, `call[2]` is call to `rbind.mids`\cr
#' `nmis`     \tab `x$nmis` + `y$nmis`\cr
#' `method`   \tab Taken from `x$method`\cr
#' `predictorMatrix` \tab Taken from `x$predictorMatrix`\cr
#' `visitSequence`   \tab Taken from `x$visitSequence`\cr
#' `formulas`  \tab Taken from `x$formulas`\cr
#' `post`      \tab Taken from `x$post`\cr
#' `dots`     \tab Taken from `x$dots`\cr
#' `ignore`    \tab Concatenate `x$ignore` and `y$ignore`\cr
#' `seed`            \tab Taken from `x$seed`\cr
#' `iteration`       \tab Taken from `x$iteration`\cr
#' `lastSeedValue`   \tab Taken from `x$lastSeedValue`\cr
#' `chainMean`       \tab Set to `NA`\cr
#' `chainVar`        \tab Set to `NA`\cr
#' `loggedEvents`    \tab Taken from `x$loggedEvents`\cr
#' `version`    \tab Taken from `x$version`\cr
#' `date`       \tab Taken from `x$date`
#' }
#' @return An S3 object of class `mids`
#' @author Karin Groothuis-Oudshoorn, Stef van Buuren
#' @seealso [base::cbind()], [ibind()],
#' [`mids()`][mids-class]
#' @references van Buuren S and Groothuis-Oudshoorn K (2011). `mice`:
#' Multivariate Imputation by Chained Equations in `R`. *Journal of
#' Statistical Software*, **45**(3), 1-67.
#' \doi{10.18637/jss.v045.i03}
#' @keywords manip
#' @examples
#' # --- cbind ---
#' # impute four variables at once (default)
#' imp <- mice(nhanes, m = 1, maxit = 1, print = FALSE)
#' imp$predictorMatrix
#'
#' # impute two by two
#' data1 <- nhanes[, c("age", "bmi")]
#' data2 <- nhanes[, c("hyp", "chl")]
#' imp1 <- mice(data1, m = 2, maxit = 1, print = FALSE)
#' imp2 <- mice(data2, m = 2, maxit = 1, print = FALSE)
#'
#' # Append two solutions
#' imp12 <- cbind(imp1, imp2)
#'
#' # This is a different imputation model
#' imp12$predictorMatrix
#'
#' # Append the other way around
#' imp21 <- cbind(imp2, imp1)
#' imp21$predictorMatrix
#'
#' # Append 'forgotten' variable chl
#' data3 <- nhanes[, 1:3]
#' imp3 <- mice(data3, maxit = 1, m = 2, print = FALSE)
#' imp4 <- cbind(imp3, chl = nhanes$chl)
#'
#' # Of course, chl was not imputed
#' head(complete(imp4))
#'
#' # Combine mids object with data frame
#' imp5 <- cbind(imp3, nhanes2)
#' head(complete(imp5))
#'
#' # --- rbind ---
#' imp1 <- mice(nhanes[1:13, ], m = 2, maxit = 1, print = FALSE)
#' imp5 <- mice(nhanes[1:13, ], m = 2, maxit = 2, print = FALSE)
#' mylist <- list(age = NA, bmi = NA, hyp = NA, chl = NA)
#'
#' nrow(complete(rbind(imp1, imp5)))
#' nrow(complete(rbind(imp1, mylist)))
#'
#' nrow(complete(rbind(imp1, data.frame(mylist))))
#' nrow(complete(rbind(imp1, complete(imp5))))
#' @export
cbind <- function(...) {
  if (is.null(attr(list(...)[[1]], "class"))) {
    return(base::cbind(...))
  }
  if ("mids" %in% attr(list(...)[[1]], "class")) {
    cbind.mids(...)
  } else {
    base::cbind(...)
  }
}

#' @rdname cbind
#' @export
rbind <- function(...) {
  if (is.null(attr(list(...)[[1]], "class"))) {
    return(base::rbind(...))
  }
  if ("mids" %in% attr(list(...)[[1]], "class")) {
    rbind.mids(...)
  } else {
    base::rbind(...)
  }
}
