#' Missing data pattern
#'
#' Display missing-data patterns.
#'
#' This function is useful for investigating any structure of missing
#' observations in the data. In specific case, the missing data pattern could be
#' (nearly) monotone. Monotonicity can be used to simplify the imputation model.
#' See Schafer (1997) for details. Also, the missing pattern could suggest which
#' variables could potentially be useful for imputation of missing entries.
#'
#' @param x A data frame or a matrix containing the incomplete data. Missing
#' values are coded as NA's.
#' @param plot Should the missing data pattern be made into a plot. Default is
#' `plot = TRUE`.
#' @param rotate.names Rotation of variables names. Default is `rotate.names = 0`.
#' @param pattern.ord Should patterns be presented by increasing order ? Then `nb.pat`
#' is sorted. Default is `pattern.ord = FALSE`.
#' @param min.ind The minimum number of individuals for a pattern to be displayed on the plot.
#' Default is NULL.
#' @param nb.pat Should number of variables per pattern be displayed on the
#' right side of the graph ? Default is `nb.pat = TRUE`.
#' @param nb.var Should number of individuals presenting missing values per
#' variable be displayed at the bottom of the graph ? Automatically sets
#' `nb.tot = FALSE`. Default is `nb.var = TRUE`.
#' @param nb.ind Should number of individuals per pattern be displayed at the
#' left of the graph ? Default is `nb.ind = TRUE`.
#' @param nb.tot Should total number of missing values
#' be displayed at the bottom-right corner of the graph ? Default is `nb.tot = TRUE`.
#' @param names A char vector with customized colnames. Default is `names = colnames(x)`.
#' @param colors Specify colors you want to display. Default is
#' `colors = c("#006CC2B3", "#B61A51B3")`.

#' @return A matrix with \code{ncol(x)+1} columns, in which each row corresponds
#' to a missing data pattern (1=observed, 0=missing). Rows and columns are
#' sorted in increasing amounts of missing information. The last column and row
#' contain row and column counts, respectively.
#' @author Mathis Brier, 2025, based on the original mice::md.pattern by Gerko Vink
#' in 2018 and previous work of Stef van Buuren, Karin Groothuis-Oudshoorn, 2000
#' @references Schafer, J.L. (1997), Analysis of multivariate incomplete data.
#' London: Chapman&Hall.
#'
#' Van Buuren, S., Groothuis-Oudshoorn, K. (2011). \code{mice}: Multivariate
#' Imputation by Chained Equations in \code{R}. \emph{Journal of Statistical
#' Software}, \bold{45}(3), 1-67. \doi{10.18637/jss.v045.i03}
#' @keywords univar
#' @examples
#' md.pattern(nhanes)
#' #     age hyp bmi chl
#' #  13   1   1   1   1  0
#' #   1   1   1   0   1  1
#' #   3   1   1   1   0  1
#' #   1   1   0   0   1  2
#' #   7   1   0   0   0  3
#' #   0   8   9  10 27
#' @export
md.pattern <- function(x, plot = TRUE, rotate.names = 0, pattern.ord = FALSE, min.ind = NULL, nb.pat = TRUE, nb.var = TRUE, nb.ind = TRUE, nb.tot = TRUE, names = colnames(x), colors = c("#006CC2B3", "#B61A51B3")) {
  if (!(is.matrix(x) || is.data.frame(x))) {
    stop("Data should be a matrix or dataframe")
  }
  if (ncol(x) < 2) {
    stop("Data should have at least two columns")
  }
  if(length(names) != ncol(x)) { # Checking consistency of @param names
    stop("Argument `names` should be the same size as the data")
  }
  if(!nb.var) { # to consistently remove the bottom numbers
    nb.tot <- FALSE
  }
  R <- is.na(x)
  colnames(R) <- names # Assign names if provided
  nmis <- colSums(R)
  # sort columnwise
  R <- matrix(R[, order(nmis)], dim(x))
  pat <- apply(R, 1, function(x) paste(as.numeric(x), collapse = ""))
  # sort rowwise
  sortR <- matrix(R[order(pat), ], dim(x))
  if (nrow(x) == 1) {
    mpat <- is.na(x)
  } else {
    mpat <- sortR[!duplicated(sortR), ]
  }
  # update row and column margins
  if (all(!is.na(x))) {
    cat(" /\\     /\\\n{  `---'  }\n{  O   O  }\n==>  V <==")
    cat("  No need for mice. This data set is completely observed.\n")
    cat(" \\  \\|/  /\n  `-----'\n\n")
    mpat <- t(as.matrix(mpat, byrow = TRUE))
    rownames(mpat) <- table(pat)
  } else {
    if (is.null(dim(mpat))) {
      mpat <- t(as.matrix(mpat))
    }
    rownames(mpat) <- table(pat)
  }
  if(!is.null(min.ind)) { # drop rows without enough individuals
    mpat <- mpat[as.numeric(rownames(mpat)) > min.ind, ]
  }
  r <- cbind(abs(mpat - 1), rowSums(mpat))
  if(pattern.ord) { # order rows by number of missing variables
    r <- r[order(r[, ncol(r)], decreasing = FALSE), ]
  }  # binding then to be resilient if sum(nmis) would be smaller than max(rowSums(mpat))
  r <- rbind(r, c(nmis[order(nmis)], sum(nmis)))
  if (plot) {
    op <- par(mar = rep(0, 4))
    on.exit(par(op))
    plot.new()
    if (is.null(dim(sortR[!duplicated(sortR), ]))) {
      R <- t(as.matrix(r[1:nrow(r) - 1, 1:ncol(r) - 1]))
    } else {
      if (is.null(dim(R))) {
        R <- t(as.matrix(R))
      }
      R <- r[1:nrow(r) - 1, 1:ncol(r) - 1]
    } # modified adj to allow various angles
    adj <- c(0.5 - (rotate.names / 200 + 0.05), (rotate.names / 200 + 0.05))
    srt <- rotate.names
    length_of_longest_colname <- max(nchar(colnames(r)))
    plot.window(
      xlim = c(-1, ncol(R) + 1),
      ylim = c(-1, nrow(R) + length_of_longest_colname),
      asp = 1)
    M <- cbind(c(row(R)), c(col(R))) - 1
    shade <- ifelse(R[nrow(R):1, ], colors[1], colors[2]) # changed to allow color variation
    rect(M[, 2], M[, 1], M[, 2] + 1, M[, 1] + 1, col = shade)
    for (i in 1:ncol(R)) {
      text(i - 0.5, nrow(R) + .3, colnames(r)[i], adj = adj, srt = srt)
      if(nb.var) { # changed to allow the deletion of individuals per variable - bottom numbers
        text(i - .5, -.3, r[nrow(r), i])
      }
    }
    for (i in 1:nrow(R)) {
      if(nb.pat) { # changed to allow the deletion of number of variables per pattern - right numbers
        text(ncol(R) + .3, i - .5, r[(nrow(r) - 1):1, ncol(r)][i], adj = 0)
      }
      if(nb.ind) { # changed to allow the deletion of individuals per pattern - left numbers
        text(-.3, i - .5, rownames(r)[(nrow(r) - 1):1][i], adj = 1)
      }
    }
    if(nb.tot) { # change to allow deletion of total number of missing values - bottom-right number
      text(ncol(R) + .3, -.3, r[nrow(r), ncol(r)])
    }
    return(r)
  } else {
    return(r)
  }
}
