#' Missing data pattern - extended customisation
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
#' values are coded as `NA`'s.
#' @param plot Should the missing data pattern be made into a plot. Default is
#' `plot = TRUE`.
#' @param rotate.names Rotation of variables names. Default is `rotate.names = 0`.
#' @param rotate.var Rotation of variable counts. Default is `rotate.var = FALSE`.
#' @param order Should the plot be presented in a specific order ?
#' Valid arguments are `"pat.increase"` and `"pat.decrease"` to order values
#' of the patterns on the right of the graph, or `"ind.increase"` and `"ind.decrease"`
#' to order number of individuals on the left of the graph. Default is `order = "none"`.
#' @param min.ind The minimum number of individuals for a pattern to be displayed on the plot.
#' Default is `NULL`.
#' @param drop.zero.vars Should columns without any missing value be plotted ?
#' Default is `drop.zero.vars = FALSE`.
#' @param nb.pat Should number of variables per pattern be displayed on the
#' right side of the graph ? Default is `nb.pat = TRUE`.
#' @param nb.var Should number of individuals presenting missing values per
#' variable be displayed at the bottom of the graph ? Default is `nb.var = TRUE`.
#' @param nb.ind Should number of individuals per pattern be displayed at the
#' left of the graph ? Default is `nb.ind = TRUE`.
#' @param tot.mis Should total number of missing values
#' be displayed at the bottom-right corner of the graph ? Default is `tot.mis = TRUE`.
#' @param tot.ind Should total number of individuals with at least one missing
#' value be displayed at the bottom-left corner of the graph ? Default is `tot.ind = FALSE`.
#' @param names A char vector with customized colnames. Default is `names = colnames(x)`.
#' @param colors Specify colors you want to display. Default is
#' `colors = c("#006CC2B3", "#B61A51B3")`.
#'
#' @return A matrix with \code{ncol(x)+1} columns, in which each row corresponds
#' to a missing data pattern (1 = observed, 0 = missing). Rows and columns are
#' sorted in increasing amounts of missing information. The last column and row
#' contain row and column counts, respectively.
#' @author BRIER Mathis, 2025, based on the original mice::md.pattern by Gerko Vink
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
#' #   25  0   8   9  10 27
#' @export
md.pattern2 <- function(x, plot = TRUE, rotate.names = 0, rotate.var = FALSE, order = "none", min.ind = NULL, drop.zero.vars = FALSE, nb.pat = TRUE, nb.var = TRUE, nb.ind = TRUE, tot.mis = TRUE, tot.ind = FALSE, names = colnames(x), colors = c("#006CC2B3", "#B61A51B3")) {
  if (!(is.matrix(x) || is.data.frame(x))) {
    stop("Data should be a matrix or dataframe")
  }
  if (ncol(x) < 2) {
    stop("Data should have at least two columns")
  }
  if(length(names) != ncol(x)) {
    stop("Argument `names` should be the same size as the data")
  }
  if(!all(order %in% c("pat.increase", "pat.decrease", "ind.increase", "ind.decrease", "none"))) {
    warning("Unknown order specified, data won't be ordered")
  }
  
  R <- is.na(x)
  colnames(R) <- names
  nmis <- colSums(R)
  
  # sort columnwise and keep track of names
  R <- matrix(R[, order(nmis)], dim(x))
  current.names <- names[order(nmis)]
  pat <- apply(R, 1, function(x) paste(as.numeric(x), collapse = ""))
  
  # Apply min.ind filter if specified
  if(!is.null(min.ind)) {
    pat.counts <- table(pat)
    keep.patterns <- names(pat.counts)[as.numeric(pat.counts) > min.ind]
    if(length(keep.patterns) > 0) {
      # Filter to keep only desired patterns
      keep.idx <- pat %in% keep.patterns
      x <- x[keep.idx, ]
      R <- R[keep.idx, ]
      pat <- pat[keep.idx]
    } else {
      warning("No patterns meet the min.ind criterion")
      return(invisible(NULL))
    }
  }
  
  # Apply drop.zero.vars if TRUE
  if(drop.zero.vars) {
    zero.vars <- which(colSums(R) == 0)
    
    if(length(zero.vars) > 0) {
      # Remove columns with 0 missing values from the filtered data
      R <- R[, -zero.vars, drop = FALSE]
      # Remove corresponding names
      current.names <- current.names[-zero.vars]
      x <- x[, -zero.vars, drop = FALSE]
    }
  }
  
  # sort rowwise
  sortR <- matrix(R[order(pat), ], dim(R))
  
  if (nrow(x) == 1) {
    mpat <- is.na(x)
  } else {
    mpat <- sortR[!duplicated(sortR), ]
  }
  
  # In case of a single pattern, prevent mpat being a vector
  if (is.null(dim(mpat))) {
    mpat <- matrix(mpat, nrow = 1)
  }
  
  if (all(!R)) {
    cat(" /\\     /\\\n{  `---'  }\n{  O   O  }\n==>  V <==")
    cat("  No need for mice. This data set is completely observed.\n")
    cat(" \\  \\|/  /\n  `-----'\n\n")
    plot <- F
    return(invisible(NULL))
  }

  rownames(mpat) <- as.character(table(pat))

  r <- cbind(abs(mpat - 1), rowSums(mpat))
  if(order == "pat.decrease" && nrow(mpat) > 1) {
    r <- r[order(r[, ncol(r)], decreasing = TRUE), ]
  }
  
  if(order == "pat.increase" && nrow(mpat) > 1) {
    r <- r[order(r[, ncol(r)], decreasing = FALSE), ]
  }
  
  nmis <- colSums(R)
  names(nmis) <- current.names
  r <- rbind(r, c(nmis, sum(nmis)))
  
  if(order == "ind.decrease"){
    r <- r[order(as.numeric(rownames(r)), decreasing = TRUE), ]
  }
  if(order == "ind.increase"){
    r <- r[order(as.numeric(rownames(r)), decreasing = FALSE), ]
  }
  rownames(r)[nrow(r)] <- sum(as.numeric(rownames(r)[1:nrow(r)-1]))
  
  if (plot) {
    op <- par(mar = rep(0, 4))
    on.exit(par(op))
    plot.new()
    
    # Ensuring the plot is based on a matrix when there is only 1 row
    if (nrow(r) > 1) {
      R.plot <- r[1:(nrow(r) - 1), 1:(ncol(r) - 1)]
    } else {
      R.plot <- matrix(r[1, 1:(ncol(r) - 1)], nrow = 1)
    }
    
    adj <- c(0.5 - (rotate.names / 200 + 0.05), (rotate.names / 200 + 0.05))
    srt <- rotate.names
    
    # If there is only one pattern
    one.line <- is.numeric(R.plot) && !is.matrix(R.plot)
    R.ncol <- ifelse(one.line, length(R.plot), ncol(R.plot))
    R.nrow <- ifelse(one.line, 1, nrow(R.plot))
    
    plot.window(
      xlim = c(-1, R.ncol + 1),
      ylim = c(-1, R.nrow + (max(nchar(current.names)) / 2.6)),
      asp = 1)
    
    if (one.line) {
      # For single row, create coordinates
      M <- cbind(rep(0, length(R.plot)), seq(0, length(R.plot) - 1))
      shade <- ifelse(R.plot[1:length(R.plot)], colors[1], colors[2])
    } else {
      M <- cbind(c(row(R.plot)), c(col(R.plot))) - 1
      shade <- ifelse(R.plot[nrow(R.plot):1, ], colors[1], colors[2])
    }
    
    rect(M[, 2], M[, 1], M[, 2] + 1, M[, 1] + 1, col = shade)
    
    for (i in 1:R.ncol) {
      text(i - 0.5, R.nrow + .3, current.names[i], adj = adj, srt = srt)
      if (nb.var) {
        if(rotate.var){
          text(i - .5, -.3, r[nrow(r), i], adj = 1, srt = 90)
        } else {
          text(i - .5, -.3, r[nrow(r), i])
        }
      }
    }
    
    for (i in 1:R.nrow) {
      if(nb.pat) {
        text(R.ncol + .3, i -.5, r[(nrow(r) - 1):1, ncol(r)][i], adj = 0)
      }
      if(nb.ind) {
        text(-.3, i -.5, rownames(r)[(nrow(r) - 1):1][i], adj = 1)
      }
    }
    
    if(tot.mis) {
      text(R.ncol + .3, -.5, r[nrow(r), ncol(r)], adj = 0)
    }
    
    if(tot.ind) {
      text(-.3, -.5, rownames(r)[nrow(r)], adj = 1)
    }
    
    return(r)
  } else {
    return(r)
  }
}
