# The sampler controls the actual Gibbs sampling iteration scheme.
# This function is called by mice and mice.mids
sampler <- function(data, m, ignore, where, imp, blocks, method,
                    visitSequence, predictorMatrix, formulas,
                    modeltype, blots,
                    post, fromto, printFlag, ...) {

  # create long matrix with m imputations
  long <- rbindlist(replicate(m, data, simplify = FALSE))
  # set(long, j = ".imp", value = rep(seq_len(m), each = nrow(data)))
  r <- !is.na(data)

  # replace NA values that should be imputed
  impute.long(long, imp, where, vars = names(data), r)

  # initialize iterations
  from <- fromto[1L]
  to <- fromto[2L]
  maxit <- to - from + 1L
  chainMean <- chainVar <- initialize.chain(names(data), maxit, m)

  # early exit if no iterations are requested
  if (maxit < 1) {
    return(list(iteration = maxit,
                imp = imp,
                chainMean = chainMean,
                chainVar = chainVar))
  }

  ## THE MAIN LOOP: GIBBS SAMPLER ##
  if (printFlag) cat("\n iter variable")
  for (k in from:to) {
    # begin k loop : main iteration loop
    if (printFlag) cat("\n", k)

    # impute block-by-block
    for (h in visitSequence) {
      # univariate/multivariate logic
      theMethod <- method[h]
      empt <- theMethod == ""
      univ <- !empt && !is.passive(theMethod) &&
        !handles.format(paste0("mice.impute.", theMethod))
      mult <- !empt && !is.passive(theMethod) &&
        handles.format(paste0("mice.impute.", theMethod))
      pass <- !empt && is.passive(theMethod) && length(blocks[[h]]) == 1L
      if (printFlag & !empt) cat(" ", blocks[[h]])

      # store current state
      oldstate <- get("state", pos = parent.frame())
      newstate <- list(
        it = k, im = 1L,dep = h,
        meth = theMethod ,
        log = oldstate$log
      )
      assign("state", newstate, pos = parent.frame(), inherits = TRUE)

      mtype <- modeltype[[h]]
      b <- blocks[[h]]

      # (repeated) univariate imputation - pred method
      if (univ) {
        for (j in b) {
          imputations <-
            sampler.univ(
              data = long,
              r = r,
              where = where,
              pred = predictorMatrix[h, ],
              formula = formulas[[h]],
              method = theMethod,
              j = j,
              k = k,
              mtype = mtype,
              user = blots[[h]],
              ignore = ignore,
              jimp = imp[[j]],
              ...
            )
          # imp[[j]] is updated by sampler.univ()
          # optonal post-processing command to alter imp[[j]]
          if (post[j] != "") {
            eval(parse(text = post[j]))
          }

          # update long for next j
          # for rows with missing values in j and with imputations available
          # update column j with the new values in imp[[j]]
          impute.long(long = long, imp = imp, where = where, vars = j, r = r)
        }
      }

      # multivariate imputation - pred and formula
      if (mult) {
        mis <- !r
        for (i in seq_len(m)) {
          idx <- ((i - 1L) * nrow(data) + 1L):(i * nrow(data))
          datai <- long[idx, ]
          mis[, setdiff(colnames(datai), b)] <- FALSE
          datai[mis] <- NA

          fm <- paste("mice.impute", theMethod, sep = ".")
          if (mtype == "formula") {
            imputes <- do.call(fm, args = list(
              data = datai,
              formula = formulas[[h]],
              ...
            ))
          } else if (mtype == "pred") {
            imputes <- do.call(fm, args = list(
              data = datai,
              type = predictorMatrix[h, ],
              ...
            ))
          } else {
            stop("Cannot call function of type ", mtype,
                 call. = FALSE
            )
          }
          if (is.null(imputes)) {
            stop("No imputations from ", theMethod,
                 h,
                 call. = FALSE
            )
          }

          for (j in names(imputes)) {
            set(imp[[j]], j = i, value = imputes[[j]])
          }
        } # end i loop

        for (j in names(imputes)) {
          impute.long(long = long, imp = imp, where = where, vars = j, r = r)
        }
      } # end mult

      # Passive imputation
      if (pass) {
        row_offset <- (0L:(m - 1L)) * nrow(data)
        for (j in b) {
          row_ids <- imp[[j]][["row_id"]]
          rows_in_long <- rep(row_ids, m) + rep(row_offset, each = length(row_ids))
          val <- as.numeric(model.frame(as.formula(theMethod),
                                        long[rows_in_long, ],
                                        na.action = na.pass
          )[[1L]])
          split_vals <- split(val, rep(seq_len(m), each = length(row_ids)))
          set(imp[[j]], j = seq_len(m), value = as.data.table(split_vals))
          impute.long(long = long, imp = imp, where = where, vars = j, r = r)
        }
      }

      if (empt) {
        for (j in b) {
          impute.long(long = long, imp = imp, where = where, vars = j, r = r)
        }
      }
    } # end h loop (blocks)

    # store means and sd of m imputes
    k2 <- k - from + 1L
    if (length(visitSequence) > 0L) {
      for (h in visitSequence) {
        for (j in blocks[[h]]) {
          if (!is.factor(data[, j])) {
            chainVar[j, k2, ] <- apply(imp[[j]], 2L, var, na.rm = TRUE)[1:m]
            chainMean[j, k2, ] <- colMeans(as.matrix(imp[[j]]), na.rm = TRUE)[1:m]
          }
          if (is.factor(data[, j])) {
            for (mm in seq_len(m)) {
              nc <- as.integer(factor(imp[[j]][, mm, with = FALSE], levels = levels(data[, j])))
              chainVar[j, k2, mm] <- var(nc, na.rm = TRUE)
              chainMean[j, k2, mm] <- mean(nc, na.rm = TRUE)
            }
          }
        }
      }
    }
  } # end k loop, end main iteration

  if (printFlag) {
    r <- get("loggedEvents", parent.frame(1))
    ridge.used <- any(grepl("A ridge penalty", r$out))
    if (ridge.used) {
      cat("\n * Please inspect the loggedEvents \n")
    } else {
      cat("\n")
    }
  }

  list(iteration = maxit,
       imp = imp,
       chainMean = chainMean,
       chainVar = chainVar)
}


sampler.univ <- function(data, r, where, pred, formula, method, j, k,
                            mtype = "pred", user, ignore, jimp, ...) {
  # nothing to impute
  if (!nrow(jimp)) {
    return(jimp)
  }

  # handle model terms
  if (mtype == "pred") {
    vars <- names(pred)[pred != 0]
    xnames <- setdiff(vars, j)
    if (length(xnames) > 0L) {
      formula <- reformulate(backticks(xnames), response = backticks(j))
      formula <- update(formula, ". ~ . ")
    } else {
      formula <- as.formula(paste0(j, " ~ 1"))
    }
  } else if (mtype == "formula") {
    # move terms other than j from lhs to rhs
    ymove <- setdiff(lhs(formula), j)
    formula <- update(formula, paste(j, " ~ . "))
    if (length(ymove) > 0L) {
      formula <- update(formula, paste("~ . + ", paste(backticks(ymove), collapse = "+")))
    }
  } else {
    stop("Unknown model type ", mtype, " for variable ", j, ".", call. = FALSE)
  }

  # get the model matrix
  x <- obtain.design(data, formula)

  # expand pred vector to model matrix, remove intercept
  if (mtype == "pred") {
    type <- pred[labels(terms(formula))][attr(x, "assign")]
    x <- x[, -1L, drop = FALSE]
    names(type) <- colnames(x)
  }
  if (mtype == "formula") {
    x <- x[, -1L, drop = FALSE]
    type <- rep(1L, length = ncol(x))
    names(type) <- colnames(x)
  }

  # define y, ry and wy

  # y: vector of target variable, including previous imputations.
  # Normally imputations are unused for model estimation (see ry, case 2),
  # but they may have a role for creating imputations for MNAR scenarios
  y <- data[[j]]

  # ry: logical vector that defines the subset of y to which the imputation
  # model is fitted. The subset is the combinations of three conditions:
  # 1) incomplete cases in x, y or both are excluded
  # 2) cases with missing values in y are excluded (prevent double dipping)
  # 3) cases that the user wants to ignore are excluded
  ry <- complete.cases(x, y) & r[, j] & !ignore

  # wy: logical vector that defines the subset of y that is imputed. It
  # is the combination of two conditions:
  # 1) cases with incomplete x are not imputed (and set to NA, see below)
  # 2) cases the user excluded for imputation are not imputed
  wy <- complete.cases(x) & where[, j]

  # cc: logical vector indicating x-complete cases that are imputed.
  # Imputations for x-incomplete cases x are set to NA.
  cc <- wy[where[, j]]
  imputes <- unlist(jimp[, setdiff(names(jimp), "row_id"), with = FALSE], use.names = FALSE)

  # imputes <- data[wy, get(j)]

  # from here on, code is specific to imputation group
  # wrapper for mice.impute
  for (i in seq_len(ncol(jimp) - 1L)) {

    # data for imputation i
    m <- ncol(jimp) - 1L
    n <- nrow(x) / m
    ncc <- length(cc) / m
    idx <- seq_len(n) + (i - 1) * n
    idx2 <- seq_len(ncc) + (i - 1) * ncc
    xi <- x[idx, , drop = FALSE]
    yi <- y[idx]
    ryi <- ry[idx]
    wyi <- wy[idx]
    typei <- type
    cci <- cc[idx2]

    # only warning on df in first iteration
    if (k == 1L) check.df(xi, yi, ryi)

    # remove linear dependencies
    keep <- remove.lindep(xi, yi, ryi, ...)
    xi <- xi[, keep, drop = FALSE]
    typei <- typei[keep]
    if (ncol(xi) != length(typei)) {
      stop("Internal error: length(type) != number of predictors")
    }

    # here we go
    f <- paste("mice.impute", method, sep = ".")
    imputes <- unlist(jimp[, i, with = FALSE], use.names = FALSE)
    imputes[!cci] <- NA

    args <- c(list(y = yi, ry = ryi, x = xi, wy = wyi, type = typei), user, list(...))
    imputes[cci] <- do.call(f, args = args)
    set(jimp, j = i, value = imputes)
  }

  return(jimp)
}
