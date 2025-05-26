# The sampler controls the actual Gibbs sampling iteration scheme.
# This function is called by mice and mice.mids
sampler <- function(data, m, ignore, where, imp, blocks, method,
                    visitSequence, predictorMatrix, formulas,
                    calltype, blots,
                    post, fromto, printFlag, ...) {
  from <- fromto[1]
  to <- fromto[2]
  maxit <- to - from + 1
  r <- !is.na(data)

  # set up array for convergence checking
  chainMean <- chainVar <- initialize.chain(names(data), maxit, m)

  ## THE MAIN LOOP: GIBBS SAMPLER ##
  if (maxit < 1) iteration <- 0
  if (maxit >= 1) {
    if (printFlag) {
      cat("\n iter imp variable")
    }
    for (k in from:to) {
      # begin k loop : main iteration loop
      iteration <- k
      for (i in seq_len(m)) {
        # begin i loop: repeated imputation loop
        if (printFlag) {
          cat("\n ", iteration, " ", i)
        }

        # prepare the i'th imputation
        # do not overwrite any observed data
        for (h in visitSequence) {
          for (j in blocks[[h]]) {
            y <- data[, j]
            ry <- r[, j]
            wy <- where[, j]
            data[(!ry) & wy, j] <- imp[[j]][(!ry)[wy], i]
          }
        }

        # impute block-by-block
        for (h in visitSequence) {
          ct <- calltype[[h]]
          b <- blocks[[h]]
          if (ct == "formula") ff <- formulas[[h]] else ff <- NULL
          pred <- predictorMatrix[h, ]
          user <- blots[[h]]

          # univariate/multivariate logic
          theMethod <- method[h]
          empt <- theMethod == ""
          univ <- !empt && !is.passive(theMethod) &&
            !handles.format(paste0("mice.impute.", theMethod))
          mult <- !empt && !is.passive(theMethod) &&
            handles.format(paste0("mice.impute.", theMethod))
          pass <- !empt && is.passive(theMethod) && length(blocks[[h]]) == 1
          if (printFlag & !empt) cat(" ", b)

          ## store current state
          oldstate <- get("state", pos = parent.frame())
          newstate <- list(
            it = k, im = i,
            dep = h,
            meth = theMethod,
            log = oldstate$log
          )
          assign("state", newstate, pos = parent.frame(), inherits = TRUE)

          # (repeated) univariate imputation - pred method
          if (univ) {
            for (j in b) {
              imp[[j]][, i] <-
                sampler.univ(
                  data = data, r = r, where = where,
                  pred = pred, formula = ff,
                  method = theMethod,
                  yname = j, k = k,
                  ct = ct,
                  user = user, ignore = ignore,
                  ...
                )

              data[(!r[, j]) & where[, j], j] <-
                imp[[j]][(!r[, j])[where[, j]], i]

              # optional post-processing
              cmd <- post[j]
              if (cmd != "") {
                eval(parse(text = cmd))
                data[(!r[, j]) & where[, j], j] <-
                  imp[[j]][(!r[, j])[where[, j]], i]
              }
            }
          }

          # multivariate imputation - pred and formula
          if (mult) {
            mis <- !r
            mis[, setdiff(colnames(data), b)] <- FALSE
            data[mis] <- NA

            fm <- paste("mice.impute", theMethod, sep = ".")
            if (ct == "formula") {
              imputes <- do.call(fm, args = list(
                data = data,
                formula = ff, ...
              ))
            } else if (ct == "pred") {
              imputes <- do.call(fm, args = list(
                data = data,
                type = pred, ...
              ))
            } else {
              stop("Cannot call function of type ", ct,
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
              imp[[j]][, i] <- imputes[[j]]
              data[!r[, j], j] <- imp[[j]][, i]
            }
          }

          # passive imputation
          # applies to all rows, so no ignore needed
          if (pass) {
            for (j in b) {
              wy <- where[, j]
              ry <- r[, j]
              imp[[j]][, i] <- model.frame(as.formula(theMethod), data[wy, ],
                na.action = na.pass
              )
              data[(!ry) & wy, j] <- imp[[j]][(!ry)[wy], i]
            }
          }
        } # end h loop (blocks)
      } # end i loop (imputation number)

      # store means and sd of m imputes
      k2 <- k - from + 1L
      if (length(visitSequence) > 0L) {
        for (h in visitSequence) {
          for (j in blocks[[h]]) {
            if (!is.factor(data[, j])) {
              chainVar[j, k2, ] <- apply(imp[[j]], 2L, var, na.rm = TRUE)
              chainMean[j, k2, ] <- colMeans(as.matrix(imp[[j]]), na.rm = TRUE)
            }
            if (is.factor(data[, j])) {
              for (mm in seq_len(m)) {
                nc <- as.integer(factor(imp[[j]][, mm], levels = levels(data[, j])))
                chainVar[j, k2, mm] <- var(nc, na.rm = TRUE)
                chainMean[j, k2, mm] <- mean(nc, na.rm = TRUE)
              }
            }
          }
        }
      }
    } # end main iteration

    if (printFlag) {
      r <- get("loggedEvents", parent.frame(1))
      ridge.used <- any(grepl("A ridge penalty", r$out))
      if (ridge.used) {
        cat("\n * Please inspect the loggedEvents \n")
      } else {
        cat("\n")
      }
    }
  }
  list(iteration = maxit, imp = imp, chainMean = chainMean, chainVar = chainVar)
}


sampler.univ <- function(data, r, where, pred, formula, method, yname, k,
                         ct = "pred", user, ignore, ...) {
  j <- yname[1L]

  if (ct == "pred") {
    vars <- colnames(data)[pred != 0]
    xnames <- setdiff(vars, j)
    if (length(xnames) > 0L) {
      formula <- reformulate(backticks(xnames), response = backticks(j))
      formula <- update(formula, ". ~ . ")
    } else {
      formula <- as.formula(paste0(j, " ~ 1"))
    }
  }

  if (ct == "formula") {
    # move terms other than j from lhs to rhs
    ymove <- setdiff(lhs(formula), j)
    formula <- update(formula, paste(j, " ~ . "))
    if (length(ymove) > 0L) {
      formula <- update(formula, paste("~ . + ", paste(backticks(ymove), collapse = "+")))
    }
  }

  # get the model matrix
  x <- obtain.design(data, formula)

  # expand pred vector to model matrix, remove intercept
  if (ct == "pred") {
    type <- pred[labels(terms(formula))][attr(x, "assign")]
    x <- x[, -1L, drop = FALSE]
    names(type) <- colnames(x)
  }
  if (ct == "formula") {
    x <- x[, -1L, drop = FALSE]
    type <- rep(1L, length = ncol(x))
    names(type) <- colnames(x)
  }

  # define y, ry and wy
  y <- data[, j]
  ry <- complete.cases(x, y) & r[, j] & !ignore
  wy <- complete.cases(x) & where[, j]

  # nothing to impute
  if (all(!wy)) {
    return(numeric(0))
  }

  cc <- wy[where[, j]]
  if (k == 1L) check.df(x, y, ry)

  # remove linear dependencies
  keep <- remove.lindep(x, y, ry, ...)
  x <- x[, keep, drop = FALSE]
  type <- type[keep]
  if (ncol(x) != length(type)) {
    stop("Internal error: length(type) != number of predictors")
  }

  # here we go
  f <- paste("mice.impute", method, sep = ".")
  imputes <- data[wy, j]
  imputes[!cc] <- NA

  args <- c(list(y = y, ry = ry, x = x, wy = wy, type = type), user, list(...))
  imputes[cc] <- do.call(f, args = args)
  imputes
}
