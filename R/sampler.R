sampler <- function(data, m, ignore, where, imp, blocks, method,
                    visitSequence, predictorMatrix, formulas,
                    calltype, blots,
                    post, fromto, printFlag, ...,
                    parallel = FALSE, future.packages = c("stats", "dplyr"),
                    logenv = logenv) {
  from <- fromto[1]
  to <- fromto[2]
  maxit <- to - from + 1
  r <- !is.na(data)

  chainMean <- chainVar <- initialize.chain(names(data), maxit, m)
  loggedEvents <- data.frame(it = integer(), im = integer(), dep = character(),
                             meth = character(), out = character(),
                             stringsAsFactors = FALSE)
  state <- list(it = 0, im = 0, dep = "", meth = "", log = FALSE)

  if (maxit < 1) iteration <- 0
  if (maxit >= 1) {
    if (!parallel && printFlag) cat("\n iter imp variable")

    for (k in from:to) {
      iteration <- k
      if (parallel) {
        t0 <- Sys.time()
      }

      if (!parallel) {
        for (i in seq_len(m)) {
          if (printFlag) cat("\n ", iteration, " ", i)
          for (h in visitSequence) {
            for (j in blocks[[h]]) {
              y <- data[, j]
              ry <- r[, j]
              wy <- where[, j]
              data[(!ry) & wy, j] <- imp[[j]][(!ry)[wy], i]
            }
          }
          result <- run_imputation_cycle(data, imp, r, where, i, k, visitSequence,
                                         blocks, method, calltype, formulas,
                                         predictorMatrix, blots, post,
                                         ignore, printFlag, ...)
          data <- result$data
          imp <- result$imp
        }
        k2 <- k - from + 1L
        stat <- update_chain_stats(data, imp, chainMean, chainVar, k2, m, blocks, visitSequence)
        chainMean <- stat$chainMean
        chainVar <- stat$chainVar

      } else {
        results_i <- future.apply::future_lapply(seq_len(m), function(i) {
          data_i <- data
          imp_i <- imp

          for (h in visitSequence) {
            for (j in blocks[[h]]) {
              y <- data_i[, j]
              ry <- r[, j]
              wy <- where[, j]
              data_i[(!ry) & wy, j] <- imp_i[[j]][(!ry)[wy], i]
            }
          }

          result <- run_imputation_cycle(data_i, imp_i, r, where, i, k, visitSequence,
                                         blocks, method, calltype, formulas,
                                         predictorMatrix, blots, post,
                                         ignore, printFlag = FALSE, ...)

          data_i <- result$data
          imp_i <- result$imp

          mean_i <- initialize.chain(names(data), 1, 1)[[1]]
          var_i  <- initialize.chain(names(data), 1, 1)[[1]]

          for (h in visitSequence) {
            for (j in blocks[[h]]) {
              if (!is.factor(data[, j])) {
                var_i[j] <- var(imp_i[[j]][, i], na.rm = TRUE)
                mean_i[j] <- mean(imp_i[[j]][, i], na.rm = TRUE)
              } else {
                nc <- as.integer(factor(imp_i[[j]][, i], levels = levels(data[, j])))
                var_i[j] <- var(nc, na.rm = TRUE)
                mean_i[j] <- mean(nc, na.rm = TRUE)
              }
            }
          }

          log_i <- if (exists("loggedEvents", inherits = FALSE)) get("loggedEvents", inherits = FALSE) else NULL
          list(imp = imp_i, mean = mean_i, var = var_i, log = log_i)
        },
        future.packages = future.packages,
        future.globals = list(run_imputation_cycle = run_imputation_cycle,
                              update_chain_stats = update_chain_stats),
        future.seed = TRUE)

        t1 <- Sys.time()
        if (printFlag) cat(sprintf("\n iter %d (%s)", k, format(round(difftime(t1, t0), 2))))

        k2 <- k - from + 1L
        for (i in seq_len(m)) {
          imp_i <- results_i[[i]]$imp
          for (j in names(imp_i)) {
            imp[[j]][, i] <- imp_i[[j]][, i]
          }
          mean_i <- results_i[[i]]$mean
          var_i  <- results_i[[i]]$var
          for (j in names(mean_i)) {
            if (j %in% dimnames(chainMean)[[1]]) {
              chainMean[j, k2, i] <- mean_i[[j]]
              chainVar[j, k2, i] <- var_i[[j]]
            }
          }
          if (!is.null(results_i[[i]]$log)) {
            loggedEvents <- rbind(loggedEvents, results_i[[i]]$log)
            state$log <- TRUE
          }
        }
      }
    }

    if (!parallel && printFlag) {
      if (state$log && any(grepl("A ridge penalty", loggedEvents$out))) {
        cat("\n * Please inspect the loggedEvents \n")
      } else {
        cat("\n")
      }
    }
  }

  list(iteration = maxit, imp = imp, chainMean = chainMean, chainVar = chainVar)
}

run_imputation_cycle <- function(data, imp, r, where, i, k, visitSequence,
                                 blocks, method, calltype, formulas, predictorMatrix,
                                 blots, post, ignore, printFlag, ...) {
  for (h in visitSequence) {
    ct <- calltype[[h]]
    b <- blocks[[h]]
    ff <- if (ct == "formula") formulas[[h]] else NULL
    pred <- predictorMatrix[h, ]
    user <- blots[[h]]
    theMethod <- method[h]
    empt <- theMethod == ""
    univ <- !empt && !is.passive(theMethod) && !handles.format(paste0("mice.impute.", theMethod))
    mult <- !empt && !is.passive(theMethod) && handles.format(paste0("mice.impute.", theMethod))
    pass <- !empt && is.passive(theMethod) && length(b) == 1

    if (printFlag & !empt) cat(" ", b)

    if (univ) {
      for (j in b) {
        imp[[j]][, i] <- sampler.univ(
          data = data, r = r, where = where,
          pred = pred, formula = ff,
          method = theMethod,
          yname = j, k = k,
          ct = ct, user = user, ignore = ignore, ...
        )
        data[(!r[, j]) & where[, j], j] <- imp[[j]][(!r[, j])[where[, j]], i]
        if (post[j] != "") {
          eval(parse(text = post[j]))
          data[(!r[, j]) & where[, j], j] <- imp[[j]][(!r[, j])[where[, j]], i]
        }
      }
    }

    if (mult) {
      mis <- !r
      mis[, setdiff(colnames(data), b)] <- FALSE
      data[mis] <- NA
      fm <- paste("mice.impute", theMethod, sep = ".")
      imputes <- switch(ct,
                        formula = do.call(fm, list(data = data, formula = ff, ...)),
                        pred = do.call(fm, list(data = data, type = pred, ...)),
                        stop("Cannot call function of type ", ct)
      )
      for (j in names(imputes)) {
        imp[[j]][, i] <- imputes[[j]]
        data[!r[, j], j] <- imp[[j]][, i]
      }
    }

    if (pass) {
      for (j in b) {
        wy <- where[, j]
        ry <- r[, j]
        imp[[j]][, i] <- model.frame(as.formula(theMethod), data[wy, ], na.action = na.pass)
        data[(!ry) & wy, j] <- imp[[j]][(!ry)[wy], i]
      }
    }
  }

  list(data = data, imp = imp)
}

update_chain_stats <- function(data, imp, chainMean, chainVar, k2, m, blocks, visitSequence) {
  for (h in visitSequence) {
    for (j in blocks[[h]]) {
      for (i in seq_len(m)) {
        if (!is.factor(data[, j])) {
          chainVar[j, k2, i] <- var(imp[[j]][, i], na.rm = TRUE)
          chainMean[j, k2, i] <- mean(imp[[j]][, i], na.rm = TRUE)
        } else {
          nc <- as.integer(factor(imp[[j]][, i], levels = levels(data[, j])))
          chainVar[j, k2, i] <- var(nc, na.rm = TRUE)
          chainMean[j, k2, i] <- mean(nc, na.rm = TRUE)
        }
      }
    }
  }
  list(chainMean = chainMean, chainVar = chainVar)
}


