worker <- function(i, data, imp, r, where, k, visitSequence,
                   blocks, method, calltypes, formulas,
                   predictorMatrix, blots,
                   tasks, models,
                   post = NULL, ignore = NULL, ...) {
  loggedEvents <- NULL
  result <- one.cycle(data, imp, r, where, i, k, visitSequence,
                      blocks, method, calltypes, formulas,
                      predictorMatrix, blots,
                      tasks, models,
                      post, ignore, printFlag = FALSE, ...)

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

  list(imp = imp_i, mean = mean_i, var = var_i, loggedEvents = loggedEvents)
}

one.cycle <- function(data, imp, r, where, i, k, visitSequence,
                      blocks, method, calltypes, formulas, predictorMatrix,
                      blots, tasks, models, post, ignore, printFlag, ...) {
  # initialize data with the i-th imputation
  # do not overwrite observed data
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
    calltype <- calltypes[[h]]
    b <- blocks[[h]]
    ff <- if (calltype == "formula") formulas[[h]] else NULL
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

    # (repeated) univariate imputation - pred method
    if (univ) {
      for (j in b) {
        # miceadds support
        newstate <- list(it = k, im = i, dep = j, meth = theMethod)
        state <- newstate
        # if m outruns m.train, recycle m.train
        m.train <- length(models[[j]])
        mod <- (i - 1L) %% m.train + 1L
        imp[[j]][, i] <-
          sampler.univ(
            data = data, r = r, where = where,
            pred = pred, formula = ff,
            method = theMethod,
            task = tasks[j],
            model = models[[j]][[as.character(mod)]],
            yname = j, k = k,
            calltype = calltype,
            user = user, ignore = ignore,
            ...
          )

        # update data
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
      # miceadds support
      newstate <- list(it = k, im = i, dep = b, meth = theMethod)
      state <- newstate
      mis <- !r
      mis[, setdiff(colnames(data), b)] <- FALSE
      data[mis] <- NA

      fm <- paste("mice.impute", theMethod, sep = ".")
      imputes <- switch(calltype,
                        formula = do.call(fm, list(data = data, formula = ff, ...)),
                        pred = do.call(fm, list(data = data, type = pred, ...)),
                        stop("Cannot call function of type ", calltype))

      # Abort if imputes is NULL
      if (is.null(imputes)) {
        stop("No imputations from ", theMethod, h)
      }

      # Update imputations and data
      for (j in names(imputes)) {
        imp[[j]][, i] <- imputes[[j]]
        data[!r[, j], j] <- imp[[j]][, i]
      }
    }
    # passive imputation
    # applies to all rows, so no ignore needed
    if (pass) {
      for (j in b) {
        # miceadds support
        newstate <- list(it = k, im = i, dep = b, meth = theMethod)
        state <- newstate
        wy <- where[, j]
        ry <- r[, j]
        imp[[j]][, i] <- model.frame(as.formula(theMethod), data[wy, ],
                                     na.action = na.pass)
        data[(!ry) & wy, j] <- imp[[j]][(!ry)[wy], i]
      }
    }
  } # end h loop (blocks)

  list(data = data, imp = imp)
}

get.chain.stats <- function(data, imp, chainMean, chainVar, k2, m, blocks, visitSequence) {
  # Updates mean and variance of imputed values
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

collect_logs <- function(path = tempdir(), pattern = "^log_it\\d+_im\\d+\\.rds$",
                         remove = TRUE, verbose = FALSE) {
  log_files <- list.files(path, pattern = pattern, full.names = TRUE)

  if (length(log_files) == 0L) {
    if (verbose) message("No log files found.")
    return(NULL)
  }

  logs <- lapply(log_files, function(file) {
    tryCatch(
      readRDS(file),
      error = function(e) {
        if (verbose) warning("Failed to read log file: ", file)
        NULL
      }
    )
  })

  logs <- do.call(rbind, logs[!vapply(logs, is.null, logical(1))])

  if (remove) {
    file.remove(log_files)
  }

  logs
}

emit_worker_log <- function(log_entry, file) {
  saveRDS(log_entry, file = file)
}

