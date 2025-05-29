sampler <- function(data, m, ignore, where, imp, blocks, method,
                    visitSequence, predictorMatrix, formulas,
                    calltype, blots, tasks, models,
                    post, fromto, printFlag, ...,
                    parallel = FALSE, future.packages = NULL, future.seed = TRUE) {
  from <- fromto[1]
  to <- fromto[2]
  maxit <- to - from + 1
  r <- !is.na(data)

  # set up array for convergence checking
  chainMean <- chainVar <- initialize.chain(names(data), maxit, m)

  ## THE MAIN LOOP: GIBBS SAMPLER ##
  if (maxit < 1) iteration <- 0
  if (maxit >= 1) {
    if (!parallel && printFlag) {
      cat("\n iter imp variable")
    }

    for (k in from:to) {
      # begin k loop : main iteration loop
      iteration <- k
      if (parallel) {
        t0 <- Sys.time()
      }

      if (!parallel) {
        # single-threaded processing
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

          result <- one.cycle(data, imp, r, where, i, k, visitSequence,
                              blocks, method, calltype, formulas,
                              predictorMatrix, blots,
                              tasks, models,
                              post, ignore, printFlag, ...)
          data <- result$data
          imp <- result$imp
        }
        k2 <- k - from + 1L
        stat <- get.chain.stats(data, imp, chainMean, chainVar, k2, m, blocks, visitSequence)
        chainMean <- stat$chainMean
        chainVar <- stat$chainVar

      } else {
        # parallel processing with future.apply
        results_i <- future.apply::future_lapply(seq_len(m), function(i) {
          emit_worker_log <- function(log_entry, file) {
            saveRDS(log_entry, file = file)
          }

          data_i <- data
          imp_i <- imp

          # loop over blocks
          for (h in visitSequence) {
            for (j in blocks[[h]]) {
              y <- data_i[, j]
              ry <- r[, j]
              wy <- where[, j]
              data_i[(!ry) & wy, j] <- imp_i[[j]][(!ry)[wy], i]
            }
          }

          result <- one.cycle(data_i, imp_i, r, where, i, k, visitSequence,
                              blocks, method, calltype, formulas,
                              predictorMatrix, blots,
                              tasks, models,
                              post, ignore, printFlag = FALSE, ...)

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

          # Create a log record
          log.entry <- data.frame(
            it   = k, im   = i, dep  = "cycle", meth = NA_character_,
            out  = "success", msg  = "I1001", fn   = "one.cycle",
            stringsAsFactors = FALSE
          )

          logfile <- file.path(tempdir(), sprintf("log_it%02d_im%02d.rds", k, i))
          emit_worker_log(log.entry, logfile)

          list(imp = imp_i, mean = mean_i, var = var_i)
        },
        future.packages = future.packages,
        future.globals = list(initialize.chain = initialize.chain,
                              one.cycle = one.cycle,
                              get.chain.stats = get.chain.stats),
        future.seed = future.seed)

        # Combine with existing log if needed
        new.loggedEvents <- collect_logs()
        if (!is.null(new.loggedEvents)) {
          pos.loggedEvents <- parent.frame(1L)
          loggedEvents <- get("loggedEvents", envir = pos.loggedEvents)
          loggedEvents <- rbind(loggedEvents, new.loggedEvents)
          assign("loggedEvents", loggedEvents, envir = pos.loggedEvents)
        }

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
        }
      }
    } # end k loop : main iteration loop

    # if (!parallel && printFlag) {
    #   if (state$log && any(grepl("A ridge penalty", loggedEvents$out))) {
    #     cat("\n * Please inspect the loggedEvents \n")
    #   } else {
    #     cat("\n")
    #   }
    # }
  }

  list(iteration = maxit, imp = imp, chainMean = chainMean, chainVar = chainVar)
}

one.cycle <- function(data, imp, r, where, i, k, visitSequence,
                      blocks, method, calltype, formulas, predictorMatrix,
                      blots, tasks, models, post, ignore, printFlag, ...) {
  # this function makes one pass through the data

  # impute block-by-block
  for (h in visitSequence) {
    ct <- calltype[[h]]
    b <- blocks[[h]]
    ff <- if (ct == "formula") formulas[[h]] else NULL
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
            calltype = ct,
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
      mis <- !r
      mis[, setdiff(colnames(data), b)] <- FALSE
      data[mis] <- NA

      fm <- paste("mice.impute", theMethod, sep = ".")
      imputes <- switch(ct,
                        formula = do.call(fm, list(data = data, formula = ff, ...)),
                        pred = do.call(fm, list(data = data, type = pred, ...)),
                        stop("Cannot call function of type ", ct))

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
