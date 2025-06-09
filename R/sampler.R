sampler <- function(data, m, ignore, where, imp, blocks, method,
                    visitSequence, predictorMatrix, formulas,
                    calltypes, blots, tasks, models,
                    post, fromto, printFlag, ...,
                    use.future = TRUE,
                    future.packages = NULL, future.seed = TRUE) {
  from <- fromto[1]
  to <- fromto[2]
  maxit <- to - from + 1
  r <- !is.na(data)

  # set up array for convergence checking
  chainMean <- chainVar <- initialize.chain(names(data), maxit, m)

  ## THE MAIN LOOP: GIBBS SAMPLER ##
  loggedEvents <- NULL
  if (maxit < 1) iteration <- 0
  if (maxit >= 1) {
    if (!use.future && printFlag) {
      cat("\n iter imp variable")
    }

    for (k in from:to) {
      # begin k loop : main iteration loop
      iteration <- k

      if (!use.future) {
        # single-threaded processing
        results <- lapply(seq_len(m), function(i) {
          worker(i, data = data, imp = imp, r = r, where = where, k = k,
                 visitSequence = visitSequence, blocks = blocks,
                 method = method, calltypes = calltypes, formulas = formulas,
                 predictorMatrix = predictorMatrix, blots = blots,
                 tasks = tasks, models = models,
                 post = post, ignore = ignore, printFlag = printFlag, ...)
        })
      } # end !use.future

      if (use.future) {
        # parallel processing with future.apply
        results <- future_lapply(seq_len(m), function(i) {
          worker(i, data = data, imp = imp, r = r, where = where, k = k,
                 visitSequence = visitSequence, blocks = blocks,
                 method = method, calltypes = calltypes, formulas = formulas,
                 predictorMatrix = predictorMatrix, blots = blots,
                 tasks = tasks, models = models,
                 post = post, ignore = ignore, ...)
        },
        future.packages = future.packages,
        future.globals = list(worker = worker,
                              initialize.chain = initialize.chain,
                              one.cycle = one.cycle,
                              get.chain.stats = get.chain.stats),
        future.seed = future.seed)
      } # end !use.future

      if (printFlag) {
        for (i in seq_len(m)) {
          result <- results[[i]]
          cat("\n ", iteration, " ", i, result$out)
        }
      }

      for (i in seq_len(m)) {
        imp_i <- results[[i]]$imp
        for (j in names(imp_i)) {
          imp[[j]][, i] <- imp_i[[j]][, i]
        }
        # mean_i <- results[[i]]$mean
        # var_i  <- results[[i]]$var
        # for (j in names(mean_i)) {
        #   if (j %in% dimnames(chainMean)[[1]]) {
        #     chainMean[j, k2, i] <- mean_i[[j]]
        #     chainVar[j, k2, i] <- var_i[[j]]
        #   }
        # }
      }
      k2 <- k - from + 1L
      stat <- get.chain.stats(data, imp, chainMean, chainVar, k2, m, blocks, visitSequence)
      chainMean <- stat$chainMean
      chainVar <- stat$chainVar

      loggedEvents.new <- do.call(rbind, lapply(results, `[[`, "loggedEvents"))
      loggedEvents <- rbind(loggedEvents, loggedEvents.new)
      if (!is.null(loggedEvents)) {
        rownames(loggedEvents) <- as.character(seq_len(nrow(loggedEvents)))
      }
    } # end k loop : main iteration loop
    if (printFlag) {
      cat("\n")
    }
  }

  list(iteration = maxit, imp = imp,
       chainMean = chainMean, chainVar = chainVar,
       loggedEvents = loggedEvents)
}
