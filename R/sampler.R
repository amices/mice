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
      if (use.future) {
        t0 <- Sys.time()
      }

      if (!use.future) {

        # single-threaded processing
        for (i in seq_len(m)) {
          # begin i loop: repeated imputation loop
          if (printFlag) {
            cat("\n ", iteration, " ", i)
          }
          result <- one.cycle(data, imp, r, where, i, k, visitSequence,
                              blocks, method, calltypes, formulas,
                              predictorMatrix, blots,
                              tasks, models,
                              post, ignore, printFlag, ...)
          data <- result$data
          imp <- result$imp
        } # end i loop: repeated imputation loop

        k2 <- k - from + 1L
        stat <- get.chain.stats(data, imp, chainMean, chainVar, k2, m, blocks, visitSequence)
        chainMean <- stat$chainMean
        chainVar <- stat$chainVar

      } else {
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

        t1 <- Sys.time()
        if (printFlag) cat(sprintf("\n iter %d (%s)", k, format(round(difftime(t1, t0), 2))))

        k2 <- k - from + 1L
        for (i in seq_len(m)) {
          imp_i <- results[[i]]$imp
          for (j in names(imp_i)) {
            imp[[j]][, i] <- imp_i[[j]][, i]
          }
          mean_i <- results[[i]]$mean
          var_i  <- results[[i]]$var
          for (j in names(mean_i)) {
            if (j %in% dimnames(chainMean)[[1]]) {
              chainMean[j, k2, i] <- mean_i[[j]]
              chainVar[j, k2, i] <- var_i[[j]]
            }
          }
        }
        loggedEvents.new <- do.call(rbind, lapply(results, `[[`, "loggedEvents"))
        loggedEvents <- rbind(loggedEvents, loggedEvents.new)
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
