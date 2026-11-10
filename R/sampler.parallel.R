# Runs the m Gibbs sampling chains in parallel. Chain i only depends on
# column i of imp, so each worker calls the sequential sampler() with
# m = 1 for one chain over all iterations. Results are recombined in
# the order of the chains. This function is called by mice.
sampler.parallel <- function(
  data,
  m,
  ignore,
  where,
  imp,
  blocks,
  method,
  visitSequence,
  predictorMatrix,
  formulas,
  calltype,
  blots,
  post,
  fromto,
  printFlag,
  ...,
  progress = NULL
) {
  from <- fromto[1]
  to <- fromto[2]
  maxit <- to - from + 1
  t0 <- Sys.time()

  run.chain <- function(i, ...) {
    # local event log, found by updateLog() through the call stack
    .mice.state <- list(it = 0, im = 0, dep = "", meth = "", log = FALSE)
    loggedEvents <- data.frame(it = 0, im = 0, dep = "", meth = "", out = "")

    imp.i <- lapply(imp, function(x) x[, i, drop = FALSE])
    chain.progress <- if (!is.null(progress)) {
      function(k, ii) progress(k, i)
    }
    q <- sampler(
      data,
      1L,
      ignore,
      where,
      imp.i,
      blocks,
      method,
      visitSequence,
      predictorMatrix,
      formulas,
      calltype,
      blots,
      post,
      fromto,
      FALSE,
      ...,
      progress = chain.progress
    )
    if (.mice.state$log) {
      loggedEvents$im <- i
    } else {
      loggedEvents <- NULL
    }
    list(q = q, loggedEvents = loggedEvents)
  }

  chains <- future.apply::future_lapply(
    seq_len(m),
    run.chain,
    ...,
    future.seed = TRUE
  )

  # combine chains
  chainMean <- chainVar <- initialize.chain(names(data), maxit, m)
  for (i in seq_len(m)) {
    q <- chains[[i]]$q
    for (j in names(imp)) {
      imp[[j]][, i] <- q$imp[[j]][, 1L]
    }
    chainMean[, , i] <- q$chainMean[, , 1L]
    chainVar[, , i] <- q$chainVar[, , 1L]
  }

  # combine event logs in the order of the sequential sampler
  logs <- do.call(rbind, lapply(chains, function(x) x$loggedEvents))
  if (!is.null(logs)) {
    logs <- logs[order(logs$it, logs$im), , drop = FALSE]
  }

  if (printFlag && maxit >= 1) {
    cat(
      "\n Imputed ",
      m,
      " chains in parallel, ",
      maxit,
      " iterations (",
      format(round(difftime(Sys.time(), t0), 2)),
      ")",
      sep = ""
    )
    ridge.used <- any(grepl("A ridge penalty", logs$out))
    if (ridge.used) {
      cat("\n * Please inspect the loggedEvents \n")
    } else {
      cat("\n")
    }
  }

  list(
    iteration = maxit,
    imp = imp,
    chainMean = chainMean,
    chainVar = chainVar,
    loggedEvents = logs
  )
}

# Activates a parallel backend for mice(parallel = TRUE). A plan set by
# the user is respected. Otherwise, a multisession plan is set up with
# n.core workers. Returns the previous plan, or NULL if unchanged.
setup.parallel <- function(m, n.core = NULL) {
  install.on.demand("future")
  install.on.demand("future.apply")
  if (!is.null(n.core)) {
    n.core <- as.integer(n.core)
    if (length(n.core) != 1L || is.na(n.core) || n.core < 1L) {
      stop("Argument `n.core` must be a positive integer.", call. = FALSE)
    }
  }

  if (!is.null(n.core) && n.core == 1L) {
    return(future::plan(future::sequential))
  }
  if (is.null(n.core) && !inherits(future::plan(), "sequential")) {
    return(NULL)
  }
  if (is.null(n.core)) {
    n.core <- max(1L, future::availableCores() - 1L)
  }
  future::plan(future::multisession, workers = min(m, n.core))
}
