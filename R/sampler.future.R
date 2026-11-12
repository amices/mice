# Runs the m Gibbs sampling chains as futures, for mice(future = TRUE).
# Chain i only depends on column i of imp, so each future calls the
# sequential sampler() with m = 1 for one chain over all iterations.
# Results are recombined in the order of the chains. The chains run on
# the current future plan of the user, which mice never changes. This
# function is called by mice and mice.mids.
#
# chain.seed is NULL, which derives a new random number stream for each
# chain from the current seed, or a list with the state of the stream of
# each chain, as returned in chainSeed, to continue the chains exactly.
sampler.future <- function(
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
  progress = NULL,
  chain.seed = NULL
) {
  from <- fromto[1]
  to <- fromto[2]
  maxit <- to - from + 1
  t0 <- Sys.time()

  # methods defined outside mice, e.g. in the global environment, are not
  # on the search path of the workers, so send them along
  impute.funs <- user.impute.funs(method)
  post.objs <- post.globals(post)

  run.chain <- function(i, ...) {
    for (fn in names(impute.funs)) {
      assign(fn, impute.funs[[fn]], envir = .impute.funs)
    }
    on.exit(rm(list = names(impute.funs), envir = .impute.funs), add = TRUE)

    # local event log, found by updateLog() through the call stack
    .mice.state <- list(it = 0, im = 0, dep = "", meth = "", log = FALSE)
    loggedEvents <- data.frame(it = 0, im = 0, dep = "", meth = "", out = "")

    imp.i <- lapply(imp, function(x) x[, i, drop = FALSE])
    chain.progress <- if (!is.null(progress)) {
      function(k, ii) progress(k, i)
    }
    q <- sampler(
      data = data,
      m = 1L,
      ignore = ignore,
      where = where,
      imp = imp.i,
      blocks = blocks,
      method = method,
      visitSequence = visitSequence,
      predictorMatrix = predictorMatrix,
      formulas = formulas,
      calltype = calltype,
      blots = blots,
      post = post,
      fromto = fromto,
      printFlag = FALSE,
      ...,
      progress = chain.progress,
      chain.id = i,
      post.globals = post.objs
    )
    if (.mice.state$log) {
      loggedEvents$im <- i
    } else {
      loggedEvents <- NULL
    }
    # state of the random number stream of this chain
    seed <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
    list(q = q, loggedEvents = loggedEvents, seed = seed, pid = Sys.getpid())
  }

  chains <- future.apply::future_lapply(
    seq_len(m),
    run.chain,
    ...,
    future.seed = if (is.null(chain.seed)) TRUE else chain.seed
  )

  # combine chains
  chainMean <- chainVar <- initialize.chain(names(data), maxit, m)
  for (i in seq_len(m)) {
    q <- chains[[i]]$q
    for (j in names(imp)) {
      imp[[j]][, i] <- q$imp[[j]][, 1L]
    }
    chainMean[,, i] <- q$chainMean[,, 1L]
    chainVar[,, i] <- q$chainVar[,, 1L]
  }

  # combine event logs in the order of the sequential sampler
  logs <- do.call(rbind, lapply(chains, function(x) x$loggedEvents))
  if (!is.null(logs)) {
    logs <- logs[order(logs$it, logs$im), , drop = FALSE]
  }

  # backend and number of processes that ran the chains
  pid <- vapply(chains, function(x) x$pid, integer(1))
  info <- list(
    plan = plan.name(),
    workers = length(unique(pid)),
    pid = pid
  )

  if (printFlag && maxit >= 1) {
    cat(
      "\n Imputed ",
      m,
      " chains with future (",
      info$plan,
      ") on ",
      info$workers,
      if (info$workers == 1L) " worker, " else " workers, ",
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
    loggedEvents = logs,
    chainSeed = lapply(chains, function(x) x$seed),
    future = info
  )
}

# Checks that the packages for mice(future = TRUE) are available. The
# chains run on the current future plan, so nothing is set up here.
check.future <- function() {
  install.on.demand("future")
  install.on.demand("future.apply")
  invisible(NULL)
}

# Name of the current future plan, e.g. "sequential" or "multisession"
plan.name <- function() {
  cls <- setdiff(class(future::plan()), c("FutureStrategy", "tweaked"))
  cls[1L]
}

# Combines the future fields of two mids objects, for ibind()
combine.future <- function(x, y) {
  if (is.null(x) || is.null(y)) {
    return(NULL)
  }
  pid <- c(x$pid, y$pid)
  list(
    plan = paste(unique(c(x$plan, y$plan)), collapse = ", "),
    workers = length(unique(pid)),
    pid = pid
  )
}
