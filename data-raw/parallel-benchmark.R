# Timings for the vignette "Parallel imputation: benchmarks".
#
# Compares sequential mice(), mice(parallel = TRUE) and futuremice() for
# three data sets, several numbers of imputations m and of workers. Run
# from the package root with the version of mice to be benchmarked
# installed (workers load the installed mice):
#
#   Rscript data-raw/parallel-benchmark.R
#
# The results are saved in vignettes/parallel-benchmark.rds, which the
# vignette reads, so that building the vignette does not rerun the
# benchmark. Takes about half an hour on a 14-core laptop.

suppressPackageStartupMessages({
  library(mice)
  library(future)
})

reps <- 3L
ms <- c(5L, 20L, 50L, 100L)
workers <- c(2L, 4L, 8L)
maxit <- 5L

# --- data ----------------------------------------------------------------

# n cases on p multivariate normal variables with correlation 0.5, and
# 80 percent of the cases with missing values (MCAR), as in Volker and
# Vink (2022)
make.data <- function(n, p, seed) {
  set.seed(seed)
  sigma <- matrix(0.5, p, p)
  diag(sigma) <- 1
  data <- as.data.frame(MASS::mvrnorm(n, rep(0, p), sigma))
  ampute(data, prop = 0.8, mech = "MCAR")$amp
}

scenarios <- list(
  small = list(
    label = "Small: 1000 x 4, pmm",
    data = make.data(1000, 4, 123),
    method = "pmm"
  ),
  large = list(
    label = "Large: 10000 x 8, pmm",
    data = make.data(10000, 8, 123),
    method = "pmm"
  ),
  slow = list(
    label = "Slow method: 2000 x 8, rf",
    data = make.data(2000, 8, 123),
    method = "rf"
  )
)

# --- helpers -------------------------------------------------------------

elapsed <- function(expr) {
  gc()
  unname(system.time(expr)[["elapsed"]])
}

# start a multisession plan and load mice on every worker, so that the
# timings below exclude the start-up of the workers
start.workers <- function(n) {
  plan(multisession, workers = n)
  invisible(mice(
    nhanes,
    m = n,
    maxit = 1,
    print = FALSE,
    parallel = TRUE
  ))
}

results <- list()
add <- function(...) {
  results[[length(results) + 1L]] <<- data.frame(...)
}

# --- start-up cost of the workers ----------------------------------------

startup <- list()
for (n in workers) {
  for (r in seq_len(reps)) {
    plan(sequential)
    t.start <- elapsed(start.workers(n))
    t.warm <- elapsed(mice(
      nhanes,
      m = n,
      maxit = 1,
      print = FALSE,
      parallel = TRUE
    ))
    startup[[length(startup) + 1L]] <- data.frame(
      workers = n,
      rep = r,
      cold = t.start,
      warm = t.warm
    )
  }
}
plan(sequential)
startup <- do.call(rbind, startup)

# --- imputation timings --------------------------------------------------

for (s in names(scenarios)) {
  sc <- scenarios[[s]]
  message("scenario ", s)

  # sequential mice()
  plan(sequential)
  for (m in ms) {
    for (r in seq_len(reps)) {
      t <- elapsed(mice(
        sc$data,
        m = m,
        maxit = maxit,
        method = sc$method,
        seed = r,
        print = FALSE
      ))
      add(
        scenario = s,
        approach = "mice()",
        workers = 1L,
        m = m,
        rep = r,
        time = t
      )
    }
  }

  # mice(parallel = TRUE) on workers that are already running
  for (n in workers) {
    start.workers(n)
    for (m in ms) {
      for (r in seq_len(reps)) {
        t <- elapsed(mice(
          sc$data,
          m = m,
          maxit = maxit,
          method = sc$method,
          seed = r,
          print = FALSE,
          parallel = TRUE
        ))
        add(
          scenario = s,
          approach = "mice(parallel = TRUE)",
          workers = n,
          m = m,
          rep = r,
          time = t
        )
      }
    }
  }
  plan(sequential)

  # futuremice(), which starts its own workers in every call
  for (n in workers) {
    for (m in ms) {
      for (r in seq_len(reps)) {
        t <- elapsed(suppressWarnings(futuremice(
          sc$data,
          m = m,
          maxit = maxit,
          method = sc$method,
          parallelseed = r,
          n.core = n
        )))
        add(
          scenario = s,
          approach = "futuremice()",
          workers = n,
          m = m,
          rep = r,
          time = t
        )
      }
    }
  }
}

timings <- do.call(rbind, results)

benchmark <- list(
  timings = timings,
  startup = startup,
  scenarios = vapply(scenarios, function(x) x$label, character(1)),
  maxit = maxit,
  reps = reps,
  info = list(
    date = Sys.Date(),
    mice = as.character(packageVersion("mice")),
    future = as.character(packageVersion("future")),
    R = R.version.string,
    platform = R.version$platform,
    cpu = tryCatch(
      system("sysctl -n machdep.cpu.brand_string", intern = TRUE),
      error = function(e) NA_character_,
      warning = function(w) NA_character_
    ),
    cores = parallelly::availableCores()
  )
)

saveRDS(benchmark, "vignettes/parallel-benchmark.rds")
message("saved vignettes/parallel-benchmark.rds")
