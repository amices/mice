data <- matrix(
  c(
    1.0,
    10.5,
    1.5,
    13.2,
    1.8,
    8.0,
    1.7,
    15.0,
    23.0,
    40.0,
    2.0,
    21.0,
    3.3,
    38.0,
    4.5,
    -2.3,
    NA,
    -2.4
  ),
  nrow = 9,
  ncol = 2,
  byrow = TRUE
)
df <- data.frame(data)
par <- list(
  y = df$X1,
  ry = !is.na(df$X1),
  x = df[, "X2", drop = FALSE]
)

test_that("MICE-IMPUTE-RF-001: runs with a single missing value using ranger", {
  skip_if_not_installed("ranger")
  expect_visible(do.call(mice.impute.rf, c(par, list(rfPackage = "ranger"))))
})

test_that("MICE-IMPUTE-RF-002: runs with a single missing value using randomForest", {
  skip_if_not_installed("randomForest")
  expect_visible(do.call(
    mice.impute.rf,
    c(par, list(rfPackage = "randomForest"))
  ))
})

test_that("MICE-IMPUTE-RF-003: runs with a single missing value using literanger", {
  skip_if_not_installed("literanger")
  expect_visible(do.call(
    mice.impute.rf,
    c(par, list(rfPackage = "literanger"))
  ))
})

# set options and R_RANGER_NUM_THREADS, returns a function that restores
# them; the option mc.cores sets parallelly::availableCores(), as future
# does on its workers
set.rf.env <- function(..., ranger.env = "") {
  oldopt <- options(...)
  oldenv <- Sys.getenv("R_RANGER_NUM_THREADS", unset = NA)
  Sys.setenv(R_RANGER_NUM_THREADS = ranger.env)
  function() {
    options(oldopt)
    if (is.na(oldenv)) {
      Sys.unsetenv("R_RANGER_NUM_THREADS")
    } else {
      Sys.setenv(R_RANGER_NUM_THREADS = oldenv)
    }
  }
}

# number of threads that mice.impute.rf() passes to fun of package pkg,
# NULL if the argument is not given; the mock keeps the formals of fun
captured.threads <- function(fun, pkg, arg, ...) {
  mock <- function() {
    value <- if (eval(call("missing", as.name(arg)))) NULL else get(arg)
    stop(structure(
      class = c("threads", "condition"),
      list(message = "", call = NULL, value = value)
    ))
  }
  formals(mock) <- formals(getExportedValue(pkg, fun))
  mock <- list(mock)
  names(mock) <- fun
  tryCatch(
    {
      do.call(local_mocked_bindings, c(mock, .package = pkg))
      mice.impute.rf(par$y, par$ry, par$x, rfPackage = pkg, ...)
    },
    threads = function(e) e$value
  )
}

test_that("MICE-IMPUTE-RF-004: rf.threads() caps the default at availableCores()", {
  skip_if_not_installed("parallelly")
  restore <- set.rf.env(
    mc.cores = 1L,
    Ncpus = NULL,
    ranger.num.threads = NULL
  )
  on.exit(restore(), add = TRUE)
  expect_identical(rf.threads(NULL, "ranger"), 1L)
  expect_identical(rf.threads(NULL, "literanger"), 1L)

  # availableCores() may be lower than 3, e.g. 2 under R CMD check
  options(mc.cores = 3L)
  cores <- unname(parallelly::availableCores())
  expect_identical(rf.threads(NULL, "ranger"), as.integer(min(2L, cores)))
  expect_identical(rf.threads(NULL, "literanger"), as.integer(cores))

  # ranger's own default follows the Ncpus option
  options(Ncpus = 8L)
  expect_identical(rf.threads(NULL, "ranger"), as.integer(cores))
})

test_that("MICE-IMPUTE-RF-005: rf.threads() lets settings of the user win", {
  restore <- set.rf.env(mc.cores = 1L, ranger.num.threads = NULL)
  on.exit(restore(), add = TRUE)
  expect_identical(rf.threads(4, "ranger"), 4)
  expect_identical(rf.threads(0, "literanger"), 0)

  options(ranger.num.threads = 3)
  expect_null(rf.threads(NULL, "ranger"))
  options(ranger.num.threads = NULL)

  Sys.setenv(R_RANGER_NUM_THREADS = "3")
  expect_null(rf.threads(NULL, "ranger"))
})

test_that("MICE-IMPUTE-RF-006: the number of threads reaches ranger", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("parallelly")
  restore <- set.rf.env(mc.cores = 1L, Ncpus = NULL, ranger.num.threads = NULL)
  on.exit(restore(), add = TRUE)
  threads <- function(...) {
    captured.threads("ranger", "ranger", "num.threads", ...)
  }
  expect_identical(threads(), 1L)
  expect_identical(threads(num.threads = 3), 3)
  options(ranger.num.threads = 3)
  expect_null(threads())
})

test_that("MICE-IMPUTE-RF-007: the number of threads reaches literanger", {
  skip_if_not_installed("literanger")
  skip_if_not_installed("parallelly")
  restore <- set.rf.env(mc.cores = 1L)
  on.exit(restore(), add = TRUE)
  threads <- function(...) {
    captured.threads("train", "literanger", "n_thread", ...)
  }
  expect_identical(threads(), 1L)
  expect_identical(threads(n_thread = 3), 3)
})

test_that("MICE-IMPUTE-RF-008: rf runs as futures with ranger and literanger", {
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")
  for (pkg in c("ranger", "literanger")) {
    skip_if_not_installed(pkg)
    imp <- mice(
      nhanes,
      m = 2,
      maxit = 2,
      method = "rf",
      rfPackage = pkg,
      seed = 1,
      print = FALSE,
      future = TRUE
    )
    expect_false(anyNA(complete(imp)))
  }
})
