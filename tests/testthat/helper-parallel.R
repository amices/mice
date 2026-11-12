# TRUE if mice is loaded by devtools::load_all(). Workers of a
# multisession plan then load the installed mice, not the development
# version, so tests that compare with such workers are skipped.
mice_is_dev_loaded <- function() {
  exists(".__DEVTOOLS__", envir = asNamespace("mice"), inherits = FALSE)
}

# evaluates code under a multisession plan with the given number of
# workers, and restores the plan of the caller afterwards
with_workers <- function(code, workers = 2L) {
  oldplan <- future::plan(future::multisession, workers = workers)
  on.exit(future::plan(oldplan), add = TRUE)
  force(code)
}
