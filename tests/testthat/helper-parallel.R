# TRUE if mice is loaded by devtools::load_all(). Workers of a
# multisession plan then load the installed mice, not the development
# version, so tests that compare with such workers are skipped.
mice_is_dev_loaded <- function() {
  exists(".__DEVTOOLS__", envir = asNamespace("mice"), inherits = FALSE)
}
