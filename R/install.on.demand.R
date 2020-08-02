install.on.demand <- function(pkg, quiet = FALSE, ...) {
  # internal function that checks whether package pkg is
  # in the library. If not found, it asks the user permission
  # to install from CRAN.
  if (requireNamespace(pkg, quietly = TRUE)) return()
  answer <- askYesNo(paste("Package", pkg, "needed. Install from CRAN?"))
  if (answer) install.packages(pkg, repos = "https://cloud.r-project.org/", quiet = quiet)
}


