install.on.demand <- function(pkg, quiet = FALSE, ...) {
  # internal function that checks whether package pkg is
  # in the library. If not found, it write a message to
  # the console (if quiet = TRUE) and installs it from CRAN
  if (requireNamespace(pkg, quietly = TRUE)) return()
  if (!quiet) cat(paste0("\nInstalling '", pkg, "' package...\n"))
  install.packages(pkg, repos = "https://cloud.r-project.org/", 
                   quiet = quiet)
  if (!quiet) cat("\n")
}

