# contributed by Simon Grund, #137
check.deprecated <- function(...) {
  # print warnings for deprecated argument names
  nms <- names(list(...))
  replace.args <- list(imputationMethod = "method",
                       defaultImputationMethod = "defaultMethod",
                       form = "formulas")
  
  wrn <- names(replace.args) %in% nms
  if(any(wrn)) {
    for(i in which(wrn)) {
      msg <- paste0("The '", names(replace.args)[i], 
                    "' argument is no longer supported. Please use '",
                    replace.args[i], "' instead.")
      warning(msg)
    }
  }
  
  invisible(NULL)
}
