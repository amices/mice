#'Compare several nested models
#'
#'@rdname anova
#'@param object Two or more objects of class \code{mira}
#'@param ... Other parameters passed down to \code{D1()}, \code{D2()}, 
#'\code{D3()} and \code{mitml::testModels}.
#'@return Object of class \code{mice.anova}
#'@export
anova.mira <- function(object, ...) {

  modlist <- list(object, ...)
  first <- lapply(modlist, getfit, 1L) %>% sapply(glance)
  if (is.null(names(modlist))) colnames(first) <- 1L:length(modlist)
  else colnames(first) <- names(modlist)
  
  # order by model complexity
  idx <- order(unlist(first["df.residual", ]), decreasing = FALSE)
  modlist <- modlist[idx]
  df.com <- first["df.residual", idx]
  names(df.com) <- names(modlist)
  
  # get model formulas
  formulas <- lapply(modlist, getfit, 1L) %>% lapply(formula)
  names(formulas) <- names(modlist)

  # obtain the method
  method <- getOption("anova.method")
  if (is.null(method)) method <- "D1"
  
  # test successive models
  nm <- length(modlist)
  result <- vector("list", nm - 1L)
  names(result) <- paste(names(modlist), lead(names(modlist)), sep = " = ")[-nm]
  for(j in seq_along(result)) {
    args <- alist(fit1 = modlist[[j]], fit0 = modlist[[j + 1L]], 
                  df.com = c(df.com[j], df.com[j + 1]))
    result[[j]] <- do.call(method, args = args)
  }

  out <- list(
    call = match.call(),
    test = result,
    formula = formulas,
    method = method,
    use = "likelihood",
    df.com = df.com,
    reml = NULL
  )

  class(out) <- "mice.anova"
  out
}

