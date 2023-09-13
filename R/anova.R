#' Compare several nested models
#'
#' @rdname anova
#' @param object Two or more objects of class `mira`
#' @param method Either `"D1"`, `"D2"` or `"D3"`
#' @param use An character indicating the test statistic
#' @param ... Other parameters passed down to `D1()`, `D2()`,
#' `D3()` and `mitml::testModels`.
#' @return Object of class `mice.anova`
#' @export
anova.mira <- function(object, ..., method = "D1", use = "wald") {
  modlist <- list(object, ...)
  first <- lapply(modlist, getfit, 1L) %>% sapply(glance)
  if (is.null(names(modlist))) {
    names(modlist) <- names(first) <- 1L:length(modlist)
  } else {
    names(first) <- names(modlist)
  }

  # order by model complexity
  dfcom <- rep(NA, ncol(first))
  for (j in 1:ncol(first)) {
    model <- getfit(modlist[[j]], 1L)
    dfcom[j] <- get.dfcom(model)
  }
  idx <- order(dfcom, decreasing = FALSE)
  modlist <- modlist[idx]
  dfcom <- dfcom[idx]
  names(dfcom) <- names(modlist)

  # get model formulas
  formulas <- lapply(modlist, getfit, 1L) %>% lapply(formula)
  names(formulas) <- names(modlist)

  # test successive models
  nm <- length(modlist)
  out <- vector("list", nm - 1L)
  names(out) <- paste(names(modlist), lead(names(modlist)), sep = " ~~ ")[-nm]
  for (j in seq_along(out)) {
    if (method == "D2") {
      args <- alist(fit1 = modlist[[j]], fit0 = modlist[[j + 1L]], use = use)
    } else {
      args <- alist(
        fit1 = modlist[[j]], fit0 = modlist[[j + 1L]],
        dfcom = as.numeric(unlist(dfcom[j]))
      )
    }
    out[[j]] <- do.call(method, args = args)
  }

  obj <- list(
    call = match.call(),
    out = out,
    formulas = formulas,
    m = length(getfit(modlist[[1L]])),
    method = method,
    use = use
  )
  class(obj) <- c("mice.anova", class(first))
  obj
}
