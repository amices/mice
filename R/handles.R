handles.arg <- function(f, a = "data") {
  # determine whether function f handles argument a
  if (!is.function(f)) {
    return(FALSE)
  }
  a %in% names(formals(f))
}


handles.format <- function(fn) {
  # determine whether function fn handles the `format` argument
  f <- impute.fun(fn)
  if (is.character(f)) {
    f <- get(f)
  }
  handles.arg(f, "format")
}


# Functions of imputation methods that are not part of mice, set by
# sampler.parallel() on the workers, where the search path of the user
# session is not available.
.impute.funs <- new.env(parent = emptyenv())

# Returns the function stored for imputation method fn (e.g.
# "mice.impute.mymethod"), or the name fn itself, which is then looked up
# as usual.
impute.fun <- function(fn) {
  if (exists(fn, envir = .impute.funs, inherits = FALSE)) {
    return(get(fn, envir = .impute.funs, inherits = FALSE))
  }
  fn
}

# Collects the functions of the imputation methods in method that are not
# defined in mice, as found from the user session. A function that is not
# part of a package, e.g. one defined in the global environment, gets a new
# enclosing environment that holds the objects it uses from there, such as
# helper functions, so that these are sent along to the workers.
user.impute.funs <- function(method) {
  method <- unique(unname(method[method != "" & !is.passive(method)]))
  fns <- paste0("mice.impute.", method)
  ns <- topenv()
  fns <- fns[!vapply(fns, exists, logical(1), envir = ns, inherits = FALSE)]
  fns <- fns[vapply(fns, exists, logical(1), envir = ns, mode = "function")]
  funs <- lapply(fns, get, envir = ns, mode = "function")
  names(funs) <- fns
  for (fn in fns) {
    funs[[fn]] <- bundle.globals(funs[[fn]])
  }
  funs
}

# Returns function f with the global objects it uses (recursively) copied
# into a new enclosing environment, unless f belongs to a package.
bundle.globals <- function(f) {
  fenv <- environment(f)
  if (is.null(fenv) || isNamespace(fenv) || identical(fenv, baseenv())) {
    return(f)
  }
  install.on.demand("globals")
  objs <- globals::globalsOf(
    quote(.mice.fun),
    envir = list2env(list(.mice.fun = f), parent = fenv),
    recursive = TRUE,
    mustExist = FALSE
  )
  objs <- globals::cleanup(objs)
  objs <- objs[setdiff(names(objs), ".mice.fun")]
  env <- new.env(parent = fenv)
  for (nm in names(objs)) {
    obj <- objs[[nm]]
    if (is.function(obj) && identical(environment(obj), globalenv())) {
      environment(obj) <- env
    }
    assign(nm, obj, envir = env)
  }
  environment(f) <- env
  f
}
