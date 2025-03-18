#' Initialize Models Environment
#'
#' Creates an environment structure for models based on specified task types.
#' It ensures that a nested environment is created for each variable in `tasks`
#' that is labeled as `"train"`, with sub-environments for each iteration
#' from `1` to `m`.
#'
#' @inheritParams mice
#' @param m An integer specifying the number of nested sub-environments to
#' create under each `"train"` variable.
#'
#' @return An environment containing model environments structured as:
#'   \itemize{
#'     \item \code{models$varname} - An environment for each `"train"` variable.
#'     \item \code{models$varname$i} - Nested environments for each iteration from `1` to `m`.
#'   }
initialize.models.env <- function(models = NULL, tasks, method, blocks, m) {

  # Import models into environment from a model list object
  if (is.list(models)) {
    models <- import.models.env(models)
  }

  # Ensure `models` is an environment
  if (is.null(models)) {
    models <- new.env(parent = emptyenv())
  }

  # Identify variables that require models (i.e., "train" or "fill" tasks)
  imported.models <- names(models)
  model.vars <- names(tasks[tasks %in% c("train", "fill")])
  empty.methods <- character(0L)
  for (h in names(blocks)) {
    varnames <- blocks[[h]]
    if (method[h] == "") {
      empty.methods <- c(empty.methods, varnames)
    }
  }
  model.vars <- setdiff(model.vars, c(imported.models, empty.methods))

  for (varname in model.vars) {
    if (tasks[varname] == "train") {
      # Create an environment for the variable if it doesn't exist
      if (!exists(varname, envir = models)) {
        models[[varname]] <- new.env(parent = emptyenv())
      }

      # Create nested environments for `1:m`
      for (i in seq_len(m)) {
        if (!exists(as.character(i), envir = models[[varname]])) {
          models[[varname]][[as.character(i)]] <- new.env(parent = emptyenv())
        }
      }
    }
  }

  return(models)
}

#' Convert Nested Environments to a List of m-Lists
#'
#' Recursively converts a three-level environment structure into a user-friendly
#' list where the imputation level is stored as a vector of length `m`.
#'
#' @param env The root environment containing task environments.
#' @param m The number of imputations (assumes all tasks have the same `m`).
#' @return A named list where:
#'   \itemize{
#'     \item Each element corresponds to a task (e.g., "train" variables).
#'     \item Each task is stored as a vector of `m` lists (one per imputation iteration).
#'     \item Each list contains the objects stored for that imputation.
#'   }
#' @examples
#' # Create a nested environment structure
#' models_env <- new.env()
#' models_env$a <- new.env()
#' models_env$a$`1` <- new.env()
#' models_env$a$`1`$model <- "Model A1"
#' models_env$a$`2` <- new.env()
#' models_env$a$`2`$model <- "Model A2"
#' models_env$b <- new.env()
#' models_env$b$`1` <- new.env()
#' models_env$b$`1`$model <- "Model B1"
#'
#' # Convert to a list
#' models_list <- mice:::export.models.env(models_env, m = 2)
#' print(models_list)
#'
export.models.env <- function(env, m = NULL) {
  if (!is.environment(env)) stop("Input must be an environment")

  env_to_list <- function(env) {
    obj_list <- as.list(env, all.names = TRUE)
    for (name in names(obj_list)) {
      if (is.environment(obj_list[[name]])) {
        obj_list[[name]] <- env_to_list(obj_list[[name]])
      }
    }
    return(obj_list)
  }

  # Convert first-level environment into a list
  models_list <- env_to_list(env)
  m <- ifelse(is.null(m),  max(sapply(models_list, length)), m)

  # Restructure each task's models into a vector of m lists
  for (varname in names(models_list)) {
    task_models <- models_list[[varname]]

    # Initialize an empty list of length m
    imputation_list <- vector("list", m)

    # Fill in models from the extracted task_models
    for (i in seq_len(m)) {
      iter_name <- as.character(i)
      imputation_list[[i]] <-
        if (iter_name %in% names(task_models)) {
          task_models[[iter_name]]
        } else {
          list()
        }
    }

    # Replace with the m-length vector of lists
    models_list[[varname]] <- imputation_list
  }

  return(models_list)
}

#' Convert a List of m-Lists Back to a Nested Environment
#'
#' Converts a structured list back into a nested environment where:
#'   - The first level contains task names (e.g., "train" variables).
#'   - The second level contains iteration indices (`1:m`).
#'   - The third level contains stored objects within each iteration.
#'
#' @param models_list A list where:
#'   - Each element corresponds to a task (e.g., "train" variables).
#'   - Each task contains a vector of `m` lists (one per imputation iteration).
#'   - Each list contains the stored objects for that iteration.
#' @return A nested environment structured as:
#'   - `models_env$varname` (An environment for each task).
#'   - `models_env$varname$i` (Nested environments for each iteration).
#'   - Objects within each iteration are stored inside their respective environments.
#'
#' @examples
#' # Example list structure
#' models_list <- list(
#'   a = list(
#'     list(model = "Model A1"),
#'     list(model = "Model A2")
#'   ),
#'   b = list(
#'     list(model = "Model B1"),
#'     list()  # Empty list for missing iteration
#'   )
#' )
#'
#' # Convert list to environment
#' models_env <- mice:::import.models.env(models_list)
#' print(ls(models_env))  # Should list "a" and "b"
#' print(ls(models_env$a))  # Should list "1" and "2"
#' print(models_env$a$`1`$model)  # Should be "Model A1"
import.models.env <- function(models_list) {
  if (!is.list(models_list)) stop("Input must be a list")

  models_env <- new.env(parent = emptyenv())

  for (varname in names(models_list)) {
    models_env[[varname]] <- new.env(parent = emptyenv())  # Create first-level environment

    for (i in seq_along(models_list[[varname]])) {
      iteration_data <- models_list[[varname]][[i]]

      if (length(iteration_data) > 0) {  # Only create non-empty environments
        models_env[[varname]][[as.character(i)]] <- list2env(iteration_data, parent = emptyenv())
      }
    }
  }

  return(models_env)
}
