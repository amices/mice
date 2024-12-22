#' Create modeltype of the imputation model
#'
#' The helper `make.modeltype()` creates a vector that identifies per block if
#' the imputation model is taken from `predictorMatrix` or `formulas`. The
#' function is used internally by `mice()`.
#'
#' @param modeltype A character vector of length equal to the number of blocks in \code{predictorMatrix}.
#'   Each element specifies how the imputation model for the corresponding block is defined.
#'   Valid values are \code{"pred"} and \code{"formula"}. If \code{NULL}, the modeltype
#'   will be `"pred"` for all blocks, unless `priority` is `"formula"`.
#' @param predictorMatrix A matrix specifying the predictors for each block. Each row corresponds
#'   to a block, and each column corresponds to a variable. Non-zero entries indicate that the variable
#'   is used as a predictor for the block.
#' @param formulas A list of formulas, where each element corresponds to a block in \code{predictorMatrix}.
#'   If a formula is provided for a block, the corresponding \code{modeltype} entry is set to \code{"formula"}.
#'   If \code{NULL}, formulas are not used to modify \code{modeltype}.
#' @param priority A character string specifying the default value for \code{modeltype} when it is \code{NULL}.
#'   Defaults to \code{"pred"}. If \code{priority == "formula"}, the modeltype will be `"formula"`
#'   for blocks found in `formulas` with a matching name.
#'
#' @return A character vector of length equal to the number of rows in \code{predictorMatrix}.
#'   Each element is either \code{"pred"} or \code{"formula"}, indicating how the imputation model
#'   is specified for the corresponding block.
#'
#' @examples
#' # Example predictorMatrix
#' predictorMatrix <- matrix(1, nrow = 3, ncol = 3,
#'  dimnames = list(c("block1", "block2", "block3"), c("x1", "x2", "y")))
#' predictorMatrix[1, 3] <- 0
#'
#' # Case 1: No modeltype or formulas specified
#' make.modeltype(NULL, predictorMatrix, NULL)
#'
#' # Case 2: Formulas provided
#' formulas <- list(
#'   NULL,
#'   y ~ x1 + x2,
#'   NULL
#' )
#' make.modeltype(NULL, predictorMatrix, formulas)
#'
#' # Case 3: Custom modeltype
#' modeltype <- c("pred", "formula", "pred")
#' make.modeltype(modeltype, predictorMatrix, NULL)
#'
#' @export
make.modeltype <- function(modeltype, predictorMatrix, formulas, priority = "pred") {
  # Validate modeltype length
  if (!is.null(modeltype) && length(modeltype) != nrow(predictorMatrix)) {
    stop("Length of modeltype must match the number of blocks in predictorMatrix.")
  }

  # Default modeltype setup
  if (is.null(modeltype)) {
    modeltype <- rep("pred", nrow(predictorMatrix))
  }
  names(modeltype) <- dimnames(predictorMatrix)[[1L]]

  # Adjust modeltype based on formulas
  if (priority == "formula") {
    for (name in names(modeltype)) {
      if (!is.null(formulas[[name]])) {
        modeltype[name] <- "formula"
      }
    }
  }

  # Validate entries
  valid_modeltypes <- c("pred", "formula")
  if (!all(modeltype %in% valid_modeltypes)) {
    stop("All entries in modeltype must be one of: ", paste(valid_modeltypes, collapse = ", "))
  }

  return(modeltype)
}
