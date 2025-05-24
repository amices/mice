#' Export \code{mids} object to Mplus
#'
#' Converts a \code{mids} object into a format recognized by Mplus, and writes
#' the data and the Mplus input files
#'
#' This function automates most of the work needed to export a \code{mids}
#' object to \code{Mplus}. The function writes the multiple imputation datasets,
#' the file that contains the names of the multiple imputation data sets and an
#' \code{Mplus} input file. The \code{Mplus} input file has the proper file
#' names, so in principle it should run and read the data without alteration.
#' \code{Mplus} will recognize the data set as a multiply imputed data set, and
#' do automatic pooling in procedures where that is supported.
#'
#' @param imp The \code{imp} argument is an object of class \code{mids},
#' typically produced by the \code{mice()} function.
#' @param file.prefix A character string describing the prefix of the output
#' data files.
#' @param path A character string containing the path of the output file.  By
#' default, files are written to the current \code{R} working directory.
#' @param sep The separator between the data fields.
#' @param dec The decimal separator for numerical data.
#' @param silent A logical flag stating whether the names of the files should be
#' printed.
#' @return The return value is \code{NULL}.
#' @author Gerko Vink, 2011.
#' @seealso \code{\link[=mids-class]{mids}}, \code{\link{mids2spss}}
#' @keywords manip
#' @export
mids2mplus <- function(imp, file.prefix = "imp", path = getwd(), sep = "\t", dec = ".", silent = FALSE) {
  m <- imp$m
  file.list <- matrix(0, m, 1)
  script <- matrix(0, 3, 1)
  for (i in seq_len(m)) {
    write.table(complete(imp, i),
      file = file.path(path, paste0(file.prefix, i, ".dat")),
      sep = sep, dec = dec, col.names = FALSE, row.names = FALSE
    )
    file.list[i, ] <- paste0(file.prefix, i, ".dat")
  }
  write.table(file.list,
    file = file.path(path, paste0(file.prefix, "list.dat")),
    sep = sep, dec = dec, col.names = FALSE, row.names = FALSE, quote = FALSE
  )
  names <- paste(colnames(complete(imp, 1)), collapse = " ")
  script[1, ] <- paste0("DATA: FILE IS ", file.prefix, "list.dat;")
  script[2, ] <- "TYPE = IMPUTATION;"
  script[3, ] <- paste0("VARIABLE: NAMES ARE ", names, ";")
  write.table(script,
    file = file.path(path, paste0(file.prefix, "list.inp")),
    sep = sep, dec = dec, col.names = FALSE, row.names = FALSE, quote = FALSE
  )

  if (!silent) {
    cat(
      "Data values written to", file.path(path, paste0(file.prefix, 1, ".dat")),
      "through", paste0(file.prefix, m, ".dat"), "\n"
    )
    cat("Data  names written to", file.path(path, paste0(file.prefix, "list.dat")), "\n")
    cat("Mplus  code written to", file.path(path, paste0(file.prefix, "list.inp")), "\n")
  }
}
