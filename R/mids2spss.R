#' Export \code{mids} object to SPSS
#'
#' Converts a \code{mids} object into a format recognized by SPSS, and writes
#' the data and the SPSS syntax files.
#'
#' This function automates most of the work needed to export a \code{mids}
#' object to SPSS. It uses \code{haven::write_sav()} to facilitate the export to an
#' SPSS \code{.sav} or \code{.zsav} file.
#'
#' Below are some things to pay attention to.
#'
#' The \code{SPSS} syntax file has the proper file names and separators set, so
#' in principle it should run and read the data without alteration. \code{SPSS}
#' is more strict than \code{R} with respect to the paths. Always use the full
#' path, otherwise \code{SPSS} may not be able to find the data file.
#'
#' Factors in \code{R} translate into categorical variables in \code{SPSS}. The
#' internal coding of factor levels used in \code{R} is exported. This is
#' generally acceptable for \code{SPSS}. However, when the data are to be
#' combined with existing \code{SPSS} data, watch out for any changes in the
#' factor levels codes.
#'
#' \code{SPSS} will recognize the data set as a multiply imputed data set, and
#' do automatic pooling in procedures where that is supported. Note however that
#' pooling is an extra option only available to those who license the
#' \code{MISSING VALUES} module. Without this license, \code{SPSS} will still
#' recognize the structure of the data, but it will not pool the multiply imputed
#' estimates into a single inference.
#'
#' @param imp The \code{imp} argument is an object of class \code{mids},
#' typically produced by the \code{mice()} function.
#' @param filename A character string describing the name of the output data
#' file and its extension.
#' @param path A character string containing the path of the output file. The
#' value in \code{path} is appended to \code{filedat}. By
#' default, files are written to the current \code{R} working directory. If
#' \code{path=NULL} then no file path appending is done.
#' @param compress A logical flag stating whether the resulting SPSS set should
#' be a compressed \code{.zsav} file.
#' @param silent A logical flag stating whether the location of the saved file should be
#' printed.
#' @return The return value is \code{NULL}.
#' @author Gerko Vink, dec 2020.
#' @seealso \code{\link[=mids-class]{mids}}
#' @keywords manip
#' @export
mids2spss <- function(imp, filename = "midsdata",
                      path = getwd(), compress = FALSE, silent = FALSE) {
  .id <- NULL # avoid empty global variable binding
  install.on.demand("haven")
  # extract a completed dataset (long format - all imputations stacked)
  # rename the .imp variable to imputation_, such that SPSS can identify a multiply imputed dataset
  out <-
    imp %>%
    complete(action = "long", include = TRUE) %>%
    dplyr::select(-.id) %>%
    dplyr::rename("Imputation_" = ".imp")
  # write the data to a .sav file with package haven and print (optional) the saved location
  if (!compress) {
    whereto <- paste(path, "/", filename, ".sav", sep = "")
  } else {
    whereto <- paste(path, "/", filename, ".zsav", sep = "")
  }
  haven::write_sav(data = out, path = whereto, compress = compress)
  if (!silent) {
    cat("SPSS file written to", whereto, "\n")
  }
}
