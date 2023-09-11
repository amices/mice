#' Export `mids` object to SPSS
#'
#' Converts a `mids` object into a format recognized by SPSS, and writes
#' the data and the SPSS syntax files.
#'
#' This function automates most of the work needed to export a `mids`
#' object to SPSS. It uses `haven::write_sav()` to facilitate the export to an
#' SPSS `.sav` or `.zsav` file.
#'
#' Below are some things to pay attention to.
#'
#' The `SPSS` syntax file has the proper file names and separators set, so
#' in principle it should run and read the data without alteration. `SPSS`
#' is more strict than `R` with respect to the paths. Always use the full
#' path, otherwise `SPSS` may not be able to find the data file.
#'
#' Factors in `R` translate into categorical variables in `SPSS`. The
#' internal coding of factor levels used in `R` is exported. This is
#' generally acceptable for `SPSS`. However, when the data are to be
#' combined with existing `SPSS` data, watch out for any changes in the
#' factor levels codes.
#'
#' `SPSS` will recognize the data set as a multiply imputed data set, and
#' do automatic pooling in procedures where that is supported. Note however that
#' pooling is an extra option only available to those who license the
#' `MISSING VALUES` module. Without this license, `SPSS` will still
#' recognize the structure of the data, but it will not pool the multiply imputed
#' estimates into a single inference.
#'
#' @param imp The `imp` argument is an object of class `mids`,
#' typically produced by the `mice()` function.
#' @param filename A character string describing the name of the output data
#' file and its extension.
#' @param path A character string containing the path of the output file. The
#' value in `path` is appended to `filedat`. By
#' default, files are written to the current `R` working directory. If
#' `path=NULL` then no file path appending is done.
#' @param compress A logical flag stating whether the resulting SPSS set should
#' be a compressed `.zsav` file.
#' @param silent A logical flag stating whether the location of the saved file should be
#' printed.
#' @return The return value is `NULL`.
#' @author Gerko Vink, dec 2020.
#' @seealso [`mids()`][mids-class]
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
