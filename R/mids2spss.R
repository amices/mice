#'Export \code{mids} object to SPSS
#'
#'Converts a \code{mids} object into a format recognized by SPSS, and writes
#'the data and the SPSS syntax files.
#'
#'This function automates most of the work needed to export a \code{mids}
#'object to SPSS. It uses a modified version of \code{writeForeignSPSS()} from
#'the \code{foreign} package. The modified version allows for a choice of the
#'field and decimal separators, and makes some improvements to the formatting,
#'so that the generated syntax file is amenable to the \code{INCLUDE} statement
#'in SPSS.
#'
#'Below are some things to pay attention to.
#'
#'The \code{SPSS} syntax file has the proper file names and separators set, so
#'in principle it should run and read the data without alteration. \code{SPSS}
#'is more strict than \code{R} with respect to the paths. Always use the full
#'path, otherwise \code{SPSS} may not be able to find the data file.
#'
#'Factors in \code{R} translate into categorical variables in \code{SPSS}. The
#'internal coding of factor levels used in \code{R} is exported. This is
#'generally acceptable for \code{SPSS}. However, when the data are to be
#'combined with existing \code{SPSS} data, watch out for any changes in the
#'factor levels codes. The \code{read.spss()} in package \code{foreign} for
#'reading \code{.sav} uses its own internal numbering scheme \code{1,2,3,...}
#'for the levels of a factor. Consequently, changes in factor code can cause
#'discrepancies in factor level when re-imported to \code{SPSS}. The solution
#'is to manually recode the factor level in \code{SPSS}.
#'
#'\code{SPSS} will recognize the data set as a multiply imputed data set, and
#'do automatic pooling in procedures where that is supported. Note however that
#'pooling is an extra option only available to those who license the
#'\code{MISSING VALUES} module. Without this license, \code{SPSS} will still
#'recognize the structure of the data, but not do any pooling.
#'
#'@param imp The \code{imp} argument is an object of class \code{mids},
#'typically produced by the \code{mice()} function.
#'@param filedat A character string describing the name of the output data
#'file.
#'@param filesps A character string describing the name of the output syntax
#'file.
#'@param path A character string containing the path of the output file. The
#'value in \code{path} is appended to \code{filedat} and \code{filesps}. By
#'default, files are written to the current \code{R} working directory. If
#'\code{path=NULL} then no file path appending is done.
#'@param sep The separator between the data fields.
#'@param dec The decimal separator for numerical data.
#'@param silent A logical flag stating whether the names of the files should be
#'printed.
#'@return The return value is \code{NULL}.
#'@author Stef van Buuren, dec 2010.
#'@seealso \code{\link[=mids-class]{mids}}
#'@keywords manip
#'@export
mids2spss <- function(imp, filedat="midsdata.txt", filesps="readmids.sps",
                      path=getwd(), sep="\t", dec=".", silent=FALSE) {
    
    miceWriteForeignSPSS <- function (df, datafile, codefile, varnames = NULL, dec=".", sep="\t") 
    {
        ##adapted version of writeForeignSPSS from foreign package to write mids-objects
        adQuote <- function (x) paste0("\"", x, "\"")
        dfn <- lapply(df, function(x) if (is.factor(x)) as.numeric(x) else x)
        eol <- paste0(sep,"\n")
        write.table(dfn, file = datafile, row.names = FALSE, col.names = FALSE, 
                    sep = sep, dec = dec, quote = FALSE, na = "", eol=eol)
        varlabels <- names(df)
        if (is.null(varnames)) {
            varnames <- abbreviate(names(df), 8L)
            if (any(nchar(varnames) > 8L)) 
                stop("I cannot abbreviate the variable names to eight or fewer letters")
            if (any(varnames != varlabels)) 
                warning("some variable names were abbreviated")
        }
        varnames <- gsub("[^[:alnum:]_\\$@#]", "\\.", varnames)
        dl.varnames <- varnames
        if (any(chv <- vapply(df, is.character, logical(1)))) {
            lengths <- vapply(df[chv], function(v) max(nchar(v)), numeric(1))
            if (any(lengths > 255L)) 
                stop("Cannot handle character variables longer than 255")
            lengths <- paste0("(A", lengths, ")")
            star <- ifelse(c(FALSE, diff(which(chv) > 1L)), " *", 
                           " ")
            dl.varnames[chv] <- paste(star, dl.varnames[chv], lengths)
        }
        if (sep=="\t") freefield <- " free (TAB)\n"
        if (sep!="\t") freefield <- cat(' free (\"',sep,'\")\n',sep="")
        cat("DATA LIST FILE=", adQuote(datafile), freefield, 
            file = codefile)
        cat(" /", dl.varnames, ".\n\n", file = codefile, append = TRUE, fill=60, labels=" ")
        cat("VARIABLE LABELS\n", file = codefile, append = TRUE)
        cat(" ",paste(varnames, adQuote(varlabels), "\n"), ".\n", file = codefile, 
            append = TRUE)
        factors <- vapply(df, is.factor, logical(1))
        if (any(factors)) {
            cat("\nVALUE LABELS\n", file = codefile, append = TRUE)
            for (v in which(factors)) {
                cat(" /", varnames[v], "\n", file = codefile, append = TRUE)
                levs <- levels(df[[v]])
                for (u in seq_along(levs)) 
                    cat(paste("  ",seq_along(levs)[u], adQuote(levs)[u], sep = " "), 
                        file = codefile, append = TRUE, fill=60)
            }
            cat(" .\n", file = codefile, append = TRUE)
        }
        cat("\nEXECUTE.\n", file = codefile, append = TRUE)
        cat("SORT CASES by Imputation_.\n",  file = codefile, append = TRUE)
        cat("SPLIT FILE layered by Imputation_.\n",  file = codefile, append=TRUE)
    }
    
    if(!is.mids(imp)) stop("Exports only objects of class 'mids'.")
    imputed <- complete(imp, "long", include=TRUE)[,-2]
    names(imputed)[1] <- "Imputation_"
    f <- imputed[,"Imputation_"]
    imputed[,"Imputation_"] <- as.numeric(c(levels(f),NA))[f]
    if (!is.null(path)) {
        filedat <- file.path(path,filedat)
        filesps <- file.path(path,filesps)
    }
    miceWriteForeignSPSS(imputed, filedat, filesps, varnames=names(imputed),sep=sep,dec=dec)
    if (!silent) {
        cat("Data values written to",filedat,"\n")
        cat("Syntax file written to",filesps,"\n")
    }
}
