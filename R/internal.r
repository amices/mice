# ------------------------------PadModel-------------------------------

padModel <- function(data, method, predictorMatrix, visitSequence, form, post, nmis, nvar) {
    # Called by mice().  Augments the imputation model by including dummy variables. Adapts data, predictorMatrix, method and
    # visitSequence.  Returns a list whose components make up the padded model.
    categories <- data.frame(yes.no.categorical = factor(rep(FALSE, nvar), levels = c("TRUE", "FALSE")), number.of.dummies = rep(0,
        nvar), yes.no.dummy = factor(rep(FALSE, nvar), levels = c("TRUE", "FALSE")), corresponding.column.dummy = rep(0, nvar))

    # zero is default in corresponding.column.dummy for no dummy variable
    for (j in 1:nvar) {
        if (is.factor(data[, j]) && any(predictorMatrix[, j] != 0)) {
            ## SvB 11mar2013 account other roles for multilevel data
            categories[j, 1] <- TRUE
            data[, j] <- C(data[, j], contr.treatment)  #assign contrast-attribute, choice treatment, to factor   SvB 14/12/08
            n.dummy <- length(levels(data[, j])) - 1
            categories[j, 2] <- n.dummy
            predictorMatrix <- rbind(predictorMatrix, matrix(0, ncol = ncol(predictorMatrix), nrow = n.dummy))
            predictorMatrix <- cbind(predictorMatrix, matrix(rep(predictorMatrix[, j], times = n.dummy), ncol = n.dummy))
            predictorMatrix[1:nvar, j] <- rep(0, times = nvar)
            form <- c(form, rep("", n.dummy))
            ## if (any(predictorMatrix[j,]==1)){ condition outcommented June 30, 2009 SvB to allow for intercept imputation of
            ## categorical variables
            if (any(visitSequence == j)) {
                # if j is imputed, changed June 30, 2009 SvB
                predictorMatrix[(ncol(predictorMatrix) - n.dummy + 1):ncol(predictorMatrix), j] <- rep(1, times = n.dummy)
                ## changed to a loop to correct error in append function when a cat pred is visited more than once SvB Aug 2009
                newcol <- ncol(predictorMatrix) - n.dummy + 1
                nloops <- sum(visitSequence == j)
                for (ii in 1:nloops) {
                  idx <- (1:length(visitSequence))[visitSequence == j][ii]
                  visitSequence <- append(visitSequence, newcol, idx)
                }
            }
            data <- (cbind(data, matrix(0, ncol = n.dummy, nrow = nrow(data))))
            data[is.na(data[, j]), (ncol(predictorMatrix) - n.dummy + 1):ncol(predictorMatrix)] <- NA
            ## PM
            cat.column <- data[!is.na(data[, j]), j]
            data[!is.na(data[, j]), (ncol(predictorMatrix) - n.dummy + 1):ncol(predictorMatrix)] <- model.matrix(~cat.column -
                1)[, -1]
            names(data)[(ncol(predictorMatrix) - n.dummy + 1):ncol(predictorMatrix)] <- paste(attr(data, "names")[j], (1:n.dummy),
                sep = ".")
            method <- c(method, rep("dummy", n.dummy))
            post <- c(post, rep("", n.dummy))  # added SvB Aug 2009
            categories <- rbind(categories, data.frame(yes.no.categorical = rep(FALSE, n.dummy), number.of.dummies = rep(0,
                n.dummy), yes.no.dummy = rep(TRUE, n.dummy), corresponding.column.dummy = rep(j, n.dummy)))
        }
    }

    varnames <- dimnames(data)[[2]]
    dimnames(predictorMatrix) <- list(varnames, varnames)
    names(method) <- varnames
    names(form) <- varnames
    names(post) <- varnames
    names(visitSequence) <- varnames[visitSequence]
    dimnames(categories)[[1]] <- dimnames(data)[[2]]
    return(list(data = as.data.frame(data), predictorMatrix = predictorMatrix, method = method, visitSequence = visitSequence,
        form = form, post = post, categories = categories))
}


# ------------------------------sampler-------------------------------

sampler <- function(p, data, m, imp, r, visitSequence, fromto, printFlag, ...)
# The sampler controls the actual Gibbs sampling iteration scheme This function is called by mice and mice.mids
#
# Authors: S van Buuren, K Groothuis-Oudshoorn Copyright (c) 1999-2008 TNO Quality of Life
{
    ## set up array for convergence checking
    from <- fromto[1]
    to <- fromto[2]
    maxit <- to - from + 1
    if (maxit > 0)
        chainVar <- chainMean <- array(0, dim = c(length(visitSequence), maxit, m), dimnames = list(dimnames(data)[[2]][visitSequence],
            1:maxit, paste("Chain", 1:m))) else chainVar <- chainMean <- NULL

    ## THE ITERATION MAIN LOOP: GIBBS SAMPLER
    if (maxit < 1)
        iteration <- 0 else {
        if (printFlag)
            cat("\n iter imp variable")
        for (k in from:to) {
            #begin k loop : iteration loop
            iteration <- k
            for (i in 1:m) {
                #begin i loop    : repeated imputation loop
                if (printFlag)
                  cat("\n ", iteration, " ", i)

                ## fill the data with the last set of imputations
                for (j in visitSequence) p$data[!r[, j], j] <- imp[[j]][, i]

                ## augment the data with the actual dummy variables
                for (j in setdiff(p$visitSequence, visitSequence)) {
                  cat.columns <- p$data[, p$categories[j, 4]]
                  p$data[, (j:(j + p$categories[p$categories[j, 4], 2] - 1))] <- matrix((model.matrix(~cat.columns - 1)[, -1]),
                    ncol = p$categories[p$categories[j, 4], 2], nrow = nrow(p$data))
                }

                ## iterate once over the variables of the augmented model

                for (j in p$visitSequence) {
                  theMethod <- p$method[j]
                  vname <- dimnames(p$data)[[2]][j]

                  ## store current state
                  oldstate <- get("state", pos = parent.frame())
                  newstate <- list(it = k, im = i, co = j, dep = vname, meth = theMethod, log = oldstate$log)
                  assign("state", newstate, pos = parent.frame(), inherits = TRUE)

                  if (printFlag & theMethod != "dummy")
                    cat(" ", vname)
                  if (theMethod != "" & (!is.passive(theMethod)) & theMethod != "dummy") {
                    # for a true imputation method
                    if (substring(tolower(theMethod), 1, 2) != "2l") {
                      # RJ: for an non-multilevel imputation method
                      # RB: formula-based  specification
                      if (! is.null(p$form) && nchar(p$form[j])>0) {
                          myform <- paste(p$form[j], "0", sep="+")
                          x <- model.matrix(formula(myform), p$data)
                      } else
                          x <- p$data[, p$predictorMatrix[j, ] == 1, drop = FALSE]
                      y <- p$data[, j]
                      ry <- r[, j]
                      nam <- vname
                      if (k == 1)
                        check.df(x, y, ry, ...)  # added 31/10/2012, throw warning for n(obs) < p case
                      f <- paste("mice.impute", theMethod, sep = ".")
                      keep <- remove.lindep(x, y, ry, ...)
                      x <- x[, keep, drop = FALSE]
                      imp[[j]][, i] <- do.call(f, args = list(y, ry, x, ...))
                    } else {
                      # for a multilevel imputation method
                      predictors <- p$predictorMatrix[j, ] != 0
                      # RB: formula-based specification
                      if (! is.null(p$form) && nchar(p$form[j])>0) {
                          myform <- paste(p$form[j], "0", sep="+")
                          x <- model.matrix(formula(myform), p$data)
                      } else
                          x <- p$data[, predictors, drop = FALSE]
                      y <- p$data[, j]
                      ry <- r[, j]
                      type <- p$predictorMatrix[j, predictors]
                      nam <- vname
                      if (k == 1)
                        check.df(x, y, ry, ...)  # added 31/10/2012, throw warning for n(obs) < p case
                      f <- paste("mice.impute", tolower(theMethod), sep = ".")
                      keep <- remove.lindep(x, y, ry, ...)
                      x <- x[, keep, drop = FALSE]
                      type <- type[keep]
                      imp[[j]][, i] <- do.call(f, args = list(y, ry, x, type, ...))
                    }
                    p$data[!r[, j], j] <- imp[[j]][, i]
                  } else if (is.passive(theMethod)) {
                    imp[[j]][, i] <- model.frame(as.formula(theMethod), p$data[!r[, j], ])  #RJ - FIXED passive imputation: as.formula()
                    p$data[!r[, j], j] <- imp[[j]][, i]
                  } else if (theMethod == "dummy") {
                    ## FEH
                    cat.columns <- p$data[, p$categories[j, 4]]
                    p$data[, (j:(j + p$categories[p$categories[j, 4], 2] - 1))] <- matrix((model.matrix(~cat.columns - 1)[,
                      -1]), ncol = p$categories[p$categories[j, 4], 2], nrow = nrow(p$data))
                    remove("cat.columns")
                  }

                  ## optional post-processing
                  cmd <- p$post[j]  # SvB Aug 2009
                  if (cmd != "") {
                    eval(parse(text = cmd))
                    p$data[!r[, j], j] <- imp[[j]][, i]
                  }
                }  # end j loop
            }  # end i loop
            k2 <- k - from + 1
            for (j in 1:length(visitSequence)) {
                jj <- visitSequence[j]
                if (!is.factor(data[, jj])) {
                  chainVar[j, k2, ] <- apply(imp[[jj]], 2, var)
                  chainMean[j, k2, ] <- colMeans(as.matrix(imp[[jj]]))  ##pm 04/02
                }
                if (is.factor(data[, jj])) {
                  for (mm in 1:m) {
                    nc <- as.integer(factor(imp[[jj]][, mm], levels = levels(data[, jj])))
                    chainVar[j, k2, mm] <- var(nc)
                    chainMean[j, k2, mm] <- mean(nc)
                  }
                }
            }
        }  # end iteration loop
        if (printFlag)
            cat("\n")
    }
    return(list(iteration = maxit, imp = imp, chainMean = chainMean, chainVar = chainVar))
}


check.df <- function(x, y, ry, ...) {
    # if needed, writes the df warning message to the log
    df <- sum(ry) - ncol(x)
    mess <- paste("df set to 1. # observed cases:", sum(ry), " # predictors:", ncol(x))
    if (df < 1)
        updateLog(out = mess, frame = 3)
}

remove.lindep <- function(x, y, ry, eps = 1e-04, maxcor = 0.99, allow.na = FALSE, ...) {
    if (ncol(x) == 0)
        return(NULL)
    if (eps <= 0)
        stop("\n Argument 'eps' must be positive.")
    xobs <- x[ry, , drop = FALSE]
    if (allow.na) {
        if (sum(ry) == 0) {
            # escape for columns with only missing data  SvB 10/3/2011
            updateLog(out = "No observed cases, predictor removal skipped", frame = 3)
            return(rep(TRUE, ncol(x)))
        }
    }
    yobs <- as.numeric(y[ry])
    keep <- unlist(apply(xobs, 2, var) > eps)
    keep[is.na(keep)] <- FALSE
    highcor <- suppressWarnings((unlist(apply(xobs, 2, cor, yobs)) < maxcor))
    keep <- keep & highcor
    if (all(!keep))
        updateLog(out = "All predictors are constant or have too high correlation.", frame = 3)
    if (length(keep) == 1) keep[1] <- TRUE  # SvB 19/1/14
    k <- sum(keep)
    cx <- cor(xobs[, keep, drop = FALSE], use = "all.obs")
    eig <- eigen(cx, symmetric = TRUE)
    ncx <- cx
    while (eig$values[k]/eig$values[1] < eps) {
        j <- (1:k)[order(abs(eig$vectors[, k]), decreasing = TRUE)[1]]
        keep[keep][j] <- FALSE
        ncx <- cx[keep[keep], keep[keep], drop = FALSE]
        k <- k - 1
        eig <- eigen(ncx)
    }
    if (!all(keep)) {
        out <- paste(dimnames(x)[[2]][!keep], collapse = ", ")
        updateLog(out = out, frame = 3)
    }
    return(keep)
}


## make list of collinear variables to remove
find.collinear <- function(x, threshold = 0.999, ...) {
    nvar <- ncol(x)
    x <- data.matrix(x)
    r <- !is.na(x)
    nr <- apply(r, 2, sum, na.rm = TRUE)
    ord <- order(nr, decreasing = TRUE)
    xo <- x[, ord, drop = FALSE]  ## SvB 24mar2011
    varnames <- dimnames(xo)[[2]]
    z <- suppressWarnings(cor(xo, use = "pairwise.complete.obs"))
    hit <- outer(1:nvar, 1:nvar, "<") & (abs(z) >= threshold)
    out <- apply(hit, 2, any, na.rm = TRUE)
    return(varnames[out])
}


updateLog <- function(out = NULL, meth = NULL, frame = 2) {
    s <- get("state", parent.frame(frame))
    r <- get("loggedEvents", parent.frame(frame))

    rec <- data.frame(it = s$it, im = s$im, co = s$co, dep = s$dep, meth = ifelse(is.null(meth), s$meth, meth), out = ifelse(is.null(out),
        "", out))

    if (s$log)
        rec <- rbind(r, rec)
    s$log <- TRUE
    assign("state", s, pos = parent.frame(frame), inherits = TRUE)
    assign("loggedEvents", rec, pos = parent.frame(frame), inherits = TRUE)
    return()
}
