###
### This file contains methods for mids-objects for
### the following generic functions defined in lattice:
###
### xyplot
### stripplot
###
### SvB, Feb 2011.


mice.theme <- function(transparent=TRUE, alpha.fill=0.3){
  filler <- function(transparent, alpha){
    if(transparent) return(c(hcl(240,100,40,alpha),hcl(0,100,40,alpha)))
    return(c(hcl(240,100,40),hcl(0,100,40)))
  }
  if (missing(transparent)) transparent <- supports.transparent()
  if (missing(alpha.fill)) alpha.fill <- ifelse(transparent, 0.3, 0)
  list(superpose.symbol = list(
         col = mdc(1:2),
         fill = filler(transparent, alpha.fill),
         pch = 1),
       superpose.line = list(
         col = mdc(4:5),
         lwd = 1
         ),
       box.dot = list(
         col = mdc(1:2)
      ),
       box.rectangle = list(
         col = mdc(4:5)
         ),
       box.symbol = list(
         col = mdc(1:2)
         ),
       plot.symbol = list(
         col = mdc(1:2),
         fill = filler(transparent, alpha.fill),
         pch = 1
         ),
       plot.line = list(
         col = mdc(4:5)
         ),
       superpose.polygon = list(
         col = filler(transparent, alpha.fill)
       ),
       strip.background = list(
         col = "grey95"
       ),
       mice = list(
         flag = TRUE
         )
     )
}

xyplot.mids <- function(x,
                        data,
                        na.groups = NULL,
                        groups = NULL,
                        as.table = TRUE,
                        theme = mice.theme(),
                        allow.multiple = TRUE,
                        outer = TRUE,
                        drop.unused.levels = lattice.getOption("drop.unused.levels"),
                        ...,
                        subscripts = TRUE,
                        subset = TRUE)
{
  call <- match.call()
  if (!is.mids(x)) stop("Argument 'x' must be a 'mids' object")
  if (missing(data)) stop("Missing formula")
  formula <- data

  ## unpack data and response indicator
  cd <- data.frame(complete(x, "long", include=TRUE))
  r <- as.data.frame(is.na(x$data))

  ## evaluate na.group in response indicator 
  nagp <- eval(expr=substitute(na.groups), envir=r, enclos=parent.frame())
  if (is.expression(nagp)) nagp <- eval(expr=nagp, envir=r, enclos=parent.frame())

  ## evaluate groups in imputed data
  ngp <- eval(expr=substitute(groups), envir=cd, enclos=parent.frame())
  if (is.expression(ngp)) ngp <- eval(expr=ngp, envir=cd, enclos=parent.frame())
  groups <- ngp
  
  ## evaluate subset in imputed data
  ss <- eval(expr=substitute(subset), envir=cd, enclos=parent.frame())
  if (is.expression(ss)) ss <- eval(expr=ss, envir=cd, enclos=parent.frame())
  subset <- ss

  ## evaluate further arguments before parsing
  dots <- list(...)
  args <- list(allow.multiple = allow.multiple,
               outer = outer,
               drop.unused.levels = drop.unused.levels,
               subscripts = subscripts,
               as.table = as.table)

  ## determine the y-variables
  form  <- latticeParseFormula(model=formula, data=cd, subset = subset,
                               groups = groups, multiple = allow.multiple,
                               outer = outer, subscripts = TRUE,
                               drop = drop.unused.levels)
  ynames <- unlist(lapply(strsplit(form$left.name," \\+ "), rm.whitespace))   ## Jul2011

  ## calculate selection vector gp
  nona <- is.null(call$na.groups)
  if (!is.null(call$groups) & nona) gp <- call$groups
  else {
    if (nona) {
      na.df <- r[, ynames, drop=FALSE]
      gp <- unlist(lapply(na.df, rep, x$m+1))
    } else {
      gp <- rep(nagp, length(ynames)*(x$m+1))
    }
  }
  
  ## change axis defaults of extended formula interface
  if (is.null(call$ylab)) {
    args$ylab <- ""
    if (length(ynames)==1) args$ylab <- ynames
  }
  if (is.null(call$scales)) {
      args$scales <- list()
      if (length(ynames)>1)
        args$scales <- list(x=list(relation="free"), y=list(relation="free"))
    }

  ## ready
  args <- c(x=formula, data=list(cd),
            groups=list(gp), 
            args, dots, subset=call$subset)

  ## go
  tp <- do.call("xyplot", args)
  tp <- update(tp, par.settings = theme)
  return(tp)
}


stripplot.mids <- function(x,
                           data,
                           na.groups = NULL,
                           groups = NULL,
                           as.table = TRUE,
                           theme = mice.theme(),
                           allow.multiple = TRUE,
                           outer = TRUE,
                           drop.unused.levels = lattice.getOption("drop.unused.levels"),
                           panel = lattice.getOption("panel.stripplot"),
                           default.prepanel = lattice.getOption("prepanel.default.stripplot"),
                           jitter.data = TRUE,
                           horizontal = FALSE,
                           ...,
                           subscripts = TRUE,
                           subset = TRUE)
{
  call <- match.call()
  if (!is.mids(x)) stop("Argument 'x' must be a 'mids' object")

  ## unpack data and response indicator
  cd <- data.frame(complete(x, "long", include=TRUE))
  r <- as.data.frame(is.na(x$data))

  ## evaluate na.group in response indicator 
  nagp <- eval(expr=substitute(na.groups), envir=r, enclos=parent.frame())
  if (is.expression(nagp)) nagp <- eval(expr=nagp, envir=r, enclos=parent.frame())

  ## evaluate groups in imputed data
  ngp <- eval(expr=substitute(groups), envir=cd, enclos=parent.frame())
  if (is.expression(ngp)) ngp <- eval(expr=ngp, envir=cd, enclos=parent.frame())
  groups <- ngp
  
  ## evaluate subset in imputed data
  ss <- eval(expr=substitute(subset), envir=cd, enclos=parent.frame())
  if (is.expression(ss)) ss <- eval(expr=ss, envir=cd, enclos=parent.frame())
  subset <- ss

  ## evaluate further arguments before parsing
  dots <- list(...)
  args <- list(panel = panel,
               default.prepanel = default.prepanel,
               allow.multiple = allow.multiple,
               outer = outer,
               drop.unused.levels = drop.unused.levels,
               subscripts = subscripts,
               as.table = as.table,
               jitter.data = jitter.data,
               horizontal = horizontal)

  ## create formula if not given (in call$data !)
  vnames <- names(cd)[-(1:2)]
  allfactors <- unlist(lapply(cd,is.factor))[-(1:2)]
  if (missing(data)) {
    vnames <- vnames[!allfactors]
    formula <- as.formula(paste(paste(vnames,collapse="+",sep=""),"~.imp",sep=""))
  } else {
    ## pad abbreviated formula
    abbrev <- length(grep("~", call$data))==0
    if (abbrev) {
      if (length(call$data)>1) stop("Cannot pad extended formula.")
      else formula <- as.formula(paste(call$data,"~.imp",sep=""))
    } else {
      formula <- data
    }
  }
  
  ## determine the y-variables
  form  <- latticeParseFormula(model=formula, data=cd, subset = subset,
                               groups = groups, multiple = allow.multiple,
                               outer = outer, subscripts = TRUE,
                               drop = drop.unused.levels)
  ynames <- unlist(lapply(strsplit(form$left.name," \\+ "), rm.whitespace))  ## Jul2011
  xnames <- unlist(lapply(strsplit(form$right.name," \\+ "), rm.whitespace))  ## Jul2011

  ## calculate selection vector gp
  nona <- is.null(call$na.groups)
  if (!is.null(call$groups) & nona) gp <- call$groups
  else {
    if (nona) {
      na.df <- r[, ynames, drop=FALSE]
      gp <- unlist(lapply(na.df, rep, x$m+1))
    } else {
      gp <- rep(nagp, length(ynames)*(x$m+1))
    }
  }

  ## change axis defaults of extended formula interface
  if (is.null(call$xlab) & !is.na(match(".imp",xnames))) {
    dots$xlab <- ""
    if (length(xnames)==1) dots$xlab <- "Imputation number"
  }
  
  if (is.null(call$ylab)) {
    args$ylab <- ""
    if (length(ynames)==1) args$ylab <- ynames
  }
  if (is.null(call$scales)) {
      args$scales <- list()
      if (length(ynames)>1)
        args$scales <- list(x=list(relation="free"), y=list(relation="free"))
    }

  ## ready
  args <- c(x=formula, data=list(cd),
            groups=list(gp), 
            args, dots, subset=call$subset)  

  ## go
  tp <- do.call("stripplot", args)
  tp <- update(tp, par.settings = theme)
  return(tp)

}




bwplot.mids <- function(x,
                        data,
                        na.groups = NULL,
                        groups = NULL,
                        as.table = TRUE,
                        theme = mice.theme(),
                        mayreplicate = TRUE,
                        allow.multiple = TRUE,
                        outer = TRUE,
                        drop.unused.levels = lattice.getOption("drop.unused.levels"),
                        ...,
                        subscripts = TRUE,
                        subset = TRUE)
{
  call <- match.call()
  if (!is.mids(x)) stop("Argument 'x' must be a 'mids' object")

  ## unpack data and response indicator
  cd <- data.frame(complete(x, "long", include=TRUE))
  r <- as.data.frame(is.na(x$data))

  ## evaluate na.group in response indicator 
  nagp <- eval(expr=substitute(na.groups), envir=r, enclos=parent.frame())
  if (is.expression(nagp)) nagp <- eval(expr=nagp, envir=r, enclos=parent.frame())

  ## evaluate groups in imputed data
  ngp <- eval(expr=substitute(groups), envir=cd, enclos=parent.frame())
  if (is.expression(ngp)) ngp <- eval(expr=ngp, envir=cd, enclos=parent.frame())
  groups <- ngp
  
  ## evaluate subset in imputed data
  ss <- eval(expr=substitute(subset), envir=cd, enclos=parent.frame())
  if (is.expression(ss)) ss <- eval(expr=ss, envir=cd, enclos=parent.frame())
  subset <- ss

  ## evaluate further arguments before parsing
  dots <- list(...)
  args <- list(allow.multiple = allow.multiple,
               outer = outer,
               drop.unused.levels = drop.unused.levels,
               subscripts = subscripts,
               as.table = as.table)

  ## create formula if not given (in call$data !)
  vnames <- names(cd)[-(1:2)]
  allfactors <- unlist(lapply(cd,is.factor))[-(1:2)]
  if (missing(data)) {
    vnames <- vnames[!allfactors]
    formula <- as.formula(paste(paste(vnames,collapse="+",sep=""),"~.imp",sep=""))
  } else {
    ## pad abbreviated formula
    abbrev <- length(grep("~", call$data))==0
    if (abbrev) {
      if (length(call$data)>1) stop("Cannot pad extended formula.")
      else formula <- as.formula(paste(call$data,"~.imp",sep=""))
    } else {
      formula <- data
    }
  }
  
  ## determine the y-variables
  form  <- latticeParseFormula(model=formula, data=cd, subset = subset,
                               groups = groups, multiple = allow.multiple,
                               outer = outer, subscripts = TRUE,
                               drop = drop.unused.levels)
  ynames <- unlist(lapply(strsplit(form$left.name," \\+ "), rm.whitespace))  ## Jul2011
  xnames <- unlist(lapply(strsplit(form$right.name," \\+ "), rm.whitespace))  ## Jul2011

  ## groups is not useful in bwplot
  ## in order to force subgroup analysis,
  ## make the observed data in .imp>0 missing data
  nona <- is.null(call$na.groups)
  if (!is.null(call$groups) & nona) gp <- call$groups
  else {
    if (nona) {
      for (i in 1:length(ynames)) {
        yvar <- ynames[i]
        select <- cd$.imp!=0 & !r[,yvar]
        cd[select, yvar] <- NA
      }
    } else {
      for (i in 1:length(ynames)) {
        yvar <- ynames[i]
        select <- cd$.imp!=0 & !nagp
        cd[select, yvar] <- NA
      }
    }
  }

  ## replicate color 2 if .imp is part of xnames
  mustreplicate <- !is.na(match(".imp",xnames)) & mayreplicate
  if (mustreplicate) {
    theme$box.dot$col <- rep(theme$box.dot$col[1:2], c(1,x$m))
    theme$box.rectangle$col <- rep(theme$box.rectangle$col[1:2], c(1,x$m))
    theme$box.umbrella$col  <- rep(theme$box.rectangle$col[1:2], c(1,x$m))
    theme$plot.symbol$col <- mdc(3)
    theme$plot.symbol$pch <- 1
  }
  
  ## change axis defaults of extended formula interface
  if (is.null(call$xlab) & !is.na(match(".imp",xnames))) {
    dots$xlab <- ""
    if (length(xnames)==1) dots$xlab <- "Imputation number"
  }

  if (is.null(call$ylab)) {
    args$ylab <- ""
    if (length(ynames)==1) args$ylab <- ynames
  }
  if (is.null(call$scales)) {
      args$scales <- list()
      if (length(ynames)>1)
        args$scales <- list(x=list(relation="free"), y=list(relation="free"))
    }
  
  ## ready
  args <- c(x = formula, data = list(cd),
            groups = list(groups),
            args, dots, subset = call$subset)

  ## go
  tp <- do.call("bwplot", args)
  tp <- update(tp, par.settings = theme)
  return(tp)
}


densityplot.mids <- function(x,
             data,
             na.groups = NULL,
             groups = NULL,
             as.table = TRUE,
             plot.points = FALSE,
             theme = mice.theme(),
             mayreplicate = TRUE,
             thicker = 2.5,
             allow.multiple = TRUE,
             outer = TRUE,
             drop.unused.levels = lattice.getOption("drop.unused.levels"),
             panel = lattice.getOption("panel.densityplot"),
             default.prepanel = lattice.getOption("prepanel.default.densityplot"),
             ...,
             subscripts = TRUE,
             subset = TRUE)
{
  call <- match.call()
  if (!is.mids(x)) stop("Argument 'x' must be a 'mids' object")

  ## unpack data and response indicator
  cd <- data.frame(complete(x, "long", include=TRUE))
  r <- as.data.frame(is.na(x$data))

  ## evaluate na.group in response indicator 
  nagp <- eval(expr=substitute(na.groups), envir=r, enclos=parent.frame())
  if (is.expression(nagp)) nagp <- eval(expr=nagp, envir=r, enclos=parent.frame())

  ## evaluate groups in imputed data
  ngp <- eval(expr=substitute(groups), envir=cd, enclos=parent.frame())
  if (is.expression(ngp)) ngp <- eval(expr=ngp, envir=cd, enclos=parent.frame())
  groups <- ngp
  
  ## evaluate subset in imputed data
  ss <- eval(expr=substitute(subset), envir=cd, enclos=parent.frame())
  if (is.expression(ss)) ss <- eval(expr=ss, envir=cd, enclos=parent.frame())
  subset <- ss

  ## evaluate further arguments before parsing
  dots <- list(...)
  args <- list(panel = panel,
               default.prepanel = default.prepanel,
               allow.multiple = allow.multiple,
               outer = outer,
               drop.unused.levels = drop.unused.levels,
               subscripts = subscripts,
               as.table = as.table,
               plot.points = plot.points)

  ## create formula if not given (in call$data !)
  vnames <- names(cd)[-(1:2)]
  allfactors <- unlist(lapply(cd,is.factor))[-(1:2)]
  if (missing(data)) {
    vnames <- vnames[!allfactors & x$nmis>2 & x$nmis < nrow(x$data)-1]
    formula <- as.formula(paste("~",paste(vnames,collapse="+",sep=""),sep=""))
  } else formula <- data
  
  ## determine the y-variables
  form  <- latticeParseFormula(model=formula, data=cd, subset = subset,
                               groups = groups, multiple = allow.multiple,
                               outer = outer, subscripts = TRUE,
                               drop = drop.unused.levels)
  xnames <- unlist(lapply(strsplit(form$right.name," \\+ "), rm.whitespace))  ## Jul2011

  ## calculate selection vector gp
  nona <- is.null(call$na.groups)
  if (!is.null(call$groups) & nona) gp <- call$groups
  else {
    if (nona) {
      ## na.df <- r[, xnames, drop=FALSE]
      ## imp0 <- rep(cd$.imp==0, x$m+1)
      ## ss <- rep(subset, x$m+1)
      ## gp <- unlist(lapply(na.df, rep, x$m+1))
      ## gp[imp0] <- !gp[imp0]
      ## call$subset <- ss & gp
      for (i in 1:length(xnames)) {
        xvar <- xnames[i]
        select <- cd$.imp!=0 & !r[,xvar]
        cd[select, xvar] <- NA
      }
      gp <- rep(cd$.imp, length(xnames))
    } else {
      for (i in 1:length(xnames)) {
        xvar <- xnames[i]
        select <- cd$.imp!=0 & !nagp
        cd[select, xvar] <- NA
      }
      gp <- rep(cd$.imp, length(xnames))
    }
  }

  ## replicate color 2 if group=.imp is part of xnames
  mustreplicate <- !(!is.null(call$groups) & nona) & mayreplicate
  if (mustreplicate) {
    theme$superpose.line$col <- rep(theme$superpose.line$col[1:2], c(1,x$m))
    theme$superpose.line$lwd <- rep(c(theme$superpose.line$lwd[1]*thicker, theme$superpose.line$lwd[1]),c(1,x$m))
    theme$superpose.symbol$col <- rep(theme$superpose.symbol$col[1:2], c(1,x$m))
    theme$superpose.symbol$pch <- c(NA,49:(49+x$m-1))
  }

  ## change axis defaults of extended formula interface  
  if (is.null(call$xlab)) {
    args$xlab <- ""
    if (length(xnames)==1) args$xlab <- xnames
  }
  if (is.null(call$scales)) {
      args$scales <- list()
      if (length(xnames)>1)
        args$scales <- list(x=list(relation="free"), y=list(relation="free"))
    }

  ## ready
  args <- c(x=formula, data=list(cd),
            groups=list(gp),
            args, dots, subset=call$subset)  

  ## go
  tp <- do.call("densityplot", args)
  tp <- update(tp, par.settings = theme)
  return(tp)
}
