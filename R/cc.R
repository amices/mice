###--------------------------cc------------------------------
### complete cases, convenience functions

setGeneric("cc",
           def = function(x, drop=TRUE) standardGeneric("cc"),
           package = "mice",
           useAsDefault = function(x, drop) return(x[cci(x)]))
setMethod("cc", signature(x="mids"),
          function(x, drop) return(x$data[cci(x),,drop=drop]))
#setMethod("cc", signature(x="mi"),
#          function(x, drop) return(x@data[cci(x),,drop=drop]))
setMethod("cc", signature(x="data.frame"),
          function(x, drop) return(x[cci(x),,drop=drop]))
setMethod("cc", signature(x="matrix"),
          function(x, drop) return(x[cci(x),,drop=drop]))


###--------------------------cci-----------------------------

setGeneric("cci",
           def = function(x) standardGeneric("cci"),
           package = "mice",
           useAsDefault = function(x) return(!is.na(x)))
setMethod("cci", signature(x="mids"),
          function(x) return(apply(!is.na(x$data),1,all)))
#setMethod("cci", signature(x="mi"),
#          function(x) return(apply(!is.na(x@data),1,all)))
setMethod("cci", signature(x="data.frame"),
          function(x) return(apply(!is.na(x),1,all)))
setMethod("cci", signature(x="matrix"),
          function(x) return(apply(!is.na(x),1,all)))


###--------------------------ccn-----------------------------

ccn <- function(x){
  return(sum(cci(x)))
}


###--------------------------ic----------------------------------
### incomplete cases, convenience functions

setGeneric("ic",
           def = function(x, drop=TRUE) standardGeneric("ic"),
           package = "mice",
           useAsDefault = function(x, drop) return(x[ici(x)]))
setMethod("ic", signature(x="mids"),
          function(x, drop) return(x$data[ici(x),,drop=drop]))
#setMethod("ic", signature(x="mi"),
#          function(x, drop) return(x@data[ici(x),,drop=drop]))
setMethod("ic", signature(x="data.frame"),
          function(x, drop) return(x[ici(x),,drop=drop]))
setMethod("ic", signature(x="matrix"),
          function(x, drop) return(x[ici(x),,drop=drop]))


###--------------------------ici-----------------------------

setGeneric("ici",
           def = function(x) standardGeneric("ici"),
           package = "mice",
           useAsDefault = function(x) return(is.na(x)))
setMethod("ici", signature(x="mids"),
          function(x) return(apply(is.na(x$data),1,any)))
#setMethod("ici", signature(x="mi"),
#          function(x) return(apply(is.na(x@data),1,any)))
setMethod("ici", signature(x="data.frame"),
          function(x) return(apply(is.na(x),1,any)))
setMethod("ici", signature(x="matrix"),
          function(x) return(apply(is.na(x),1,any)))


###--------------------------icn-----------------------------

icn <- function(x){
  return(sum(ici(x)))
}

