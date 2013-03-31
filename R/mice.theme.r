#'Set the theme for the plotting Trellis functions
#'
#'The \code{mice.theme()} function sets default choices for  
#'Trellis plots that are built into \pkg{mice}. 
#'
#'@aliases mice.theme
#'@param transparent A logical indicating whether alpha-transparancy is
#'allowed. The default is \code{TRUE}.
#'@param alpha.fill A numerical values between 0 and 1 that indicates the
#'default alpha value for fills.
#'@return \code{mice.theme()} returns a named list that can be used as a theme in the functions in
#'\pkg{lattice}. By default, the \code{mice.theme()} function sets
#'\code{transparent <- TRUE} if the current device \code{.Device} supports
#'semi-transparent colors.
#'@author Stef van Buuren 2011
#'@export
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
