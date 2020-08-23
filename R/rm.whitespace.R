## Authors: Gerko Vink, Stef van Buuren

rm.whitespace <- function(string, side = "both") {
  side <- match.arg(side, c("left", "right", "both"))
  pattern <- switch(side,
    left = "^\\s+",
    right = "\\s+$",
    both = "^\\s+|\\s+$"
  )
  sub(pattern, "", string)
}
