

# --------------------------------IS.MIDS--------------------------------------

is.mids <- function(x) {
    inherits(x, "mids")
}


# --------------------------------IS.MIRA--------------------------------------

is.mira <- function(x) {
    inherits(x, "mira")
}


# --------------------------------IS.MIPO--------------------------------------

is.mipo <- function(x) {
    inherits(x, "mipo")
}

# ------------------------------is.passive------------------------------------

is.passive <- function(string) {
    return("~" == substring(string, 1, 1))
}
