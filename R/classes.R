### class definitions

setClass("mids",
            representation(
                call      = "call",
                data      = "data.frame" ,
                m         = "numeric",
                nmis      = "integer",
                imp       = "list",
                method    = "character",
                predictorMatrix = "matrix",
                visitSequence = "numeric",
                post      = "character",
                seed      = "numeric",
                iteration = "integer",
                lastSeedValue = "integer",
                chainMean = "array",
                chainVar  = "array",
                pad       = "list"),
            contains  = "list"
)


setClass("mira", 
            representation(
                call      = "call",
                call1     = "call",
                nmis      = "integer",
                analyses  = "list"),
         contains = "list"
)

setClass("mipo",
            representation(
                call      = "call",
                call1     = "call",
                call2     = "call",
                data      = "data.frame" ,
                nmis      = "integer",
                m         = "numeric",
                qhat      = "matrix",
                u         = "array",
                qbar      = "numeric",
                ubar      = "matrix",
                b         = "matrix",
                t         = "matrix",
                r         = "numeric",
                df        = "numeric",
                f         = "numeric"),
            contains  = "list"
)

