setMethod("subset", signature(x = "sequences"),
            function(x, subset) {
            if (missing(subset))
                return(x)
            i <- eval(substitute(subset), envir = c(x@quality, x@sequenceInfo), parent.frame(2))
                  x[i]
              })


          