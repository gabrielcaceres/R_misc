### Method to keep ts properties while subsetting
"[.ts" <- function (x, i, j, drop = TRUE, tsp.keep){
  ## Original method when tsp.keep missing
  if (missing(tsp.keep)){
    y <- NextMethod("[")
    if (missing(i))
      y <- ts(y, start = start(x), frequency = frequency(x))
    ## If tsp.keep given
  }else {
    ## Make sure call order is correct with appropriate missing i or j
    fcall <- match.call()
    fcall$tsp.keep <- NULL
    if ((NCOL(x) > 1) && missing(j)){
      fcall$drop <- NULL
      fcall$j <- quote(expr=)
      fcall$drop <- drop
    }
    if ((NCOL(x) > 1) && missing(i)){
      fcall$drop <- NULL
      fcall$j <- NULL
      fcall$i <- quote(expr=)
      fcall$j <- j
      fcall$drop <- drop
    }
    ## Keep tsp attributes if tsp.keep=TRUE
    if (isTRUE(tsp.keep)){
      y <- eval(fcall)
      if (!missing(i)){
        xtime <- time(x)
        l <- length(i)
        y <- ts(y, start=xtime[i[1]], end=xtime[i[l]], frequency = frequency(x))
      }
      ## Force drop of tsp attributes if tsp.keep!=TRUE
    } else{
      y <- eval(fcall)
      tsp(y) <- NULL
    }
  }
  return(y)
}
