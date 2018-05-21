## MCMC drivers

## basicMCMC <- function(logll, init, niter, proposal, npop=1)
##   {
##     npar <- length(init)
##     x <- matrix(init, nrow=npar, ncol=npop)
##     chain <- array(NA_real_, dim=c(npar, npop, niter+1))
##     chain[, , 1] <- x
##     for (i in 2:(niter+1)) {
##       for (j in 1:npop){
##         xs <- proposal(x[, j]) #+ sample(c(-1,0, 1), 1)*[, sample(npop, 1)]
##         A <- exp(logll(xs) - logll(x[, j]))
##         x[, j] <- ifelse(rep(runif(1) < A, npar), xs, x[, j])
##       }
##       chain[, , i] <- x
##     }
##     return(chain)
##   }


simpleMH <- function(f, init, niter, proposal)
  {
    npar <- length(init)
    x <- init
    chain <- array(NA_real_, dim=c(npar, niter+1))
    chain[, 1] <- x
    for (i in 2:(niter+1)) {
      xs <- proposal(x)
      A <- f(xs)/f(x)
      if (runif(1) < A) x <- xs
      chain[, i] <- x
    }
    return(t(chain))
  }

covMH <- function(f, init, niter, proposal)
  {
    npar <- length(init)
    x <- init
    chain <- array(NA_real_, dim=c(npar, niter+1))
    chain[, 1] <- x
    m <- init
    sigma <- diag(npar)
    sigma1 <- sigma2 <- sigma
    m1 <- m2 <- m
    for (i in 2:(niter+1)) {
      xs <- proposal(x, sigma)
      ## A <- f(xs)/f(x)
      A <- exp(f(xs)-f(x))
      ## if (is.nan(A) | is.infinite(A)) A <- -1
      if (runif(1) < A) x <- xs
      chain[, i] <- x
      ##
      dd <- x - m1
      m1 <- m1 + dd/i
      sigma1 <- sigma1 + dd%*%t(x-m1)
      m2 <- m2*0.97 + x*0.03
      sigma2 <- sigma2*0.97 + dd%*%t(x-m2)*0.03
      ##
      if (runif(1) < 0.75)
        sigma <- sigma1/(i-1)
      else
        sigma <- sigma2
    }
    print(sigma1/(i-1))
   return(t(chain))
  }
