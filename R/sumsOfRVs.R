
#' Add two RVS
#' 
#' Create a random variable that is the sum of two other random variables
#' 
#' @param X the first RV to add
#' @param Y the second RV to add
addRV <- function(X, Y){
    probsMat <- outer(probs(X), probs(Y), FUN = "*")
    sumsMat <- outer(values(X), values(Y), FUN = "+")
    
    probs <- tapply(probsMat, sumsMat, sum)
    vals <- as.numeric(names(probs))
    make.RV(vals, probs)
}

#' Sum of iid RVs
#' 
#' Create a random variable that is the sum of independent 
#' and identically distributed random variables.
#' 
#' @param X A random variable that has the distribution of interest
#' @param n The number of Xs to sum
#' @export
#' @examples
#' fair.die <- make.RV(1:6, rep(1/6, 6))
#' SofIID(fair.die, n = 3)
SofIID <- function(X, n = 2) {
    stopifnot(is.RV(X))
    RVs <- rep(list(X), n)
    # Probably could just use do.call to call SofI
    # so that if we speed that implementation up
    # this is sped up as well... But this works just fine
    out <- Reduce(addRV, RVs)
    out
}

#' Sum of independent RVs
#' 
#' Creates a random variable that is the sum of 
#' the input random variables.
#' 
#' @param ... list of arbitrary many random variables

#' @export
#' @examples
#' fair.coin <- make.RV(0:1, .5)
#' threecoins <- SofI(fair.coin, fair.coin, fair.coin)
#' all.equal(probs(threecoins)[1], dbinom(0, 3, .5), check.attributes = FALSE)
SofI <- function(...) {
    # Collect the input
    RVs <- list(...)
    # Check that they're all random variables
    stopifnot(all(sapply(RVs, is.RV)))
    # Add em' up
    out <- Reduce(addRV, RVs)
    out
}
