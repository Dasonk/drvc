
#' Probability of event
#' 
#' Compute the probability of an event for a random variable
#' 
#' @param event The outcome of some comparison acting on a random variable
#' 
#' @export
#' @examples
#' fair.die <- make.RV(1:6, rep(1/6, 6))
#' P(fair.die == 1)
#' P(fair.die > 3)
#' P(fair.die^2 == 16)
P <- function(event){
    if(is.null(names(event))){
        stop("Input to P requires a names attribute")
    }
    probs <- as.numeric(names(event))
    sum(probs[event])
}


#' Expected value
#' 
#' Get the expected value of a random variable
#' 
#' @param X A random variable
#' @export
E <- function(X){
    if(!is.RV(X)){
        stop("Input to E must be a random variable")
    }
    sum(X * probs(X))
}

#' Variance of RV
#' 
#' Get the variance of a random variable
#' 
#' @param X A random variable
#' @export
Var <- function(X){
    if(!is.RV(X)){
        stop("Input to Var must be a random variable")
    }
    sum((X - E(X))^2 * probs(X))
}

V <- Var

#' Standard deviation
#' 
#' Standard deviation of a random variable
#' 
#' @param X A random variable
#' @export
Sd <- function(X){
    if(!is.RV(X)){
        stop("Input to Sd must be a random variable")
    }
    sqrt(Var(X))
}

SD <- Sd

#' Skewness
#' 
#' Skewness of a random variable
#' 
#' @param X A random variable
#' @export
#' @examples
#' fair.die <- make.RV(1:6, rep(1/6, 6))
#' Skew(fair.die)
Skew <- function(X){
    if(!is.RV(X)){
        stop("Input to Skew must be a random variable")
    }
    E((X-E(X))^3) / Sd(X)^3
}

#' Kurtosis
#' 
#' Kurtosis of a random variable
#'
#' @param X random variable
#' @export
#' @examples
#' fair.die <- make.RV(1:6, rep(1/6, 6))
#' Kurt(fair.die)
Kurt <- function(X){
    if(!is.RV(X)){
        stop("Input to Kurt must be a random variable")
    }
    E((X-E(X))^4) / Var(X)^2
}