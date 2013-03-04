#' Make a random variable
#' 
#' Make a random variable from a set of values and probabilities
#' 
#' @param values The possible values the random variable can take
#' @param probs The corresponding probabilities. Defaults to uniform
#' for every value if nothing is provided
#' 
#' @export
#' @examples
#' fair.die <- make.RV(1:6, rep(1/6, 6))
#' fair.die
make.RV <- function(values, probs = NULL){
    values <- checkValues(values)
    probs <- checkProbabilities(probs, length(values))
    names(values) <- probs
    class(values) <- "RV"
    values
}

checkValues <- function(values){
    # No need to check if numeric... just convert!
    values <- suppressWarnings(as.numeric(values))
    if(any(is.na(values))){
        stop("Bad input for values")
    }
    
    values
}

checkProbabilities <- function(probs, lengthX){
    
    # If null assume a uniform distribution...
    if(is.null(probs)){
        probs <- rep(1/lengthX, lengthX)
    }
    
    # If not numeric attempt to coerce and set any values
    # that can't be coerced to 0.
    if(!is.numeric(probs)){
        probs <- as.numeric(probs)
        if(any(is.na(probs))){
            warning("Could not convert all probability values to numeric. ", 
                    "These values will be treated as 0s.")
            probs[is.na(probs)] <- 0
        }
    }
    
    
    # TODO: Find a way to clean up these checks into something
    #       more concise.
    if(lengthX %% length(probs) != 0){
        warning("Values length is not a multiple of probs length. ",
                "Results may not be as expected.")
    }
    
    if(length(probs) < lengthX){
        probs <- rep(probs, length.out = lengthX)    
    }
    
    if(length(probs) > lengthX){
        warning("Probs length is longer than values length. ",
                "Only the required number of probabilities will be used")
        probs <- rep(probs, length.out = lengthX)    
        
    }
    
    # stop if we have negative probabilities
    # or the probabilities sum to 0
    stopifnot(all(probs >= 0), !isTRUE(all.equal(sum(probs), 0)))
    
    probs <- probs/sum(probs)
    probs
}

#' Check if input is RV
#' 
#' Function to check if an object is a random variable
#' 
#' @param X Object to check if it is a random variable
#' @export
#' @examples
#' coin <- make.RV(0:1)
#' is.RV(coin)
#' is.RV(0:1)
is.RV <- function(X){
    inherits(X, "RV")
}

#' Coerce into RV
#' 
#' Coerces something that is a vector with names
#' into a RV
#' 
#' @param X The object to coerce into a RV
#' @export
#' @examples
#' obj <- 1:6
#' names(obj) <- rep(1/6, 6)
#' as.RV(obj)
as.RV <- function(X){
    # maybe do some checks?
    make.RV(as.numeric(X), names(X))
}

#' Get probabilities
#' 
#' Get the probabilities from a random variable
#' 
#' @param X A random variable
#' 
#' @export
#' @examples
#' coin <- make.RV(0:1, .5)
#' probs(coin)
probs <- function(X){
    if(!is.RV(X)){
        stop("Not a random variable")
    }
    as.numeric(names(X))
}

#' Get values
#' 
#' Get the possible output values from a random variable
#' 
#' @param X A random variable
#' 
#' @export
#' @examples
#' fair.die <- make.RV(1:6)
#' values(fair.die)
values <- function(X){
    if(!is.RV(X)){
        stop("Not a random variable")
    }
    as.numeric(X)
}




