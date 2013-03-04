
#' Comparisons
#' 
#' Comparing random variables
#' 
#' @param X The first item to compare
#' @param Y The second item to compare
#' @method > RV
#' @S3method > RV
#' @rdname RVcomparisons
#' @export
#' @examples
#' X <- make.RV(0:1, .5)
#' Y <- make.RV(0:1, c(.2, .8))
#' P(X > Y)
#' P(X >= Y)
#' P(X < Y)
#' P(X <= Y)
#' P(X == Y)
#' P(X != Y)
#' P(X == 1)
#' P(X - Y == 0)
`>.RV` <- function(X, Y){
    RVs <- c(is.RV(X), is.RV(Y))
    both <- all(RVs)
    if(!both){
        if(RVs[1]){
            p <- probs(X)
            a <- values(X)
            b <- Y
        }else{
            p <- probs(Y)
            a <- X
            b <- values(Y)
        }
        out <- setNames(a > b, p)
        return(out)
    }else{
        out <- addRV(X, -1*Y)
        return(out > 0)
    }
}

#' @method >= RV
#' @S3method >= RV
#' @rdname RVcomparisons
#' @export
`>=.RV` <- function(X, Y){
    RVs <- c(is.RV(X), is.RV(Y))
    both <- all(RVs)
    if(!both){
        if(RVs[1]){
            p <- probs(X)
            a <- values(X)
            b <- Y
        }else{
            p <- probs(Y)
            a <- X
            b <- values(Y)
        }
        out <- setNames(a >= b, p)
        return(out)
    }else{
        out <- addRV(X, -1*Y)
        return(out >= 0)
    }
}

#' @method < RV
#' @S3method < RV
#' @rdname RVcomparisons
#' @export
`<.RV` <- function(X, Y){
    RVs <- c(is.RV(X), is.RV(Y))
    both <- all(RVs)
    if(!both){
        if(RVs[1]){
            p <- probs(X)
            a <- values(X)
            b <- Y
        }else{
            p <- probs(Y)
            a <- X
            b <- values(Y)
        }
        out <- setNames(a < b, p)
        return(out)
    }else{
        out <- addRV(X, -1*Y)
        return(out < 0)
    }
    
}

#' @method <= RV
#' @S3method <= RV
#' @rdname RVcomparisons
#' @export
`<=.RV` <- function(X, Y){
    RVs <- c(is.RV(X), is.RV(Y))
    both <- all(RVs)
    if(!both){
        if(RVs[1]){
            p <- probs(X)
            a <- values(X)
            b <- Y
        }else{
            p <- probs(Y)
            a <- X
            b <- values(Y)
        }
        out <- setNames(a <= b, p)
        return(out)
    }else{
        out <- addRV(X, -1*Y)
        return(out <= 0)
    }
}

#' @method == RV
#' @S3method == RV
#' @rdname RVcomparisons
#' @export
`==.RV` <- function(X, Y){
    RVs <- c(is.RV(X), is.RV(Y))
    both <- all(RVs)
    if(!both){
        if(RVs[1]){
            p <- probs(X)
            a <- values(X)
            b <- Y
        }else{
            p <- probs(Y)
            a <- X
            b <- values(Y)
        }
        out <- setNames(a == b, p)
        return(out)
    }else{
        out <- addRV(X, -1*Y)
        return(out == 0)
    }
}

#' @method != RV
#' @S3method != RV
#' @rdname RVcomparisons
#' @export
`!=.RV` <- function(X, Y){
    RVs <- c(is.RV(X), is.RV(Y))
    both <- all(RVs)
    if(!both){
        if(RVs[1]){
            p <- probs(X)
            a <- values(X)
            b <- Y
        }else{
            p <- probs(Y)
            a <- X
            b <- values(Y)
        }
        out <- setNames(a != b, p)
        return(out)
    }else{
        out <- addRV(X, -1*Y)
        return(out != 0)
    }
}

#' Variable addition
#' 
#' Addition involving a random variable
#' 
#' @param X The first item to compare
#' @param Y The second item to compare
#' 
#' @method + RV
#' @S3method + RV
#' @rdname additionRV
#' @export
`+.RV` <- function(X, Y){
    RVs <- c(is.RV(X), is.RV(Y))
    both <- all(RVs)
    if(!both){
        if(RVs[1]){
            p <- probs(X)
            a <- values(X)
            b <- Y
        }else{
            p <- probs(Y)
            a <- X
            b <- values(Y)
        }
        out <- as.RV(setNames(a + b, p))
        return(out)
    }else{
        out <- addRV(X, Y)
        return(out)
    }
}


#' @method - RV
#' @S3method - RV
#' @rdname additionRV
#' @export
`-.RV` <- function(X, Y){
    RVs <- c(is.RV(X), is.RV(Y))
    both <- all(RVs)
    if(!both){
        if(RVs[1]){
            p <- probs(X)
            a <- values(X)
            b <- Y
        }else{
            p <- probs(Y)
            a <- X
            b <- values(Y)
        }
        out <- as.RV(setNames(a - b, p))
        return(out)
    }else{
        out <- addRV(X, -1*Y)
        return(out)
    }
}