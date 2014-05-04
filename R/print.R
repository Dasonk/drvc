
#' Print random variable
#' 
#' Provide a nice way to view a random variable
#' 
#' @param x A random variable
#' @param digits The number of digits to use when printing
#' @param \ldots Further parameters to pass to print.data.frame
#' 
#' @export
#' @method print RV
#' @S3method print RV
print.RV <- function(x, ..., digits = 4){
    print.data.frame(data.frame(x = values(x), 
                                "P(X=x)" = probs(x), 
                                check.names = FALSE), 
                     row.names = FALSE, 
                     digits = digits,
                     ...)
}
