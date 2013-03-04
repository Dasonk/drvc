
#' Plot a RV
#' 
#' Plot a random variable of class "RV"
#' 
#' @param X A random variable
#' @param ... Additional arguments to be passed to the "plot" function
#' @param pch Either an integer specifying a symbol or a single character to be used as the default in plotting points.
#' @param cex A numerical value giving the amount by which plotting text and symbols should be magnified relative to the default.
#' @param lwd The line width, a positive number, defaulting to 2.
#' @param col A specification for the default plotting color
#' @param stretch.x A numeric by which to extend the x axis limits
#' @param stretch.y A numeric by which to extend the y axis limits
#' @param xlab Label for the X axis
#' @param ylab Label for the Y axis
#' @param xlim Lower and upper limit for the x axis
#' @param ylim Lower and upper limit for the y axis
#' @param expected Logical: If true plots a dashed vertical red line at
#' the expected value
#' @method plot RV
#' @S3method plot RV
#' @export
#' @examples
#' fair.die <- make.RV(1:6, rep(1/6, 6))
#' plot(fair.die)
plot.RV <- function(X, pch = 16, cex = 1.2, lwd = 2, col="black",
                    stretch.x = 1.2, stretch.y = 1.2,
                    xlab = "x",
                    ylab = "P(X = x)", 
                    xlim = NULL,
                    ylim = NULL, 
                    expected = TRUE,
                    ...){
    
    x <- as.numeric(X)
    ps <- probs(X)
    
    if(is.null(xlim)){
        xlim <- mean(range(X)) + (range(X) - mean(range(X))) * stretch.x
    }
    
    if(is.null(ylim)){
        ylim <- c(0, max(probs(X)) * stretch.y)
    }
    
    
    plot(x, ps, 
         type = "h", 
         lwd = lwd, 
         col = col, 
         xlab = xlab, 
         ylab = ylab, 
         xlim = xlim, 
         ylim = ylim,
         ...)
    
    abline(h = 0, col="gray")
    
    if(expected){
        abline(v = E(X), lty = 2, col = "red")
    }
    
    points(x, ps, pch = pch, cex = cex, col = col)
}