\name{plot.RV}
\alias{plot.RV}
\title{Plot a RV}
\usage{
  \method{plot}{RV} (X, pch = 16, cex = 1.2, lwd = 2,
    col = "black", stretch.x = 1.2, stretch.y = 1.2,
    xlab = "x", ylab = "P(X = x)", xlim = NULL,
    ylim = NULL, expected = TRUE, ...)
}
\arguments{
  \item{X}{A random variable}

  \item{...}{Additional arguments to be passed to the
  "plot" function}

  \item{pch}{Either an integer specifying a symbol or a
  single character to be used as the default in plotting
  points.}

  \item{cex}{A numerical value giving the amount by which
  plotting text and symbols should be magnified relative to
  the default.}

  \item{lwd}{The line width, a positive number, defaulting
  to 2.}

  \item{col}{A specification for the default plotting
  color}

  \item{stretch.x}{A numeric by which to extend the x axis
  limits}

  \item{stretch.y}{A numeric by which to extend the y axis
  limits}

  \item{xlab}{Label for the X axis}

  \item{ylab}{Label for the Y axis}

  \item{xlim}{Lower and upper limit for the x axis}

  \item{ylim}{Lower and upper limit for the y axis}

  \item{expected}{Logical: If true plots a dashed vertical
  red line at the expected value}
}
\description{
  Plot a random variable of class "RV"
}
\examples{
fair.die <- make.RV(1:6, rep(1/6, 6))
plot(fair.die)
}

