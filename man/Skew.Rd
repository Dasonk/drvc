\name{Skew}
\alias{Skew}
\title{Skewness}
\usage{
  Skew(X)
}
\arguments{
  \item{X}{A random variable}
}
\description{
  Skewness of a random variable
}
\examples{
fair.die <- make.RV(1:6, rep(1/6, 6))
Skew(fair.die)
}

