\name{Kurt}
\alias{Kurt}
\title{Kurtosis}
\usage{
  Kurt(X)
}
\arguments{
  \item{X}{random variable}
}
\description{
  Kurtosis of a random variable
}
\examples{
fair.die <- make.RV(1:6, rep(1/6, 6))
Kurt(fair.die)
}

