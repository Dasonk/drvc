\name{values}
\alias{values}
\title{Get values}
\usage{
  values(X)
}
\arguments{
  \item{X}{A random variable}
}
\description{
  Get the possible output values from a random variable
}
\examples{
fair.die <- make.RV(1:6)
values(fair.die)
}

