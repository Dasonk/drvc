\name{make.RV}
\alias{make.RV}
\title{Make a random variable}
\usage{
  make.RV(values, probs = NULL)
}
\arguments{
  \item{values}{The possible values the random variable can
  take}

  \item{probs}{The corresponding probabilities. Defaults to
  uniform for every value if nothing is provided}
}
\description{
  Make a random variable from a set of values and
  probabilities
}
\examples{
fair.die <- make.RV(1:6, rep(1/6, 6))
fair.die
}

