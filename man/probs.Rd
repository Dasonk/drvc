\name{probs}
\alias{probs}
\title{Get probabilities}
\usage{
  probs(X)
}
\arguments{
  \item{X}{A random variable}
}
\description{
  Get the probabilities from a random variable
}
\examples{
coin <- make.RV(0:1, .5)
probs(coin)
}

