\name{SofI}
\alias{SofI}
\title{Sum of independent RVs}
\usage{
  SofI(...)
}
\arguments{
  \item{...}{list of arbitrary many random variables}
}
\description{
  Creates a random variable that is the sum of the input
  random variables.
}
\examples{
fair.coin <- make.RV(0:1, .5)
threecoins <- SofI(fair.coin, fair.coin, fair.coin)
all.equal(probs(threecoins)[1], dbinom(0, 3, .5), check.attributes = FALSE)
}

