\name{SofIID}
\alias{SofIID}
\title{Sum of iid RVs}
\usage{
  SofIID(X, n = 2)
}
\arguments{
  \item{X}{A random variable that has the distribution of
  interest}

  \item{n}{The number of Xs to sum}
}
\description{
  Create a random variable that is the sum of independent
  and identically distributed random variables.
}
\examples{
fair.die <- make.RV(1:6, rep(1/6, 6))
SofIID(fair.die, n = 3)
}

