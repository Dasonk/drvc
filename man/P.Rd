\name{P}
\alias{P}
\title{Probability of event}
\usage{
  P(event)
}
\arguments{
  \item{event}{The outcome of some comparison acting on a
  random variable}
}
\description{
  Compute the probability of an event for a random variable
}
\examples{
fair.die <- make.RV(1:6, rep(1/6, 6))
P(fair.die == 1)
P(fair.die > 3)
P(fair.die^2 == 16)
}

