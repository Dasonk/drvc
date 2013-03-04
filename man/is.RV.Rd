\name{is.RV}
\alias{is.RV}
\title{Check if input is RV}
\usage{
  is.RV(X)
}
\arguments{
  \item{X}{Object to check if it is a random variable}
}
\description{
  Function to check if an object is a random variable
}
\examples{
coin <- make.RV(0:1)
is.RV(coin)
is.RV(0:1)
}

