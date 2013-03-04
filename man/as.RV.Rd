\name{as.RV}
\alias{as.RV}
\title{Coerce into RV}
\usage{
  as.RV(X)
}
\arguments{
  \item{X}{The object to coerce into a RV}
}
\description{
  Coerces something that is a vector with names into a RV
}
\examples{
obj <- 1:6
names(obj) <- rep(1/6, 6)
as.RV(obj)
}

