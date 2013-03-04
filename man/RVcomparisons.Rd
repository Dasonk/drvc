\name{>.RV}
\alias{!=.RV}
\alias{<.RV}
\alias{<=.RV}
\alias{==.RV}
\alias{>.RV}
\alias{>=.RV}
\title{Comparisons}
\usage{
  \method{>}{RV} (X, Y)

  \method{>=}{RV} (X, Y)

  \method{<}{RV} (X, Y)

  \method{<=}{RV} (X, Y)

  \method{==}{RV} (X, Y)

  \method{!=}{RV} (X, Y)
}
\arguments{
  \item{X}{The first item to compare}

  \item{Y}{The second item to compare}
}
\description{
  Comparing random variables
}
\examples{
X <- make.RV(0:1, .5)
Y <- make.RV(0:1, c(.2, .8))
P(X > Y)
P(X >= Y)
P(X < Y)
P(X <= Y)
P(X == Y)
P(X != Y)
P(X == 1)
P(X - Y == 0)
}

