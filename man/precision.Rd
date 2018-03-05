\name{precision}
\alias{precision}
\alias{precision<-}
\title{Precisions}
\description{Get or set precisions of columns on a data frame}
\usage{
precision(col)
precision(col) <- value
}
\arguments{
  \item{col}{a column in a data frame}
  \item{value}{a two-element numeric vector, or \code{NULL} if no precision is
      set}
}
\value{a two-element numeric vector}
\details{
  In EDP, numeric columns in a population have optional "precisions," which are
  two-element numeric values. Either the first or second must be 1. The other
  must be a positive integer (which may also be one). For example, \code{c(100,
  1)} means the column is measured in units of hundreds, and \code{c(1, 4)}
  means the column is measured in units of 0.25.

  These functions get and set the precision on a data frame column you will
  build a population from; you can also get the precision of a column from
  \code{select} or \code{simulate}.
}
\examples{
  d <- data.frame(x = c(1.0, 2.5, -2.5), y = runif(3))
  precision(d$x) <- c(1, 2)
  px <- precision(d$x)  # returns c(1, 2)
  py <- precision(d$y)  # returns NULL
  \dontshow{
    stopifnot(all.equal(px, c(1, 2)))
    stopifnot(is.null(py))
  }
}
