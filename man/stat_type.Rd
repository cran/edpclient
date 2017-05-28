\name{stat_type}
\alias{stat_type}
\alias{stat_type<-}
\title{Stat Types}
\description{Get or set stat types of columns on a data frame}
\usage{
stat_type(col)
stat_type(col) <- value
}
\arguments{
  \item{col}{a column in a data frame}
  \item{value}{a single-element character vector}
}
\value{a single-element character vector, \code{NA} if no stat type was set}
\details{
  In EDP, columns in a population have "stat types."  Numeric columns are
  usually "realAdditive" and "realMultiplicative". Factors are usually
  "categorical." These functions get and set the stat type on a data frame you
  will build a population from; you can also get the stat type of a column from
  \code{select} or \code{simulate}.
}
\examples{
  d <- data.frame(x = c(1000, NA, 10000))
  stat_type(d$x) <- "realMultiplicative"
  st <- stat_type(d$x)  # returns "realMultiplicative"
  \dontshow{
    stopifnot(st == "realMultiplicative")
  }
}
