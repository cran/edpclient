\name{latest_popmod}
\alias{latest_popmod}
\title{Return latest population model}
\description{Returns the latest population model for a population}
\usage{
latest_popmod(pop)
}
\arguments{
  \item{pop}{a population object from \code{\link{population}}}
}

\value{
  a population model object, as you would get from \code{\link{popmod}},
  representing the most-recently-built model in \code{pop}
}

\seealso{
  \code{\link{popmods}}
}
