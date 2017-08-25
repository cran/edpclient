\name{popmods}
\alias{popmods}
\title{List population models}
\description{Lists population models from a population as a data frame}
\usage{
popmods(pop)
}
\arguments{
  \item{pop}{a \code{population} object from \code{population()}}
}

\value{
  a data frame with column names "name", "id", "creation_date",
  and "build_status"
}

\seealso{
  \code{\link{edp_session}},
  \code{\link{population}}
}
