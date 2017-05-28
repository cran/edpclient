\name{popmods}
\alias{popmods}
\title{List population models}
\description{Lists population models as a data frame}
\usage{
popmods(x)
}
\arguments{
  \item{x}{an \code{edp_session} object from \code{edp_session()} or a
      \code{population} object from \code{population()}}
}

\value{
  a data frame with column names "name", "id", "parent_id", "creation_date",
  and "build_status"
}

\seealso{
  \code{\link{edp_session}},
  \code{\link{population}}
}
