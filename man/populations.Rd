\name{populations}
\alias{populations}
\title{List populations}
\description{Lists populations as a data frame}
\usage{
populations(sess)
}
\arguments{
  \item{sess}{an \code{edp_session} object from \code{edp_session()}}
}

\value{
  a data frame with column names "name", "id", "nmodels", and "creation_date"
}

\seealso{
  \code{\link{edp_session}},
  \code{\link{population}}
}
