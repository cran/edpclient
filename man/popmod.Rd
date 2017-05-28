\name{popmod}
\alias{popmod}
\alias{is.popmod}
\title{EDP population model objects}
\description{Create or test for \code{popmod} objects.}
\usage{
popmod(sess, pmid)
is.popmod(x)
}
\arguments{
  \item{sess}{an \code{edp_session} object from \code{edp_session()}}
  \item{pmid}{a population model id, which begins with "pm-"}
  \item{x}{object to be tested}
}
\value{
  \code{popmod} creates an EDP population model object. \code{is.popmod}
  returns a logical indicating whether its argument came from \code{popmod}.
}
