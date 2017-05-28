\name{population}
\alias{population}
\alias{is.population}
\title{EDP population objects}
\description{Create or test for \code{population} objects.}
\usage{
population(sess, pid)
is.population(x)
}
\arguments{
  \item{sess}{an \code{edp_session} from \code{edp_session(...)}}
  \item{pid}{a population id, a length-one character vector starting with "p-"}
  \item{x}{object to be tested}
}

\value{
\code{population} creates an EDP population object. \code{is.population}
returns a logical indicating whether the argument came from \code{population}.
}

\seealso{
  \code{\link{edp_session}}, \code{\link{populations}}
}

\examples{
  \dontrun{p <- population(sess, "p-abcde0123456789")}
  \dontrun{is.population(p)  # TRUE}
}
