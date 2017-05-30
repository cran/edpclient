\name{visibility}
\alias{visibility}
\alias{add_reader}
\title{Visibility}
\description{Get or set visibility of populations}
\usage{
  visibility(x)
  add_reader(x, reader = NULL, reader_domain = NULL)
}
\arguments{
  \item{x}{a population (from \code{\link{population}}) or a population model
      (from \code{\link{popmod}})}
  \item{reader}{email address of a person to give read access to}
  \item{reader_domain}{email domain of an organization to give read access to}
}
\value{
  Both \code{visibility} and \code{add_reader} return an object with keys
  \code{owner} (a length-one character vector), \code{public} (a length-one
  logical vector), \code{readers} (a character vector of email addresses), and
  \code{reader_domains} (a character vector of email domains).
}
\details{
  \code{visibility} and \code{add_reader} manipulate the permissions of a
  population. They can be applied to population models, but that is only a
  convenience; population models do not have ACLs independent of populations.

  When calling \code{add_reader}, exactly one of \code{reader} and
  \code{reader_domain} must be specified.
}
\examples{
  \dontrun{
    pm <- popmod(sess, "p-0123456789abcdef")
    add_reader(pm, "test@example.com")
    add_reader(pm, reader_domain = "example.com")
    print(visibility(pm))
  }
}
