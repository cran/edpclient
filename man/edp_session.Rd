\name{edp_session}
\alias{edp_session}
\alias{is.edp_session}
\title{edp_session objects}
\description{Create or test for objects of type \code{edp_session}. The
  \code{edp_session} function is the first entry point you'll have into
  edpclient.}
\usage{
edp_session(profile = "default")
is.edp_session(x)
}
\arguments{
  \item{profile}{A profile name, which is a section name in \code{~/.edp_auth}}
  \item{x}{object to be tested}
}

\value{
\code{edp_session} creates an EDP session object. \code{is.edp_session} returns
a logical indicating whether the argument came from \code{edp_session}.
}

\examples{
  \dontrun{sess <- edp_session()}
  \dontrun{is.edp_session(sess)  # TRUE}
}
