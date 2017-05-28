\name{wait_for}
\alias{wait_for}
\title{Wait until a population model is built}
\description{Wait until a population model is built, possibly showing progress.}
\usage{
wait_for(pm, quiet = FALSE, seconds = Inf)
}
\arguments{
  \item{pm}{a population model, probably recently created with
      \code{\link{build_popmod}}, but any population model will do}
  \item{quiet}{if \code{TRUE}, no output is printed; if \code{FALSE}, a
      progress bar is shown}
  \item{seconds}{maximum number of seconds to wait; must be the default,
      \code{Inf}, if \code{quiet} is \code{FALSE}}
}
\value{
  \code{pm}, possibly with an updated \code{build_status} attribute
}
\seealso{
  \code{\link{build_popmod}}
}
