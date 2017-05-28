\name{display_name}
\alias{display_name}
\alias{display_name<-}
\alias{display_names}
\alias{display_names<-}
\title{Display Names}
\description{Get or set display names of columns on a data frame}
\usage{
display_name(col)
display_name(col) <- value
display_names(data)
display_names(data) <- value
}
\arguments{
  \item{col}{a column in a data frame}
  \item{data}{a data frame}
  \item{value}{a character vector}
}

\value{
  \code{display_name} returns a single-element character-vector.
  \code{display_names} returns a character vector with as many elements as the
  data frame has columns. If a column has no display name set, the
  corresponding element will be \code{NA}.
}

\details{
  In EDP, columns in a population can have "display names," which are
  human-readable names, often longer than the regular \code{names}.
  \code{display_name} and \code{display_name<-} get and set these on a single
  column. \code{display_names} and \code{display_names<-} get and set them on
  all columns of a data frame at once. The setters are used when preparing a
  data frame to build a population from. The getters can be applied to results
  from \code{select} and \code{simulate}.
}

\examples{
  d <- data.frame(inc = c(25, NA, 17), age = c(21, 70, 30))
  display_name(d$inc) <- "income (thousands of USD)"
  dn <- display_names(d)  # returns c(inc = "income (thousands of USD)", age = NA)
  \dontshow{
    stopifnot(isTRUE(all.equal(dn, c(inc = "income (thousands of USD)", age = NA))))
  }
}
