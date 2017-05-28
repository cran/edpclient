\name{identifying}
\alias{identifying}
\alias{identifying<-}
\alias{identifying_columns}
\alias{identifying_columns<-}
\title{Identifying Columns}
\description{Get or set identifying columns on a data frame}
\usage{
identifying(col)
identifying(col) <- value
identifying_columns(data)
identifying_columns(data) <- value
}
\arguments{
  \item{col}{a column in a data frame}
  \item{data}{a data frame}
  \item{value}{a single logical in the case of \code{identifying}, or a
      character vector in the case of \code{identifying_columns}}
}

\value{
  \code{identifying} returns a single logical. \code{identifying_columns}
  returns a character vector.
}

\details{
  In EDP, "identifying columns" are columns that uniquely identify a row. When
  preparing a data frame to build a population from, you can mark columns as
  identifying. \code{select} and \code{simulate} return this annotation.
}

\examples{
  d <- data.frame(stringsAsFactors = FALSE, a = c(1, 4), id = c("A", "B"))
  identifying(d$id) <- TRUE
  identifying_columns(d) <- c("id")
}
